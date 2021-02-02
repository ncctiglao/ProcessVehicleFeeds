##################################################
#R program to process SafeTravelPH vehicle feeds
#Developed by Noriel Christopher C. Tiglao
#26 January 2021
##################################################

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dbscan")
install.packages("sp")
install.packages("dplyr")
install.packages("rgdal")
install.packages("geosphere")

#load packages
library(ggplot2)
library(tidyverse)
library(dbscan)
library(sp)
library(dplyr)
library(rgdal)
library(lubridate)
library(geosphere)

options(scipen=999)

######################################
#read vehicle feeds into dataframe
######################################
df<-read.csv("C:\\Users\\User\\Desktop\\safetravelph-vehiclefeeds\\khivs.csv", header=TRUE)
#df<-read.csv("C:\\Users\\User\\Desktop\\safetravelph-vehiclefeeds\\mohaimengani26.csv", header=TRUE)
head(df)

#remove rows with null coordinates
df <- df %>% dplyr::filter(!is.na(Lng) | !is.na(Lat))


#create Datetime column
df$Datetime <- dmy_hms(paste(df$Date,df$Time))


#arrange rows by Datetime
df2 <- df %>%
  mutate(Datetime = ymd_hms(Datetime),
         Hour = as.numeric(hour(Datetime)),
         Min = as.numeric(minute(Datetime)),
         Sec = as.numeric(second(Datetime))) %>%
  arrange(Hour, Min, Sec) 


#add Feedno column
df2 <- df2 %>% mutate(Feedno = 1:n())


#flag rows with non-zero Numpass
#if Numpass is non-zero then Withpass is set to 1, else 2 (to facilitate ordering)
df2 <- df2 %>% mutate(
  Withpass = ifelse(Numpass > 0, 1, 2))


#flag rows with board or alight event
#if Board or Alight is TRUE then Event is set to 1, else 2 (to facilitate ordering)
df2 <- df2 %>% mutate(
  Withevent = ifelse(Board == "TRUE", 1, ifelse(Alight == "TRUE", 1, 2)))


#arrange rows
df3 <- df2 %>%
  group_by(Datetime, Withpass, Withevent) %>%
  filter(row_number(`Datetime`) == 1)%>%
  arrange(Datetime, Withpass, Withevent, Numpass) 


df4 <- df3 %>%
  group_by(Datetime, Withevent) %>%
  filter(row_number(`Datetime`) == 1)


#correct Numpass values
df5 <- df4 %>% add_column(Pass = 0)
df5 <- df4 %>% add_column(Cumpass = 0)

df5 <- df5 %>%
  group_by(Userid) %>%
  mutate(Pass = case_when(
    Board == "TRUE" ~ 1,
    Alight == "TRUE" ~ -1,
    TRUE ~ 0))

df5 <- df5 %>%
  mutate(Cumpass = cumsum(Pass))


######################################
#extract vehicle coordinates
######################################
vehicle_coords <- df5 %>%
  dplyr::transmute(                      # create new columns and drop all the others
    Lng = as.numeric(as.character(Lng)), # make this text column numeric
    Lat = as.numeric(as.character(Lat))
  ) %>% 
  dplyr::rename(Longitude = Lng, Latitude = Lat)  # rename


#remove unwanted columns
vehicle_coords$Userid <- NULL


#extract vehicle data
vehicle_data <- df5 %>% dplyr::select(-Lng, -Lat)

######################################
#create spatial data frame
######################################
vehicle_spdf <- sp::SpatialPointsDataFrame(  # create a SPDF
  coords = vehicle_coords,                   # the vehicle co-ordinates
  data = vehicle_data,                       # the vehicle data
  proj4string = CRS("+init=epsg:4326")       # WGS84 geographic projection
  ) %>% 
  sp::spTransform(CRS("+init=epsg:32651"))   # re-project to UTM Zone 51N


######################################
#perform clustering using HDBSCAN
######################################
cl <- hdbscan(vehicle_coords, minPts = 15)
plot(vehicle_coords, col=cl$cluster+1, pch=20)
plot(cl, gradient = c("yellow", "orange", "red", "blue"))
print(cl$cluster_scores)
head(cl$membership_prob)


#compute distance using Haversine formula
df6 <- df5 %>% add_column(Distance = 0)
df6$Distance[2:nrow(df6)] <- sapply(2:nrow(df6), 
                                  function(x) distm(df5[x-1,c('Lng', 'Lat')], df5[x,c('Lng', 'Lat')], fun = distHaversine))
df6$Distance <- as.numeric(format(df6$Distance, decimal.mark = ".", digits = 2))


#compute time difference
df7 <- df6 %>%
  add_column(Timediff = 0) %>% 
  group_by(Userid) %>% 
  mutate(Timediff=difftime(strptime(Datetime, "%Y-%m-%d %H:%M:%S"), strptime(lag(Datetime), "%Y-%m-%d %H:%M:%S")), Timediff=as.numeric(Timediff, units = 'secs'))

#compute segment speed in kph
df8 <- df7 %>%
  add_column(Speed = 0) %>% 
  mutate(Speed = Distance/Timediff * 3.6)

df8$Speed <- as.numeric(format(df8$Speed, decimal.mark = ".", digits = 2))

#replace NA with 0
df8$Speed[is.na(df8$Speed)] <- 0
df8$Timediff[is.na(df8$Timediff)] <- 0

#replace Inf with 0
df8$Speed[is.infinite(df8$Speed)] <- 0


#plot the occupancy
#--cleaned data * needs to be improved
plot(df5$Cumpass~as.POSIXct(df5$Time, format="%H:%M:%S"), type="l",col="blue", xlab="Time", ylab="Passengers")
#--raw data --> alternative
plot(df$Numpass~as.POSIXct(df$Time, format="%H:%M:%S"), type="l",col="blue", xlab="Time", ylab="Passengers")


######################################
#compute metrics
######################################
#--boarding and alighting per hour
Stats_Loading <- df8 %>%
  group_by(Hour) %>%
  summarize(
    n = n(),
    Total_Board = sum(Board=="TRUE", na.rm=T),
    Total_Alight = sum(Alight=="TRUE", na.rm=T)
  )

#--average passengers per hour
Stats_Pass <- df8 %>%
  group_by(Hour) %>%
  summarize(
    n = n(),
    Ave_Numpass = mean(Numpass, na.rm=T)
  )

#--average speed per hour >>>for checking!
Stats_Speed <- df8 %>%
  group_by(Hour) %>%
  summarize(
    n = n(),
    Ave_Speed = mean(Speed)
  )

#--distance traveled per hour in km
Stats_Distance <- df8 %>%
  group_by(Hour) %>%
  summarize(
    n = n(),
    Total_Distance = sum(Distance, na.rm=T) / 1000
  )


#plot the speed
plot(df8$Speed~as.POSIXct(df4$Time, format="%H:%M:%S"), type="l",col="blue", xlab="Time", ylab="Speed")

