# safetravelph-vehiclefeeds

R script to do the following:

1. Arrange the data points based on time 
2. Remove duplicate rows but retain the one with correct numPass 
3. Perform anomaly detection and correct the trajectory point based on road network using HDDSCAN
4. Prepare clean vehicle tracks in polyline format
5. Extract network performance metrics (speed, +/-accel, load factor, boarding/alighting, delay)
