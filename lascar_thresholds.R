#set QA-QC parameters

max_1hr_avg_threshold <- 87 #ppm (uncalibrated)
Daily_avg_threshold <- 6.1
Daily_stdev_threshold <- 10/2
elevated_night_threshold <- 6.1
bl_shift_threshold <- 5
sample_duration_thresholds <- c(1440*.90,1440*100.0) #too long is never an issue as it is truncated.

slope_threshold <- c(.25,2)#Lascar ppm/actual ppm
intercept_threshold <- c(-5,5)
stdev0_threshold <- 2
CoV_threshold <- c(0,0.25)
spanpercent_threshold <- c(-.66,.66)






