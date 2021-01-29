#set QA-QC parameters

#sample_duration_thresholds <- c(1440*.90,1440*1.10)
sample_duration_thresholds <- c(1200,1680)# +/- 4 hours

#flow_thresholds <- c(0.25, 0.35)
flow_thresholds <- c(0.2, 0.4)
flow_cutoff_threshold <- (1440-sample_duration_thresholds[1])/1440 #Was 0.1
flow_rolling_sd_threshold <- flow_cutoff_threshold #Was 0.2
flow_min_threshold <- 0.10 #Cease usage, somewhat redundant and unnecessary
flow_max_threshold <- 0.50	#Cease usage, somewhat redundant and unnecessary

bl_shift_threshold <- 30

neph_neg_threshold <- 10
neph_neg_magnitude_threshold <- -20
saturation_neph <- 9000
large_negative_threshold <- -50

#Depositions to corroborate with BC data, or photos of filters
large_negative_threshold_mg = -.01 #Values of micrograms (depositions) (10µg).  This value is approx the detection limit (3*sd of blanks)
large_positive_threshold_mg = .500
bc_outlier_threshold_µg = 10 #only excludes grav value if BC deposition is less than this value (unit of µg), and grav value is greater than .5mg.
bc_large_positive_threshold_µg = 100 #Values greater than this will be excluded - there are no values above 56 in bl p1 p2.


inlet_pressure_threshold <- 5

temp_thresholds <- c(0, 50)

rh_threshold <- 85

accel_compliance_threshold <- 0.02 #Compliance flag
window_width <- 20
overall_compliance_threshold <- 0.2 #If more than this fraction of samples is not categorize as being compliant, run is flagged.  Includes nighttime data.


