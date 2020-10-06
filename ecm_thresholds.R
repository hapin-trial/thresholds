#set QA-QC parameters

#sample_duration_thresholds <- c(1440*.90,1440*1.10)
sample_duration_thresholds <- c(1200,1680)# +/- 4 hours

#flow_thresholds <- c(0.25, 0.35)
flow_thresholds <- c(0.2, 0.4)
flow_cutoff_threshold <- (1440-sample_duration_thresholds[1])/1440 #Was 0.1
flow_rolling_sd_threshold <- flow_cutoff_threshold #Was 0.1?
flow_min_threshold <- 0.10 #Cease usage, somewhat redundant and unnecessary
flow_max_threshold <- 0.50	#Cease usage, somewhat redundant and unnecessary

bl_shift_threshold <- 30

neph_neg_threshold <- 20
neph_neg_magnitude_threshold <- -20

inlet_pressure_threshold <- 5

temp_thresholds <- c(0, 50)

rh_threshold <- 85

accel_compliance_threshold <- 0.02 #Compliance flag
window_width <- 20
overall_compliance_threshold <- 0.2 #If more than this fraction of samples is not categorize as being compliant, run is flagged.  Includes nighttime data.
