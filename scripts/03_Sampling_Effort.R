##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 03: Quantify Camera Trap Sampling Effort and CPUE Estimates #####
#-------------------------------------------------------------------------

################################################
# Import Data to Quantify Sampling Effort ######
################################################

deployments <- read.csv("data/processed/deployments.csv")

####################
# Process Data #####
####################

#Create dataframe for all sampling days, labeled by month and week of year
sampling_days <- deployments %>%
  rowwise() %>%
  mutate(date = list(seq.Date(as.Date(start_date), as.Date(end_date), by = "day"))) %>%
  unnest(date) %>%
  mutate(
    year_month = format(date, "%Y-%m"),
    year_week = paste0(isoyear(date), "-W", sprintf("%02d", isoweek(date))))

# Count sampling days per site per month
monthly_effort <- sampling_days %>%
  select(placename, year_month, date) %>% 
  unique() %>% 
  group_by(placename, year_month) %>%
  summarise(sampling_days = n(), .groups = "drop")

# Count sampling days per site per week
weekly_effort <- sampling_days %>%
  select(placename, year_week, date) %>% 
  unique() %>% 
  group_by(placename, year_week) %>%
  summarise(sampling_days = n(), .groups = "drop")


####################
# Export Data ######
####################

#Export sampling effort datasets
write_csv(monthly_effort, 
          "data/processed/monthly_sampling_effort.csv")   
write_csv(weekly_effort, 
          "data/processed/weekly_sampling_effort.csv")   

#-------------------------------------------------------------------------------


###########################################################
# Import Data to Estimate Detections Per Unit Effort ######
###########################################################

rm(list = ls()) #Clean console

#Import data
independent_mammal_detections <- read_csv("data/processed/independent_mammal_detections.csv")
monthly_sampling_effort <- read_csv("data/processed/monthly_sampling_effort.csv")
weekly_sampling_effort <- read_csv("data/processed/weekly_sampling_effort.csv")

####################
# Process Data #####
####################

# Monthly detection count summary
summarized_detection_count_monthly <- independent_mammal_detections %>% 
  group_by(placename,common_name,year_month) %>% 
  summarise(n_detections = n(), .groups = "drop") %>% 
  pivot_wider(names_from = common_name, values_from = n_detections, values_fill = 0) %>% 
  pivot_longer(cols = c(3:ncol(.)), names_to ="common_name", values_to = "n_detections") %>% 
  left_join(monthly_sampling_effort, by = join_by(placename, year_month))

summarized_detection_count_monthly_wide <- summarized_detection_count_monthly %>% 
  pivot_wider(names_from = common_name, values_from = n_detections) %>% 
  clean_names()

# Weekly detection count summary
summarized_detection_count_weekly <- independent_mammal_detections %>% 
  group_by(placename,common_name,year_week) %>% 
  summarise(n_detections = n(), .groups = "drop") %>% 
  pivot_wider(names_from = common_name, values_from = n_detections, values_fill = 0) %>% 
  pivot_longer(cols = c(3:ncol(.)), names_to ="common_name", values_to = "n_detections") %>% 
  left_join(weekly_sampling_effort, by = join_by(placename, year_week))

summarized_detection_count_weekly_wide <- summarized_detection_count_weekly %>% 
  pivot_wider(names_from = common_name, values_from = n_detections) %>% 
  clean_names()

# Monthly detection rate summary

summarized_detection_rate_monthly <- independent_mammal_detections %>% 
  group_by(placename,common_name,year_month) %>% 
  summarise(n_detections = n(), .groups = "drop") %>% 
  pivot_wider(names_from = common_name, values_from = n_detections, values_fill = 0) %>% 
  pivot_longer(cols = c(3:ncol(.)), names_to ="common_name", values_to = "n_detections") %>% 
  left_join(monthly_sampling_effort, by = join_by(placename, year_month)) %>% 
  mutate(detection_rate = n_detections/sampling_days) %>% 
  separate(year_month, into = c("year", "month"), sep = "-", remove = FALSE)

summarized_detection_rate_monthly_wide <- summarized_detection_rate_monthly %>% 
  select(-n_detections, -sampling_days) %>% 
  pivot_wider(names_from = common_name, values_from = detection_rate) %>% 
  clean_names()

# Weekly detection rate summary

summarized_detection_rate_weekly <- independent_mammal_detections %>% 
  group_by(placename,common_name,year_week) %>% 
  summarise(n_detections = n(), .groups = "drop") %>% 
  pivot_wider(names_from = common_name, values_from = n_detections, values_fill = 0) %>% 
  pivot_longer(cols = c(3:ncol(.)), names_to ="common_name", values_to = "n_detections") %>% 
  left_join(weekly_sampling_effort, by = join_by(placename, year_week)) %>% 
  mutate(detection_rate = n_detections/sampling_days) %>% 
  separate(year_week, into = c("year", "week"), sep = "-", remove = FALSE)

summarized_detection_rate_weekly_wide <- summarized_detection_rate_weekly %>% 
  select(-n_detections, -sampling_days) %>% 
  pivot_wider(names_from = common_name, values_from = detection_rate) %>% 
  clean_names()

####################
# Export Data ######
####################

#Export detection count datasets
write_csv(summarized_detection_count_monthly, 
          "data/processed/summarized_detection_count_monthly.csv")   
write_csv(summarized_detection_count_monthly_wide, 
          "data/processed/summarized_detection_count_monthly_wide.csv")   
write_csv(summarized_detection_count_weekly, 
          "data/processed/summarized_detection_count_weekly.csv")   
write_csv(summarized_detection_count_weekly_wide, 
          "data/processed/summarized_detection_count_weekly_wide.csv")   


#Export detection rate datasets
write_csv(summarized_detection_rate_monthly, 
          "data/processed/summarized_detection_rate_monthly.csv")   
write_csv(summarized_detection_rate_monthly_wide, 
          "data/processed/summarized_detection_rate_monthly_wide.csv")   
write_csv(summarized_detection_rate_weekly, 
          "data/processed/summarized_detection_rate_weekly.csv")   
write_csv(summarized_detection_rate_weekly_wide, 
          "data/processed/summarized_detection_rate_weekly_wide.csv")   

