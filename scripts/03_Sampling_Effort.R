##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 03: Generate Camera Trap Sampling Effort Datasets ###############
#-------------------------------------------------------------------------

####################
# Import Data ######
####################

deployments <- read.csv("data/processed/deployments.csv")

####################
# Process Data #####
####################

#Create dataframe for all sampling days, labeled by month and week of year
sampling_days <- deployments %>%
  rowwise() %>%
  mutate(dates = list(seq.Date(as.Date(start_date), as.Date(end_date), by = "day"))) %>%
  unnest(dates) %>%
  mutate(
    year_month = format(dates, "%Y-%m"),
    year_week = paste0(isoyear(dates), "-W", sprintf("%02d", isoweek(dates))))

# Count sampling days per site per month
monthly_effort <- sampling_days %>%
  group_by(placename, year_month) %>%
  summarise(sampling_days = n(), .groups = "drop")

# Count sampling days per site per week
weekly_effort <- sampling_days %>%
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

