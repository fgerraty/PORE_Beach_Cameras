##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 0X: Describe Wildlife Diel Activity Patterns  ###################
#-------------------------------------------------------------------------

################################################
# Prepare Data for Diel Activity Analyses ######
################################################

#Import clean deployment data
deployments <- read.csv("data/processed/deployments.csv") %>% 
  # Convert columns to date-times 
  mutate(start_date = ymd_hms(start_date),
         end_date = ymd_hms(end_date)) 

#Import clean sequence data
sequences <- read.csv("data/processed/sequences.csv") %>% 
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time))

#Create coyote dataframe
coyotes <- sequences %>% 
  filter(common_name == "Coyote") %>% 
  mutate(start = floor_date(start_time, unit = "hour")) %>% 
  select(placename, start) %>% 
  mutate(detection = TRUE)

#Create dataframe of all sampling hours per site, which we will later populate with detections
sampling_hours <- deployments %>%
  rowwise() %>%
  mutate(data = list(
    tibble(
      placename = placename,
      start = seq(
        ymd_hms(paste(start_date, "00:00:00")),
        ymd_hms(paste(end_date, "23:59:59")),
        by = "60 min"
      )
    ) %>%
      mutate(end = lead(start, default = last(start) + minutes(60)))
  )) %>%
  pull(data) %>%
  bind_rows() %>% 
  unique()

#Append coyote detections to all sampling hours dataset
coyote_detections <- sampling_hours %>% 
  left_join(coyotes, by = c("placename", "start")) %>% 
  mutate(detection = replace_na(detection, FALSE)) %>% 
  unique()
 
