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

bobcats <- sequences %>% 
  filter(common_name == "Bobcat") %>% 
  mutate(start = floor_date(start_time, unit = "hour")) %>% 
  select(placename, start) %>% 
  mutate(detection = TRUE)

mule_deer <- sequences %>% 
  filter(common_name == "Mule Deer") %>% 
  mutate(start = floor_date(start_time, unit = "hour")) %>% 
  select(placename, start) %>% 
  mutate(detection = TRUE)

raccoons <- sequences %>% 
  filter(common_name == "Raccoon") %>% 
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

bobcat_detections <- sampling_hours %>% 
  left_join(bobcats, by = c("placename", "start")) %>% 
  mutate(detection = replace_na(detection, FALSE)) %>% 
  unique() 

mule_deer_detections <- sampling_hours %>% 
  left_join(mule_deer, by = c("placename", "start")) %>% 
  mutate(detection = replace_na(detection, FALSE)) %>% 
  unique() 

raccoon_detections <- sampling_hours %>% 
  left_join(raccoons, by = c("placename", "start")) %>% 
  mutate(detection = replace_na(detection, FALSE)) %>% 
  unique() 


# Define a function to create the diel summary for a given detection dataframe
summarize_detections <- function(df, species_name) {
  df %>%
    mutate(time = hour(start),
           placename = factor(placename),
           species = species_name) %>%
    group_by(placename, time, species) %>%
    summarize(success = sum(detection),
              failure = n() - success,
              .groups = "drop")
}

# Apply the function to each detection dataframe
diel_detection_summary <- bind_rows(
  summarize_detections(coyote_detections, "coyote"),
  summarize_detections(bobcat_detections, "bobcat"),
  summarize_detections(mule_deer_detections, "mule_deer"),
  summarize_detections(raccoon_detections, "raccoon")
)



# run model
trig_rand_int <- mixed_model(fixed = cbind(success, failure) ~ 
                               cos(2 * pi * time/24)  + sin(2 * pi * time/24) +
                               cos(2 * pi * time/12)  + sin(2 * pi * time/12),
                             random = ~  1  |   placename, 
                             data = filter(diel_detection_summary, species == "coyote"), 
                             family = binomial(), 
                             iter_EM = 0
)
summary(trig_rand_int)

