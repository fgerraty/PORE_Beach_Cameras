##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 02: Generate Independent Mammal Detection Dataset ###############
#-------------------------------------------------------------------------

############################
# PART 1: Import Data ######
############################

#Import clean deployment data
deployments <- read.csv("data/processed/deployments.csv") %>% 
  # Convert columns to date-times 
  mutate(start_date = ymd_hms(start_date),
         end_date = ymd_hms(end_date)) 

#Import clean sequence data
sequences <- read.csv("data/processed/sequences.csv") %>% 
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time))


#################################
# PART 2: Data Preparation ######
#################################

mammal_seq <- sequences %>% 
  filter(is.na(sequences$species)==FALSE, # Remove classifications which don't have species 
         class=="Mammalia") # Subset to mammals

mammal_seq %>% group_by(common_name) %>% summarize(n())         


# Create "daily camera activity lookup"
daily_lookup <- deployments %>%
  mutate(date = map2(start_date, end_date, seq, by = "days")) %>%
  unnest(date) %>%
  select(date, placename) %>% 
  distinct()

#########################################
# PART 3: Detect independent events #####
#########################################


# Set the "independence" interval 
independent <- 30 * 60  # 30 (minutes) * 60 (seconds)


independent_mammal_detections <- mammal_seq %>%
  
  # Ensure chronological order within each deployment and species
  arrange(deployment_id, species, start_time) %>% 
  group_by(deployment_id, species) %>% # Group by deployment + species
  
  mutate(
    #Compute the time gap (duration) between consecutive observations
    duration = as.numeric(difftime(start_time, lag(start_time), units = "secs")), # Time difference in seconds
    
    #If the duration is NA (first row) or exceeds the threshold, it's a new event
    new_event = is.na(duration) | duration > independent) %>% 
  
  ungroup() %>% 
    
  # Assign unique event ID
  mutate(
    event_id = cumsum(new_event),  # Assigns a unique increasing number
    event_id = str_pad(event_id, width = nchar(max(event_id)), pad = "0"),  # Add leading zeros
    event_id = paste0("E", event_id)  # Prefix with "E"
  ) %>%    
    
    
  #Group by event ID and other relevant columns
  group_by(deployment_id, placename, event_id, class, order, family, genus, species, common_name) %>%
  
  #Summarize event information
  summarise(
    event_start = min(start_time), # Earliest timestamp in the event
    event_end = max(end_time), # Latest timestamp in the event
    n_sequences = n(), # Number of sequences in the event
    group_size = max(group_size), # Maximum observed group size
    .groups = "drop"
  ) %>% 
  
  #Append month and year to detection
  rowwise() %>% mutate(
    year_month = format(event_start, "%Y-%m"),
    year_week = paste0(isoyear(event_start), "-", sprintf("%02d", isoweek(event_start))))


#Compare filtered and unfiltered data
mammal_seq %>% group_by(common_name) %>% summarize(n())       
independent_mammal_detections %>% group_by(common_name) %>% summarize(n())       


#################################
# PART 4: Export Clean File #####
#################################

write_csv(independent_mammal_detections, 
          "data/processed/independent_mammal_detections.csv")   
