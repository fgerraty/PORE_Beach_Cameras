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
# PART 1: Data Preparation ######
#################################

mammal_seq <- sequences %>% 
  filter(is_blank==0, # Remove the blanks
         is.na(sequences$species)==FALSE, # Remove classifications which don't have species 
         class=="Mammalia") # Subset to mammals

mammal_seq %>% group_by(common_name) %>% summarize(n())         


#Create "daily camera activity lookup"
daily_lookup <- list()


for(i in 1:nrow(deployments))
  {
  daily_lookup[[i]] <- data.frame(
  "date"= seq(deployments$start_date[i], deployments$end_date[i], by="days"),
  "placename"=deployments$placename[i])
  }

# Merge the lists into a dataframe
row_lookup <- bind_rows(daily_lookup) %>% 
  distinct() #Remove duplicates , e.g. when start and end days are the same for successive deployments

#########################################
# PART 3: Detect independent events #####
#########################################


# Set the "independence" interval in minutes
independent <- 30


#Order the dataframe by deployment code and species
mammal_tmp <- mammal_seq %>%
  arrange(deployment_id, species, start_time) %>% # Order by deployment_id, species, and start_time
  group_by(deployment_id, species) %>% # Group species together
  mutate(duration = int_length(start_time %--% lag(start_time))) # Calculate the gap between successive detections (in seconds)


# Give a random value to all cells
mammal_tmp$event_id <- 9999

# Create a counter
counter <- 1

# Make a unique code that has one more zero than rows in your dataframe  
num_code <- as.numeric(paste0(nrow(mammal_seq),0))         


# Loop through mammal_tmp - if gap is greater than the threshold -> give it a new event ID
for (i in 2:nrow(mammal_tmp)) {
  mammal_tmp$event_id[i-1]  <- paste0("E", str_pad(counter, nchar(num_code), pad = "0"))
  
  if(is.na(mammal_tmp$duration[i]) | abs(mammal_tmp$duration[i]) > (independent * 60))
  {
    counter <- counter + 1
  }
}


# event ID  for the last row
if(mammal_tmp$duration[nrow(mammal_tmp)] < (independent * 60)|
   is.na(mammal_tmp$duration[nrow(mammal_tmp)])){
  mammal_tmp$event_id[nrow(mammal_tmp)] <- mammal_tmp$event_id[nrow(mammal_tmp)-1]
} else{
  counter <- counter + 1
  mammal_tmp$event_id[nrow(mammal_tmp)] <- paste0("E", str_pad(counter, nchar(num_code), pad = "0"))
}



independent_mammal_detections <- mammal_tmp %>% 
  group_by(deployment_id, placename, event_id, class, order, family, genus, species, common_name) %>% 
  summarise(event_start = min(start_time),
         event_end = max(end_time),
         n_sequences = n(),
         group_size = max(group_size),
         .groups = "drop") 


#Compare filtered and unfiltered data
mammal_seq %>% group_by(common_name) %>% summarize(n())       
independent_mammal_detections %>% group_by(common_name) %>% summarize(n())       


#################################
# PART X: Export Clean File #####
#################################

write_csv(independent_mammal_detections, 
          "data/processed/independent_mammal_detections.csv")   
