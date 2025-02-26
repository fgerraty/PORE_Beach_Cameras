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
  filter(is_blank==0,                # Remove the blanks
         is.na(sequences$species)==FALSE,  # Remove classifications which don't have species 
         class=="Mammalia")          # Subset to mammals

mammal_seq %>% group_by(common_name) %>% summarize(n())         
