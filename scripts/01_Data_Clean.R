##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Clean and Summarise Datasets ################################
#-------------------------------------------------------------------------

#Import Raw Data --------------------------------------------------------------

#PORE Beach Camera Project Data
PORE_deployments <- read_csv("data/raw/PORE_deployments.csv") 
PORE_sequences <- read_csv("data/raw/PORE_sequences.csv") 

#Snapshot USA 2023 Data
SnapshotUSA_2023_Deployments <- read_csv("data/raw/SnapshotUSA_2023_Deployments.csv") %>%
  mutate(deployment_id = paste0(deployment_id, "_2023"))
SnapshotUSA_2023_Sequences <- read_csv("data/raw/SnapshotUSA_2023_Sequences.csv") %>% 
  mutate(deployment_id = paste0(deployment_id, "_2023"))


# Data Clean -----------------------------------------------------------------

#Make SNAPSHOT USA deployment names unique between years

deployments <- bind_rows(PORE_deployments, SnapshotUSA_2023_Deployments) %>% 
  #Calculate deployment length (in # days/nights)
  mutate(deployment_length = floor(interval(start_date, end_date)/days(1)))

sequences <- rbind(PORE_sequences, SnapshotUSA_2023_Sequences)


# Export clean files -----------------------------------------------------------------

write_csv(deployments, "data/processed/deployments.csv")
write_csv(sequences, "data/processed/sequences.csv")

