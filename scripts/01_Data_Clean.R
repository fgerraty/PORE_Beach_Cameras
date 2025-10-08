##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Clean and Summarise Datasets ################################
#-------------------------------------------------------------------------

#################################
# PART 1: Import Raw Data #######
#################################

#PORE Beach Camera Project Data
PORE_deployments <- read_csv("data/raw/PORE_deployments.csv") 
PORE_sequences <- read_csv("data/raw/PORE_sequences.csv",
                           col_types = cols(
                             sex = col_character(),
                             age = col_character()
                           )) 

#Snapshot USA 2023 Data
SnapshotUSA_2023_Deployments <- read_csv("data/raw/SnapshotUSA_2023_Deployments.csv") %>%
  mutate(deployment_id = paste0(deployment_id, "_2023")) 
SnapshotUSA_2023_Sequences <- read_csv("data/raw/SnapshotUSA_2023_Sequences.csv",
                                       col_types = cols(
                                         sex = col_character(),
                                         age = col_character())) %>% 
  mutate(deployment_id = paste0(deployment_id, "_2023"))


#Snapshot USA 2024 Data (be sure to make deployment names unique between years)
SnapshotUSA_2024_Deployments <- read_csv("data/raw/SnapshotUSA_2024_Deployments.csv") %>%
  mutate(deployment_id = paste0(deployment_id, "_2024"),
         start_date = mdy_hm(start_date),
         end_date = mdy_hm(end_date))
SnapshotUSA_2024_Sequences <- read_csv("data/raw/SnapshotUSA_2024_Sequences.csv") %>% 
  mutate(deployment_id = paste0(deployment_id, "_2024"))

###############################################
# PART 2: Clean and Merge Camera Trap Data ####
###############################################

deployments <- bind_rows(PORE_deployments, 
                         SnapshotUSA_2023_Deployments, 
                         SnapshotUSA_2024_Deployments) %>% 
  
  #Calculate deployment length (in # days/nights)
  mutate(deployment_length = floor(interval(start_date, end_date)/days(1))) %>% 
  
  #Correct place names from SNAPSHOT USA Projects
  mutate(placename = case_when(
    placename == "CA_Beach_PointReyes_Loc1" ~ "BCAM14",
    placename == "CA_Beach_PointReyes_Loc01" ~ "BCAM14",
    placename == "CA_Beach_PointReyes_Loc2" ~ "BCAM9",
    placename == "CA_Beach_PointReyes_Loc02" ~ "BCAM9",
    #  placename == "CA_Beach_PointReyes_Loc3" ~ "SNAP3",  #No BCAM name for SNAP3 
    placename == "CA_Beach_PointReyes_Loc4" ~ "BCAM3",
    placename == "CA_Beach_PointReyes_Loc04" ~ "BCAM3",
    placename == "CA_Beach_PointReyes_Loc5" ~ "BCAM26",
    placename == "CA_Beach_PointReyes_Loc05" ~ "BCAM26",
    placename == "CA_Beach_PointReyes_Loc6" ~ "BCAM1",
    placename == "CA_Beach_PointReyes_Loc06" ~ "BCAM1",
    placename == "CA_Beach_PointReyes_Loc7" ~ "BCAM4",
    placename == "CA_Beach_PointReyes_Loc07" ~ "BCAM4",
    placename == "CA_Beach_PointReyes_Loc8" ~ "BCAM11",
    placename == "CA_Beach_PointReyes_Loc08" ~ "BCAM11",
    placename == "CA_Beach_PointReyes_Loc9" ~ "BCAM13",
    placename == "CA_Beach_PointReyes_Loc09" ~ "BCAM13",
    placename == "CA_Beach_PointReyes_Loc10" ~ "BCAM7", 
    .default = placename))


sequences <- rbind(PORE_sequences, 
                   SnapshotUSA_2023_Sequences,
                   SnapshotUSA_2024_Sequences) %>% 
  
  #Add place names to sequences 
  
  left_join(., deployments[,c("deployment_id", "placename")]) %>% 
  
  
  #Correct place names from SNAPSHOT USA Projects
  mutate(placename = case_when(
    placename == "CA_Beach_PointReyes_Loc1" ~ "BCAM14",
    placename == "CA_Beach_PointReyes_Loc01" ~ "BCAM14",
    placename == "CA_Beach_PointReyes_Loc2" ~ "BCAM9",
    placename == "CA_Beach_PointReyes_Loc02" ~ "BCAM9",
    #  placename == "CA_Beach_PointReyes_Loc3" ~ "SNAP3",  #No BCAM name for SNAP3 
    placename == "CA_Beach_PointReyes_Loc4" ~ "BCAM3",
    placename == "CA_Beach_PointReyes_Loc04" ~ "BCAM3",
    placename == "CA_Beach_PointReyes_Loc5" ~ "BCAM26",
    placename == "CA_Beach_PointReyes_Loc05" ~ "BCAM26",
    placename == "CA_Beach_PointReyes_Loc6" ~ "BCAM1",
    placename == "CA_Beach_PointReyes_Loc06" ~ "BCAM1",
    placename == "CA_Beach_PointReyes_Loc7" ~ "BCAM4",
    placename == "CA_Beach_PointReyes_Loc07" ~ "BCAM4",
    placename == "CA_Beach_PointReyes_Loc8" ~ "BCAM11",
    placename == "CA_Beach_PointReyes_Loc08" ~ "BCAM11",
    placename == "CA_Beach_PointReyes_Loc9" ~ "BCAM13",
    placename == "CA_Beach_PointReyes_Loc09" ~ "BCAM13",
    placename == "CA_Beach_PointReyes_Loc10" ~ "BCAM7", 
    .default = placename),
    
    #Ensure all blank photos have a value of 1 in the is_blank column
    is_blank = if_else(common_name == "Blank", 1,0))


######################################
# PART 3: Export Clean, Raw Files ####
######################################

write_csv(deployments, "data/processed/deployments.csv")
write_csv(sequences, "data/processed/sequences.csv")

