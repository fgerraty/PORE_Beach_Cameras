##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 02: Data Exploration ############################################
#-------------------------------------------------------------------------

#Import clean data
deployments <- read.csv("data/processed/deployments.csv") %>% 
# Convert columns to date-times 
  mutate(start_date = ymd_hms(start_date),
         end_date = ymd_hms(end_date)) 



sequences <- read.csv("data/processed/sequences.csv") %>% 
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time))

#Basic summary of deployment/sequence stats 
paste(length(unique(deployments$placename)), "locations"); paste(length(unique(deployments$deployment_id)), "deployments");paste(nrow(sequences), "sequences"); paste(nrow(sequences[sequences$is_blank == TRUE,]), "blanks")

# check all check the placenames in images are represented in deployments
# This code returns TRUE if it is and FALSE if it isn't. We can then summarize this with table()
table(unique(sequences$placename) %in% unique(deployments$placename))

# check all the placenames in deployments are represented in the images data
table(unique(deployments$placename)  %in% unique(sequences$placename))



#Diel activity check

diel_activity <- sequences %>% 
  filter(class == "Mammalia") %>% 
  mutate(decimal_hours = hour(start_time)+
           (minute(start_time)/60)+
           (second(start_time)/60))

ggplot(diel_activity, aes(y=common_name, x=decimal_hours))+
  geom_point(size = .5)


#Messing around after this! ####



# Remove observations without animals detected, where we don't know the species, and non-mammals
independent_sequences <- sequences %>% filter(is_blank==0, # Remove the blanks
                          is.na(sequences$species)==FALSE,  # Remove classifications which don't have species
                          class=="Mammalia")       # Subset to mammals

coyotes <- independent_sequences %>% 
  filter(species == "latrans") %>% 
  mutate(month = month(start_time)) %>% 
  group_by(month) %>% 
  summarize(n = n())

blanks <- sequences %>% filter(is_blank==1) %>% 
  mutate(month = month(start_time)) %>% 
  group_by(month) %>% 
  summarize(n = n())

ggplot(coyotes, aes(x=month, y=n))+
  geom_bar(stat = "identity")

ggplot(blanks, aes(x=month, y=n))+
  geom_bar(stat = "identity")

       
       