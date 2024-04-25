# Visualize PORE Beach Camera Trap Data from Winter and Fall 2023 ############
# Author: Frankie Gerraty ####################################################
##############################################################################

#Packages ####
library(tidyverse)
library(lubridate)
library(ggthemes)
`%notin%` <- Negate(`%in%`)

#Import Raw Data --------------------------------------------------------------

#Snapshot Data
SnapshotUSA_2023_Deployments <- read_csv("data/raw/SnapshotUSA_2023_Deployments.csv") %>% 
  mutate(period = "Fall_2023")
SnapshotUSA_2023_Sequences <- read_csv("data/raw/SnapshotUSA_2023_Sequences.csv") %>% 
  mutate(period = "Fall_2023") 

#PORE Beach Camera Project Data
PORE_deployments <- read_csv("data/raw/PORE_deployments.csv") %>% 
  mutate(period = "Winter_2023")
PORE_sequences <- read_csv("data/raw/PORE_sequences.csv") %>% 
  mutate(period = "Winter_2023")




# Data Clean ---------------------------------------------------------------

deployments <- bind_rows(SnapshotUSA_2023_Deployments, PORE_deployments) %>% 
  #Calculate deployment length (in # days/nights)
  mutate(deployment_length = floor(interval(start_date, end_date)/days(1)))

sequences <- rbind(SnapshotUSA_2023_Sequences, PORE_sequences)


# Calculate Sampling Effort Per Period

effort <- deployments %>% 
  group_by(period) %>% 
  summarize(effort = sum(deployment_length))

# Filter for Mammals 

mammals <- sequences %>% 
  #Remove camera with high raccoon and otter detection rates
  filter(!str_detect(deployment_id, "Loc03")) %>% 
  filter(class == "Mammalia") %>% 
  drop_na(species) %>% 
  #Remove irrelevant species
  filter(common_name %notin% 
           c("Human", "Human-Camera Trapper", 
             "Northern Elephant Seal", 
             "North American River Otter",
             "Domestic Dog",
             "Californian Sea Lion")) %>% 
  #Lump rare animals
  mutate(common_name = if_else(common_name %in% 
  c("American Badger",
    "Brush Rabbit",
    "Striped Skunk"), "Other Mammal", common_name)) %>% 
  group_by(period, common_name) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  add_row(period = "Winter_2023", common_name = "Bobcat", count = 0) %>% 
  #Calculate detection rates using sampling effort dataframe
  mutate(detection_rate = if_else(period == "Fall_2023", count/467, count/405)) %>% 
  #Convert to factor for plotting
  mutate(common_name = factor(common_name, 
                              levels = c("Coyote", "Mule Deer", "Northern Raccoon", "Bobcat", "Other Mammal")),
         period = factor(period, levels = c("Winter_2023", "Fall_2023"))) 
  



mammal_seasonality_plot <- ggplot(mammals, aes(x=common_name, y=detection_rate, fill=period))+
  geom_bar(stat = "identity", position = 'dodge')+
  theme_few()+
  labs(x="", y="Detection Rate\n(# Images / Trap Night)", fill = "Season")+
  scale_fill_manual(values = c(	"#608cf7", "#e89a50"), labels = c("Winter 2023", "Fall 2023"))+
  scale_x_discrete(labels = c("Coyote", "Mule Deer", "Raccoon", "Bobcat", "Other Mammmal"))+
  theme(legend.position = c(.85, .85))
  
ggsave("output/mammal_seasonality_plot.png", mammal_seasonality_plot, width = 7, height = 5, units = "in")
  
coyote_seasonality_plot <- ggplot(filter(mammals, common_name == "Coyote"),
                                  aes(x=common_name, y=detection_rate, fill=period))+
  geom_bar(stat = "identity", position = 'dodge')+
  theme_few()+
  labs(x="", y="Detection Rate\n(# Images / Trap Night)", fill = "Season")+
  scale_fill_manual(values = c(	"#608cf7", "#e89a50"), labels = c("Winter 2023", "Fall 2023"))+
  scale_x_discrete(labels = c("Coyote", "Mule Deer", "Raccoon", "Bobcat", "Other Mammmal"))+
  theme(legend.position = c(.75, .85))


ggsave("output/coyote_seasonality_plot.png", coyote_seasonality_plot, width = 4, height = 5, units = "in")

   