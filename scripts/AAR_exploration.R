
######################################################################
# Spatiotemporal AARs (Avoidance-Attraction Ratios) Exploration ######
######################################################################

independent_mammal_detections <- read_csv("data/processed/independent_mammal_detections.csv") 


aar_data <- independent_mammal_detections %>% 
  filter(common_name %in% 
           c("Human", "Human-Camera Trapper", "Coyote", 
             "Bobcat","Northern Raccoon", "Mule Deer"))
