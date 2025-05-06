
#####################################
# Seasonality Data Exploration ######
#####################################


#Import data
summarized_detections_monthly <- read_csv("data/processed/summarized_detections_monthly.csv")   
summarized_detections_monthly_wide <- read_csv("data/processed/summarized_detections_monthly_wide.csv")   
summarized_detections_weekly <- read_csv("data/processed/summarized_detections_weekly.csv")   
summarized_detections_weekly_wide <- read_csv("data/processed/summarized_detections_weekly_wide.csv")   





#Explore plotting
temp <- summarized_detections_monthly %>% 
  filter(common_name %in% c("Coyote", "Bobcat", "Northern Raccoon", "Mule Deer")) %>% 
  group_by(month, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            sd = sd(detection_rate)) 


ggplot(temp, aes(x=month, y=mean, fill = common_name))+
  geom_bar(stat = "identity", position = "dodge")


#Plot2
temp <- summarized_detections_weekly %>% 
  filter(common_name %in% c("Coyote", "Human", "Northern Elephant Seal")) %>% 
  group_by(week, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            sd = sd(detection_rate)) 


ggplot(temp, aes(x=week, y=mean, fill = common_name))+
  geom_bar(stat = "identity", position = "dodge")



ggplot(summarized_detections_wide, aes(x = coyote, y = bobcat))+
  geom_point() +
  geom_smooth(method = "glm")
