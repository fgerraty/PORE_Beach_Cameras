
#####################################
# Seasonality Data Exploration ######
#####################################

#Import data
summarized_detection_rate_monthly <- read_csv("data/processed/summarized_detection_rate_monthly.csv")   
summarized_detection_rate_monthly_wide <- read_csv("data/processed/summarized_detection_rate_monthly_wide.csv")   
summarized_detection_rate_weekly <- read_csv("data/processed/summarized_detection_rate_weekly.csv")   
summarized_detection_rate_weekly_wide <- read_csv("data/processed/summarized_detection_rate_weekly_wide.csv")   


#Explore plotting
selected_mammals <- summarized_detection_rate_monthly %>% 
  filter(common_name %in% c("Coyote", "Bobcat", "Northern Raccoon",
                           # "Northern Elephant Seal", 
                            
                            "Mule Deer")) %>% 
  group_by(month, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            sd = sd(detection_rate)) 


ggplot(selected_mammals, aes(x=month, y=mean, fill = common_name))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_x_discrete(labels = month.name)+
  labs(x = "Month", y = "Detection Rate\n(# Detections / Trap Night)",
       fill = "Species")+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plot2
temp <- summarized_detection_rate_weekly %>% 
  filter(common_name %in% c("Coyote", "Human", "Northern Elephant Seal")) %>% 
  group_by(week, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            sd = sd(detection_rate)) 


ggplot(temp, aes(x=week, y=mean, fill = common_name))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 90))


