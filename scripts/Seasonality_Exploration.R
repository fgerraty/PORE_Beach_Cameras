
#####################################
# Seasonality Data Exploration ######
#####################################

#Import data
summarized_detection_rate_monthly <- read_csv("data/processed/summarized_detection_rate_monthly.csv")   
summarized_detection_rate_monthly_wide <- read_csv("data/processed/summarized_detection_rate_monthly_wide.csv")   
summarized_detection_rate_weekly <- read_csv("data/processed/summarized_detection_rate_weekly.csv") %>% 
  mutate(season = if_else(week %in% c(51,52,1:15), "seal", "non-seal"))
summarized_detection_rate_weekly_wide <- read_csv("data/processed/summarized_detection_rate_weekly_wide.csv")   %>% 
  mutate(season = if_else(week %in% c(51,52,1:15), "seal", "non-seal"))





#Explore plotting
selected_mammals <- summarized_detection_rate_monthly %>% 
  filter(common_name %in% c("Coyote", "Bobcat", "Northern Raccoon",
                          #  "Northern Elephant Seal", 
                            
                            "Mule Deer")) %>% 
  group_by(month, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            sd = sd(detection_rate)) 


mammal_seasonality <- ggplot(selected_mammals, aes(x=month, y=mean, fill = common_name))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_x_discrete(labels = month.name)+
  labs(x = "Month", y = "Detection Rate\n(# Detections / Trap Night)",
       fill = "Species")+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Export Plot
ggsave("output/mammal_seasonality.png", mammal_seasonality, 
       width = 7, height = 4.5, units = "in", dpi = 600)



#Plot2
temp <- summarized_detection_rate_monthly %>% 
  filter(common_name %in% c("Coyote", "Human", "Northern Elephant Seal")) %>% 
  group_by(month, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            sd = sd(detection_rate)) 

coyote_human_seal_seasonality <- ggplot(temp, aes(x=month, y=mean, fill = common_name))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 90))

#Export Plot
ggsave("output/coyote_human_seal_seasonality.png", coyote_human_seal_seasonality, 
       width = 7, height = 4.5, units = "in", dpi = 600)



temp <- summarized_detection_rate_weekly %>% 
  filter(common_name %in% c("Coyote", "Bobcat", "Northern Raccoon",
                            "Mule Deer")) %>% 
  group_by(season, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            sd = sd(detection_rate)) 

ggplot(temp, aes(x=common_name, y=mean, fill = season))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 90))

