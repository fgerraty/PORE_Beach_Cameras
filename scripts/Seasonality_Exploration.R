
#####################################
# Seasonality Data Exploration ######
#####################################

independent_mammal_detections <- read_csv("data/processed/independent_mammal_detections.csv")
monthly_sampling_effort <- read_csv("data/processed/monthly_sampling_effort.csv")
weekly_sampling_effort <- read_csv("data/processed/weekly_sampling_effort.csv")


# Monthly detection summary

summarized_detections_monthly <- independent_mammal_detections %>% 
  group_by(placename,common_name,year_month) %>% 
  summarise(n_detections = n(), .groups = "drop") %>% 
  pivot_wider(names_from = common_name, values_from = n_detections, values_fill = 0) %>% 
  pivot_longer(cols = c(3:ncol(.)), names_to ="common_name", values_to = "n_detections") %>% 
  left_join(monthly_sampling_effort, by = join_by(placename, year_month)) %>% 
  mutate(detection_rate = n_detections/sampling_days) %>% 
  separate(year_month, into = c("year", "month"), sep = "-", remove = FALSE)


summarized_detections_monthly_wide <- summarized_detections %>% 
  select(-n_detections, -sampling_days) %>% 
  pivot_wider(names_from = common_name, values_from = detection_rate) %>% 
  clean_names()

# Weekly detection summary

summarized_detections_weekly <- independent_mammal_detections %>% 
  group_by(placename,common_name,year_week) %>% 
  summarise(n_detections = n(), .groups = "drop") %>% 
  pivot_wider(names_from = common_name, values_from = n_detections, values_fill = 0) %>% 
  pivot_longer(cols = c(3:ncol(.)), names_to ="common_name", values_to = "n_detections") %>% 
  left_join(weekly_sampling_effort, by = join_by(placename, year_week)) %>% 
  mutate(detection_rate = n_detections/sampling_days) %>% 
  separate(year_week, into = c("year", "week"), sep = "-", remove = FALSE)


summarized_detections_weekly_wide <- summarized_detections_weekly %>% 
  select(-n_detections, -sampling_days) %>% 
  pivot_wider(names_from = common_name, values_from = detection_rate) %>% 
  clean_names()







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
