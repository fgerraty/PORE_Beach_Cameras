
#####################################
# Seasonality Data Exploration ######
#####################################

#Import data

PORE_sites <- read_csv("data/raw/PORE_sites.csv") 
summarized_detection_rate_monthly <- read_csv("data/processed/summarized_detection_rate_monthly.csv") |> 
  mutate(season = case_when(
    month %in% c("01","02","03") ~ "Winter (Jan-Mar)", 
    month %in% c("04","05","06") ~ "Spring (Apr-Jun)", 
    month %in% c("07","08","09") ~ "Summer (Jul-Sep)", 
    month %in% c(10:12) ~ "Fall (Oct-Dec)"))
summarized_detection_rate_monthly_wide <- read_csv("data/processed/summarized_detection_rate_monthly_wide.csv")   

#Coyote and bobcat seasonality plot

defecator_cols <- c("darkgreen", "#F27F0C")

strip_cols <- strip_themed(
  background_x = elem_list_rect(fill = defecator_cols),
  text_x       = elem_list_text(colour = "white", face = "bold"))

custom_labels <- c(
  "Bobcat" = "Bobcat (n = 461 independent detections)",
  "Coyote" = "Coyote (n = 3650 independent detections)")

season_levels <- c("Winter (Jan-Mar)", "Spring (Apr-Jun)",
                   "Summer (Jul-Sep)", "Fall (Oct-Dec)")




plot_df <- summarized_detection_rate_monthly %>% 
  left_join(PORE_sites) %>% 
  filter(common_name %in% c("Coyote", "Bobcat")) %>% 
  group_by(season, common_name, site_type) %>% 
  summarise(mean = mean(detection_rate), 
            se = sd(detection_rate)/sqrt(n()),
            .groups = "drop") %>% 
  mutate(common_name = factor(common_name, levels = c("Coyote", "Bobcat")),
         season = factor(season, levels = season_levels),
         site_type = factor(site_type, levels = c("seal", "non-seal")))


coyote_bobcat_activity_season <- ggplot(plot_df, aes(x=season, y=mean, fill = site_type))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_linerange(aes(ymin = mean-se, ymax = mean+se), 
                 position = position_dodge(width = .9),
                 color = "grey40")+
  facet_wrap(facets = "common_name", ncol=1, scales = "free_y")+
  scale_fill_manual(labels = c("Rookery", "Non-rookery"), 
                    values = c("#e89a50","#608cf7"))+
  scale_x_discrete(labels = c("Winter\n(Jan-Mar)", "Spring\n(Apr-Jun)",
                              "Summer\n(Jul-Sep)", "Fall\n(Oct-Dec)")) +
  labs(x = "", y = "Detection Rate\n(# detections / day)",
       fill = "Site Type")+
  theme_custom()+
  theme(strip.text = element_text(face = "bold", size = 12),)

ggsave("output/extra_plots/coyote_bobcat_activity_season.png", coyote_bobcat_activity_season, 
       width=8, height=6, units="in", dpi=600)



#Explore plotting
selected_mammals <- summarized_detection_rate_monthly %>% 
  left_join(PORE_sites) %>% 
  filter(common_name %in% c("Coyote", "Bobcat", "Northern Raccoon","Mule Deer"),
         site_type == "seal") %>% 
  group_by(month, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            se = sd(detection_rate)/sqrt(n())) %>% 
  mutate(common_name = factor(common_name, levels = c("Coyote", "Bobcat", "Northern Raccoon",
                                                      "Mule Deer")))


mammal_seasonality2 <- ggplot(selected_mammals, aes(x=month, y=mean, fill = common_name))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_linerange(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(width = .9),
                 color = "grey70")+
  scale_x_discrete(labels = month.name)+
  labs(x = "Month", y = "Detection Rate\n(# Detections / Trap Night)",
       fill = "Species")+
  scale_fill_manual(values = c("#FFCB47", "#8AA1B1", "#9AC2C9", "#B9D8C2"))+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "inside",
        legend.position.inside = c(.5,.75))
mammal_seasonality2

#Export Plot
ggsave("output/mammal_seasonality2.png", mammal_seasonality2, 
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

coyote_human_seal_seasonality

#Export Plot
ggsave("output/coyote_human_seal_seasonality.png", coyote_human_seal_seasonality, 
       width = 7, height = 4.5, units = "in", dpi = 600)



temp <- summarized_detection_rate_weekly %>% 
  left_join(PORE_sites) %>% 
  filter(common_name %in% c("Coyote", "Bobcat", "Northern Raccoon","Mule Deer"),
         site_type == "seal") %>% 
  group_by(season, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            se = sd(detection_rate)/sqrt(n())) %>% 
  mutate(common_name = factor(common_name, levels = c("Coyote", "Bobcat", "Northern Raccoon",
                                                      "Mule Deer")),
         season = factor(season, levels = c("seal", "non-seal")))

ggplot(temp, aes(x=common_name, y=mean, fill = season))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_linerange(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(width = .9),
                 color = "grey40")+
  scale_fill_manual(values = c("#608cf7", "#e89a50"), labels = c("Seal", "Non-Seal"))+
  labs(x="", y="Detection Rate\n(# Images / Trap Night)", fill = "Season")+
  theme_custom()+
  theme(legend.position = "inside",
        legend.position.inside = c(.88,.85)) 



#Exploring with monthly trends


temp <- summarized_detection_rate_monthly %>% 
  left_join(PORE_sites) %>% 
  filter(site_type == "seal") %>% 
  mutate(season = if_else(month %in% c("01", "02", "03"), "seal", "non-seal")) %>% 
  filter(common_name %in% c("Coyote", "Bobcat", "Northern Raccoon",
                            "Mule Deer")) %>% 
  group_by(season, common_name) %>% 
  summarise(mean = mean(detection_rate), 
            se = sd(detection_rate)/sqrt(n())) %>% 
  mutate(common_name = factor(common_name, levels = c("Coyote", "Bobcat", "Northern Raccoon",
                                                      "Mule Deer")),
         season = factor(season, levels = c("seal", "non-seal")))

mammal_seasonality <- ggplot(temp, aes(x=common_name, y=mean, fill = season))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_linerange(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(width = .9),
                 color = "grey40")+
  scale_fill_manual(values = c("#608cf7", "#e89a50"), labels = c("Seal", "Non-Seal"))+
  labs(x="", y="Detection Rate\n(# Images / Trap Night)", fill = "Season")+
  theme_custom()+
  theme(legend.position = "inside",
        legend.position.inside = c(.88,.85)) 
mammal_seasonality

#Export Plot
ggsave("output/mammal_seasonality.png", mammal_seasonality, 
       width = 7, height = 4.5, units = "in", dpi = 600)



# Exploring between beaches

across_beaches <- summarized_detection_rate_monthly %>% 
  left_join(PORE_sites, by = join_by(placename)) %>% 
  filter(common_name %in% c("Coyote", "Bobcat", "Northern Raccoon",
                            #  "Northern Elephant Seal", 
                            
                            "Mule Deer")) %>% 
  drop_na(sampling_days) |> 
  group_by(month, common_name, beach) %>% 
  summarise(mean = mean(detection_rate), 
            se = sd(detection_rate)/sqrt(n())) %>% 
  mutate(common_name = factor(common_name, levels = c("Coyote", "Bobcat", "Northern Raccoon",
                                                      "Mule Deer")))

across_beaches_plot <- ggplot(across_beaches, aes(x=month, y=mean, fill = common_name))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_linerange(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(width = .9),
                 color = "grey70")+
  facet_grid(rows = "beach")+
  scale_x_discrete(labels = month.name)+
  labs(x = "Month", y = "Detection Rate\n(# Detections / Trap Night)",
       fill = "Species")+
  scale_fill_manual(values = c("#FFCB47", "#8AA1B1", "#9AC2C9", "#B9D8C2"))+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "inside",
        legend.position.inside = c(.38,.87))
across_beaches_plot

#Export Plot
ggsave("output/multi_beach_seasonality.png", across_beaches_plot, 
       width = 7, height = 7, units = "in", dpi = 600)
