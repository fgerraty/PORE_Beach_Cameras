#Import Seal Data
seal_data <- read_csv("data/raw/NPS_IMD_SFAN_Pinniped_ElephantSeal.csv")

#Filter Seal Data    
seal_data_filtered <- seal_data %>%   
  filter(
        # SurveyType == "Full Survey",
         SpeciesCode == "MIAN",
         SiteCode %in% c("DB", "SB", "PRH"),
        # Season %notin% c("Molt 2000", "Molt 2004", "Molt 2005")
        ) %>% 
  select(EventID, StartDate, 
         LocationID, SiteCode, 
         MatureCode, PinnipedCount) %>% 
  group_by(EventID, StartDate, 
          SiteCode, MatureCode) %>% 
  summarise(count = sum(PinnipedCount), .groups = "drop") %>% 
  mutate(MatureCode = case_when(
    MatureCode == "EPUP" ~ "pup",
    MatureCode == "WNR" ~ "weaner",
    .default = "adult"
  )) %>% 
  group_by(EventID, StartDate, 
           SiteCode, MatureCode) %>% 
  summarise(count = sum(count))
  

#Annual Adult Summaries
annual_adult_maximumum <- seal_data_filtered %>%
  filter(MatureCode == "adult") %>% 
  mutate(year = year(StartDate)) %>% 
  filter(year > 1998) %>% 
  group_by(year, SiteCode) %>% 
  summarise(maximum_count = max(count), .groups = "drop")


ggplot(annual_adult_maximumum, aes(x=year, y=maximum_count, fill = SiteCode))+
  geom_bar(stat="identity")+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


annual_pup_maximumum <- seal_data_filtered %>%
  filter(MatureCode == "pup",
         SiteCode %in% c("DB", "SB")) %>% 
  mutate(year = year(StartDate)) %>% 
  filter(year > 1998) %>% 
  group_by(year, SiteCode) %>% 
  summarise(maximum_count = max(count), .groups = "drop")


ggplot(annual_pup_maximumum, aes(x=year, y=maximum_count, fill = SiteCode))+
  geom_bar(stat="identity")+
  labs(x="Year", y="Elephant Seal Pups\n(annual max. count)", fill = "Site")+
  scale_fill_manual(values = c("#ed1c24", "#662d91"), labels = c("Drakes Beach", "South Beach"))+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020, 2025))+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "inside", 
        legend.position.inside = c(.2, .7))


ggsave("output/seal_pup_count.png", 
       width = 8, height = 3, units = "in", dpi = 600)


##### 2002-2025 Seasonality ####

seasonality <- seal_data_filtered %>% 
  mutate(year = year(StartDate),
         yday = yday(StartDate), 
         yweek = isoweek(StartDate), 
         month = month(StartDate)) %>% 
  filter(SiteCode %in% c("SB", "DB"),
         StartDate > "2022-12-01")

seal_seasonality_monthly <- seasonality %>% 
  group_by(SiteCode, year, month, MatureCode) %>% 
  summarise(mean_count = mean(count)) %>% 
  pivot_wider(names_from = MatureCode, 
              values_from = mean_count, 
              values_fill = 0) %>% 
  mutate(non_pup_seals = sum(adult,weaner)) #NOTE: Weaners included in this


ggplot(seal_seasonality_monthly, aes(x=month, y=pup))+
  geom_point()+
  facet_wrap(facets = "SiteCode")

ggplot(seal_seasonality_monthly, aes(x=month, y=non_pup_seals))+
  geom_point()+
  facet_wrap(facets = "SiteCode")



write_csv(seal_seasonality_monthly, "data/processed/seal_seasonality_monthly.csv")


seal_seasonality_weekly <- seasonality %>% 
  group_by(SiteCode, year, yweek, MatureCode) %>% 
  summarise(mean_count = mean(count)) %>% 
  pivot_wider(names_from = MatureCode, 
              values_from = mean_count, 
              values_fill = 0) %>% 
  mutate(non_pup_seals = sum(adult,weaner)) 


#Use linear interpolation to fill in seal values for missing weeks 
seal_seasonality_weekly_filled <- seal_seasonality_weekly %>%
  group_by(SiteCode, year) %>%
  complete(yweek = full_seq(yweek, 1)) %>% #Create rows for missing weeks
  arrange(yweek) %>%
  mutate(
    across(
      c(adult, pup, weaner, non_pup_seals),
      ~ na.approx(.x, x = yweek, na.rm = FALSE) #linear interpolation w/ "zoo" package
    )
  ) %>%
  ungroup()


write_csv(seal_seasonality_weekly, "data/processed/seal_seasonality_weekly.csv")


ggplot(seal_seasonality_weekly_filled, aes(x=yweek, y=pup))+
  geom_point()+
  facet_wrap(facets = "SiteCode")

ggplot(seal_seasonality_weekly_filled, aes(x=yweek, y=adult))+
  geom_point()+
  facet_wrap(facets = "SiteCode")

ggplot(seal_seasonality_weekly_filled, aes(x=yweek, y=non_pup_seals))+
  geom_point()+
  facet_wrap(facets = "SiteCode")


ggplot(seasonality, aes(x=yday, y=count, color=MatureCode))+
  geom_point()+
  facet_wrap(facets = "SiteCode")
  


  
