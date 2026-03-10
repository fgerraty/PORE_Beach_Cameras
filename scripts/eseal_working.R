#Import Seal Data
seal_data <- read_csv("data/raw/NPS_IMD_SFAN_Pinniped_ElephantSeal.csv")

#Filter Seal Data    
seal_data_filtered <- seal_data %>%   
  filter(SurveyType == "Full Survey",
         SpeciesCode == "MIAN",
         SiteCode %in% c("DB", "SB", "PRH"),
         Season %notin% c("Molt 2000", "Molt 2004", "Molt 2005"), 
         year > 1999) %>% 
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
  filter(MatureCode == "pup") %>% 
  mutate(year = year(StartDate)) %>% 
  filter(year > 1998) %>% 
  group_by(year, SiteCode) %>% 
  summarise(maximum_count = max(count), .groups = "drop")


ggplot(annual_pup_maximumum, aes(x=year, y=maximum_count, fill = SiteCode))+
  geom_bar(stat="identity")+
  theme_custom()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



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


write_csv(seal_seasonality_monthly, "data/processed/seal_seasonality_monthly.csv")


ggplot(seal_seasonality_monthly, aes(x=month, y=pup))+
  geom_point()+
  facet_wrap(facets = "SiteCode")


  
