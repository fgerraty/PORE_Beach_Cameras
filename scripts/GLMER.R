###############################

#Import data
summarized_detection_rate_monthly_wide <- read_csv("data/processed/summarized_detection_rate_monthly_wide.csv") %>% 
  group_by(placename) %>%
  mutate(site_level_sampling_days = sum(sampling_days)) %>% 
  ungroup() %>% 
  filter(site_level_sampling_days > 250) %>% 
  filter(sampling_days > 10)

PORE_sites <- read_csv("data/raw/PORE_sites.csv")

seal_seasonality_monthly <- read_csv("data/processed/seal_seasonality_monthly.csv") %>% 
  rename(seal_sitecode = SiteCode) %>% 
  mutate(month = sprintf("%02d", as.integer(month)),
         year_month = paste(year, 
                            month, 
                            sep = "-")) %>% 
  select(seal_sitecode, year_month, pup, non_pup_seals) 



#Assemble data

monthly_SEM_data <- PORE_sites %>% 
  right_join(summarized_detection_rate_monthly_wide) %>% 
  full_join(seal_seasonality_monthly) %>% 
  drop_na(placename) %>% 
  replace_na(list(
    pup = 0,
    non_pup_seals = 0)) %>% 
  select(placename, year_month, sampling_days, bobcat, coyote, human, domestic_dog,
         mule_deer, northern_raccoon, pup, non_pup_seals) %>% 
  mutate(placename = factor(placename),
         time_num = as.numeric(factor(year_month, levels = sort(unique(year_month)))),
         times = numFactor(time_num),
         log_sampling_days = log(sampling_days),
         log1p_seal = log1p(non_pup_seals),
         log1p_pup = log1p(pup)) %>% 
  mutate(month_num = month(ym(year_month)),
         season = if_else(month_num %in% c(1,2,3), "seal", "non-seal"))


ggpairs(monthly_SEM_data[,c(4:9, 15, 16)], #subset predictor columns at data for ggpairs function
        switch="both")+ #labels on left and bottom of plot
  theme_few()+ #theme
  theme(strip.background = element_rect(fill = "white"), #replace background
        strip.placement = "outside") #facet label on outside of tickmarks


# Individual GLMM Analyses ########### PROBLEMS!!!!!!!!


temp <- monthly_SEM_data %>% 
  mutate(month_num = month(ym(year_month)),
         season = if_else(month_num %in% c(1,2,3), "seal", "non-seal")) %>% 
  group_by(placename, season) %>% 
  summarise(mean_coyote = mean(coyote), 
            se_coyote = sd(coyote/sqrt(n())),
            mean_bobcat = mean(bobcat), 
            se_bobcat = sd(bobcat/sqrt(n())),
            mean_raccoon = mean(northern_raccoon), 
            se_raccoon = sd(northern_raccoon/sqrt(n())),
            mean_deer = mean(mule_deer), 
            se_deer = sd(mule_deer/sqrt(n())))

ggplot(monthly_SEM_data, aes(x=human, y=coyote))+
    geom_point()
       
ggplot(temp, aes(x=mean_coyote, y=mean_deer, color=season))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(monthly_SEM_data, aes(x=coyote, y=northern_raccoon))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(monthly_SEM_data, aes(x=coyote, y=mule_deer))+
  geom_point()+
  geom_smooth(method = "lm")



coyote_mod <- glmmTMB(
  coyote ~ domestic_dog + human +
           (1 | placename) + 
           ou(times + 0 | placename),
           family = tweedie(), 
           data = monthly_SEM_data)

summary(coyote_mod)


# Check assumptions with DHARMa package
coyote_glmer_res = simulateResiduals(coyote_mod)
plot(coyote_glmer_res, rank = T)
testDispersion(coyote_glmer_res)
plotResiduals(coyote_glmer_res, monthly_SEM_data$placename, xlab = "Site", main=NULL)






bobcat_mod <- glmmTMB(
  bobcat ~ coyote*season + human + domestic_dog +
    (1 | placename) + ou(times + 0 | placename),
  family = tweedie(), data = monthly_SEM_data)
bobcat_mod
summary(bobcat_mod)

# Check assumptions with DHARMa package
bobcat_glmer_res = simulateResiduals(bobcat_mod)
plot(bobcat_glmer_res, rank = T)
testDispersion(bobcat_glmer_res)
plotResiduals(bobcat_glmer_res, monthly_SEM_data$placename, xlab = "Site", main=NULL)



raccoon_mod <- glmmTMB(
  northern_raccoon ~ coyote*season + human + domestic_dog +
    (1 | placename) + ou(times + 0 | placename),
  family = tweedie(), data = monthly_SEM_data)
summary(raccoon_mod)

# Check assumptions with DHARMa package
raccoon_glmer_res = simulateResiduals(raccoon_mod)
plot(raccoon_glmer_res, rank = T)
testDispersion(raccoon_glmer_res)
plotResiduals(raccoon_glmer_res, monthly_SEM_data$placename, xlab = "Site", main=NULL)



deer_mod <- glmmTMB(
  mule_deer ~ coyote*season + human + domestic_dog +
    (1 | placename) + ou(times + 0 | placename),
  family = tweedie(), data = monthly_SEM_data)
summary(deer_mod)

# Check assumptions with DHARMa package
deer_glmer_res = simulateResiduals(deer_mod)
plot(deer_glmer_res, rank = T)
testDispersion(deer_glmer_res)
plotResiduals(deer_glmer_res, monthly_SEM_data$placename, xlab = "Site", main=NULL)
