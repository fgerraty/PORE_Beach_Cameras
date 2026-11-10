###############################

#Import data
summarized_detection_count_monthly_wide <- read_csv("data/processed/summarized_detection_count_monthly_wide.csv") %>% 
  group_by(placename) %>%
  mutate(site_level_sampling_days = sum(sampling_days)) %>% 
  ungroup() |> 
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
  right_join(summarized_detection_count_monthly_wide) %>% 
  full_join(seal_seasonality_monthly) %>% 
  drop_na(placename) %>% 
  replace_na(list(
    pup = 0,
    non_pup_seals = 0)) %>% 
  select(placename, beach, site_type, year_month, sampling_days, bobcat, coyote, human, domestic_dog,mule_deer, northern_raccoon, pup, non_pup_seals) %>% 
  mutate(placename = factor(placename),
         time_num = as.numeric(factor(year_month, levels = sort(unique(year_month)))),
         times = numFactor(time_num),
         log_sampling_days = log(sampling_days),
         log1p_seal = log1p(non_pup_seals),
         log1p_pup = log1p(pup)) %>% 
  mutate(month_num = month(ym(year_month)),
         season = if_else(month_num %in% c(1,2,3), "seal", "non-seal"),
         season4 = case_when(
           month_num %in% c(1:3) ~ "Winter", 
           month_num %in% c(4:6) ~ "Spring", 
           month_num %in% c(7:9) ~ "Summer", 
           month_num %in% c(10:12) ~ "Fall"),
         season4 = factor(season4, levels = c("Winter", "Spring", "Summer", "Fall")),
         year = year(ym(year_month)),
         coyote_rate = coyote/sampling_days) 


ggpairs(monthly_SEM_data[,c(5:12, 16, 17)], #subset predictor columns at data for ggpairs function
        switch="both")+ #labels on left and bottom of plot
  theme_few()+ #theme
  theme(strip.background = element_rect(fill = "white"), #replace background
        strip.placement = "outside") #facet label on outside of tickmarks



coyote_mod <- glmmTMB(coyote ~  site_type * season +
                        offset(log_sampling_days)+
                        (1 | placename) + 
                        ou(times + 0 | placename),
  family = nbinom2, 
  data = monthly_SEM_data)

coyote_mod2 <- glmmTMB(coyote ~ site_type * season +
                        offset(log_sampling_days)+
                        (1 | placename),
                      family = nbinom2, 
                      data = monthly_SEM_data)

summary(coyote_mod2)


temp <- monthly_SEM_data |> 
  group_by(placename, season, site_type) |> 
  summarise(mean_coyote = mean(coyote/sampling_days),
            se = sd(coyote/sampling_days)/sqrt(n()))

ggplot(temp, aes(x=season, y=mean_coyote, group=placename))+
  geom_point()+
  geom_line()+
  facet_wrap(facets = "site_type")









bobcat_mod <- glmmTMB(bobcat ~ site_type * season + 
                         offset(log_sampling_days)+
                         (1 | placename),
                       family = nbinom2, 
                       data = monthly_SEM_data)
summary(bobcat_mod)


raccoon_mod <- glmmTMB(northern_raccoon ~ site_type * season +
                        offset(log_sampling_days)+
                        (1 | placename),
                      family = nbinom2, 
                      data = monthly_SEM_data)
summary(raccoon_mod)

deer_mod <- glmmTMB(mule_deer ~ site_type * season +
                         offset(log_sampling_days)+
                         (1 | placename),
                       family = nbinom2, 
                       data = monthly_SEM_data)
summary(deer_mod)




monthly_SEM_data |>
  mutate(rate = coyote / sampling_days) |>
  group_by(placename, site_type, season) |>
  summarise(rate = mean(rate), .groups = "drop") |>
  ggplot(aes(season, rate, group = placename, colour = site_type)) +
  geom_line(alpha = 0.6) + geom_point() +
  geom_label(placename)+
  facet_wrap(~ site_type)


site_rates <- monthly_SEM_data |>
  mutate(rate = coyote / sampling_days) |>
  group_by(placename, site_type, season) |>
  summarise(rate = mean(rate), .groups = "drop")


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
            se_deer = sd(mule_deer/sqrt(n())),
            .groups = "drop")

ggplot(monthly_SEM_data, aes(x=human, y=coyote))+
    geom_point()
       
ggplot(temp, aes(x=mean_coyote, y=mean_deer, color=season))+
  geom_point()

ggplot(temp, aes(x=mean_coyote, y=mean_bobcat, color=season))+
  geom_point()

ggplot(temp, aes(x=mean_coyote, y=mean_raccoon, color=season, group=placename))+
  geom_point()+
  geom_line()

ggplot(monthly_SEM_data, aes(x=coyote, y=mule_deer, color=season))+
  geom_point()

ggplot(monthly_SEM_data, aes(x=coyote, y=northern_raccoon, color=season))+
  geom_point()

ggplot(monthly_SEM_data, aes(x=coyote, y=bobcat, color=season))+
  geom_point()



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
