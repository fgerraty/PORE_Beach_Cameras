
##################################
# Piecewise SEM Exploration ######
##################################

#Import data
summarized_detection_count_weekly_wide <- read_csv("data/processed/summarized_detection_count_weekly_wide.csv") %>% 
  as_tibble() %>% 
  filter(sampling_days == 7) #Filter out months with little sampling effort

#Model fitting


human <- glmmTMB(human ~ northern_elephant_seal 
                    + (1|placename),
                    family = poisson, 
                    data = summarized_detection_count_weekly_wide)
summary(human)



coyote <- glmmTMB(coyote ~ northern_elephant_seal + human 
                     + (1|placename),
                     family = poisson, 
                     data = summarized_detection_count_weekly_wide)
summary(coyote)



raccoon <- glmmTMB(northern_raccoon ~ #northern_elephant_seal + 
                     human + coyote 
                      + bobcat 
                   + (1|placename),
                      family = poisson, 
                      data = summarized_detection_count_monthly_wide)
summary(raccoon)



bobcat <- glmmTMB(bobcat ~ #northern_elephant_seal + 
                    human + coyote
                      + (1|placename),
                      family = poisson, 
                      data = summarized_detection_count_monthly_wide)
summary(bobcat)



deer <- glmmTMB(mule_deer ~ northern_elephant_seal + human + coyote +(1|placename),
                     family = poisson, 
                     data = summarized_detection_count_monthly_wide)
summary(deer)


#Structural equation model 

pSEM <- psem(human, coyote, bobcat, raccoon, deer, data = summarized_detection_count_monthly_wide)

summary(pSEM)








##################################
# Piecewise SEM Exploration Monthly ######
##################################

#Import data
summarized_detection_count_monthly_wide <- read_csv("data/processed/summarized_detection_count_monthly_wide.csv") %>% 
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
  replace_na(list(
    pup = 0,
    non_pup_seals = 0)) %>% 
  select(placename, year_month, sampling_days, bobcat, coyote, human, domestic_dog, mule_deer, northern_raccoon, pup, non_pup_seals) %>% 
  mutate(placename = factor(placename),
         log_sampling_days = log(sampling_days))
  
  

ggpairs(monthly_SEM_data[,c(4:11)], #subset predictor columns at data for ggpairs function
        switch="both")+ #labels on left and bottom of plot
  theme_few()+ #theme
  theme(strip.background = element_rect(fill = "white"), #replace background
        strip.placement = "outside") #facet label on outside of tickmarks


#Model fitting


coyote <- glmmTMB(coyote ~ log1p(pup) + human 
                  + (1|placename),
                  family = poisson, 
                  offset = monthly_SEM_data$log_sampling_days,
                  data = monthly_SEM_data)
summary(coyote)



raccoon <- glmmTMB(northern_raccoon ~ log1p(non_pup_seals) + 
                     human + coyote 
                   + bobcat 
                   + (1|placename),
                   family = poisson, 
                   offset = monthly_SEM_data$log_sampling_days,
                   data = monthly_SEM_data)
summary(raccoon)



bobcat <- glmmTMB(bobcat ~ log1p(non_pup_seals) + 
                    human + coyote
                  + (1|placename),
                  family = poisson, 
                  offset = monthly_SEM_data$log_sampling_days,
                  data = monthly_SEM_data)
summary(bobcat)



deer <- glmmTMB(mule_deer ~ log1p(non_pup_seals) + human + coyote +(1|placename),
                family = poisson, 
                offset = monthly_SEM_data$log_sampling_days,
                data = monthly_SEM_data)
summary(deer)


#Structural equation model 

pSEM <- psem(coyote, bobcat, raccoon, deer)

summary(pSEM)
