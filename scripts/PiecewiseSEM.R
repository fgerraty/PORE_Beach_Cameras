

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
  drop_na(placename) %>% 
  replace_na(list(
    pup = 0,
    non_pup_seals = 0)) %>% 
  select(placename, year_month, sampling_days, bobcat, coyote, human, domestic_dog, mule_deer, northern_raccoon, pup, non_pup_seals) %>% 
  mutate(placename = factor(placename),
         log_sampling_days = log(sampling_days),
         log1p_seal = log1p(non_pup_seals),
         log1p_pup = log1p(pup))
  
  

ggpairs(monthly_SEM_data[,c(4:9, 13, 14)], #subset predictor columns at data for ggpairs function
        switch="both")+ #labels on left and bottom of plot
  theme_few()+ #theme
  theme(strip.background = element_rect(fill = "white"), #replace background
        strip.placement = "outside") #facet label on outside of tickmarks

##########################
#PSEM MODEL FITTING 1 ####
##########################

pup <- glmmTMB(log1p_pup ~ log1p_seal
               + (1|placename),
               family = gaussian, 
               offset = monthly_SEM_data$log_sampling_days,
               data = monthly_SEM_data)
summary(pup)


coyote <- glmmTMB(coyote ~ log1p_seal + log1p_pup + human
                  + (1|placename),
                  family = poisson, 
                  offset = monthly_SEM_data$log_sampling_days,
                  data = monthly_SEM_data)
summary(coyote)



raccoon <- glmmTMB(northern_raccoon ~ log1p_seal + log1p_pup +
                     human + coyote + (1|placename),
                   family = poisson, 
                   offset = monthly_SEM_data$log_sampling_days,
                   data = monthly_SEM_data)
summary(raccoon)



bobcat <- glmmTMB(bobcat ~ log1p_seal + log1p_pup +
                    human + coyote
                  + (1|placename),
                  family = poisson, 
                  offset = monthly_SEM_data$log_sampling_days,
                  data = monthly_SEM_data)
summary(bobcat)



deer <- glmmTMB(mule_deer ~ log1p_seal + + log1p_pup + human + coyote + (1|placename),
                family = poisson, 
                offset = monthly_SEM_data$log_sampling_days,
                data = monthly_SEM_data)
summary(deer)


#Structural equation model 

pSEM <- psem(pup, coyote, bobcat, raccoon, deer)

summary(pSEM)




##########################
#PSEM MODEL FITTING 2 ####
##########################

pup <- glmmTMB(log1p_pup ~ log1p_seal
               + (1|placename),
               family = gaussian, 
               offset = monthly_SEM_data$log_sampling_days,
               data = monthly_SEM_data)
summary(pup)


coyote <- glmmTMB(coyote ~ log1p_pup + human
                  + (1|placename),
                  family = poisson, 
                  offset = monthly_SEM_data$log_sampling_days,
                  data = monthly_SEM_data)
summary(coyote)



raccoon <- glmmTMB(northern_raccoon ~
                     human + coyote + (1|placename),
                   family = poisson, 
                   offset = monthly_SEM_data$log_sampling_days,
                   data = monthly_SEM_data)
summary(raccoon)



bobcat <- glmmTMB(bobcat ~
                    human + coyote
                  + (1|placename),
                  family = poisson, 
                  offset = monthly_SEM_data$log_sampling_days,
                  data = monthly_SEM_data)
summary(bobcat)



deer <- glmmTMB(mule_deer ~ human + coyote + (1|placename),
                family = poisson, 
                offset = monthly_SEM_data$log_sampling_days,
                data = monthly_SEM_data)
summary(deer)


#Structural equation model 

pSEM <- psem(pup, coyote, bobcat, raccoon, deer)

summary(pSEM)





##########################
#PSEM MODEL FITTING 3 ####
##########################

pup <- glmmTMB(log1p_pup ~ log1p_seal
               + (1|placename),
               family = gaussian, 
               offset = monthly_SEM_data$log_sampling_days,
               data = monthly_SEM_data)
summary(pup)


dog <- glmmTMB(domestic_dog ~ human
               + (1|placename),
               family = gaussian, 
               offset = monthly_SEM_data$log_sampling_days,
               data = monthly_SEM_data)
summary(dog)


coyote <- glmmTMB(coyote ~ log1p_pup + human + domestic_dog
                  + (1|placename),
                  family = poisson, 
                  offset = monthly_SEM_data$log_sampling_days,
                  data = monthly_SEM_data)
summary(coyote)



raccoon <- glmmTMB(northern_raccoon ~ human + domestic_dog + 
                     log1p_seal + coyote + (1|placename),
                   family = poisson, 
                   offset = monthly_SEM_data$log_sampling_days,
                   data = monthly_SEM_data)
summary(raccoon)



bobcat <- glmmTMB(bobcat ~
                    human + domestic_dog + log1p_seal + coyote
                  + (1|placename),
                  family = poisson, 
                  offset = monthly_SEM_data$log_sampling_days,
                  data = monthly_SEM_data)
summary(bobcat)



deer <- glmmTMB(mule_deer ~ human + domestic_dog + log1p_seal + coyote + (1|placename),
                family = poisson, 
                offset = monthly_SEM_data$log_sampling_days,
                data = monthly_SEM_data)
summary(deer)


#Structural equation model 

pSEM <- psem(pup, coyote, bobcat, raccoon, deer)

summary(pSEM)




#Exploring plots 

monthly_SEM_plot_data <- monthly_SEM_data %>% 
  group_by(placename) %>% 
  summarize(bobcat_mean = mean(bobcat), bobcat_SE = sd(bobcat)/sqrt(n()),
            coyote_mean = mean(coyote), coyote_SE = sd(coyote)/sqrt(n()),
            domestic_dog_mean = mean(domestic_dog), domestic_dog_SE = sd(domestic_dog)/sqrt(n()),
            human_mean = mean(human), human_SE = sd(human)/sqrt(n()),
            pup_mean = mean(log1p_pup), pup_SE = sd(log1p_pup)/sqrt(n()),
            raccoon_mean = mean(northern_raccoon), raccoon_SE = sd(northern_raccoon)/sqrt(n()), 
            seal_mean = mean(log1p_seal), seal_SE = sd(log1p_seal)/sqrt(n()),
            mule_deer_mean = mean(mule_deer), mule_deer_SE = sd(mule_deer)/sqrt(n()))


ggplot(monthly_SEM_plot_data, aes(x=coyote_mean, y=bobcat_mean))+
  geom_point()


