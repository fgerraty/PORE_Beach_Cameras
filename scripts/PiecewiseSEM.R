
##################################
# Piecewise SEM Exploration ######
##################################

#Import data
summarized_detection_count_weekly_wide <- read_csv("data/processed/summarized_detection_count_weekly_wide.csv") %>% 
  as_tibble() %>% 
  filter(sampling_days == 7) #Filter for only complete weeks

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



raccoon <- glmmTMB(northern_raccoon ~ northern_elephant_seal + human + coyote 
                      + (1|placename),
                      family = poisson, 
                      data = summarized_detection_count_weekly_wide)
summary(raccoon)



bobcat <- glmmTMB(bobcat ~ northern_elephant_seal + human + coyote
                      + (1|placename),
                      family = poisson, 
                      data = summarized_detection_count_weekly_wide)
summary(bobcat)



deer <- glmmTMB(mule_deer ~ northern_elephant_seal + human + coyote + (1|placename),
                     family = poisson, 
                     data = summarized_detection_count_weekly_wide)
summary(deer)


#Structural equation model 

pSEM <- psem(human, coyote, bobcat, raccoon, deer, data = summarized_detection_count_weekly_wide)

summary(pSEM)
