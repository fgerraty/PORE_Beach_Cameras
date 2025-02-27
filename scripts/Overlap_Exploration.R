#Activity overlap exploration

independent_mammal_detections <- read_csv("data/processed/independent_mammal_detections.csv") 



temp <- independent_mammal_detections %>% 
  mutate(seconds_into_day = hour(event_start)*60*60 +
           minute(event_start) * 60 +
           second(event_start),
         fraction_of_day = seconds_into_day / (24*60*60),
         time_of_day_radians = fraction_of_day*2*pi) %>% 
  mutate(day_of_year_fraction = yday(event_start)/365,
         day_of_year_radians = day_of_year_fraction*2*pi)


#Diel Patterns
coyote <- temp %>% filter(common_name == "Coyote")
densityPlot(coyote$time_of_day_radians, rug=TRUE)

mule_deer <- temp %>% filter(common_name == "Mule Deer")
densityPlot(mule_deer$time_of_day_radians, rug=TRUE)

bobcat <- temp %>% filter(common_name == "Bobcat")
densityPlot(bobcat$time_of_day_radians, rug=TRUE)

raccoon <- temp %>% filter(common_name == "Northern Raccoon")
densityPlot(raccoon$time_of_day_radians, rug=TRUE)


overlapPlot(coyote$time_of_day_radians,mule_deer$time_of_day_radians)
overlapPlot(coyote$time_of_day_radians,bobcat$time_of_day_radians)
overlapPlot(coyote$time_of_day_radians,raccoon$time_of_day_radians)

overlapEst(coyote$time_of_day_radians,mule_deer$time_of_day_radians)
overlapEst(coyote$time_of_day_radians,bobcat$time_of_day_radians)
overlapEst(coyote$time_of_day_radians,raccoon$time_of_day_radians)

#Seasonal Patterns: NOTE, dont trust these b/c we need to account for seasonal differences in sampling effort!!

coyote <- temp %>% filter(common_name == "Coyote")
densityPlot(coyote$day_of_year_radians, rug=TRUE)

mule_deer <- temp %>% filter(common_name == "Mule Deer")
densityPlot(mule_deer$day_of_year_radians, rug=TRUE)

bobcat <- temp %>% filter(common_name == "Bobcat")
densityPlot(bobcat$day_of_year_radians, rug=TRUE)

raccoon <- temp %>% filter(common_name == "Northern Raccoon")
densityPlot(raccoon$day_of_year_radians, rug=TRUE)


overlapPlot(coyote$day_of_year_radians,mule_deer$day_of_year_radians)
overlapPlot(coyote$day_of_year_radians,bobcat$day_of_year_radians)
overlapPlot(coyote$day_of_year_radians,raccoon$day_of_year_radians)

overlapEst(coyote$day_of_year_radians,mule_deer$day_of_year_radians)
overlapEst(coyote$day_of_year_radians,bobcat$day_of_year_radians)
overlapEst(coyote$day_of_year_radians,raccoon$day_of_year_radians)

