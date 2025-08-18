##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Working with tide data #################################################
#-------------------------------------------------------------------------

# Import raw data ####

deployments <- read_csv("data/processed/deployments.csv")
sequences <- read_csv("data/processed/sequences.csv")

###################################################################
# Prepare Reference Dataset #######################################
# (tide data for all 10-minute periods during study duration ######
###################################################################

# Load tide data for the duration of all deployments (from study start date to study end date) ####

tide_data_raw <- rtide::tide_height(
  "Drakes Bay, Point Reyes, California", 
  from = as.Date(min(deployments$start_date)), 
  to = as.Date(max(deployments$end_date)), 
  minutes = 10L, 
  tz = "America/Los_Angeles"
)

# Manipulate tide data to extract key information
tide_data <- tide_data_raw %>% 
  arrange(DateTime) %>%
  #Determine tidal trends (high, low, rising, or falling)
  mutate(
    TideHeight_prev = lag(TideHeight),
    TideHeight_next = lead(TideHeight),
    TideTrend = case_when(
      is.na(TideHeight_prev) | is.na(TideHeight_next) ~ NA_character_, # Edge cases
      TideHeight > TideHeight_prev & TideHeight > TideHeight_next ~ "High",
      TideHeight < TideHeight_prev & TideHeight < TideHeight_next ~ "Low",
      TideHeight > TideHeight_prev ~ "Rising",
      TideHeight < TideHeight_prev ~ "Falling")) %>% 
  drop_na() %>% #Drops first and last tide height observations (no prev/next comparison)
  
  #Create unique ID for each tide cycle (high-high) or half tidecycle (high-low or low-high)
  mutate(
    TideCycleID = cumsum(!is.na(TideTrend) & TideTrend %in% c("High")),
    TideCycleHalfID = cumsum(!is.na(TideTrend) & TideTrend %in% c("High", "Low"))) %>% 
  
  # Duplicate high/low rows into the previous cycle, which ensures each cycle has both endpoints for scaling.
  { 
    orig <- .
    dups <- orig %>%
      filter(TideTrend %in% c("High", "Low")) %>% 
      mutate(
        TideCycleID     = if_else(TideTrend == "High", TideCycleID - 1, TideCycleID),
        TideCycleHalfID = TideCycleHalfID - 1
      )
    bind_rows(orig, dups)
  } %>%
  
  
  arrange(DateTime) %>%  # restore chronological order
    
  
  #Calculate relative tide within each tide cycle (Low = 0, High = 1)
  group_by(TideCycleHalfID) %>%
  mutate(
    TideHeight_min = min(TideHeight, na.rm = TRUE),
    TideHeight_max = max(TideHeight, na.rm = TRUE),
    # normalize position within the half-cycle
    RelativeTideHeight = (TideHeight - TideHeight_min) / 
      (TideHeight_max - TideHeight_min),
    # normalize position within the whole-cycle
    TideCyclePosition = case_when(TideTrend == "High" ~ 0,
                                  TideTrend == "Falling" ~ 0.5 * (1-RelativeTideHeight),
                                  TideTrend == "Low" ~ .5,
                                  TideTrend == "Rising" ~ .5 + 0.5 * RelativeTideHeight)) %>%
    
  ungroup() %>% 
  
  #Remove data from start and end (min and max TideCycleID) because these are incorrect due to the absence of high or low tide values
  filter(
    TideCycleID != min(TideCycleID),
    TideCycleID != max(TideCycleID)
  ) %>% 

  #Remove irrelevant columns
  
  select(-c(TideHeight_prev, TideHeight_next,TideHeight_min,TideHeight_max,TideCycleID, TideCycleHalfID)) %>% 
  unique()


###################################################################
# Assemble tide data for camera deployment periods ################
###################################################################


#Import tide data for each deployment ####

# Function to download tide data for one deployment
get_tide_data <- function(deployment_id, start_date, end_date) {
  tide_data <- rtide::tide_height(
    "Drakes Bay, Point Reyes, California", 
    from = as.Date(start_date), 
    to = as.Date(end_date), 
    minutes = 10L, 
    tz = "America/Los_Angeles"
  )
  tide_data <- tide_data %>%
    mutate(deployment_id = deployment_id) # Add deployment_id as a column
  return(tide_data)
}

# Iterate through each row and combine results
all_tide_data <- deployments %>%
  select(deployment_id, start_date, end_date) %>% # Select relevant columns
  pmap_dfr(~ get_tide_data(..1, ..2, ..3))   %>% 
  
  #Combine with reference dataset

  left_join(tide_data)


#Plot tide heights across all deployment periods
ggplot(all_tide_data, aes(x=TideHeight))+
  geom_histogram(fill = "grey70")+
  theme_few()

ggplot(all_tide_data, aes(x=RelativeTideHeight))+
  geom_histogram(fill = "grey70")+
  theme_few()

ggplot(all_tide_data, aes(x=TideCyclePosition))+
  geom_histogram(fill = "grey70")+
  theme_few()



# Working: Assessing mammal detections and their relation to tide height ------

independent_mammal_detections <- read_csv("data/processed/independent_mammal_detections.csv") %>% 
  filter(common_name %in% c("Coyote", "Mule Deer", "Northern Raccoon", "Bobcat", "Striped Skunk")) %>% 
  #Round start time to the nearest hour using lubridate, so as to be able to match the time to the tide height
  mutate(start_time_10m = floor_date(event_start, unit = "10 minutes")) %>% 
  left_join(tide_data %>% 
              select(DateTime, TideHeight, TideTrend, RelativeTideHeight, TideCyclePosition),
              by = c("start_time_10m" = "DateTime"))
  

#Plot tide heights for species detections
ggplot(independent_mammal_detections, aes(x=TideHeight))+
  geom_histogram()+
  facet_wrap(facets = "common_name", scales = "free_y")+
  theme_few()+
  theme(legend.position = "none")

#Plot relative tide for species detections
ggplot(independent_mammal_detections, aes(x=RelativeTideHeight, fill = TideTrend))+
  geom_histogram()+
  facet_wrap(facets = "common_name", scales = "free_y")+
  theme_few()

#Plot tide cycle position for species detections
ggplot(independent_mammal_detections, aes(x=TideCyclePosition, fill = TideTrend))+
  geom_histogram()+
  facet_wrap(facets = "common_name", scales = "free_y")+
  theme_few()


temp <- independent_mammal_detections %>% 
  select(deployment_id, placename, start_time_10m, common_name) %>% 
  mutate(values = 1) %>% 
  pivot_wider(names_from = common_name, values_from = values, values_fill = 0) %>% 
  clean_names()

temp2 <- all_tide_data %>% 
  left_join(temp, by = c("deployment_id", "DateTime" = "start_time_10m")) %>% 
  mutate(mule_deer = replace_na(mule_deer, 0),
         coyote = replace_na(coyote, 0),
         northern_raccoon = replace_na(northern_raccoon, 0),
         striped_skunk = replace_na(striped_skunk, 0),
         bobcat = replace_na(bobcat, 0))

library(glmmTMB)
mod <- glmmTMB(coyote ~ TideHeight + (1|deployment_id),
             data = temp2, 
             family = binomial(link = "logit"))
summary(mod)


ggplot(temp2, aes(x=TideHeight, y=coyote))+
  geom_smooth(method = "gam")#+
  coord_cartesian(ylim = c(0, 0.01))
