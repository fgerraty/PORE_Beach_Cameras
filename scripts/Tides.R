##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Working with tide data #################################################
#-------------------------------------------------------------------------

# Import raw data ####

deployments <- read_csv("data/processed/deployments.csv")
sequences <- read_csv("data/processed/sequences.csv")


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
  pmap_dfr(~ get_tide_data(..1, ..2, ..3))    %>% # Map the function and combine results

  # Identify tide trends (High, Low, Rising, Falling)
  group_by(deployment_id) %>%
  arrange(DateTime) %>%
  mutate(
    TideHeight_prev = lag(TideHeight),
    TideHeight_next = lead(TideHeight),
    TideTrend = case_when(
      is.na(TideHeight_prev) | is.na(TideHeight_next) ~ NA_character_, # Edge cases
      TideHeight > TideHeight_prev & TideHeight > TideHeight_next ~ "High",
      TideHeight < TideHeight_prev & TideHeight < TideHeight_next ~ "Low",
      TideHeight > TideHeight_prev ~ "Rising",
      TideHeight < TideHeight_prev ~ "Falling"
    )
  ) %>%
  ungroup() %>%
  
  # Define tide cycles
  group_by(deployment_id) %>%
  mutate(
    TideCycleID = cumsum(!is.na(TideTrend) & TideTrend %in% c("High", "Low")),
    TideType = case_when(
      TideTrend == "High" ~ 1,
      TideTrend == "Low" ~ 0,
      TRUE ~ NA_real_ # Assign NA for non-High/Low rows
    )
  ) %>%
  fill(TideType, .direction = "downup") %>% # Fill High/Low within each cycle
  
  # Step 3: Calculate relative tide within each tide cycle (Low = 0, High = 1)
  group_by(deployment_id, TideCycleID) %>%
  mutate(
    CycleStart = first(DateTime),
    CycleEnd = last(DateTime),
     relative_tide = case_when(
      TideType == 1 ~ 1 - as.numeric(difftime(DateTime, CycleStart, units = "mins")) / 
        as.numeric(difftime(CycleEnd, CycleStart, units = "mins")),
      TideType == 0 ~ as.numeric(difftime(DateTime, CycleStart, units = "mins")) / 
        as.numeric(difftime(CycleEnd, CycleStart, units = "mins")),
      TRUE ~ NA_real_ # Handle edge cases
    )
  ) %>%
  ungroup()


#Plot tide heights across all deployment periods
ggplot(all_tide_data, aes(x=TideHeight))+
  geom_histogram(fill = "grey70")+
  theme_few()



# Assessing mammal detections and their relation to tide height ----------------


mammal_detections <- sequences %>% 
  #Filter for species of interest
  filter(common_name %in% c("Coyote", "Mule Deer", "Northern Raccoon", "Bobcat", "Striped Skunk")) %>% 
  #Round start time to the nearest hour using lubridate, so as to be able to match the time to the tide height
  mutate(start_time_rounded = round_date(start_time, unit = "hour")) %>% 

  #Combine with tide data from "all_tide_data"
    left_join(
    #Remove deployment_id and filter duplicate tide values from "all_tide_data"
              all_tide_data %>% 
              select(Station, DateTime, TideHeight, relative_tide, TideTrend) %>% 
              unique(),
              by = c("start_time_rounded" = "DateTime"))



#Plot tide heights for species detections
ggplot(filter(mammal_detections), aes(x=TideHeight))+
  geom_histogram()+
  facet_wrap(facets = "common_name", scales = "free_y")+
  theme_few()+
  theme(legend.position = "none")

#Plot relative tide  for species detections
ggplot(filter(mammal_detections), aes(x=relative_tide, fill = TideTrend))+
  geom_histogram()+
  facet_wrap(facets = "common_name", scales = "free_y")+
  theme_few()



