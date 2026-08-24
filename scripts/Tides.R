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


################################################
# Data prep: 30min independence intervals, #####
# with tide and occurrence data ################
################################################

#Create 30min time bins of camera operation
time_bins <- deployments %>%
  select(deployment_id, start_date, end_date) %>%
  mutate(bin_start = map2(start_date, end_date,
      ~ seq(from = floor_date(.x, unit = "30 minutes"),
            to   = floor_date(.y, unit = "30 minutes"),
            by   = "30 mins"))) %>%
  unnest(bin_start) %>%
  mutate(bin_end = bin_start + minutes(30)) %>%
  select(deployment_id, bin_start, bin_end)

#Pull tide data of time bins
tide_summary <- tide_data %>%
  mutate(bin_start = floor_date(DateTime, unit = "30 minutes")) %>%
  group_by(bin_start) %>%
  summarise(
    tide_height = mean(TideHeight, na.rm = TRUE),
    sin_sum = sum(sin(TideCyclePosition * 2 * pi), na.rm = TRUE),
    cos_sum = sum(cos(TideCyclePosition * 2 * pi), na.rm = TRUE),
    n_obs = sum(!is.na(TideCyclePosition)),
    .groups = "drop"
  ) %>%
  mutate(
    mean_angle = atan2(sin_sum, cos_sum),
    tide_cycle_position = (mean_angle %% (2 * pi)) / (2 * pi)
  ) %>%
  select(bin_start, tide_height, tide_cycle_position, n_obs) %>% 
  filter(n_obs==3)

#Bin wildlife sequences
sequences_binned <- sequences %>%
  filter(common_name %in% c("Coyote", "Northern Raccoon", "Mule Deer", "Bobcat", 
                            "North American River Otter", "Striped Skunk")) %>% 
  mutate(species =common_name,
         bin_start = floor_date(start_time, unit = "30 minutes")) %>%
  select(deployment_id, species, bin_start) %>%
  distinct() %>% 
  mutate(present = 1)

# Expand to all bin x species combinations so absences are explicit (not just missing rows)
all_species <- c("Coyote", "Northern Raccoon", "Mule Deer", "Bobcat", 
                 "North American River Otter", "Striped Skunk")

analysis_df <- time_bins %>%
  tidyr::crossing(species = all_species) %>%
  left_join(
    sequences_binned,
    by = c("deployment_id", "bin_start", "species")) %>%
  mutate(present = replace_na(present, 0)) %>% 
  #Incorporate tide data
  left_join(tide_summary,
    by = c("bin_start")) %>% 
  left_join(select(deployments, "deployment_id", "placename"))




library(glmmTMB)

mod <- glmmTMB(present ~ tide_height + (1|placename),
               data = filter(analysis_df, species == "Coyote"), 
               family = binomial(link = "logit"))
summary(mod)


# Check assumptions with DHARMa package
tide_res = simulateResiduals(mod)
plot(tide_res, rank = T)
testDispersion(tide_res)
plotResiduals(tide_res, filter(analysis_df, species == "Coyote")$placename, xlab = "Site", main=NULL)


