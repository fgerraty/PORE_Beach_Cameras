
######################################################################
# Spatiotemporal AARs (Avoidance-Attraction Ratios) Exploration ######
######################################################################

independent_mammal_detections <- read_csv("data/processed/independent_mammal_detections.csv") 


aar_data <- independent_mammal_detections %>% 
  filter(common_name %in% 
           c("Human", "Human-Camera Trapper", "Coyote", 
             "Bobcat","Northern Raccoon", "Mule Deer")) %>% 
  #combine human detections
  mutate(common_name = if_else(common_name == "Human-Camera Trapper",
                               "Human", common_name))


#Example

species_A <- "Coyote"
species_B <- "Northern Raccoon"

df <- aar_data %>%
  filter(common_name %in% c(species_A, species_B)) %>%
  arrange(placename, event_start) %>%
  mutate(
    is_A = common_name == species_A,
    next_species = lead(common_name),
    next_start = lead(event_start),
    time_to_next = as.numeric(difftime(lead(event_start), event_start, units = "mins"))
  )


# A→B transitions (A followed by B)
AtoB <- df %>%
  filter(is_A & next_species == species_B) %>%
  group_by(placename) %>%
  summarise(T1 = median(time_to_next, na.rm = TRUE), .groups = "drop")

# B→A transitions
BtoA <- df %>%
  filter(!is_A & next_species == species_A) %>%
  group_by(placename) %>%
  summarise(T2b = median(time_to_next, na.rm = TRUE), .groups = "drop")

# A→A (same species again)
AtoA <- df %>%
  filter(is_A & next_species == species_A) %>%
  group_by(placename) %>%
  summarise(T3 = median(time_to_next, na.rm = TRUE), .groups = "drop")

# A then B appears between next A (T4)
df$group <- with(df, cumsum(is_A))
group_summaries <- df %>%
  group_by(placename, group) %>%
  reframe(
    A_time = first(event_start),
    next_A_time = lead(event_start),
    has_B_between = any(common_name == species_B),
    total_time = as.numeric(difftime(max(event_start), min(event_start), units = "mins")),
    .groups = "drop"
  ) %>%
  filter(has_B_between) %>%
  group_by(placename) %>%
  summarise(T4 = median(total_time, na.rm = TRUE), .groups = "drop")


result <- reduce(list(AtoB, BtoA, AtoA, group_summaries), full_join, by = "placename") %>%
  mutate(
    T1 = as.numeric(T1),
    T2b = as.numeric(T2b),
    T2T1 = T2b / T1,
    logT2T1 = log(T2T1),
    T3 = as.numeric(T3),
    T4 = as.numeric(T4),
    T4T3 = T4 / T3,
    logT4T3 = log(T4T3),
    interaction = paste("A:", species_A, ", B:", species_B)
  )







#Function for processing a species pair

process_species_pair_custom <- function(data, species_A, species_B, pair_label) {
  df <- data %>%
    filter(common_name %in% c(species_A, species_B)) %>%
    arrange(placename, event_start) %>%
    mutate(
      is_A = common_name == species_A,
      next_species = lead(common_name),
      next_start = lead(event_start),
      time_to_next = as.numeric(difftime(lead(event_start), event_start, units = "mins"))
    )
  
  # A→B transitions (A followed by B)
  AtoB <- df %>%
    filter(is_A & next_species == species_B) %>%
    group_by(deployment_id) %>%
    summarise(T1 = median(time_to_next, na.rm = TRUE), .groups = "drop")
  
  # B→A transitions
  BtoA <- df %>%
    filter(!is_A & next_species == species_A) %>%
    group_by(deployment_id) %>%
    summarise(T2b = median(time_to_next, na.rm = TRUE), .groups = "drop")
  
  # A→A (same species again)
  AtoA <- df %>%
    filter(is_A & next_species == species_A) %>%
    group_by(deployment_id) %>%
    summarise(T3 = median(time_to_next, na.rm = TRUE), .groups = "drop")
  
  # A then B appears between next A (T4)
  df$group <- with(df, cumsum(is_A))
  group_summaries <- df %>%
    group_by(deployment_id, group) %>%
    summarise(
      A_time = first(event_start),
      next_A_time = lead(event_start),
      has_B_between = any(common_name == species_B),
      total_time = as.numeric(difftime(max(event_start), min(event_start), units = "mins")),
      .groups = "drop"
    ) %>%
    filter(has_B_between) %>%
    group_by(deployment_id) %>%
    summarise(T4 = median(total_time, na.rm = TRUE), .groups = "drop")
  
  # Combine
  result <- reduce(list(AtoB, BtoA, AtoA, group_summaries), full_join, by = "deployment_id") %>%
    mutate(
      T1 = as.numeric(T1),
      T2b = as.numeric(T2b),
      T2T1 = T2b / T1,
      logT2T1 = log(T2T1),
      T3 = as.numeric(T3),
      T4 = as.numeric(T4),
      T4T3 = T4 / T3,
      logT4T3 = log(T4T3),
      interaction = paste("A:", species_A, ", B:", species_B),
      type = pair_label
    )
  
  return(result)
}
