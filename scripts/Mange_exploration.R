# Coyote Mange Diurnality ####


#Import site data
PORE_sites <- read_csv("data/raw/PORE_sites.csv") 

#Import clean deployment data
deployments <- read.csv("data/processed/deployments.csv") |> 
  # Convert columns to date-times 
  mutate(start_date = ymd_hms(start_date),
         end_date = ymd_hms(end_date)) 

#Import clean coyote sequence data
coyote_sequences <- read.csv("data/processed/sequences.csv") |> 
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time)) |> 
  filter(common_name == "Coyote")

head(coyote_sequences)



# Develop individual table and also screen for independence

parse_markings <- function(x) {
  if (is.na(x)) return(character(0))
  parts <- str_trim(str_split(x, "[,;]")[[1]])
  parts[parts != "" & !str_detect(parts, "\\?")]
}


coyote_seq <- coyote_sequences %>%
  filter(!is.na(species), class == "Mammalia") %>%
  mutate(
    mange_list   = map(markings, parse_markings),
    n_coyotes_id = map_int(mange_list, length),   # # of coyotes w/ usable status in the sequence
    mange_status = map_chr(mange_list, function(m) {
      if (length(m) == 0) {
        "Unknown"                # no usable marking info (NA, "?"-only, etc.)
      } else if (length(unique(m)) == 1) {
        unique(m)                # all coyotes in the sequence agree
      } else {
        "Mixed"                  # >1 coyote, differing statuses in same sequence
      }}))


coyote_individuals <- coyote_seq %>%
  select(deployment_id, sequence_id, start_time, end_time, placename, mange_list) %>%
  unnest_longer(mange_list, values_to = "status")



independent <- 30 * 60  # 30 minutes, in seconds

coyote_events <- coyote_seq %>%
  arrange(deployment_id, start_time) %>%
  group_by(deployment_id) %>%
  mutate(
    duration = as.numeric(difftime(start_time, lag(start_time), units = "secs")),
    
    # Conflict = this sequence AND the previous one both have a *known*
    # status (not "Unknown"), and those statuses differ.
    status_conflict = !is.na(lag(mange_status)) &
      mange_status != "Unknown" &
      lag(mange_status) != "Unknown" &
      mange_status != lag(mange_status),
    
    # New event if: first record in the deployment, OR gap > 30 min,
    # OR a status conflict (forces a split even inside the window)
    new_event = is.na(duration) | duration > independent | status_conflict,
    
    # Flags splits that were caused specifically by a status conflict
    # (as opposed to just being far apart in time) -- for QC/review
    health_flag = !is.na(duration) & duration <= independent & status_conflict
  ) %>%
  ungroup() %>%
  mutate(
    event_id = cumsum(new_event),
    event_id = str_pad(event_id, width = nchar(max(event_id)), pad = "0"),
    event_id = paste0("E", event_id)
  )

#####################################################
# Summarize to one row per independent coyote event
#####################################################

independent_coyote_detections <- coyote_events %>%
  group_by(deployment_id, placename, event_id, class, order, family, genus, species, common_name) %>%
  summarise(
    event_start    = min(start_time),
    event_end      = max(end_time),
    n_sequences    = n(),
    group_size     = max(group_size),
    mange_statuses = paste(sort(unique(mange_status)), collapse = "; "),
    health_flagged = any(health_flag),  # TRUE if a status conflict forced this event open
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    year_month = format(event_start, "%Y-%m"),
    year_week  = paste0(isoyear(event_start), "-", sprintf("%02d", isoweek(event_start)))
  ) %>%
  ungroup()






library(overlap)   # kernel density overlap estimates for circular time-of-day data
library(circular)  # Watson's two-sample test for circular data

#####################################################
# Build Healthy vs. Mange comparison groups
#####################################################

diurnality_events <- independent_coyote_detections %>%
  filter(!mange_statuses %in% c("Unknown", "Mixed")) %>%
  mutate(
    health_group = case_when(
      mange_statuses == "Healthy" ~ "Healthy",
      str_detect(mange_statuses, "Mange") ~ "Mange",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(health_group))

diurnality_events %>% count(health_group)


site_summary <- diurnality_events |> 
  group_by(placename) |> 
  summarise(mange_count = sum(if_else(health_group == "Mange", 1, 0)),
            healthy_count = sum(if_else(health_group == "Healthy", 1, 0)),
            prop_mange = mange_count/healthy_count) |> 
  left_join(PORE_sites)



ggplot(site_summary, aes(x=site_type, y=prop_mange))+
  geom_point()+
  geom_smooth()


#####################################################
# Convert event start time to radians (time-of-day only)
#####################################################

to_radians <- function(t) {
  secs_since_midnight <- hour(t) * 3600 + minute(t) * 60 + second(t)
  secs_since_midnight / 86400 * 2 * pi
}

diurnality_events <- diurnality_events %>%
  mutate(time_rad = to_radians(event_start))

healthy_rad <- diurnality_events %>% filter(health_group == "Healthy") %>% pull(time_rad)
mange_rad   <- diurnality_events %>% filter(health_group == "Mange") %>% pull(time_rad)

#####################################################
# Kernel density activity plots + overlap estimate
#####################################################

# Dhat4 is recommended when both samples have >= ~75 observations;
# Dhat1 is preferred when either sample is smaller than that.
est_type <- if (min(length(healthy_rad), length(mange_rad)) < 75) "Dhat1" else "Dhat4"

overlap_est <- overlapEst(healthy_rad, mange_rad, type = est_type)
overlap_est

# Bootstrap CI on the overlap estimate
set.seed(123)
boot_healthy <- resample(healthy_rad, 1000)
boot_mange   <- resample(mange_rad, 1000)
boot_overlap <- bootEst(boot_healthy, boot_mange, type = est_type)
boot_ci <- bootCI(overlap_est, boot_overlap)
boot_ci

# Plot both activity curves on one panel
overlapPlot(healthy_rad, mange_rad,
            main = "Coyote diurnal activity: Healthy vs. Mange",
            xlab = "Time of day", ylab = "Density",
            linecol = c("#1b9e77", "#d95f02"))
legend("topleft", legend = c("Healthy", "Mange"),
       col = c("#1b9e77", "#d95f02"), lty = 1, bty = "n")

#####################################################
# Statistical test for a difference in activity distribution
#####################################################

healthy_circ <- circular(healthy_rad, units = "radians", template = "clock24")
mange_circ   <- circular(mange_rad, units = "radians", template = "clock24")
watson.two.test(healthy_circ, mange_circ)
