#############################
# Coyote Mange Analyses #####
#############################

#################
#Import Data ####
#################

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


#################
# Clean Data ####
#################

# Function to split a comma/semicolon-separated field, keeping positions intact
split_codes <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(character(0))
  str_trim(str_split(x, "[,;]")[[1]])
}


coyote_seq <- coyote_sequences %>%
  mutate(
    marking_raw = map(markings, split_codes),
    note_raw    = map(individual_animal_notes, split_codes),
    n_marks     = lengths(marking_raw),
    n_notes     = lengths(note_raw),
    
    # otherwise the pairing can't be trusted, so they become NA
    note_aligned = map2(marking_raw, note_raw,
                        \(m, n) if (length(n) == length(m)) n else rep(NA_character_, length(m))),
    notes_mismatch = n_notes > 0 & n_notes != n_marks,
    
    # Drop empty / "?" markings by position, from both lists together
    keep_idx   = map(marking_raw, ~ which(.x != "" & !str_detect(.x, "\\?"))),
    mange_list = map2(marking_raw,  keep_idx, ~ .x[.y]),
    notes_list = map2(note_aligned, keep_idx, ~ .x[.y]),
    
    n_coyotes_id = map_int(mange_list, length),
    mange_status = map_chr(mange_list, function(m) {
      if (length(m) == 0) {
        "Unknown"
      } else if (length(unique(m)) == 1) {
        unique(m)
      } else {
        "Mixed"
      }})) %>%
  select(-marking_raw, -note_raw, -note_aligned, -keep_idx)


coyote_seq %>%
  filter(notes_mismatch) %>%
  select(sequence_id, markings, individual_animal_notes)

coyote_individuals <- coyote_seq %>%
  select(deployment_id, sequence_id, start_time, end_time, placename,
         mange_list, notes_list) %>%
  unnest(c(mange_list, notes_list), keep_empty = TRUE) %>%   # keep sequences with no usable marking
  rename(status = mange_list,
         individual_note = notes_list) %>%
  mutate(
    status = coalesce(status, "Unknown"),
    status_clean = str_to_lower(str_squish(status)),
    health_group = case_when(
      str_detect(status_clean, "unknown")  ~ "Unknown",          # must come before "mange"
      str_detect(status_clean, "possible") ~ "Possible mange",
      str_detect(status_clean, "mange")    ~ "Mange",            # Mange, Mange: Mild, Mange: Severe
      str_detect(status_clean, "healthy")  ~ "Healthy",
      TRUE ~ NA_character_
    ))


independent <- 30 * 60  # 30 minutes (in seconds)

coyote_events <- coyote_individuals %>%
  group_by(deployment_id, placename, sequence_id, start_time, end_time, health_group) %>%
  summarise(
    n_individuals = n(),
    statuses = paste(sort(unique(status)), collapse = "; "),
    notes    = na_if(paste(na.omit(individual_note), collapse = "; "), ""),
    .groups = "drop"
  ) %>%
  arrange(deployment_id, health_group, start_time) %>%
  group_by(deployment_id, health_group) %>%
  mutate(
    gap = as.numeric(difftime(start_time, lag(start_time), units = "secs")),
    new_event = is.na(gap) | gap > independent
  ) %>%
  ungroup() %>%
  mutate(
    event_num = cumsum(new_event),
    event_id  = paste0("E", str_pad(event_num, nchar(max(event_num)), pad = "0"))
  )

# One row per independent event
independent_coyote_detections <- coyote_events %>%
  group_by(event_id, deployment_id, placename, health_group) %>%
  summarise(
    event_start = min(start_time),
    event_end   = max(end_time),
    n_sequences = n(),
    group_size  = max(n_individuals),
    statuses    = paste(sort(unique(unlist(str_split(statuses, "; ")))), collapse = "; "),
    notes       = na_if(paste(na.omit(notes), collapse = "; "), ""),
    .groups = "drop"
  ) %>%
  mutate(
    year_month = format(event_start, "%Y-%m"),
    year_week  = paste0(isoyear(event_start), "-", sprintf("%02d", isoweek(event_start)))
  )


library(overlap)   # kernel density overlap estimates for circular time-of-day data
library(circular)  # Watson's two-sample test for circular data

#####################################################
# Build Healthy vs. Mange comparison groups
#####################################################

diurnality_events <- independent_coyote_detections %>%
  filter(health_group %in% c("Healthy", "Mange"))

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




###################
# MAP OF MANGE ####
###################

PORE <-  st_read("data/shapefiles/Administrative_Boundaries of_National Park_System_Units/nps_boundary.shp") %>% 
  subset(UNIT_CODE == "PORE")%>% 
  st_transform(crs= "WGS84")

#Import california counties shapefile, originally downloaded from https://purl.stanford.edu/jm667wq2232
counties <- st_read("data/shapefiles/stanford-jm667wq2232-shapefile/jm667wq2232.shp") %>% 
  st_make_valid() %>% 
  st_union() 



# 1. Site-level proportions --------------------------------------------
# Healthy vs. Mange only; Unknown and Possible mange are excluded
site_props <- independent_coyote_detections %>%
  filter(health_group %in% c("Healthy", "Mange")) %>%
  count(placename, health_group) %>%
  pivot_wider(names_from = health_group, values_from = n, values_fill = 0) %>%
  mutate(n_total    = Healthy + Mange,
         prop_mange = Mange / n_total) %>%
  left_join(PORE_sites, by = "placename") |> 
  mutate(r = 120 + (300 - 120) * sqrt(n_total / max(n_total))) %>%   # meters, ~120-300 m
  arrange(desc(n_total)) 

# 2. Pie generator (works in meters, so circles stay circular) ----------
pie_crs    <- 32610   # UTM zone 10N
pie_radius <- 500     # meters; adjust to taste

make_pie <- function(x, y, r, p, n = 60) {
  wedge <- function(a0, a1) {
    th <- seq(a0, a1, length.out = ceiling(n * (a1 - a0) / (2 * pi)) + 2)
    st_polygon(list(rbind(c(x, y),
                          cbind(x + r * sin(th), y + r * cos(th)),
                          c(x, y))))
  }
  circle <- st_buffer(st_point(c(x, y)), r, nQuadSegs = 30)
  geoms <- if (p <= 0) {
    list(Healthy = circle)
  } else if (p >= 1) {
    list(Mange = circle)
  } else {
    list(Mange = wedge(0, 2 * pi * p), Healthy = wedge(2 * pi * p, 2 * pi))
  }
  st_sf(health_group = names(geoms), geometry = st_sfc(geoms, crs = pie_crs))
}

# 3. Build pie polygons for every site -----------------------------------
centers <- site_props %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(pie_crs) %>%
  st_coordinates()

pies <- map(seq_len(nrow(site_props)), function(i) {
  make_pie(centers[i, "X"], centers[i, "Y"], site_props$r[i], site_props$prop_mange[i]) %>%
    mutate(placename = site_props$placename[i],
           site_type = site_props$site_type[i],
           n_total   = site_props$n_total[i])
}) %>%
  do.call(rbind, .) %>%
  st_transform(4326) %>%
  mutate(health_group = factor(health_group, levels = c("Mange", "Healthy")))

# 4. Map ------------------------------------------------------------------
# Camera sites with no coyote detections at all, shown as small grey x's
no_coyote_sites <- PORE_sites %>% filter(!placename %in% site_props$placename)

pie_map <- ggplot() +
  geom_sf(data = counties, fill = "#FAEED9") +
  geom_sf(data = PORE_land, fill = "#D1E3B3") +
  geom_point(data = no_coyote_sites, aes(longitude, latitude),
             shape = 4, size = 1.2, color = "grey40") +
  geom_sf(data = pies, aes(fill = health_group, color = site_type), linewidth = 0.5) +
  coord_sf(crs = st_crs(4326),
           xlim = c(-123.05, -122.88),
           ylim = c(37.98, 38.1),
           expand = FALSE) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#BDE8FE")) +
  scale_x_continuous(breaks = c(-123, -122.9), name = "") +
  scale_y_continuous(breaks = c(38.0, 38.05), name = "") +
  scale_fill_manual(values = c(Mange = "#B2182B", Healthy = "white"),
                    labels = c("Mange", "Healthy")) +
  scale_color_manual(values = c("darkgreen", "#F27F0C"),
                     labels = c("Non-Rookery", "Rookery")) +
  labs(fill = "Coyote\ndetections", color = "Site type") +
  theme(legend.position = "inside",
        legend.position.inside = c(.18, .78),
        legend.box.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(linewidth = 2)) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "cm"), width = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering())+
  guides(fill  = guide_legend(override.aes = list(color = "black")),
         color = guide_legend(override.aes = list(fill = "white")))
pie_map


# 1. Circle-repel in projected meters ---------------------------------------
repel_circles <- function(xy, r, pad = 40, iter = 500, k_home = 0.02) {
  pos <- xy
  n <- nrow(xy)
  for (it in seq_len(iter)) {
    disp <- matrix(0, n, 2)
    for (i in 1:(n - 1)) for (j in (i + 1):n) {
      d <- pos[j, ] - pos[i, ]
      dist <- sqrt(sum(d^2))
      if (dist == 0) { d <- runif(2, -1, 1); dist <- sqrt(sum(d^2)) }  # coincident sites
      min_d <- r[i] + r[j] + pad
      if (dist < min_d) {
        push <- (min_d - dist) / 2 * d / dist
        disp[i, ] <- disp[i, ] - push
        disp[j, ] <- disp[j, ] + push
      }
    }
    pos <- pos + disp + k_home * (xy - pos)   # spring back toward true location
  }
  pos
}

# 2. Sizes, true positions, displaced positions -------------------------------
site_props <- site_props %>%
  mutate(r = 250 + (450 - 250) * sqrt(n_total / max(n_total)))   # meters; bigger than before

xy_true <- site_props %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(pie_crs) %>%
  st_coordinates()

set.seed(1)
xy_pie <- repel_circles(xy_true, site_props$r)

# 3. Pies at displaced positions (largest first so small ones draw on top) -----
ord <- order(-site_props$n_total)

pies <- map(ord, function(i) {
  make_pie(xy_pie[i, 1], xy_pie[i, 2], site_props$r[i], site_props$prop_mange[i]) %>%
    mutate(placename = site_props$placename[i],
           site_type = site_props$site_type[i],
           n_total   = site_props$n_total[i])
}) %>%
  do.call(rbind, .) %>%
  st_transform(4326) %>%
  mutate(health_group = factor(health_group, levels = c("Mange", "Healthy")))

# 4. Leader lines from true site to pie center ---------------------------------
leaders <- st_sf(
  site_type = site_props$site_type,
  geometry  = st_sfc(map(seq_len(nrow(site_props)),
                         ~ st_linestring(rbind(xy_true[.x, ], xy_pie[.x, ]))),
                     crs = pie_crs)
) %>% st_transform(4326)


pie_map <- ggplot() +
  geom_sf(data = counties, fill = "#FAEED9") +
  geom_sf(data = PORE_land, fill = "#D1E3B3") +
  # Camera sites with no coyote detections
  geom_point(data = no_coyote_sites, aes(longitude, latitude),
             shape = 4, size = 1.2, color = "grey40") +
  # Leader lines, true site dots, then pies on top
  geom_sf(data = leaders, aes(color = site_type),
          linewidth = 0.4, show.legend = FALSE) +
  geom_point(data = site_props, aes(longitude, latitude, color = site_type),
             size = 1, show.legend = FALSE) +
  geom_sf(data = pies, aes(fill = health_group, color = site_type),
          linewidth = 0.5) +
  coord_sf(crs = st_crs(4326),
           xlim = c(-123.05, -122.88),
           ylim = c(37.98, 38.1),
           expand = FALSE) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#BDE8FE")) +
  scale_x_continuous(breaks = c(-123, -122.9), name = "") +
  scale_y_continuous(breaks = c(38.0, 38.05), name = "") +
  scale_fill_manual(values = c(Mange = "#B2182B", Healthy = "white"),
                    labels = c("Mange", "Healthy")) +
  scale_color_manual(values = c("darkgreen", "#F27F0C"),
                     labels = c("Non-Rookery", "Rookery")) +
  guides(fill  = guide_legend(override.aes = list(color = "black")),
         color = guide_legend(override.aes = list(fill = "white"))) +
  labs(fill = "Coyote\ndetections", color = "Site type") +
  theme(legend.position = "inside",
        legend.position.inside = c(.18, .78),
        legend.box.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(linewidth = 2)) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(1, "cm"), width = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering())
pie_map

ggsave("output/map/mange_pies.png", pie_map,
       width = 5, height = 4, units = "in", dpi = 600)