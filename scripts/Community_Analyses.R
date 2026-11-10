
#####################################
# Seasonal Community Shifts #########
#####################################
set.seed(999)
PORE_sites <- read_csv("data/raw/PORE_sites.csv") 

summarized_detection_rate_monthly_wide <- read_csv("data/processed/summarized_detection_rate_monthly_wide.csv") 

# Data Prep for NMDS

species_cols <- c(
  "american_badger", "bobcat", "coyote", "mule_deer",
  "north_american_river_otter", "northern_raccoon", "striped_skunk",
  "brush_rabbit", "black_tailed_jackrabbit", "elk")



all_data <- summarized_detection_rate_monthly_wide  |> 
  group_by(placename) |> 
  mutate(site_level_sampling_days = sum(sampling_days)) |> 
  ungroup() |> 
  filter(site_level_sampling_days > 250) |> 
  filter(sampling_days > 10) |> 
  left_join(PORE_sites) |> 
  mutate(
    season = case_when(
      month %in% c("01", "02", "03") ~ "Winter",
      month %in% c("04", "05", "06") ~ "Spring",
      month %in% c("07", "08", "09") ~ "Summer",
      month %in% c("10", "11", "12") ~ "Fall"),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    year = factor(year),
    placename = factor(placename)) |> 
  filter(rowSums(across(all_of(species_cols))) > 0) 




seal_data <- summarized_detection_rate_monthly_wide  |> 
  group_by(placename) |> 
  mutate(site_level_sampling_days = sum(sampling_days)) |> 
  ungroup() |> 
  filter(site_level_sampling_days > 250) |> 
  filter(sampling_days > 10) |> 
  left_join(PORE_sites) |> 
  filter(site_type == "seal") |> 
  mutate(
    season = case_when(
      month %in% c("01", "02", "03") ~ "Winter",
      month %in% c("04", "05", "06") ~ "Spring",
      month %in% c("07", "08", "09") ~ "Summer",
      month %in% c("10", "11", "12") ~ "Fall"),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    year = factor(year),
    placename = factor(placename)) |> 
  filter(rowSums(across(all_of(species_cols))) > 0) 


non_seal_data <- summarized_detection_rate_monthly_wide  |> 
  group_by(placename) |> 
  mutate(site_level_sampling_days = sum(sampling_days)) |> 
  ungroup() |> 
  filter(site_level_sampling_days > 250) |> 
  filter(sampling_days > 10) |> 
  left_join(PORE_sites) |> 
  filter(site_type == "non-seal") |> 
  mutate(
    season = case_when(
      month %in% c("01", "02", "03") ~ "Winter",
      month %in% c("04", "05", "06") ~ "Spring",
      month %in% c("07", "08", "09") ~ "Summer",
      month %in% c("10", "11", "12") ~ "Fall"),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    year = factor(year),
    placename = factor(placename)) |> 
  filter(rowSums(across(all_of(species_cols))) > 0)


#How many site-months filtered out from original dataset?
nrow(summarized_detection_rate_monthly_wide) - (nrow(seal_data) + nrow(non_seal_data))

#Community dataset, square-root transformed
all_comm <- vegdist(all_data %>%
  select(all_of(species_cols))) 

seal_comm <- vegdist(seal_data %>%
  select(all_of(species_cols)))

non_seal_comm <- non_seal_data |> 
  select(all_of(species_cols)) %>%
  mutate(across(everything(), sqrt))


#NMDS

all_nmds <- metaMDS(all_comm, distance = "bray", k = 3, trymax = 2000, autotransform = FALSE)
all_nmds$stress   # < 0.2 is generally acceptable; < 0.1 is good


seal_nmds <- metaMDS(seal_comm, distance = "bray", k = 2, trymax = 1000, autotransform = FALSE)
seal_nmds$stress   # < 0.2 is generally acceptable; < 0.1 is good
stressplot(seal_nmds)

non_seal_nmds <-metaMDS(non_seal_comm, distance = "bray", k = 2, trymax = 1000, autotransform = FALSE)
non_seal_nmds$stress   # < 0.2 is generally acceptable; < 0.1 is good
stressplot(non_seal_nmds)

# --- 3. PERMANOVA (restricted within site) + dispersion ----------------------

perm <- how(nperm = 999, blocks = all_data$placename) 

adonis2(all_comm ~ site_type*season, data = all_data, method = "bray",
        permutations = perm, by="terms")

perm <- how(nperm = 999, blocks = seal_data$placename) 

adonis2(seal_comm ~ season, data = seal_data, method = "bray",
        permutations = perm, by = "margin")

bd <- betadisper(vegdist(seal_comm, method = "bray"), seal_data$season)
anova(bd)
permutest(bd, permutations = 999)


perm <- how(nperm = 999, blocks = non_seal_data$placename) 

adonis2(non_seal_comm ~ year + season, data = non_seal_data, method = "bray",
        permutations = perm, by = "margin") 

bd <- betadisper(vegdist(non_seal_comm, method = "bray"), non_seal_data$season)
anova(bd)
permutest(bd, permutations = 999)


# --- 4. Species vectors ------------------------------------------------------

seal_comm_ef <- seal_comm[, colSums(seal_comm) > 0]

ef1 <- envfit(seal_nmds, seal_comm, permutations = 999, strata = seal_data$placename)
ef1


spp_vec1 <- as_tibble(scores(ef1, display = "vectors"), rownames = "species") %>%
  mutate(r2 = ef1$vectors$r, p = ef1$vectors$pvals) %>%
  filter(p < 0.05)

seal_site_scores <- as_tibble(scores(seal_nmds, display = "species")) %>%
  bind_cols(seal_data %>% select(placename, year, season, site_type)) |> 
  arrange(placename, year, season)



non_seal_comm_ef <- non_seal_comm[, colSums(non_seal_comm) > 0]

ef2 <- envfit(non_seal_nmds, non_seal_comm_ef, permutations = 999, strata = non_seal_data$placename)
ef2

spp_vec2 <- as_tibble(scores(ef2, display = "vectors"), rownames = "species") %>%
  mutate(r2 = ef2$vectors$r, p = ef2$vectors$pvals) %>%
  filter(p < 0.05)

non_seal_site_scores <- as_tibble(scores(non_seal_nmds, display = "sites")) %>%
  bind_cols(non_seal_data %>% select(placename, year, season, site_type))











ggplot(seal_site_scores, aes(NMDS1, NMDS2, colour = season, fill = season)) +
  stat_ellipse(geom = "polygon", alpha = 0.12, colour = NA, level = 0.68) +
  geom_point(size = 2, alpha = 0.7) +
  geom_segment(data = spp_vec1, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               inherit.aes = FALSE, arrow = arrow(length = unit(0.2, "cm")),
               colour = "grey30") +
  geom_text(data = spp_vec1, aes(NMDS1, NMDS2, label = species),
            inherit.aes = FALSE, colour = "grey30", size = 3, vjust = -0.5) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1,
           label = paste0("Stress = ", round(seal_nmds$stress, 3))) +
  scale_colour_viridis_d(end = 0.9) +
  scale_fill_viridis_d(end = 0.9) +
  theme_classic() +
  labs(colour = "Season", fill = "Season")



ggplot(non_seal_site_scores, aes(NMDS1, NMDS2, colour = season, fill = season)) +
  stat_ellipse(geom = "polygon", alpha = 0.12, colour = NA, level = 0.68) +
  geom_point(size = 2, alpha = 0.7) +
  geom_segment(data = spp_vec2, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               inherit.aes = FALSE, arrow = arrow(length = unit(0.2, "cm")),
               colour = "grey30") +
  geom_text(data = spp_vec2, aes(NMDS1, NMDS2, label = species),
            inherit.aes = FALSE, colour = "grey30", size = 3, vjust = -0.5) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -1,
           label = paste0("Stress = ", round(non_seal_nmds$stress, 3))) +
  scale_colour_viridis_d(end = 0.9) +
  scale_fill_viridis_d(end = 0.9) +
  theme_classic() +
  labs(colour = "Season", fill = "Season")
