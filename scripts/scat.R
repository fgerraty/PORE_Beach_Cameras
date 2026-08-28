##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 0X: Analyze and visualize scat metabarcoding data ###############
#-------------------------------------------------------------------------

PR_scat <- read_csv("data/raw/PR_scat.csv") %>% 
  clean_names |> 
  mutate(date = dmy(collection_date),
         month = month(date),
         season = case_when(
           month %in% c(1,2,3) ~ "Winter (Jan-Mar)", 
           month %in% c(4:6) ~ "Spring (Apr-Jun)", 
           month %in% c(7:9) ~ "Summer (Jul-Sep)", 
           month %in% c(10:12) ~ "Fall (Oct-Dec)"),
         
         sample = sprintf("PR%03d", as.integer(gsub("PR ", "", sample_id))))


scat_data <- read_csv("data/raw/sequencing_12S.csv") |> 
  filter(Percentage_filter == "0.5") |> 
  filter(!Sample %in% c("PRB9", "PRB10"))

# Identify defecators

coyote_scat_list <- scat_data |> 
  filter(grepl("Canis latrans|Canis lupus", 
             Best_match_references, ignore.case = TRUE)) |> 
  select(Sample) |> 
  unique()

bobcat_scat_list<- scat_data %>% 
  filter(grepl("Lynx", 
               Best_match_references, ignore.case = TRUE)) %>% 
  select(Sample) %>% 
  unique()


defecator_df <- scat_data |> 
  distinct(Sample) |> 
  mutate(defecator = case_when(
    Sample %in% (scat_data |> filter(grepl("Canis latrans|Canis lupus", Best_match_references, ignore.case = TRUE)) |> pull(Sample)) ~ "Coyote",
    Sample %in% (scat_data |> filter(grepl("Lynx", Best_match_references, ignore.case = TRUE)) |> pull(Sample)) ~ "Bobcat",
    TRUE ~ NA_character_
  ))


#Characterize and correct prey item species ID

coyote_all_prey <- scat_data %>% 
  filter(Sample %in% coyote_scat_list$Sample) %>% 
  group_by(Best_match_references) %>% 
  summarise(n_scats = length(unique(Sample))) %>% 
  filter(!grepl("Canis latrans|Canis lupus", 
                Best_match_references, ignore.case = TRUE)) %>% 
  mutate(Best_match_references = sub(",.*", "", Best_match_references)) %>% 
  group_by(Best_match_references) %>%
  summarise(n_scats = sum(n_scats)) %>% 
  mutate(Best_match_references = 
           fct_reorder(Best_match_references, n_scats, .desc = TRUE))


bobcat_all_prey <- scat_data %>% 
  filter(Sample %in% bobcat_scat_list$Sample) %>% 
  group_by(Best_match_references) %>% 
  summarise(n_scats = length(unique(Sample))) %>% 
  filter(!grepl("Lynx", 
                Best_match_references, ignore.case = TRUE)) %>% 
  mutate(Best_match_references = sub(",.*", "", Best_match_references)) %>% 
  group_by(Best_match_references) %>%
  summarise(n_scats = sum(n_scats)) %>% 
  mutate(Best_match_references = 
           fct_reorder(Best_match_references, n_scats, .desc = TRUE))

taxonomic_corrections <- tribble(
  ~Best_match_references,    ~local_prey ,              ~common_name, 
  "Bos taurus",             "Bos taurus",              "Domestic cow",
  "Callipepla gambelii",    "Callipepla californica",  "California quail ",
  "Callorhinus ursinus",    "Callorhinus ursinus",     "Northern fur seal",   
  "Catopuma temminckii",    "Human (contamination)",   "Human (contamination)",
  "Cervus canadensis",      "Cervus canadensis",       "Elk",
  "Colaptes auratus",       "Colaptes auratus",        "Northern flicker",
  "Cyanochen cyanoptera",   "Sea duck (unidentified)", "Sea duck",
  "Engraulis mordax",       "Engraulis mordax",        "Northern anchovy",
  "Fulmarus glacialis",     "Fulmarus glacialis",      "Northern fulmar",
  "Gallus sonneratii",      "Gallus gallus",           "Domestic chicken",
  "Gavia pacifica",         "Gavia spp.",             "Loon",
  "Homo sapiens",           "Human (contamination)",   "Human (contamination)",
  "Larus smithsonianus",    "Larus spp.",              "Gull",
  "Melospiza melodia",      "Sparrow (unidentified)",  "Sparrow",
  "Mephitis mephitis",      "Mephitis mephitis",       "Striped skunk",
  "Microtus californicus",  "Microtus californicus",   "California vole",
  "Microtus richardsoni",   "Microtus californicus",   "California vole",
  "Microtus townsendii",    "Microtus californicus",   "California vole",
  "Mirounga leonina",       "Mirounga angustirostris", "Northern elephant seal",
  "Mus musculus",           "Mus musculus",            "House mouse", 
  "Mustela frenata",        "Mustela frenata",         "Long-tailed weasel",
  "Neotoma fuscipes",       "Neotoma fuscipes",        "Dusky-footed woodrat",
  "Neurotrichus gibbsii",   "Neurotrichus gibbsii",    "American shrew mole", 
  "Oceanodroma matsudairae","Oceanodroma spp.",        "Storm-Petrel",
  "Ondatra zibethicus",     "Ondatra zibethicus",      "Muskrat",
  "Odocoileus sp",          "Odocoileus hemionus",     "Mule deer",
  "Onychomys torridus",     "Peromyscus spp.",         "Deer mouse",
  "Ovis orientalis",        "Ovis aries",              "Domestic sheep",
  "Pan troglodytes",        "Human (contamination)",   "Human (contamination)",
  "Panthera tigris",        "Bobcat (filter)",         "Bobcat (filter)", 
  "Pelecanus thagus",       "Pelecanus occidentalis",  "Brown pelican",
  "Peromyscus attwateri",   "Peromyscus spp.",         "Deer mouse",
  "Peromyscus keeni",       "Peromyscus spp.",         "Deer mouse",
  "Phalacrocorax fuscescens", "Nannopterum auritum",   "Double-crested cormorant",
  "Phalacrocorax urile",    "Phalacrocorax spp.",      "Cormorant (Brandt's / Pelagic)",
  "Puma yagouaroundi",      "Bobcat (filter)",         "Bobcat (filter)", 
  "Pusa sibirica",          "Phoca vitulina",          "Harbor seal",
  "Rattus tanezumi",        "Rattus rattus",           "Black rat",
  "Scapanus latimanus",     "Scapanus latimanus",      "Northern broad-footed mole", 
  "Speothos venaticus",     "Coyote (filter)",         "Coyote (filter)", #inquire
  "Spinus pinus",           "Passeriformes",           "Passerine bird",
  "Sturnus vulgaris",       "Sturnus vulgaris",        "European starling",
  "Sus scrofa",             "Sus scrofa",              "Domestic pig", 
  "Sylvilagus bachmani",    "Sylvilagus bachmani",     "Brush rabbit", 
  "Thomomys bottae",        "Thomomys bottae",         "Botta's pocket gopher",
  "Tyto alba",              "Tyto alba",               "Barn owl", 
  "Vulpes vulpes",          "Vulpes vulpes",           "Red fox", #INQUIRE!!!
  "Zalophus japonicus",     "Zalophus californianus",  "California sea lion")


scat_data_clean <- scat_data |> 
  left_join(defecator_df, by = join_by(Sample)) |> 
  drop_na(defecator) |> 
  mutate(Best_match_references = sub(",.*", "", Best_match_references)) |> 
  left_join(taxonomic_corrections, by = join_by(Best_match_references)) |> 
  select(Sample, defecator, local_prey, common_name) |> 
  mutate(sample = if_else(Sample == "PR60", "PR060", Sample)) |> 
  drop_na(common_name) |> #Drop coyote and bobcat sequences
  filter(!common_name %in% 
           c("Bobcat (filter)", "Human (contamination)", "Coyote (filter)")) |> 
  distinct(sample, defecator, local_prey, common_name) |>   # one row per sample x prey item
  left_join(PR_scat |> select(sample, season), by = "sample")


#Summarize scat data

n_scats_season <- scat_data_clean |> 
  distinct(sample, defecator, season) |> 
  count(defecator, season, name = "n_scats")


foo_season <- scat_data_clean |> 
  count(defecator, season, common_name, name = "n_present") |> 
  left_join(n_scats_season, by = c("defecator", "season")) |> 
  mutate(FOO_pct = 100 * n_present / n_scats)


n_scats_overall <- scat_data_clean |> 
  distinct(sample, defecator) |> 
  count(defecator, name = "n_scats")

foo_overall <- scat_data_clean |> 
  count(defecator, common_name, name = "n_present") |> 
  left_join(n_scats_overall, by = "defecator") |> 
  mutate(FOO_pct = 100 * n_present / n_scats)  






items_per_scat_overall <- scat_data_clean |> 
  distinct(sample, defecator, common_name) |> 
  count(sample, defecator, name = "n_items")

wpoo_overall <- scat_data_clean |> 
  distinct(sample, defecator, common_name) |> 
  left_join(items_per_scat_overall, by = c("sample", "defecator")) |> 
  mutate(weight = 1 / n_items) |> 
  group_by(defecator, common_name) |> 
  summarise(sum_weight = sum(weight), .groups = "drop") |> 
  left_join(n_scats_overall, by = "defecator") |> 
  mutate(wPOO_pct = 100 * sum_weight / n_scats)

# --- wPOO by season ---
items_per_scat_season <- scat_data_clean |> 
  distinct(sample, defecator, season, common_name) |> 
  count(sample, defecator, season, name = "n_items")

wpoo_season <- scat_data_clean |> 
  distinct(sample, defecator, season, common_name) |> 
  left_join(items_per_scat_season, by = c("sample", "defecator", "season")) |> 
  mutate(weight = 1 / n_items) |> 
  group_by(defecator, season, common_name) |> 
  summarise(sum_weight = sum(weight), .groups = "drop") |> 
  left_join(n_scats_season, by = c("defecator", "season")) |> 
  mutate(wPOO_pct = 100 * sum_weight / n_scats)

# --- visualize: overall ---
wpoo_overall |> 
  group_by(defecator) |> 
#  slice_max(wPOO_pct, n = 8) |> 
  ungroup() |> 
  ggplot(aes(x = reorder(common_name, wPOO_pct), y = wPOO_pct, fill = defecator)) +
  geom_col(position = position_dodge(width = 0.75)) +
  coord_flip() +
  labs(x = NULL, y = "wPOO (%)", title = "Overall diet composition (weighted % occurrence)") +
  theme_minimal()

# --- visualize: by season ---
wpoo_season |> 
  group_by(defecator, season) |> 
  slice_max(wPOO_pct, n = 6) |> 
  ungroup() |> 
  ggplot(aes(x = reorder(common_name, wPOO_pct), y = wPOO_pct, fill = defecator)) +
  geom_col(position = position_dodge(width = 0.75)) +
  coord_flip() +
  facet_wrap(~season, scales = "free_y") +
  labs(x = NULL, y = "wPOO (%)", title = "Diet composition by season (weighted % occurrence)") +
  theme_minimal()




top_items_coyote <- wpoo_season |> 
  filter(defecator == "Coyote") |> 
  group_by(common_name) |> 
  summarise(max_wPOO = max(wPOO_pct), .groups = "drop") |> 
  slice_max(max_wPOO, n = 8, with_ties = FALSE) |> 
  pull(common_name)

coyote_stacked <- wpoo_season |> 
  filter(defecator == "Coyote") |> 
  mutate(common_name = if_else(common_name %in% top_items_coyote, common_name, "Other")) |> 
  group_by(season, common_name) |> 
  summarise(wPOO_pct = sum(wPOO_pct), .groups = "drop")

ggplot(coyote_stacked, aes(x = season, y = wPOO_pct, fill = common_name)) +
  geom_col(position = "stack") +
  labs(x = NULL, y = "wPOO (%)", fill = "Prey item",
       title = "Coyote diet composition by season (wPOO)") +
  theme_minimal()
