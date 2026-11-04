##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 0X: Analyze and visualize scat metabarcoding data ###############
#-------------------------------------------------------------------------

#################
#Import data ####
#################

PR_scat <- read_csv("data/raw/PR_scat.csv") %>% 
  clean_names |> 
  mutate(date = dmy(collection_date),
         month = month(date),
         year = year(date),
         season = case_when(
           month %in% c(1,2,3) ~ "Winter (Jan-Mar)", 
           month %in% c(4:6) ~ "Spring (Apr-Jun)", 
           month %in% c(7:9) ~ "Summer (Jul-Sep)", 
           month %in% c(10:12) ~ "Fall (Oct-Dec)"),
         sample = sprintf("PR%03d", as.integer(gsub("PR ", "", sample_id))))


scat_data <- read_csv("data/raw/sequencing_12S.csv") |> 
  filter(Percentage_filter == "0.5") |> 
  filter(!Sample %in% c("PRB9", "PRB10"))

##################
# Data Clean #####
##################

# Identify defecator

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
    Sample %in% (scat_data |> 
                   filter(grepl("Canis latrans|Canis lupus", 
                                Best_match_references, 
                                ignore.case = TRUE)) |> 
                   pull(Sample)) ~ "Coyote",
    
    Sample %in% (scat_data |> 
                   filter(grepl("Lynx", 
                                Best_match_references, 
                                ignore.case = TRUE)) |> 
                   pull(Sample)) ~ "Bobcat",
    TRUE ~ NA_character_))


# Characterize and correct prey item species ID

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
  ~Best_match_references,    ~species,                 ~common_name,
  "Bos taurus",             "Bos taurus",              "Domestic cow",          
  "Callipepla gambelii",    "Callipepla californica",  "California quail",     
  "Callorhinus ursinus",    "Callorhinus ursinus",     "Northern fur seal",     
  "Catopuma temminckii",    "Human (contamination)",   "Human (contamination)", 
  "Cervus canadensis",      "Cervus canadensis",       "Elk",                   
  "Colaptes auratus",       "Colaptes auratus",        "Northern flicker",     
  "Cyanochen cyanoptera",   "Sea duck (unidentified)", "Sea duck",              
  "Engraulis mordax",       "Engraulis mordax",        "Northern anchovy",     
  "Fulmarus glacialis",     "Fulmarus glacialis",      "Northern fulmar",      
  "Gallus sonneratii",      "Gallus gallus",           "Domestic chicken",     
  "Gavia pacifica",         "Gavia spp.",              "Loon",                  
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


group_lookup <- tribble(
  ~common_name,                       ~group,                      ~megagroup,
  "California vole",                  "California vole",           "California vole",
  "Botta's pocket gopher",            "Botta's pocket gopher",     "Botta's pocket gopher",
  "Deer mouse",                       "Deer mouse",                "Other small mammal",
  "Brush rabbit",                     "Brush rabbit",              "Brush rabbit",
  "Northern elephant seal",           "Northern elephant seal",    "Elephant seal",
  "Domestic cow",                     "Domestic cow",              "Livestock/anthropogenic",
  "Mule deer",                        "Mule deer",                 "Native ungulate",
  "Elk",                              "Elk",                       "Native ungulate",
  "California sea lion",              "Other pinniped",            "Other marine",
  "Harbor seal",                      "Other pinniped",            "Other marine",
  "Northern fur seal",                "Other pinniped",            "Other marine",
  "Cormorant (Brandt's / Pelagic)",   "Cormorant",                 "Seabird",
  "Double-crested cormorant",         "Cormorant",                 "Seabird",
  "Brown pelican",                    "Other seabird",             "Seabird",
  "Loon",                             "Other seabird",             "Seabird",
  "Northern fulmar",                  "Other seabird",             "Seabird",
  "Storm-Petrel",                     "Other seabird",             "Seabird",
  "Gull",                             "Other seabird",             "Seabird",
  "Sea duck",                         "Other seabird",             "Seabird",
  "Northern anchovy",                 "Marine fish",               "Other marine",
  "California quail",                 "Landbird",                  "Other terrestrial",
  "European starling",                "Landbird",                  "Other terrestrial",
  "Passerine bird",                   "Landbird",                  "Other terrestrial",
  "Sparrow",                          "Landbird",                  "Other terrestrial",
  "Barn owl",                         "Landbird",                  "Other terrestrial",
  "Northern flicker",                 "Landbird",                  "Other terrestrial",
  "Black rat",                        "Other rodent",              "Other small mammal",
  "House mouse",                      "Other rodent",              "Other small mammal",
  "Muskrat",                          "Other rodent",              "Other small mammal",
  "Dusky-footed woodrat",             "Other rodent",              "Other small mammal",
  "American shrew mole",              "Mole/shrew",                "Other small mammal",
  "Northern broad-footed mole",       "Mole/shrew",                "Other small mammal",
  "Long-tailed weasel",               "Mesocarnivore",             "Other terrestrial",
  "Striped skunk",                    "Mesocarnivore",             "Other terrestrial",
  "Red fox",                          "Mesocarnivore",             "Other terrestrial",
  "Domestic chicken",                 "Other anthropogenic",       "Livestock/anthropogenic",
  "Domestic pig",                     "Other anthropogenic",       "Livestock/anthropogenic",
  "Domestic sheep",                   "Other anthropogenic",       "Livestock/anthropogenic",
) 
  
# Assemble clean dataframe for analysis + visualization 
scat_data_clean <- scat_data |> 
  left_join(defecator_df, by = join_by(Sample)) |> 
  drop_na(defecator) |> 
  mutate(Best_match_references = sub(",.*", "", Best_match_references)) |> 
  left_join(taxonomic_corrections, by = join_by(Best_match_references)) |> 
  select(Sample, defecator, species, common_name) |> 
  mutate(sample = if_else(Sample == "PR60", "PR060", Sample)) |> 
  drop_na(common_name) |> #Drop coyote and bobcat sequences
  filter(!common_name %in% 
           c("Bobcat (filter)", "Human (contamination)", "Coyote (filter)")) |> 
  distinct(sample, defecator, species, common_name) |>   # one row per sample x prey item
  left_join(PR_scat |> select(sample, season, year, date, latitude, longitude), by = "sample") |> 
  left_join(group_lookup)


################################################
# Data summarization and diet visualization ####
################################################

#Summaries for paper ####
scat_data_clean |> 
  distinct(sample, defecator) |> 
  count(defecator, name = "n_scats")

scat_data_clean |> 
  distinct(sample, defecator, year) |> 
  count(defecator, year, name = "n_scats")

scat_data_clean |> 
  distinct(sample, defecator, season) |> 
  count(defecator, season, name = "n_scats")


# Calculate overall weighted proportion of occurrence (wPOO) ####

n_scats_overall <- scat_data_clean |> 
  distinct(sample, defecator) |> 
  count(defecator, name = "n_scats")

items_per_scat_overall <- scat_data_clean |> 
  distinct(sample, defecator, common_name) |> 
  count(sample, defecator, name = "n_items")

wpoo_overall <- scat_data_clean |> 
  distinct(sample, defecator, common_name) |> 
  left_join(items_per_scat_overall, by = c("sample", "defecator")) |> 
  left_join(group_lookup) |> 
  mutate(weight = 1 / n_items) |> 
  group_by(defecator, group) |> 
  summarise(sum_weight = sum(weight), .groups = "drop") |> 
  left_join(n_scats_overall, by = "defecator") |> 
  mutate(wPOO_pct = 100 * sum_weight / n_scats)


# Overall wPOO coyote vs bobcat plot ####

group_order <- c("California vole", "Botta's pocket gopher", "Deer mouse",
                 "Other rodent", "Mole/shrew", "Brush rabbit", "Mesocarnivore",
                 "Mule deer","Elk","Domestic cow", "Other anthropogenic",
                 "Landbird", "Cormorant","Other seabird",
                 "Northern elephant seal","Other pinniped","Marine fish")

wpoo_plot_df <- wpoo_overall |> 
  complete(defecator, group = group_order,
           fill = list(sum_weight = 0, wPOO_pct = 0)) |> 
  mutate(group = factor(group, levels = rev(group_order)),
         wPOO_signed = if_else(defecator == "Bobcat", -wPOO_pct, wPOO_pct))


coyote_bobcat_diet_plot <- ggplot(wpoo_plot_df, aes(x=group, y = wPOO_signed, fill = defecator)) +
  geom_col(width = 0.8, linewidth = 0.2) +
  coord_flip(ylim = c(-55, 55)) +
  scale_y_continuous(
    breaks = c( -50, -25, 0, 25, 50),
    labels = c("50","25", "0", "25", "50"),
    name = "Weighted Percentage of Occurrence") +
  scale_fill_manual(
    values = c("Bobcat" = "#D55E00", "Coyote" = "#0072B2")) +
  labs(x = NULL, fill = NULL) +
  theme_custom()+
  theme(panel.grid.major = element_line(color = "grey90", 
                                        linewidth = 0.3),
        legend.box.background = element_rect(linewidth=1),
        legend.position = "inside",
        legend.position.inside = c(.8, .7)
        )
coyote_bobcat_diet_plot

ggsave("output/extra_plots/coyote_bobcat_diet_plot.png", coyote_bobcat_diet_plot, 
       width=5, height=6, units="in", dpi=600)

# Calculate seasonal weighted proportion of occurrence (wPOO) ####

n_scats_season <- scat_data_clean |> 
  distinct(sample, defecator, season) |> 
  count(defecator, season, name = "n_scats")

items_per_scat_season <- scat_data_clean |> 
  distinct(sample, defecator, season, common_name) |> 
  count(sample, defecator, season, name = "n_items")

wpoo_season <- scat_data_clean |> 
  distinct(sample, defecator, season, common_name, megagroup) |> 
  left_join(items_per_scat_season, by = c("sample", "defecator", "season")) |> 
  mutate(weight = 1 / n_items) |> 
  group_by(defecator, season, megagroup) |> 
  summarise(sum_weight = sum(weight), .groups = "drop") |> 
  left_join(n_scats_season, by = c("defecator", "season")) |> 
  mutate(wPOO_pct = 100 * sum_weight / n_scats)


season_levels <- c("Winter (Jan-Mar)", "Spring (Apr-Jun)",
                   "Summer (Jul-Sep)", "Fall (Oct-Dec)")

megagroup_cols <- c(
  "Elephant seal"         = "#08306B",
  "Seabird"               = "#2171B5",
  "Other marine"          = "#9ECAE1",
  "Native ungulate"       = "#8C6D31",
  "Livestock/anthropogenic" = "#BD9E39",
  "California vole"       = "#B35806",
  "Botta's pocket gopher" = "#E08214",
  "Other small mammal"    = "#FDB863",
  "Brush rabbit"          = "#7F7F7F",
  "Other terrestrial"     = "#D9D9D9"
)

megagroup_cols <- c(
  "California vole"         = "#A34509",
  "Botta's pocket gopher"   = "#D76D13",
  "Other small mammal"      = "#FBAA55",
  "Brush rabbit"            = "#7C9A5E",
  "Native ungulate"         = "darkgreen", 
  "Other terrestrial"       = "#C8C8C8",
  "Livestock/anthropogenic" = "pink", 
  "Other marine"            = "#9ECAE1",
  "Seabird"                 = "#2B7BBA",
  "Elephant seal"           = "#08306B"
)

megagroup_levels <- c( "California vole", "Botta's pocket gopher", 
                       "Other small mammal", "Brush rabbit", "Native ungulate",
                       "Other terrestrial", "Livestock/anthropogenic",
                       "Other marine","Seabird","Elephant seal")

plot_df <- wpoo_season |>
  mutate(
    season    = factor(season, levels = season_levels),
    megagroup = factor(megagroup, levels = megagroup_levels)
  )

# sample sizes to print above each bar
n_df <- plot_df |>
  distinct(defecator, season, n_scats)

carnivore_diet_seasonality_plot <- ggplot(plot_df, 
                                          aes(x = season, y = wPOO_pct, 
                    fill = megagroup, color=megagroup)) +
  geom_col(width = 0.75) +
  geom_text(
    data = n_df,
    aes(x = season, y = 102, label = paste0("n = ", n_scats)),
    inherit.aes = FALSE, size = 3.5, vjust = 0
  ) +
  facet_wrap(~ defecator, ncol=1) +
  scale_fill_manual(values = megagroup_cols) +
  scale_color_manual(values = megagroup_cols) +
  scale_x_discrete(labels = c("Winter\n(Jan-Mar)", "Spring\n(Apr-Jun)",
                              "Summer\n(Jul-Sep)", "Fall\n(Oct-Dec)")) +
  scale_y_continuous(limits = c(-5, 109), 
                     breaks = seq(0, 100, 25),
                     expand = FALSE) +
  labs(x = NULL, y = "Weighted Percentage of Occurrence (wPOO)", 
       fill = NULL, color= NULL) +
  theme_custom() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 12),
    axis.title.y       = element_text(face = "bold")
  )
carnivore_diet_seasonality_plot

ggsave("output/extra_plots/carnivore_diet_seasonality.png", carnivore_diet_seasonality_plot, 
       width=7, height=6, units="in", dpi=600)

# Coyote marine seasonal wPOO plot ####

coyote_wpoo_season <- scat_data_clean |> 
  distinct(sample, defecator, season, common_name, group) |> 
  left_join(items_per_scat_season, by = c("sample", "defecator", "season")) |> 
  mutate(weight = 1 / n_items) |> 
  group_by(defecator, season, group) |> 
  summarise(sum_weight = sum(weight), .groups = "drop") |> 
  left_join(n_scats_season, by = c("defecator", "season")) |> 
  mutate(wPOO_pct = 100 * sum_weight / n_scats)

group_cols <- c(
  "Northern elephant seal" = "#440154",
  "Other pinniped" = "#3b528b",
  "Cormorant" = "#21918c",
  "Other seabird" = "#5ec962",
  "Marine fish" = "#fde725") 
   

coyote_marine_wpoo <- coyote_wpoo_season |> 
  filter(defecator == "Coyote") |> 
  filter(group %in% c("Northern elephant seal", "Cormorant", "Marine fish", 
                      "Other pinniped", "Other seabird")) |> 
  mutate(season = factor(season, levels = c("Winter (Jan-Mar)", 
                                            "Spring (Apr-Jun)",
                                            "Summer (Jul-Sep)", 
                                            "Fall (Oct-Dec)")),
         group = factor(group, levels = c("Marine fish", 
                                          "Other seabird",
                                          "Cormorant", 
                                          "Other pinniped",
                                          "Northern elephant seal")))


coyote_marine_seasonality <- ggplot(coyote_marine_wpoo, aes(x=season, y=wPOO_pct, fill=group))+
  geom_col(position = "stack") +
  scale_x_discrete(labels = c("Winter\n(Jan-Mar)", 
                            "Spring\n(Apr-Jun)",
                            "Summer\n(Jul-Sep)", 
                            "Fall\n(Oct-Dec)"))+
  scale_y_continuous(breaks =  seq(0, 25, 5))+
  labs(x = NULL, y = "Weighted Proportion of Occurrence (wPOO)", 
       fill = "Prey item")+
  scale_fill_manual(values = group_cols)+
  theme_custom()
coyote_marine_seasonality

ggsave("output/extra_plots/coyote_marine_seasonality.png", coyote_marine_seasonality, 
       width=7, height=5, units="in", dpi=600)


#########################################
# Analyze diet diversity using iNEXT ####
#########################################

build_incidence <- function(df, group_name) {
  df %>%
    filter(defecator == group_name) %>%
    mutate(present = 1L) %>%
    pivot_wider(
      id_cols = species,
      names_from = sample,
      values_from = present,
      values_fill = 0L
    ) %>%
    column_to_rownames("species") %>%
    as.matrix()
}

incidence_list <- list(
  Bobcat = build_incidence(scat_data_clean, "Bobcat"),
  Coyote = build_incidence(scat_data_clean, "Coyote"))


out <- iNEXT(
  incidence_list,
  q = c(0, 1, 2),
  datatype = "incidence_raw",
  knots = 40
)


rarefaction_plot <- ggiNEXT(out, type = 1, facet.var = "Order.q") +
  labs(x = "Number of scats", y = "Diversity") +
  facet_wrap(facets = "Order.q", ncol = 1, scales = "free_y",
             labeller = labeller(Order.q = c(
               "0" = "Species richness (q = 0)",
               "1" = "Common-taxa diversity (q = 1)",
               "2" = "Dominant-taxa diversity (q = 2)"))) +
  theme_custom()+
  theme(panel.grid.major = element_line(color = "grey90", 
                                        linewidth = 0.3))
rarefaction_plot

ggsave("output/extra_plots/rarefaction_plot.png", rarefaction_plot, 
       width=5, height=6, units="in", dpi=600)


estimateD(
  incidence_list,
  datatype = "incidence_raw",
  base = "coverage",   # standardize by sample completeness, not raw n
  level = NULL          # defaults to the largest coverage shared by both groups
)


