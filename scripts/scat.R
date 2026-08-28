##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 0X: Analyze and visualize scat metabarcoding data ###############
#-------------------------------------------------------------------------

scat_data <- read_csv("data/raw/sequencing_12S.csv") |> 
  filter(Percentage_filter == "0.5")

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


#Characterize coyote prey

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
  ~Best_match_reference,    ~local_prey ,              ~common_name, 
  "Bos taurus",             "Bos taurus",              "Domestic cow",
  "Callipepla gambelii",    "Callipepla californica",  "California quail ",
  "Callorhinus ursinus",    "Callorhinus ursinus",     "Northern fur seal",   
  "Catopuma temminckii",    "Human (contamination)",   "Human (contamination)",
  "Cervus canadensis",      "Cervus canadensis",       "Elk",
  "Cyanochen cyanoptera",   "Sea duck (unidentified)", "Sea duck",
  "Engraulis mordax",       "Engraulis mordax",        "Northern anchovy",
  "Fulmarus glacialis",     "Fulmarus glacialis",      "Northern fulmar",
  "Gallus sonneratii",      "Gallus gallus",           "Domestic chicken",
  "Gavia pacifica",         "Gavia spp.",             "Loon",
  "Homo sapiens",           "Human (contamination)",   "Human (contamination)",
  "Larus smithsonianus",    "Larus spp.",              "Gull",
  "Lynx rufus", 
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
