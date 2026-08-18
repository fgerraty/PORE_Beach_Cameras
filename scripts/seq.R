

PR_scat <- read_csv("data/raw/PR_scat.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy(collection_date),
         month = month(date),
         season = case_when(
           month %in% c(1,2,3) ~ "Winter (Jan-Mar)", 
           month %in% c(4:6) ~ "Spring (Apr-Jun)", 
           month %in% c(7:9) ~ "Summer (Jul-Sep)", 
           month %in% c(10:12) ~ "Fall (Oct-Dec)", 
         )) 


sequencing_12S <- read_csv("data/raw/sequencing_12S.csv") %>% 
  filter(Percentage_filter == "0.5")


coyote_scats_seq<- sequencing_12S %>% 
  filter(grepl("Canis latrans|Canis lupus", 
               Best_match_references, ignore.case = TRUE)) %>% 
  select(Sample) %>% 
  unique()


coyote_scats <- PR_scat %>% 
  mutate(Sample = sprintf("PR%03d", as.integer(gsub("PR ", "", sample_id)))) %>% 
  filter(Sample %in% coyote_scats_seq$Sample)


seal_scat <- sequencing_12S %>% 
  filter(Sample %in% coyote_scats_seq$Sample) %>% 
  filter(Best_match_references == "Mirounga leonina, Mirounga angustirostris") %>% 
  select(Sample) %>% 
  unique() %>% 
  mutate(seal_present = TRUE)

seasonal_summary <- coyote_scats %>% 
  left_join(seal_scat) %>% 
  group_by(season) %>% 
  summarise(n_scats = n(), 
            n_seal_present = sum(seal_present, na.rm = TRUE),
            n_seal_absent = n_scats - n_seal_present) %>% 
  pivot_longer(cols = c("n_seal_present", "n_seal_absent"),
               names_to = "scat_type", 
               values_to = "n") %>% 
  mutate(season = factor(season, levels =c("Winter (Jan-Mar)", "Spring (Apr-Jun)", "Summer (Jul-Sep)", "Fall (Oct-Dec)")))
  

ggplot(seasonal_summary, aes(x=season, y=n, fill = scat_type))+
  geom_bar(stat = "identity")+
  labs (x="Season", y="# coyote scats", fill = "Elephant seal\npresence")+
  scale_fill_manual(values = c("#608cf7", "#e89a50"), 
                    labels = c("Absent", "Present"))+
  theme_custom()
  

ggsave("output/scat_seal_presence.png", 
       width = 7, height = 4, units = "in", dpi = 600)



coyote_all_prey <- sequencing_12S %>% 
  filter(Sample %in% coyote_scats_seq$Sample) %>% 
  group_by(Best_match_references) %>% 
  summarise(n_scats = length(unique(Sample))) %>% 
  filter(!grepl("Canis latrans|Canis lupus", 
               Best_match_references, ignore.case = TRUE)) %>% 
  mutate(Best_match_references = sub(",.*", "", Best_match_references)) %>% 
  group_by(Best_match_references) %>%                          # re-group after clipping
  summarise(n_scats = sum(n_scats)) %>%                        # collapse any duplicates
  mutate(Best_match_references = 
           fct_reorder(Best_match_references, n_scats, .desc = TRUE))


ggplot(coyote_all_prey, aes(x=Best_match_references, y=n_scats))+
    geom_bar(stat = "identity")+
  labs(x="Prey")+
  theme_custom()+
  theme(axis.text.x = element_text(face = "bold", angle = 60, hjust = 1, vjust = 1))

ggsave("output/scat_prey.png", 
       width = 7.5, height = 5, units = "in", dpi = 600)








bobcat_scats_seq<- sequencing_12S %>% 
  filter(grepl("Lynx", 
               Best_match_references, ignore.case = TRUE)) %>% 
  select(Sample) %>% 
  unique()


bobcat_all_prey <- sequencing_12S %>% 
  filter(Sample %in% bobcat_scats_seq$Sample) #%>% 
  group_by(Best_match_references) %>% 
  summarise(n_scats = length(unique(Sample))) %>% 
  filter(!grepl("Lynx", 
                Best_match_references, ignore.case = TRUE)) %>% 
  mutate(Best_match_references = sub(",.*", "", Best_match_references)) %>% 
  group_by(Best_match_references) %>%                          # re-group after clipping
  summarise(n_scats = sum(n_scats)) %>%                        # collapse any duplicates
  mutate(Best_match_references = 
           fct_reorder(Best_match_references, n_scats, .desc = TRUE))


ggplot(bobcat_all_prey, aes(x=Best_match_references, y=n_scats))+
  geom_bar(stat = "identity")+
  labs(x="Prey")+
  theme_custom()+
  theme(axis.text.x = element_text(face = "bold", angle = 60, hjust = 1, vjust = 1))
