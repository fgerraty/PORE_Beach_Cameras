##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 0X: Describe Wildlife Diel Activity Patterns  ###################
#-------------------------------------------------------------------------

################################################
# Prepare Data for Diel Activity Analyses ######
################################################

#Import clean deployment data
deployments <- read.csv("data/processed/deployments.csv") %>% 
  # Convert columns to date-times 
  mutate(start_date = ymd_hms(start_date),
         end_date = ymd_hms(end_date)) 

#Import clean sequence data
sequences <- read.csv("data/processed/sequences.csv") %>% 
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time))

#Create coyote dataframe
coyotes <- sequences %>% 
  filter(common_name == "Coyote") %>% 
  mutate(start = floor_date(start_time, unit = "hour")) %>% 
  select(placename, start) %>% 
  mutate(detection = TRUE)

#Create dataframe of all sampling hours per site, which we will later populate with detections
sampling_hours <- deployments %>%
  rowwise() %>%
  mutate(data = list(
    tibble(
      placename = placename,
      start = seq(
        ymd_hms(paste(start_date, "00:00:00")),
        ymd_hms(paste(end_date, "23:59:59")),
        by = "60 min"
      )
    ) %>%
      mutate(end = lead(start, default = last(start) + minutes(60)))
  )) %>%
  pull(data) %>%
  bind_rows() %>% 
  unique()

#Append coyote detections to all sampling hours dataset
coyote_detections <- sampling_hours %>% 
  left_join(coyotes, by = c("placename", "start")) %>% 
  mutate(detection = replace_na(detection, FALSE)) %>% 
  unique() %>% 
  #Manually set season (seal vs non-seal based on isoweek (week # in year))
  mutate(season = if_else(isoweek(start) %in% c(51,52,1:15), "seal", "non-seal"))

table(coyote_detections$season, coyote_detections$detection)

#Create summary dataframe
coyote_detection_diel_summary <- coyote_detections %>% 
  mutate(time = hour(start),
         season = factor(season),
         placename = factor(placename)) %>% 
  group_by(placename, season, time) %>% 
  summarize(success = sum(detection),
            failure = n() - success,
            .groups = "drop") 


######################################################
# Trigonometric GLMM Seasonal Activity Analysis ######
######################################################

# run model
coyote_diel_seasonal_tglmm <- mixed_model(fixed = cbind(success, failure) ~ 
                               cos(2 * pi * time/24) * season +
                               sin(2 * pi * time/24) * season +
                               sin(2 * pi * time/12) * season +
                               cos(2 * pi * time/12) * season,
                             random = ~  1  |   placename,
                             data = coyote_detection_diel_summary, 
                             family = binomial(), 
                             iter_EM = 0)

summary(coyote_diel_seasonal_tglmm)


# build estimate of activity
coyote_tglmm_pred_temp <- with(coyote_detection_diel_summary, 
               expand.grid(time = seq(min(time), 24, length.out = 48), 
                           season = levels(season)
               )
)

coyote_tglmm_pred <- effectPlotData(coyote_diel_seasonal_tglmm, 
                                coyote_tglmm_pred_temp, 
                                marginal = FALSE) 


coyote_tglmm_plot <- ggplot(coyote_tglmm_pred, aes(time, plogis(pred))) +
    geom_ribbon(aes(ymin = plogis(low), ymax = plogis(upp), color = season, fill = season), alpha = 0.3, linewidth = 0.25) +
    geom_line(aes(color = season), linewidth = 1) +
    scale_color_manual(values = c("orange", "darkgreen")) +
    scale_fill_manual(values = c("orange", "darkgreen")) +
    labs(x = "Time of Day (Hour)", 
         y = "Predicted Activity Pattern \n (probability)",
         fill = "Season", color = "Season")+
    theme_custom()+
  scale_x_continuous(breaks=seq(0,23,length.out=7), labels=seq(0,24,4))+
    theme(panel.grid.major.x = element_line(colour = 'lightgrey', linetype = 'dashed', linewidth=0.5)) 


coyote_tglmm_plot

#Export Plot

ggsave("output/coyote_diel_seasonality_tglmm.png", coyote_tglmm_plot, 
       width = 8, height = 5, units = "in", dpi = 600)



#Compete model with season against model without season variable
trig_rand_int_no_cov <- mixed_model(fixed = cbind(success, failure) ~ 
                                      cos(2 * pi * time/24) +
                                      sin(2 * pi * time/24) +
                                      sin(2 * pi * time/12) +
                                      cos(2 * pi * time/12),
                                    random = ~  1  |   placename,
                                    data = coyote_detection_diel_summary, 
                                    family = binomial(), 
                                    iter_EM = 0
)

AIC(trig_rand_int, trig_rand_int_no_cov)


############################################################
# Cyclic cubic spline HGAM Seasonal Activity Analysis ######
############################################################

# 'Random intercept-only'
mod_cycl1 <- bam(cbind(success, failure) ~
                   season + 
                   s(time, bs = "cc", k = 12, by = season, m = 1) +
                   s(placename, bs="re"), 
                 knots = list(time=c(0,23)),
                 family = "binomial", 
                 data = coyote_detection_diel_summary
)


# Predict activity patterns
newdat <- with(coyote_detection_diel_summary, 
               expand.grid(time = seq(min(time), max(time), 1), 
                           season = levels(season), 
                           placename = "BCAM1" #Station doesn't matter
               ) 
) 


cycl_pred1 <- predict.bam(mod_cycl1, 
                          newdata = newdat,  
                          exclude = "s(placename)", 
                          se.fit = TRUE, 
                          type = "response"
) 
cycl_pred1 <- cbind(newdat, 
                    fit=cycl_pred1$fit, 
                    se.fit=cycl_pred1$se.fit, 
                    Model = "Random intercept-only"
)



# Plot
(pl_cycl1 <- ggplot(cycl_pred1, aes(time, fit)) +
  geom_ribbon(aes(ymin = fit-1.96*se.fit, ymax = fit+1.96*se.fit, color = season, fill = season), alpha = 0.3, linewidth = 0.25) +
  geom_line(aes(color = season), linewidth = 1) +
  scale_color_manual(values = c("orange", "darkgreen")) +
  scale_fill_manual(values = c("orange", "darkgreen")) +
  labs(x = "Time of Day (Hour)", y = "Predicted Activity Pattern \n (probability)", title = "B: Hierarchical model, \nCyclic cubic spline HGAM")+
#  coord_cartesian(ylim = c(0, 0.005)) +
  theme_custom()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10,face="bold"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-10,-10,-10),
        plot.title = element_text(size=10,face="bold"),
        axis.line = element_line(colour = 'black', linetype = 'solid'),
        axis.ticks = element_line(colour = 'black', linetype = 'solid'),
        axis.title = element_text(size=9,face="bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = 'lightgrey', linetype = 'dashed', linewidth=0.5),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 9, colour = "black", face = "bold", hjust = 0)
  ) +
  scale_x_continuous(breaks=seq(0,max(cycl_pred1$time),length.out=7), labels=seq(0,24,4)) )



# bringing hierarchical models plot together
(ggarrange(pl_trig, pl_cycl1, ncol = 2, common.legend = TRUE, legend = "bottom", widths = c(0.5, 0.5)) +
    theme(plot.margin = margin(0.1,0.1,0.5,0.1, "cm")))


#Model selection
mod_cycl1_no_cov <- bam(cbind(success, failure) ~ 
                          s(placename, bs="re"), 
                        knots = list(Time=c(0,23)),
                        family = "binomial", 
                        data = coyote_detection_diel_summary
)


AIC(mod_cycl1, mod_cycl1_no_cov)
