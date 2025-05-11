##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 00: Load packages ###############################################
#-------------------------------------------------------------------------


# Part 1: Load Packages --------------------------------------------------

# Load packages
packages <- c("tidyverse", "lubridate", "rtide", "ggthemes", "overlap", "janitor", "piecewiseSEM", "glmmTMB", "GLMMadaptive", "mgcv", "ggpubr")

pacman::p_load(packages, character.only = TRUE); rm(packages)


# Part 2: Customize functions + themes ----------------------------------

#Generate %notin% function (the opposite of %in%)
`%notin%` <- Negate(`%in%`)

#Generate ggplot theme
theme_custom <- function() {
  theme_few() +
    theme(panel.border = element_rect(linewidth = 2),
          axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))
}

