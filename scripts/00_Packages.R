##########################################################################
# Point Reyes Beach Wildlife #############################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 00: Load packages ###############################################
#-------------------------------------------------------------------------


# Part 1: Load Packages --------------------------------------------------

# Load packages
packages <- c("tidyverse", "lubridate", "rtide", "ggthemes", "overlap", "janitor")

pacman::p_load(packages, character.only = TRUE); rm(packages)

#Generate %notin% function (the opposite of %in%)
`%notin%` <- Negate(`%in%`)
