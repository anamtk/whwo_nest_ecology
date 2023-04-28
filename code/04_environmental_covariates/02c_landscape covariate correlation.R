# Exploration of correlation between environmental covariates
# Ana Miller-ter Kuile
# November 9, 2021

#  this script creates a correlation exploration between 
# the variables of interest for environmental covariates
# in the nest survival model


# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "GGally", "patchwork")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#landscape covariates for all nests
land <- read.csv(here("data_outputs",
                      "01_cleaning",
                      "02_climate_trees",
                      "nest_landscape_1000_covariates.csv"))


# Select variables of interest --------------------------------------------

colnames(land)


#General approach:
# model selection with all ranked best to worst, then:
#chosen based on 
#1. lack of correlation with best predictors and
#2. lack of ecological redundancy/management relevance (e.g. forest patches should be this big, and
# variable; landscape itself should be patchy in this way...)

# EGG PROD ----------------------------------------------------------------

egg <- land %>%
  dplyr::select(Nest_ID, 
                a1000_areacv2,
                #a1000_lpi2,  #correlates with areacv2
                #a1000_areaam2, #correlates with areacv2
                #a1000_pland2, #correlates with areacv2
                a1000_proxmn2,
                a1000_contag) %>%
                #a1000_np2, 
                #a1000_np1,
                #a1000_np
                #a1000_areamn2) %>%
  column_to_rownames(var = "Nest_ID")
#areacv2, lpi2, areaam2, pland2, proxmn2, contag, np2, np1, np, areamn2 

ggpairs(egg)


# FLEDGE ------------------------------------------------------------------


fled <- land %>%
  dplyr::select(Nest_ID, 
                a1000_pland2, 
                #a1000_areacv2, #correlates with pland2
                #a1000_lpi2, #correlates with pland2
                #a1000_areaam2, # correlates with pland2
                #a1000_proxmn2, #correlates with pland2
                a1000_np) %>%
                #a1000_np2, #correlates with pland3
                #a1000_contag) %>% #has a weird correlation with np
                #a1000_np1, #correlates with pland2
                #a1000_areamn2 #correlates with pland2) %>%
  column_to_rownames(var = "Nest_ID")
#pland2, areacv2, lpi2, areaam2, proxmn2, np, np2, contag, np1, areamn2

ggpairs(fled)
