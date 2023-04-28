# Data prep for initation Day JAGS model
# Ana Miller-ter Kuile
# November 8, 2022

# this script preps dataframe that can be used to generate the JAGS model for
# nest initiation day


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse",
                  "readxl", "data.table")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}
# Load data ---------------------------------------------------------------

#import the dataset cleaned in the 03_ step that
# includes only nests of known fates and filtered out
# a bunch of observations/cleaned up covariate columns
data <- read.csv(here("data_outputs",
                      "01_cleaning",
                      "03_nest_survival",
                      "Nest_survival_data.csv"))

#get the climate data too so we can prep the "lags" 
# for the SAM model structure
climate <- read.csv(here("data_outputs",
                         "01_cleaning",
                         "02_climate_trees",
                         "nest_climate_data.csv"))

# Prep DF -----------------------------------------------------------------

init <- data %>%
  ungroup() %>%
  #group by factors that will be predictors
  distinct(Nest_ID, Year_located, Project_ID, 
           Transect_ID2, Trt_cat, Initiation_date,
           Trees_2550, Trees_50, pPIPO, 
           a1000_areamn2, a1000_areaam2,
           a1000_areacv2, a1000_contag,
           a1000_pland2,
           a1000_pland1,
           a1000_lpi, a1000_lpi1, a1000_lpi2,
           a1000_np, a1000_np2, a1000_np1,
           a1000_proxmn2, a1000_Ha, a1000_RxBu,
           a1000_WFBu, a1000_PBu,
           n_burn, n_harvest, n_tx, 
           Tm_since_b, Tm_since_h, Tm_since_tx, Time_groups) %>%
  mutate(Month = '05') %>% #set may to be month to start for all nests since most start that month
  # separate(Initiation_date,
  #          into = c("Year", "Month", "Day"), 
  #          remove  = F) %>%
  # mutate(Month = case_when(Month == 99 ~ NA_character_,
  #                          TRUE ~ Month)) %>%
  mutate(Init_day = yday(Initiation_date)) %>%
  filter(!is.na(Init_day))
  #dplyr::select(-Day, -Year)

# Get climate lag per nest ------------------------------------------------

#this preps the temperature data for the structure needed of 
# a matrix for each nest for the SAM model structure

t_lags <- climate %>%
  dplyr::select(MeasurementID, ClimateYear,
                Tmax01:Tmax12) %>%
  #make the temp data long format
  pivot_longer(Tmax01:Tmax12,
               names_to = "Month",
               values_to = "Tmax") %>%
  #make month variable a number
  mutate(Month  = str_sub(Month, -2, length(Month))) %>%
  #group by each nest_year ID
  group_by(MeasurementID) %>%
  #get the data in the right format for doing the lags
  arrange(MeasurementID, ClimateYear) %>% 
  #this creates a column for every lag 1:10 months ago
  do(data.frame(., setNames(shift(.$Tmax, 1:10), c("Tmax_l1",
                                                   "Tmax_l2", "Tmax_l3",
                                                   "Tmax_l4", "Tmax_l5", 
                                                   "Tmax_l6", "Tmax_l7", 
                                                   "Tmax_l8", "Tmax_l9",
                                                   "Tmax_l10")))) %>%
  ungroup() %>%
  #get the year located variale from measurement ID
  mutate(Year_located = as.numeric(str_sub(MeasurementID, -4, length(MeasurementID)))) %>%
  #set so that this only calculates values for the correct year of measurment
  filter(ClimateYear == Year_located)

p_lags <- climate %>%
  dplyr::select(MeasurementID, ClimateYear,
                PPT01:PPT12) %>%
  pivot_longer(PPT01:PPT12,
               names_to = "Month",
               values_to = "PPT") %>%
  mutate(Month  = str_sub(Month, -2, length(Month)))  %>%
  group_by(MeasurementID) %>%
  arrange(MeasurementID, ClimateYear) %>% 
  #this creates a column for every lag 1:10 months ago
  do(data.frame(., setNames(shift(.$PPT, 1:10), c("PPT_l1",
                                                  "PPT_l2", "PPT_l3",
                                                  "PPT_l4", "PPT_l5",
                                                  "PPT_l6", "PPT_l7",
                                                  "PPT_l8", "PPT_l9",
                                                  "PPT_l10")))) %>%
  ungroup() %>%
  mutate(Year_located = as.numeric(str_sub(MeasurementID, -4, length(MeasurementID)))) %>%
  filter(ClimateYear == Year_located)

#just in case - also making a variable list of more simple groupings of
# climate into "active" and "dormant" insect periods 
climate2 <- climate %>%
  dplyr::select(MeasurementID, ClimateYear, Tmax_activeinsects, PPT_activeinsects,
                Tmax_dormantinsects, PPT_dormantinsects)

#join all the climate data to the egg dataframe
init2 <- init %>%
  left_join(t_lags, by = c("Month", 
                           "Year_located",
                           "Nest_ID" = "MeasurementID")) %>%
  left_join(p_lags, by = c("Month", 
                           "Year_located",
                           "Nest_ID" = "MeasurementID",
                           "ClimateYear")) %>%
  left_join(climate2, by = c("Nest_ID" = "MeasurementID",
                             "Year_located" = "ClimateYear"))

# Export ------------------------------------------------------------------

write.csv(init2, here("data_outputs",
                     "01_cleaning",
                     "03b_nest_initiation",
                     "Nest_initiation_data.csv"))

# END SCRIPT