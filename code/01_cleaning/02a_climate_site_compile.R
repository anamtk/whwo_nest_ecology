# Transect-level climate variables
# Ana Miller-ter Kuile
# October 29, 2021

# this script brings in and selects variables of interest from
# the climate data for nest site selection and survival


# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse", 
                  "readxl")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


#custom functions 
source(here("code", 
            "00_functions", 
            "tidy_functions.R"))

# Load datasets -----------------------------------------------------------

#load climate dtaa
climate <- read_csv(here("data_raw",
                         "climate_data",
                         "CFLRP_ClimateNA_data_transects_nests_2011-2021.csv"))


# Make climate transect names consistent with sites df --------------------

#Some of the EMPAID transects were missing the middle characters
# that are present in the other datasets.
#using the insert function from tidy_functions.R
climate <- climate %>%
  mutate(TransID = case_when(TransID %in% c("EMPAID_A",
                                            "EMPAID_B",
                                            "EMPAID_D",
                                            "EMPAID_E",
                                            "EMPAID_G") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "CW-"),
                             TransID %in% c("EMPAID_H",
                                            "EMPAID_I",
                                            "EMPAID_J") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "RB-"),
                             TransID %in% c("EMPAID_K",
                                            "EMPAID_L",
                                            "EMPAID_M",
                                            "EMPAID_N") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "WRF-"),
                             TransID %in% c("EMPAID_P",
                                            "EMPAID_Q",
                                            "EMPAID_R") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "LC-"),
                             TransID %in% c("EMPAID_S",
                                            "EMPAID_T",
                                            "EMPAID_U") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "B-"),
                             TransID %in% c("EMPAID_V",
                                            "EMPAID_W") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "MFW-"),
                             TRUE ~ TransID))



# Create lags -------------------------------------------------------------

#Create climate lags for each transect
climate1 <- climate %>%
  ungroup() %>%
  group_by(TransID) %>%
  mutate(Tmax_lag12 = lag(Tmax12, order_by = ClimateYear),
                 Tmax_lag11 = lag(Tmax11, order_by = ClimateYear),
                 Tmax_lag10 = lag(Tmax10, order_by = ClimateYear),
                 Tmax_lag09 = lag(Tmax09, order_by = ClimateYear),
                 Tmax_lag08 = lag(Tmax08, order_by = ClimateYear),
                 Tmax_lag07 = lag(Tmax07, order_by = ClimateYear),
                 Tmax_lag06 = lag(Tmax06, order_by = ClimateYear),
                 Tmax_lag05 = lag(Tmax05, order_by = ClimateYear),
                 Tmax_lag04 = lag(Tmax04, order_by = ClimateYear),
                 Tmax_lag03 = lag(Tmax03, order_by = ClimateYear),
                 Tmax_lag02 = lag(Tmax02, order_by = ClimateYear),
                 Tmax_lag01 = lag(Tmax01, order_by = ClimateYear),
                 PPT_lag12 = lag(PPT12, order_by = ClimateYear),
                 PPT_lag11 = lag(PPT11, order_by = ClimateYear) ,
                 PPT_lag10 = lag(PPT10, order_by = ClimateYear) ,
                 PPT_lag09 = lag(PPT09, order_by = ClimateYear) ,
                 PPT_lag08 = lag(PPT08, order_by = ClimateYear) ,
                 PPT_lag07 = lag(PPT07, order_by = ClimateYear) ,
                 PPT_lag06 = lag(PPT06, order_by = ClimateYear),
                 PPT_lag05 = lag(PPT05, order_by = ClimateYear) ,
                 PPT_lag04 = lag(PPT04, order_by = ClimateYear) ,
                 PPT_lag03 = lag(PPT03, order_by = ClimateYear) ,
                 PPT_lag02 = lag(PPT02, order_by = ClimateYear) ,
                 PPT_lag01 = lag(PPT01, order_by = ClimateYear)) %>% 
  ungroup() %>%
  rowwise() %>%
  #use those to get at active/dormant insect values for temp and precip
  mutate(Tmax_activeinsects = max(c(Tmax_lag05, Tmax_lag06,
                                  Tmax_lag07, Tmax_lag08,
                                  Tmax_lag09, Tmax_lag10), na.rm = T),
         PPT_activeinsects = sum(c(PPT_lag05, PPT_lag06,
                                   PPT_lag07, PPT_lag08,
                                   PPT_lag09, PPT_lag10), na.rm = T),
         Tmax_dormantinsects = max(c(Tmax_lag11, Tmax_lag12, 
                                     Tmax_lag01, Tmax_lag02,
                                     Tmax_lag03, Tmax_lag04), na.rm = T),
         PPT_dormantinsects = sum(c(PPT_lag11, PPT_lag12,
                                    PPT_lag01, PPT_lag02,
                                    PPT_lag03, PPT_lag04), na.rm= T)) %>%
  ungroup()


# Select variables of interest --------------------------------------------

colnames(climate1)

climate2 <- climate1 %>%
  dplyr::select(Location, TransID, ClimateYear, MeasurementID, 
                Tmax01, Tmax02, Tmax03, Tmax04, 
                Tmax05, Tmax06, Tmax07, Tmax08,
                Tmax09, Tmax10, Tmax11, Tmax12,
                PPT01, PPT02, PPT03, PPT04, 
                PPT05, PPT06, PPT07, 
                PPT08, PPT09, PPT10,
                PPT11, PPT12, 
                Tmin01:Tmin12,
                Tave01:Tave12,
                Tmax_activeinsects, PPT_activeinsects, 
                Tmax_dormantinsects, PPT_dormantinsects)


# Export DF ---------------------------------------------------------------

write.csv(climate2, here("data_outputs",
                         "01_cleaning",
                         "02_climate_trees",
                         "nest_climate_data.csv"))

#END SCRIPT

