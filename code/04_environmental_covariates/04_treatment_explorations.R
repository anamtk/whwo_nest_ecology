# Nest data compiling
# March 2, 2022
# Ana Miller-ter Kuile

# this script uses QCed nest data with associated treatment codes
# to generate the treatment and nest-level covariates for the 
# nest model


# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse", 
                  "readxl")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#need: locations dataframe

locations <- read_xlsx(here("data_raw",
                            "bird_data",
                            "Birds01_nest_locations.xlsx"))


# FACTS dataset
facts <- read_xlsx(here("data_raw",
                        "bird_data",
                        "Birds07_nest_FACTS.xlsx"))


# Select CFLRP nests ------------------------------------------------------

#select locations only in the three CFLRP programs
locs <- locations %>%
  filter(Project_ID %in% c("EM_FWOR", "EM_MAOR", "EM_PAID")) %>%
  mutate(Project_ID = str_replace(Project_ID, "_", ""))


# Select Variables of interest for model ----------------------------------

#get all variables we might want for model
locs1 <- locs %>%
  dplyr::select(Year_located, Date_located,
                Project_ID, Nest_ID, Nest_Trt,
                Transect_ID, Point_ID, Direction,
                Distance, UTM_E, UTM_N, UTM_datum_zone,
                Nest_Ht, Cavity_age, Decay_class,
                Tree_Snag_Log, Tree_sp, Tree_ht,
                DBH, Orientation, Aspect, Slope)


# Consolidate treatment IDs -----------------------------------------------

facts2 <- facts %>%
  mutate(category = case_when(ACTIVITY %in% c("Wildfire - Human Ignition", 
                                              "Wildfire - Natural Ignition") ~ "Wildfire",
                              ACTIVITY %in% c("Burning of Piled Material",
                                              "Jackpot Burning - Scattered concentrations") ~ "Pile_Burn",
                              ACTIVITY %in% c("Broadcast Burning - Covers a majority of the unit",
                                              "Underburn - Low Intensity (Majority of Unit)",
                                              "Control of Understory Vegetation- Burning") ~ "Broadcast_Burn",
                              str_detect(ACTIVITY, "Thin|Cut|thinned|thinning") ~ "Harvest_Thin",
                              TRUE ~ "O"))  


facts3 <- facts2 %>%
  distinct(Nest_ID, category) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = "category",
              values_from = "value") %>%
  mutate(groups = case_when((Pile_Burn == 1 & Harvest_Thin == 1 & is.na(Broadcast_Burn)) ~ "HT_P",
                            (Pile_Burn == 1 & is.na(Harvest_Thin) & is.na(Broadcast_Burn))~ "P_only",
                            ((is.na(Pile_Burn) & Harvest_Thin == 1 & is.na(Broadcast_Burn))) ~ "HT_only",
                            ((Pile_Burn == 1 & Harvest_Thin == 1 & Broadcast_Burn == 1)) ~ "HT_P_B",
                            (is.na(Pile_Burn) & is.na(Harvest_Thin) & Broadcast_Burn == 1) ~ "B_only",
                            (is.na(Pile_Burn) & Harvest_Thin == 1 & Broadcast_Burn == 1) ~ "HT_B",
                            (Pile_Burn == 1 & is.na(Harvest_Thin & Broadcast_Burn == 1)) ~ "P_B",
                            TRUE ~ NA_character_))

facts3 %>%
  filter(!is.na(groups)) %>%
  group_by(groups) %>%
  tally()

