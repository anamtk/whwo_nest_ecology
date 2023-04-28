# Data prep for egg JAGS model
# Ana Miller-ter Kuile
# April 13, 2022

# this script preps data for the JAGS model for egg and 
# nestling survival

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse",
                  "readxl", "lubridate")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#temp and precip functions
source(here("code",
            "00_functions",
            "tidy_functions.R"))

# Load data ---------------------------------------------------------------

#cleaned data for each nest-visit 
data <- read.csv(here("data_outputs",
                      "01_cleaning",
                      "03_nest_survival",
                      "Nest_survival_data.csv"))

#dataframe for info on when nests were first observed
first <- read_xlsx(here("data_raw",
                        "bird_data",
                        "Birds03_nest_fates.xlsx"))

#climate data, which we'll need to compute
#stage-specific temperature maximums
climate <- read.csv(here("data_outputs",
                         "01_cleaning",
                         "02_climate_trees",
                         "nest_climate_data.csv"))



# Prep DF -----------------------------------------------------------------

#Get a bunch of data in the right format plus
# get only one data point for each nest
prod <- data %>%
  separate(Initiation_date,
           into = c("Year", "Month", "Day"),
           remove = F) %>%
  group_by(Nest_ID) %>%
  mutate(start_date = Month,
         end_date = max(end_date, na.rm = T)) %>%
  ungroup() %>%
  #select variables of interest
  distinct(Nest_ID, Year_located, Project_ID,
           Transect_ID2, Trt_cat,
           Nest_Ht, Tree_sp, Orientation,
           cosOrientation,
           Initiation_date, NoFL_uncert, 
           Trees_2550, Trees_50, pPIPO,
           start_date, end_date,
           a1000_areamn2, a1000_areaam2,
           a1000_areacv2, a1000_contag,
           a1000_pland2,
           a1000_pland1,
           a1000_lpi, a1000_lpi1, a1000_lpi2,
           a1000_np, a1000_np2, a1000_np1,
           a1000_proxmn2, a1000_Ha, a1000_PBu,
           a1000_RxBu, a1000_WFBu,
           n_burn, n_harvest, n_tx, 
           Tm_since_b, Tm_since_h, Tm_since_tx, Time_groups) %>%
  mutate(Init_day = yday(as.Date(Initiation_date))) %>%
  mutate(end_date = as.character(end_date)) %>%
  mutate(end_date = paste("0", end_date, sep = "")) %>%
  ungroup() 


# Start and end dates of each stage for each nest -------------------------


#get start and end date for egg and nestlings for each nest
stage_starts <- data %>%
  #get the start date for the nest from initation date month
  separate(Initiation_date,
           into = c("Year", "Month", "Day"),
           remove = F) %>%
  #set each stage group
  mutate(Stage = case_when(Stage == "E" ~ "Ex_start",
                           Stage %in% c("I", "L") ~ "Eg_start",
                           Stage == "N" ~ "Ne_start",
                           TRUE ~ Stage)) %>%
  #only select the starts of eggs and nestlings for stages
  filter(Stage %in% c("Eg_start", "Ne_start")) %>%
  #get a nest per stage grouping of the start date
  group_by(Nest_ID, Stage) %>%
  mutate(start_date = Month) %>%
  ungroup() %>%
  #select a distinct start date for each nest for each stage
  distinct(Nest_ID, Stage, start_date) %>%
  #make this wider format so that there's a column for egg and nestlings
  pivot_wider(names_from = "Stage",
              values_from = "start_date")

#do something similar to the start df but with end of each period 
# for each nest
stage_ends <- data %>%
  separate(Initiation_date,
           into = c("Year", "Month", "Day"),
           remove = F) %>%
  mutate(Stage = case_when(Stage == "E" ~ "Ex_end",
                           Stage %in% c("I", "L") ~ "Eg_end",
                           Stage == "N" ~ "Ne_end",
                           TRUE ~ Stage)) %>%
  filter(Stage %in% c("Eg_end", "Ne_end")) %>%
  group_by(Nest_ID, Stage) %>%
  #this looks per nest and stage at the maximum of the 
  #value for the end month specified above
  mutate(end_date = max(end_date, na.rm = T)) %>%
  ungroup() %>%
  distinct(Nest_ID, Stage, end_date) %>%
  #again, pivot so there is a column per stage for ends
  pivot_wider(names_from = "Stage",
              values_from = "end_date")

#add these variables to the DF
prod <- prod %>%
  left_join(stage_starts, by = "Nest_ID") %>%
  left_join(stage_ends, by = "Nest_ID") %>%
  #make these characters for the function to follow
  mutate(Eg_start = as.character(Eg_start),
         Ne_start = as.character(Ne_start),
         Eg_end = as.character(Eg_end),
         Ne_end = as.character(Ne_end)) %>%
  #paste a 0 so they work in the function as written
  mutate(Eg_end = paste("0", Eg_end, sep = ""),
         Ne_end = paste("0", Ne_end, sep = "")) %>%
  #set NA values
  mutate(Eg_end = case_when(Eg_end == "0NA" ~ NA_character_,
                            TRUE ~ Eg_end),
         Ne_end = case_when(Ne_end == "0NA" ~ NA_character_,
                            TRUE ~ Ne_end))
  

# Get info on number of eggs into dataframe -------------------------------

#Prep identification of what stage the nest was first observed in
first <- first %>%
  filter(str_detect(Nest_ID, "EMFWOR|EMPAID|EMMAOR")) %>%
  #give E's an "egg" identifier, ENs a egg+nestling, etc.
  mutate(Type = case_when(Init_date_source %in% c("E3", "E6", "E4", 
                                                  "E5", "E2", "E7") ~ "egg",
                          Init_date_source %in% c("EN5", "EN4", "EN3",
                                                  "EN2") ~ "egg+nestling",
                          Init_date_source %in% c("N3", "N2", "N4", "N1",
                                                  "N5") ~ "nestling",
                          Init_date_source %in% c("NF2") ~ "nest+fledgling",
                          Init_date_source %in% c("F3", "F2") ~ "fledgling",
                          TRUE ~ NA_character_)) %>%
  #select only those columns and nest_ID for joining
  dplyr::select(Nest_ID, Init_date_source, Type) 

#filter a productivity number per nest
egg <- data %>%
  #reset NA values
  mutate(No_eggs = case_when(No_eggs == 999 ~ NA_integer_,
                             TRUE ~ No_eggs),
         No_young = case_when(No_young == 999 ~ NA_integer_,
                              TRUE ~ No_young)) %>%
  #combine with the dataframe indicating stage of first observation
  left_join(first, by = "Nest_ID") %>%
  #select only nests observed with eggs and/or nestlings
  filter(Type %in%  c("egg", "egg+nestling", "nestling")) %>%
  ungroup() %>%
  mutate(No_eggs = as.numeric(No_eggs),
         No_young = as.numeric(No_young)) %>%
  #group by factors that will be predictors
  group_by(Nest_ID, Year_located, Project_ID, 
           Transect_ID2, Trt_cat, Type,
           Trees_2550, Trees_50, pPIPO) %>%
  #find the maxiumum per nest of #eggs 
  summarise(No_eggs = max(No_eggs, na.rm = T),
            No_young = max(No_young, na.rm = T)) %>%
  #round to integer values
  ungroup() %>%
  dplyr::select(Nest_ID,  No_eggs, No_young, Type)

prod1 <- prod %>%
  left_join(egg, by = "Nest_ID") 

# Add climate data --------------------------------------------------------

monthly_t <- climate %>%
  dplyr::select(MeasurementID, TransID, ClimateYear, 
                Tmax04, Tmax05, Tmax06,
                Tmax07, Tmax08) %>%
  pivot_longer(cols = Tmax04:Tmax08,
               values_to = "Tmax",
               names_to  = "Month") %>%
  mutate(Month = str_sub(Month, -2, length(Month))) %>%
  unite('Month_year',
        c("Month", "ClimateYear"),
        sep = "_",
        remove = F) %>%
  mutate(Month_year = format(lubridate::parse_date_time(Month_year, 
                                                        orders = c("m_Y")), 
                             "%d-%m-%Y")) %>%
  unite(Trans_Year,
        c("TransID", "ClimateYear"),
        sep = "_",
        remove = F) 

 monthly_p <- climate %>%
   dplyr::select(TransID, MeasurementID, ClimateYear, 
                 PPT04, PPT05, PPT06,
                 PPT07, PPT08) %>%
   pivot_longer(cols = PPT04:PPT08,
                values_to = "PPT",
                names_to  = "Month") %>%
   mutate(Month = str_sub(Month, -2, length(Month))) %>%
   unite('Month_year',
         c("Month", "ClimateYear"),
         sep = "_",
         remove = F) %>%
   mutate(Month_year = format(lubridate::parse_date_time(Month_year, 
                                                         orders = c("m_Y")), 
                              "%d-%m-%Y")) %>%
   unite(Trans_Year,
         c("TransID", "ClimateYear"),
         sep = "_",
         remove = F)
# 

# output vector y which will be filled in by the for loop
prod1$Tmax <- rep(NA, length(prod1$Nest_ID)) # create an empty vector of NAs

for(i in 1:length(prod1$Nest_ID)) {
  prod1$Tmax[i] <- t_max(pointID = prod1$Nest_ID[i], 
                          start_date = prod1$start_date[i],
                          end_date = prod1$end_date[i]) 
}
 prod1$PPT <- rep(NA, length(prod1$Nest_ID)) # create an empty vector of NAs
# 
 for(i in 1:length(prod1$Nest_ID)) {
   prod1$PPT[i] <- ppt_sum(pointID = prod1$Nest_ID[i], 
                            start_date = prod1$start_date[i],
                            end_date = prod1$end_date[i]) 
 }

prod1$Tmax_eg <- rep(NA, length(prod1$Nest_ID)) # create an empty vector of NAs

for(i in 1:length(prod1$Nest_ID)) {
  prod1$Tmax_eg[i] <- t_max(pointID = prod1$Nest_ID[i], 
                         start_date = prod1$Eg_start[i],
                         end_date = prod1$Eg_end[i]) 
}

prod1$PPT_eg <- rep(NA, length(prod1$Nest_ID))

for(i in 1:length(prod1$Nest_ID)){
  prod1$PPT_eg[i] <- ppt_sum(pointID = prod1$Nest_ID[i],
                             start_date = prod1$Eg_start[i],
                             end_date = prod1$Eg_end[i])
}

prod1$Tmax_ne <- rep(NA, length(prod1$Nest_ID)) # create an empty vector of NAs

for(i in 1:length(prod1$Nest_ID)) {
  prod1$Tmax_ne[i] <- t_max(pointID = prod1$Nest_ID[i], 
                            start_date = prod1$Ne_start[i],
                            end_date = prod1$Ne_end[i]) 
}

prod1$PPT_ne <- rep(NA, length(prod1$Nest_ID))

for(i in 1:length(prod1$Nest_ID)){
  prod1$PPT_ne[i] <- ppt_sum(pointID = prod1$Nest_ID[i],
                             start_date = prod1$Ne_start[i],
                             end_date = prod1$Ne_end[i])
}
#fix Inf and 0 values
prod1 <- prod1 %>%
  mutate(Tmax = case_when(Tmax == -Inf ~ NA_real_,
                          TRUE ~ Tmax),
         Tmax_eg = case_when(Tmax_eg == -Inf ~ NA_real_,
                             TRUE ~ Tmax_eg),
         Tmax_ne = case_when(Tmax_ne == -Inf ~ NA_real_,
                             TRUE ~ Tmax_ne))


# Export ------------------------------------------------------------------

write.csv(prod1, here("data_outputs",
                      "01_cleaning",
                      "03_nestling_survival",
                      "Egg_Nestling_survival_data.csv"))

#END SCRIPT