# Data summary tables/stats/graphs
# Ana Miller-ter Kuile
# March 16, 2022

# this script imports the survival and productivity datasets and summarises them


# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse", 
                  "readxl", "lubridate")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())
# Load data ---------------------------------------------------------------

#survival data 
survival <- read.csv(here('data_outputs', 
                          "01_cleaning", 
                          "03_nest_survival",
                          "Nest_survival_data.csv"))


#categories of first observation
first <- read_xlsx(here("data_raw",
                        "bird_data",
                        "Birds03_nest_fates.xlsx"))




# Prep data ---------------------------------------------------------------

#filter a productivity number per nest
prod <- survival %>%
  #only select nests that were successful
  filter(Fate_cat == "success") %>%
  #select variables of interest
  distinct(Nest_ID, Year_located, Project_ID,
           Transect_ID, Trt_50,
           Nest_Ht, Tree_sp, Orientation,
           Initiation_date, NoFL_uncert, 
           Trees_2550, Trees_50, pPIPO, Tave_0408,
           Tmax_0408, Tmin_0408, PPT_0408)

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
egg <- survival %>%
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
  #make an estimated number of eggs based on egg-> nestling survival
  #average calculated elsewhere. 
  #when observed in egg stage, est is just # eggs
  mutate(No_eggs_est = case_when(Type == "egg" ~ No_eggs,
                                 #this correction value is from an estimate of 
                                 #egg->nestling survival from the 00_firstobs... script
                                 Type == "nestling" ~ No_young/0.607,
                                 #multiple the number of young by the survival factor
                                 Type == "egg+nestling" ~ No_eggs + No_young/0.607,
                                 TRUE ~ NA_real_)) %>%
  #group by factors that will be predictors
  group_by(Nest_ID, Year_located, Project_ID, Region_ID,
           Transect_ID, Trt_50, Type,
           Trees_2550, Trees_50, pPIPO) %>%
  #find the maxiumum per nest of #eggs and #estimated eggs
  summarise(No_eggs_est = max(No_eggs_est, na.rm  = T),
            No_eggs = max(No_eggs, na.rm = T)) %>%
  #round to integer values
  mutate(No_eggs_est = round(No_eggs_est, 0)) %>%
  #remove 0 and infinite vlaues
  filter(No_eggs_est != 0) %>%
  filter(No_eggs != 0) %>%
  filter(No_eggs != -Inf) %>%
  filter(No_eggs_est != -Inf) 


# Survival summaries ------------------------------------------------------

#how many total nests:
survival %>%
  distinct(Nest_ID, Year_located, Project_ID, Trt_50, Fate,
           Fate_cat) %>%
  tally()

#how many nests per year:
survival %>%
  distinct(Nest_ID, Year_located, Project_ID, Trt_50, Fate,
           Fate_cat) %>%
  group_by(Year_located) %>%
  tally()

#how many nests per year:
survival %>%
  distinct(Nest_ID, Year_located, Project_ID, Trt_50, Fate,
           Fate_cat) %>%
  group_by(Year_located) %>%
  tally() %>%
  summarise(mean = mean(n),
            sd = sd(n),
            total = n(),
            se = sd/sqrt(total))

#how many nests per treatment category? 
survival %>%
  distinct(Nest_ID, Year_located, Project_ID, Trt_50, Fate,
           Fate_cat) %>%
  group_by(Trt_50) %>%
  tally() 

survival %>%
  distinct(Nest_ID, Year_located, Project_ID, Trt_50, Fate,
           Fate_cat) %>%
  mutate(Trt_cat = case_when (Trt_50 %in% c("B", "H", "HB") ~ "treated",
                              Trt_50 == "U" ~ "untreated",
                              TRUE ~ NA_character_)) %>%
  group_by(Trt_cat) %>%
  tally()

#survival overall
survival %>%
  distinct(Nest_ID, Year_located, Project_ID, Trt_50, Fate,
         Fate_cat) %>%
  group_by(Fate_cat) %>%
  tally()

#survival by year 
survival %>%
  distinct(Nest_ID, Year_located, Project_ID, Trt_50, Fate,
           Fate_cat) %>%
  group_by(Fate_cat, Year_located) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(names_from = "Fate_cat",
              values_from = "n") %>%
  rowwise() %>%
  summarise(success_rate = success/(success + failure)) %>%
  summarise(mean_rate = mean(success_rate,na.rm =T),
            sd = sd(success_rate, na.rm =T),
            total = n(),
            se = sd/sqrt(total))

# cause of mortality
survival %>%
  distinct(Nest_ID, Year_located, Project_ID, Trt_50, Fate,
           Fate_cat) %>%
  group_by(Fate) %>%
  tally()


# Egg production summaries ------------------------------------------------

#how many observed in egg stage?
egg %>%
  ungroup() %>%
  filter(Type == "egg") %>%
  tally()

#how many nests in egg and/or nestling
egg %>%
  ungroup() %>%
  tally()

#how many nests per year
egg %>%
  ungroup() %>%
  group_by(Year_located) %>%
  tally()

#how many nests per year
egg %>%
  ungroup() %>%
  group_by(Year_located) %>%
  tally() %>%
  summarise(mean = mean(n),
            sd = sd(n),
            total = n(),
            se = sd/sqrt(total))

#how many nests per treatment type
egg %>%
  ungroup() %>%
  group_by(Trt_50) %>%
  tally() 

#how many eggs?
egg %>%
  ungroup() %>%
  summarise(mean = mean(No_eggs_est),
            sd = sd(No_eggs_est),
            total = n(),
            se = sd/sqrt(total)) 


# Fledgling counts --------------------------------------------------------

#how many total?
prod %>%
  ungroup() %>%
  tally()

#how many total?
prod %>%
  ungroup() %>%
  group_by(Year_located) %>%
  tally() %>%
  summarise(mean = mean(n),
            sd = sd(n),
            total = n(),
            se = sd/sqrt(total))

#how many per treatment? 
prod %>%
  ungroup() %>%
  group_by(Trt_50) %>%
  tally() 

#how many fledged?
prod %>%
  ungroup() %>%
  summarise(mean = mean(NoFL_uncert),
            sd = sd(NoFL_uncert),
            total = n(),
            se = sd/sqrt(total))
