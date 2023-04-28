# Data prep for egg JAGS model
# Ana Miller-ter Kuile
# April 13, 2022

# this script preps data for the JAGS model for egg productivity


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse",
                  "readxl", "lubridate")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

source(here("code",
            "00_functions",
            "tidy_functions.R"))
# Load data ---------------------------------------------------------------


egg_s <- read.csv(here("data_outputs",
                       "01_cleaning",
                       "03_nestling_survival",
                       "Egg_Nestling_survival_data.csv"))



# Filter for nests with >0 nestlings --------------------------------------

egg_s %>%
  group_by(Type) %>%
  tally()

egg_s <- egg_s %>%
  filter(Type == "egg") %>%
  filter(No_eggs >= No_young)

# Extract model objects ---------------------------------------------------


# Numbers for loops -------------------------------------------------------


n.nests <- nrow(egg_s)

n.transects <- egg_s %>%
  distinct(Transect_ID2) %>%
  summarise(n.transects = n()) %>%
  dplyr::select(n.transects) %>%
  as_vector() 

n.years <- egg_s %>%
  distinct(Year_located) %>%
  summarise(n.years = n()) %>%
  dplyr::select(n.years) %>%
  as_vector() 

n.trt <- egg_s %>%
  distinct(Trt_cat) %>%
  summarise(n.trt = n()) %>%
  dplyr::select(n.trt) %>%
  as_vector() 

n.species <- egg_s %>%
  distinct(Tree_sp) %>%
  summarise(n.species = n()) %>%
  dplyr::select(n.species) %>%
  as_vector()

n.forests <- egg_s %>%
  distinct(Project_ID) %>%
  summarise(n.forests = n()) %>%
  dplyr::select(n.forests) %>%
  as_vector()

n.times <- egg_s %>%
  distinct(Time_groups) %>%
  summarise(n.times = n()) %>%
  dplyr::select(n.times) %>%
  as_vector()

# Hierarchical number identifiers -----------------------------------------


Transect.num <- egg_s %>%
  dplyr::select(Transect_ID2) %>%
  as_vector() %>%
  nums()

Year.num <- egg_s %>%
  dplyr::select(Year_located) %>%
  as_vector() %>%
  nums()

Forest.num <- egg_s %>%
  dplyr::select(Project_ID) %>%
  as_vector() %>%
  nums()

egg_s %>%
  group_by(Trt_cat) %>%
  tally()


# Covariates --------------------------------------------------------------


TreatmentID <- egg_s %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  as_vector() %>%
  nums()

egg_s %>%
  group_by(Trt_cat) %>%
  tally() %>%
  ggplot(aes(x = Trt_cat, y = n)) +
  geom_bar(stat = "identity")

#Treatment history
TrtTime <- egg_s %>%
  mutate(Time_groups = factor(Time_groups, 
                              levels = c("oot", "0-5", "6-10", "11-14"))) %>%
  dplyr::select(Time_groups) %>%
  as_vector() %>%
  nums()
  
NTrt <- as.vector(scale(egg_s$n_tx))

NestHt <- as.vector(scale(egg_s$Nest_Ht))
CosOrientation <- as.vector(scale(egg_s$cosOrientation))

#figure out which has most to order this correctly (most = first)
egg_s %>%
  group_by(Tree_sp) %>%
  tally()

SpeciesID <- egg_s %>%
  mutate(Tree_sp = factor(Tree_sp, levels = c("PIPO", "POTR5", 
                                              "JUOC", "PSME", "Abies"))) %>%
  dplyr::select(Tree_sp) %>%
  as_vector() %>%
  nums()

InitDay <- as.vector(scale(egg_s$Init_day))
Trees50 <- as.vector(scale(egg_s$Trees_50))
Trees2550 <- as.vector(scale(egg_s$Trees_2550))
PercPonderosa <- as.vector(scale(egg_s$pPIPO))

Tmax <- as.vector(scale(egg_s$Tmax_eg))
PPT <- as.vector(scale(egg_s$PPT_eg))
egg_s %>%
  filter(is.na(Tmax_eg))
egg_s %>%
  filter(is.na(PPT_eg))

LandHa <- as.vector(scale(egg_s$a1000_Ha))
LandBu <- as.vector(scale(egg_s$a1000_RxBu))
PForest <- as.vector(scale(egg_s$a1000_pland2))
NPatches <- as.vector(scale(egg_s$a1000_np))

# response data -----------------------------------------------------------


y <- as.vector(egg_s$No_young)

#get the number of "trials" per nest
N.eggs <- as.vector(egg_s$No_eggs)
N.nestlings <- as.vector(egg_s$No_young)

# Compile and export ------------------------------------------------------

all_data <- list(n.nests = n.nests,
                 n.transects = n.transects,
                 n.years = n.years,
                 n.trt = n.trt,
                 n.forests = n.forests,
                 n.species = n.species,
                 n.times = n.times,
                 Transect.num = Transect.num,
                 Year.num = Year.num,
                 Forest.num = Forest.num,
                 TreatmentID = TreatmentID,
                 TrtTime = TrtTime,
                 NTrt = NTrt,
                 NestHt = NestHt, 
                 CosOrientation = CosOrientation,
                 SpeciesID = SpeciesID,
                 InitDay = InitDay,
                 Trees50 = Trees50,
                 Trees2550 = Trees2550,
                 PercPonderosa = PercPonderosa,
                 Tmax = Tmax,
                 PPT = PPT,
                 LandHa = LandHa,
                 LandBu = LandBu,
                 PForest = PForest,
                 NPatches = NPatches,
                 N.eggs = N.eggs,
                 N.nestlings = N.nestlings,
                 y = y)

saveRDS(all_data, here("data_outputs",  
                       "02_monsoon",
                       "model_input_data",
                       "egg_survival_JAGS_input_data.RDS"))



# Data summaries ----------------------------------------------------------

egg_s %>%
  group_by(Year_located) %>%
  tally()

egg_s %>%
  group_by(Year_located) %>%
  tally() %>%
  summarise(mean = mean(n),
            sd = sd(n),
            total = n(),
            se = sd/sqrt(total))

egg_s %>%
  group_by(Trt_cat) %>%
  tally()

hist(egg_s$No_eggs)
hist(egg_s$No_young)

egg_s %>%
  summarise(mean = mean(No_young),
            sd = sd(No_young),
            total = n(),
            se = sd/sqrt(total))
