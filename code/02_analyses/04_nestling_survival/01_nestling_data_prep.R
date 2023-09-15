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


prod1 <- read.csv(here("data_outputs",
                       "01_cleaning",
                       "03_nestling_survival",
                       "Egg_Nestling_survival_data.csv"))



# Filter for nests with >0 nestlings --------------------------------------

prod1 <- prod1 %>%
  filter(No_young >= NoFL_uncert)

prod1 <- prod1 %>%
  filter(No_young > 0)

# Extract model objects ---------------------------------------------------


# Numbers for loops -------------------------------------------------------


n.nests <- nrow(prod1)

n.transects <- prod1 %>%
  distinct(Transect_ID2) %>%
  summarise(n.transects = n()) %>%
  dplyr::select(n.transects) %>%
  as_vector() 

n.years <- prod1 %>%
  distinct(Year_located) %>%
  summarise(n.years = n()) %>%
  dplyr::select(n.years) %>%
  as_vector() 

n.trt <- prod1 %>%
  distinct(Trt_cat) %>%
  summarise(n.trt = n()) %>%
  dplyr::select(n.trt) %>%
  as_vector() 

n.species <- prod1 %>%
  distinct(Tree_sp) %>%
  summarise(n.species = n()) %>%
  dplyr::select(n.species) %>%
  as_vector()

n.forests <- prod1 %>%
  distinct(Project_ID) %>%
  summarise(n.forests = n()) %>%
  dplyr::select(n.forests) %>%
  as_vector()

n.times <- prod1 %>%
  distinct(Time_groups) %>%
  summarise(n.times = n()) %>%
  dplyr::select(n.times) %>%
  as_vector()


# Hierarchical number identifiers -----------------------------------------


Transect.num <- prod1 %>%
  dplyr::select(Transect_ID2) %>%
  as_vector() %>%
  nums()

Year.num <- prod1 %>%
  dplyr::select(Year_located) %>%
  as_vector() %>%
  nums()

Forest.num <- prod1 %>%
  dplyr::select(Project_ID) %>%
  as_vector() %>%
  nums()

prod1 %>%
  group_by(Trt_cat) %>%
  tally()


# Covariates --------------------------------------------------------------


TreatmentID <- prod1 %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  as_vector() %>%
  nums()

prod1 %>%
  group_by(Trt_cat) %>%
  tally() %>%
  ggplot(aes(x = Trt_cat, y = n)) +
  geom_bar(stat = "identity")

TrtTime <- prod1 %>%
  mutate(Time_groups = factor(Time_groups, 
                              levels = c("oot", "0-5", "6-10", "11-14"))) %>%
  dplyr::select(Time_groups) %>%
  as_vector() %>%
  nums()

NTrt <- as.vector(scale(prod1$n_tx))
NestHt <- as.vector(scale(prod1$Nest_Ht))
CosOrientation <- as.vector(scale(prod1$cosOrientation))

#figure out which has most to order this correctly (most = first)
prod1 %>%
  group_by(Tree_sp) %>%
  tally()

SpeciesID <- prod1 %>%
  mutate(Tree_sp = factor(Tree_sp, levels = c("PIPO", "POTR5", 
                                              "JUOC", "PSME", "Abies"))) %>%
  dplyr::select(Tree_sp) %>%
  as_vector() %>%
  nums()

InitDay <- as.vector(scale(prod1$Init_day))
Trees50 <- as.vector(scale(prod1$Trees_50))
Trees2550 <- as.vector(scale(prod1$Trees_2550))
PercPonderosa <- as.vector(scale(prod1$pPIPO))

Tmax <- as.vector(scale(prod1$Tmax_ne))
PPT <- as.vector(scale(prod1$PPT_ne))
LandHa <- as.vector(scale(prod1$a1000_Ha))
LandBu <- as.vector(scale(prod1$a1000_RxBu))
PForest <- as.vector(scale(prod1$a1000_pland2))
NForestPatches <- as.vector(scale(prod1$a1000_np))
ForestCV <- as.vector(scale(prod1$a1000_areacv2))
Contag <- as.vector(scale(prod1$a1000_contag))
LPI <- as.vector(scale(prod1$a1000_lpi))

# response data -----------------------------------------------------------


y <- as.vector(prod1$NoFL_uncert)

#get the number of "trials" per nest
N.eggs <- as.vector(prod1$No_eggs)
N.nestlings <- as.vector(prod1$No_young)

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
                 NForestPatches = NForestPatches,
                 ForestCV = ForestCV,
                 Contag = Contag,
                 LPI = LPI,
                 N.eggs = N.eggs,
                 N.nestlings = N.nestlings,
                 y = y)

saveRDS(all_data, here("data_outputs",  
                       "02_monsoon",
                       "model_input_data",
                       "nestling_JAGS_input_data.RDS"))



# Data summaries ----------------------------------------------------------
prod1 %>%
  group_by(Year_located) %>%
  tally()

prod1 %>%
  group_by(Year_located) %>%
  tally() %>%
  summarise(mean = mean(n),
            sd = sd(n),
            total = n(),
            se = sd/sqrt(total))

prod1 %>%
  group_by(Trt_cat) %>%
  tally()

prod1 %>%
  summarise(mean = mean(NoFL_uncert),
            sd = sd(NoFL_uncert),
            total = n(),
            se = sd/sqrt(total))




