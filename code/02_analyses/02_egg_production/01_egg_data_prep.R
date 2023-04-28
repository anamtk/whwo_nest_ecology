# Data prep for egg JAGS model
# Ana Miller-ter Kuile
# April 13, 2022

# this script preps data for the JAGS model for egg productivity


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse",
                  "readxl")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

source(here("code",
            "00_functions",
            "tidy_functions.R"))
# Load data ---------------------------------------------------------------


egg <- read.csv(here("data_outputs",
                      "01_cleaning",
                      "03_egg_production",
                      "Egg_production_data.csv"))

# Output model objects ----------------------------------------------------

egg <- egg %>% 
  filter(Type == "egg")

# Numbers for loops -------------------------------------------------------

n.nests <- nrow(egg)

n.transects <- egg %>%
  distinct(Transect_ID2) %>%
  summarise(n.transects = n()) %>%
  dplyr::select(n.transects) %>%
  as_vector()

n.years <- egg %>%
  distinct(Year_located) %>%
  summarise(n.years = n()) %>%
  dplyr::select(n.years) %>%
  as_vector()

n.trt <- egg %>%
  distinct(Trt_cat) %>%
  summarise(n.trt = n()) %>%
  dplyr::select(n.trt) %>%
  as_vector()

n.forests <- egg %>%
  distinct(Project_ID) %>%
  summarise(n.forests = n()) %>%
  dplyr::select(n.forests) %>%
  as_vector()

n.lag <- egg %>%
  dplyr::select(Tmax:Tmax_l10) %>%
  ncol() 

n.times <- egg %>%
  distinct(Time_groups) %>%
  summarise(n.times = n()) %>%
  dplyr::select(n.times) %>%
  as_vector()
# Numbered factors for transects and years --------------------------------

Transect.num <- egg %>%
  dplyr::select(Transect_ID2) %>%
  as_vector() %>%
  nums()
  
Year.num <- egg %>%
  dplyr::select(Year_located) %>%
  as_vector() %>%
  nums()

Forest.num <- egg %>%
  dplyr::select(Project_ID) %>%
  as_vector() %>%
  nums()

# Main effects ------------------------------------------------------------

TrtID <- egg %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "B", "H", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  as_vector() %>%
  nums()
#levels(unique(as.factor(egg$Trt_50)))
# U = 1
# B = 2
# H = 3
# HB = 4
TrtTime <- egg %>%
  mutate(Time_groups = factor(Time_groups, levels = c("oot", "0-5",
                                                      "6-10", "11-14"))) %>%
  dplyr::select(Time_groups) %>%
  as_vector() %>%
  nums()

NTrt <- as.vector(scale(egg$n_tx))
Trees50 <- as.vector(scale(egg$Trees_50))
Trees2550 <- as.vector(scale(egg$Trees_2550))
PercPonderosa <- as.vector(scale(egg$pPIPO))
InitDay <- as.vector(scale(egg$Init_day))

Tmax_activeinsects <- as.vector(scale(egg$Tmax_activeinsects))
PPT_activeinsects <- as.vector(scale(egg$PPT_activeinsects))
Tmax_dormantinsects <- as.vector(scale(egg$Tmax_dormantinsects))
PPT_dormantinsects <- as.vector(scale(egg$Tmax_dormantinsects))

#for lag model - make a matrix of the lags
Tmax <- egg %>%
  dplyr::select(Nest_ID, Tmax:Tmax_l10) %>%
  pivot_longer(Tmax:Tmax_l10,
               names_to = "lag",
               values_to = "temp") %>%
  mutate(temp = scale(temp)) %>%
  pivot_wider(names_from = "lag",
              values_from = "temp") %>%
  dplyr::select(-Nest_ID) %>%
  as.matrix()

PPT <- egg %>%
  dplyr::select(Nest_ID, PPT:PPT_l10) %>%
  pivot_longer(PPT:PPT_l10,
               names_to = "lag",
               values_to = "ppt") %>%
  mutate(ppt = scale(ppt)) %>%
  pivot_wider(names_from = "lag",
              values_from = "ppt") %>%
  dplyr::select(-Nest_ID) %>%
  as.matrix()



#landscape variables

ForestCV <- as.vector(scale(egg$a1000_areacv))
ForestProx <- as.vector(scale(egg$a1000_proxmn2))
Contag <- as.vector(scale(egg$a1000_contag))
LandHa <- as.vector(scale(egg$a1000_Ha))
LandBu <- as.vector(scale(egg$a1000_RxBu))

# Response data -----------------------------------------------------------

y <- egg$No_eggs

# Compile and export ------------------------------------------------------

all_data <- list(n.nests = n.nests,
                 n.transects = n.transects,
                 n.years = n.years,
                 n.trt = n.trt,
                 n.times = n.times,
                 Transect.num = Transect.num,
                 Year.num = Year.num,
                 TrtID = TrtID,
                 TrtTime = TrtTime,
                 NTrt = NTrt,
                 InitDay = InitDay,
                 Trees50 = Trees50,
                 Trees2550 = Trees2550,
                 PercPonderosa = PercPonderosa,
                 Tmax_activeinsects =Tmax_activeinsects,
                 PPT_activeinsects = PPT_activeinsects,
                 Tmax_dormantinsects = Tmax_dormantinsects,
                 PPT_dormantinsects = PPT_dormantinsects,
                 Tmax = Tmax,
                 PPT = PPT,
                 ForestCV = ForestCV,
                 ForestProx = ForestProx,
                 Contag = Contag,
                 LandHa = LandHa,
                 LandBu = LandBu,
                 n.lag = n.lag,
                 y = y,
                 n.forests = n.forests,
                 Forest.num = Forest.num)

saveRDS(all_data, here("data_outputs",  
                       "02_monsoon",
                       "model_input_data",
                       "egg_JAGS_input_data_date.RDS"))


# Data summaries ----------------------------------------------------------

egg %>%
  group_by(Year_located) %>%
  tally()

egg %>%
  group_by(Year_located) %>%
  tally() %>%
  summarise(mean = mean(n),
            sd = sd(n),
            total = n(),
            se = sd/sqrt(total))

egg %>%
  group_by(Year_located, Trt_cat) %>%
  tally()

egg %>%
  group_by(Transect_ID2, Trt_cat) %>%
  tally()

egg %>%
  summarise(mean = mean(No_eggs),
            sd = sd(No_eggs),
            total = n(),
            se = sd/sqrt(total))

# What distributions for missing covariate data ---------------------------

hist(Trees2550)
hist(Trees50)
hist(PercPonderosa)
hist(Tmax_activeinsects)
hist(PPT_activeinsects)
