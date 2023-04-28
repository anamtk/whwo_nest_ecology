# Data prep for initiation date JAGS model
# Ana Miller-ter Kuile
# November 8, 2022

# this script preps data for the JAGS model for nest initation day


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


init <- read.csv(here("data_outputs",
                      "01_cleaning",
                      "03b_nest_initiation",
                      "Nest_initiation_data.csv"))

# Numbers for loops -------------------------------------------------------

n.nests <- nrow(init)

n.transects <- init %>%
  distinct(Transect_ID2) %>%
  summarise(n.transects = n()) %>%
  dplyr::select(n.transects) %>%
  as_vector()

n.years <- init %>%
  distinct(Year_located) %>%
  summarise(n.years = n()) %>%
  dplyr::select(n.years) %>%
  as_vector()

n.trt <- init %>%
  distinct(Trt_cat) %>%
  summarise(n.trt = n()) %>%
  dplyr::select(n.trt) %>%
  as_vector()

n.forests <- init %>%
  distinct(Project_ID) %>%
  summarise(n.forests = n()) %>%
  dplyr::select(n.forests) %>%
  as_vector()

n.lag <- init %>%
  dplyr::select(Tmax:Tmax_l9) %>%
  ncol() 

n.times <- init %>%
  distinct(Time_groups) %>%
  summarise(n.times = n()) %>%
  dplyr::select(n.times) %>%
  as_vector()
# Numbered factors for transects and years --------------------------------

Transect.num <- init %>%
  dplyr::select(Transect_ID2) %>%
  as_vector() %>%
  nums()

Year.num <- init %>%
  dplyr::select(Year_located) %>%
  as_vector() %>%
  nums()

Forest.num <- init %>%
  dplyr::select(Project_ID) %>%
  as_vector() %>%
  nums()

# Main effects ------------------------------------------------------------

TrtID <- init %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "B", "H", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  as_vector() %>%
  nums()
#levels(unique(as.factor(egg$Trt_50)))
# U = 1
# B = 2
# H = 3
# HB = 4
TrtTime <- init %>%
  mutate(Time_groups = factor(Time_groups, levels = c("oot", "0-5",
                                                      "6-10", "11-14"))) %>%
  dplyr::select(Time_groups) %>%
  as_vector() %>%
  nums()

NTrt <- as.vector(scale(init$n_tx))
Trees50 <- as.vector(scale(init$Trees_50))
Trees2550 <- as.vector(scale(init$Trees_2550))
PercPonderosa <- as.vector(scale(init$pPIPO))

Year <- as.vector(scale(init$Year_located))

Tmax_activeinsects <- as.vector(scale(init$Tmax_activeinsects))
PPT_activeinsects <- as.vector(scale(init$PPT_activeinsects))
Tmax_dormantinsects <- as.vector(scale(init$Tmax_dormantinsects))
PPT_dormantinsects <- as.vector(scale(init$Tmax_dormantinsects))

#scale all Temp data to same scale
Tmax <- init %>%
  dplyr::select(Nest_ID, Tmax:Tmax_l9) %>% #adjust if needed
  pivot_longer(Tmax:Tmax_l9,
               names_to = "lag",
               values_to = "temp") %>%
  mutate(temp = scale(temp)) %>%
  pivot_wider(names_from = "lag",
              values_from = "temp") %>%
  dplyr::select(-Nest_ID) %>%
  as.matrix()

PPT <- init %>%
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

ForestCV <- as.vector(scale(init$a1000_areacv))
ForestProx <- as.vector(scale(init$a1000_proxmn2))
Contag <- as.vector(scale(init$a1000_contag))
LandHa <- as.vector(scale(init$a1000_Ha))
LandBu <- as.vector(scale(init$a1000_RxBu))

# Response data -----------------------------------------------------------

y <- log(init$Init_day)

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
                 Year = Year,
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
                       "initday_JAGS_input_data.RDS"))

