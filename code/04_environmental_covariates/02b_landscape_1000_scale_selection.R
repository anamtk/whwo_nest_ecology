# Landscape covariate correlation
# Ana Miller-ter Kuile
# June 7, 2022

# this script looks at which landscape scale variables
# are best predictors of responses

# followed up by 02c script, which then determines which of those
# top variables are uncorrelated
# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", "glmmTMB",
                  "readxl",
                  "MuMIn", "DHARMa",
                  "glmmTMB", "emmeans",
                  "effects", "ggeffects",
                  "emmeans", 
                  "glmm", "lubridate",
                  "caret")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

nests <- read.csv(here("data_outputs", "01_cleaning", 
                       "03_nest_survival",
                       "Nest_survival_data.csv"))

eggs <- read.csv(here("data_outputs",
                      "01_cleaning",
                      "03_egg_production",
                      "Egg_production_data.csv"))

fledge <- read.csv(here("data_outputs",
                        "01_cleaning",
                        "03_fledge_production",
                        "Fledge_production_data.csv"))


# NEST SURVIVAL -----------------------------------------------------------


# Prep data ---------------------------------------------------------------

colnames(nests)

nests1 <- nests %>%
  distinct(Nest_ID, Year_located, Project_ID,
           Transect_ID, Trt_50, Fate_cat, 
           a1000_areamn2, a1000_areaam2,
           a1000_areacv2, a1000_contag,
           a1000_pland2,
           a1000_pland1,
           a1000_lpi, a1000_lpi1, a1000_lpi2,
           a1000_np, a1000_np2, a1000_np1,
           a1000_proxmn2, a1000_Ha, a1000_Bu) %>%
  mutate(Fate_cat = case_when(Fate_cat == "success" ~ 1,
                              Fate_cat == "failure" ~ 0))



# Landscape variable selection --------------------------------------------

m1 <- glmmTMB(Fate_cat ~ a1000_areamn2 +
                (1|Year_located) + (1|Transect_ID),
              data = nests1,
              family = "binomial")

m2 <- update(m1,  ~ -a1000_areamn2 + a1000_areacv2)

m3 <- update(m1,  ~ -a1000_areamn2 + a1000_contag)

m4 <- update(m1,  ~ -a1000_areamn2 + a1000_pland2)

m5 <- update(m1,  ~ -a1000_areamn2 + a1000_areaam2)

m7 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi2)

m8 <- update(m1,  ~ -a1000_areamn2 + a1000_np)

m9 <- update(m1,  ~ -a1000_areamn2 + a1000_np1)

m10 <- update(m1,  ~ -a1000_areamn2 + a1000_np2)

m11 <- update(m1,  ~ -a1000_areamn2 + a1000_proxmn2)

AICc(m1, m2, m3, m4, m5,
     m7, m8, m9, m10, m11)

#best to worst:
#areaCV2, pland2, contag, np1, np, lpi2, areaam2,  np2, proxmn2, areamn2


# EGG PRODUCTION ----------------------------------------------------------


m1 <- glmmTMB(No_eggs_est ~ a1000_areamn2 +
                (1|Year_located) + (1|Transect_ID),
              data = eggs,
              family = "genpois")

m2 <- update(m1,  ~ -a1000_areamn2 + a1000_areacv2)

m3 <- update(m1,  ~ -a1000_areamn2 + a1000_contag)

m4 <- update(m1,  ~ -a1000_areamn2 + a1000_pland2)

m5 <- update(m1,  ~ -a1000_areamn2 + a1000_areaam2)

m7 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi2)

m8 <- update(m1,  ~ -a1000_areamn2 + a1000_np)

m9 <- update(m1,  ~ -a1000_areamn2 + a1000_np1)

m10 <- update(m1,  ~ -a1000_areamn2 + a1000_np2)

m11 <- update(m1,  ~ -a1000_areamn2 + a1000_proxmn2)

AICc(m1, m2, m3, m4, m5,
     m7, m8, m9, m10, m11)

#areacv2, lpi2, areaam2, pland2, proxmn2, contag, np2, np1, np, areamn2 

#Treatment codes:

met1 <-  glmmTMB(No_eggs_est ~ a1000_Ha +
                   (1|Year_located) + (1|Transect_ID),
                 data = eggs,
                 family = "poisson")

met2 <-  glmmTMB(No_eggs_est ~ a1000_Bu +
                   (1|Year_located) + (1|Transect_ID),
                 data = eggs,
                 family = "poisson")

AICc(met1, met2)

# FLEDGLING ---------------------------------------------------------------

# FLEDGLINGS ----------------------------------------------------------


m1 <- glmmTMB(NoFL_uncert ~ a1000_areamn2 +
                (1|Year_located) + (1|Transect_ID),
              data = fledge,
              family = "genpois")

m2 <- update(m1,  ~ -a1000_areamn2 + a1000_areacv2)

m3 <- update(m1,  ~ -a1000_areamn2 + a1000_contag)

m4 <- update(m1,  ~ -a1000_areamn2 + a1000_pland2)

m5 <- update(m1,  ~ -a1000_areamn2 + a1000_areaam2)

m7 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi2)

m8 <- update(m1,  ~ -a1000_areamn2 + a1000_np)

m9 <- update(m1,  ~ -a1000_areamn2 + a1000_np1)

m10 <- update(m1,  ~ -a1000_areamn2 + a1000_np2)

m11 <- update(m1,  ~ -a1000_areamn2 + a1000_proxmn2)

AICc(m1, m2, m3, m4, m5,
     m7, m8, m9, m10, m11)

#pland2, areacv2, lpi2, areaam2, proxmn2, np, np2, contag, np1, areamn2

# Harvest vs. burn at landsape

mt1 <- glmmTMB(NoFL_uncert ~ a1000_Ha +
                       (1|Year_located) + (1|Transect_ID),
                     data = fledge,
                     family = "poisson")

mt2 <- glmmTMB(NoFL_uncert ~ a1000_Bu +
                       (1|Year_located) + (1|Transect_ID),
                     data = fledge,
                     family = "poisson")

AICc(mt1, mt2)

#burn is a better predictor


