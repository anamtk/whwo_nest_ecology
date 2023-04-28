# Exploration of correlation between environmental covariates
# Ana Miller-ter Kuile
# November 9, 2021

#  this script creates a correlation exploration between 
# the variables of interest for environmental covariates
# in the nest survival model


# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "GGally", "patchwork","glmmTMB",
                  "MuMIn", "DHARMa", "emmeans",
                  "effects", "ggeffects",
                  "emmeans", 
                  "glmm","caret")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Initiation --------------------------------------------------------------


# Load data ---------------------------------------------------------------

init_data <- read.csv(here("data_outputs",
                           "01_cleaning",
                           "03b_nest_initiation",
                           "Nest_initiation_data.csv"))


# Get variables to check for correlation ----------------------------------

colnames(init_data)

init_corr <- init_data %>%
  dplyr::select(Trees_2550, Trees_50,
                pPIPO, a1000_areacv2,
                a1000_contag, a1000_proxmn2,
                a1000_Ha, a1000_Bu, n_tx)

init_pairs <- ggpairs(init_corr)

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "init_env_corr.jpeg"),
       plot = init_pairs,
       width= 8,
       height =11,
       units = 'in')

#n_tx, and both area burned covariates are correlated

init_clim <- init_data %>%
  dplyr::select(Tmax:Tmax_l10, PPT:PPT_l10)

init_clim_pairs <- ggpairs(init_clim)

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "init_clim_corr.jpeg"),
       plot = init_clim_pairs,
       width= 15,
       height =15,
       units = 'in')
# Model selection ---------------------------------------------------------

m1 <- lm(log(Init_day) ~ n_tx,
              data = init_data)

m2 <- update(m1,  ~ -n_tx + a1000_Ha)

m3 <- update(m1,  ~ -n_tx + a1000_Bu)

AICc(m1, m2, m3)
# df      AICc
# m1  3 -881.5872
# m2  3 -884.3388
# m3  3 -881.3645

# Egg number --------------------------------------------------------------

# Load data ---------------------------------------------------------------

egg_data <- read.csv(here("data_outputs",
                           "01_cleaning",
                           "03_egg_production",
                           "Egg_production_data.csv"))


# Get variables to check for correlation ----------------------------------

colnames(egg_data)

egg_corr <- egg_data %>%
  dplyr::select(Trees_2550, Trees_50,
                pPIPO, a1000_areacv2,
                a1000_contag, a1000_proxmn2,
                a1000_Ha, a1000_Bu, n_tx, Init_day)

egg_cov <- ggpairs(egg_corr)


#n_tx, and both area burned covariates are correlated
ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "egg_corr.jpeg"),
       plot = egg_cov,
       width= 7,
       height =5,
       units = 'in')

#climate variables now
egg_clim <- egg_data %>%
  dplyr::select(Tmax:Tmax_l10, PPT:PPT_l10)

egg_clim_pairs <- ggpairs(egg_clim)

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "egg_clim_corr.jpeg"),
       plot = egg_clim_pairs,
       width= 15,
       height =15,
       units = 'in')

# Model selection ---------------------------------------------------------

m1 <- glm(No_eggs ~ n_tx,
         data = egg_data,
         family = "poisson")

m2 <- update(m1,  ~ -n_tx + a1000_Ha)

m3 <- update(m1,  ~ -n_tx + a1000_Bu)

AICc(m1, m2, m3)
# df     AICc
# m1  2 670.3733
# m2  2 670.3212
# m3  2 669.5200
#going with % tx variables


# Egg survival ------------------------------------------------------------


# Load data ---------------------------------------------------------------

eggnest_data <- read.csv(here("data_outputs",
                          "01_cleaning",
                          "03_nestling_survival",
                          "Egg_Nestling_survival_data.csv"))


# Get variables to check for correlation ----------------------------------

colnames(eggnest_data)

eggs_corr <- eggnest_data %>%
  dplyr::select(n_tx, Nest_Ht, cosOrientation,
                Init_day, Trees_2550, Trees_50,
                pPIPO, Tmax_eg, PPT_eg, a1000_pland2,
                a1000_np, a1000_Ha, a1000_Bu)

egg_corr_pairs <- ggpairs(eggs_corr)

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "eggs_corr.jpeg"),
       plot = egg_corr_pairs,
       width= 7,
       height =5,
       units = 'in')

# Model selection ---------------------------------------------------------

eggs_data <- eggnest_data %>% 
  filter(No_eggs >= No_young) %>%
  rowwise() %>%
  mutate(No_noyoung = No_eggs - No_young)

m1 <- glm(cbind(No_young, No_noyoung) ~ n_tx,
          data = eggs_data,
          family = "binomial")

m2 <- update(m1,  ~ -n_tx + a1000_Ha)

m3 <- update(m1,  ~ -n_tx + a1000_Bu)

AICc(m1, m2, m3)

#going with % tx variables


# Nestling survival -------------------------------------------------------

# Get variables to check for correlation ----------------------------------

colnames(eggnest_data)

nestl_corr <- eggnest_data %>%
  dplyr::select(n_tx, Nest_Ht, cosOrientation,
                Init_day, Trees_2550, Trees_50,
                pPIPO, Tmax_ne, PPT_ne, a1000_pland2,
                a1000_np, a1000_Ha, a1000_Bu)

ggpairs(eggs_corr)


# Model selection ---------------------------------------------------------

nestl_data <- eggnest_data %>% 
  filter(No_young >= NoFL_uncert) %>%
  filter(No_young > 0) %>%
  rowwise() %>%
  mutate(No_nofl = No_young - NoFL_uncert)

m1 <- glm(cbind(NoFL_uncert, No_nofl) ~ n_tx,
          data = nestl_data,
          family = "binomial")

m2 <- update(m1,  ~ -n_tx + a1000_Ha)

m3 <- update(m1,  ~ -n_tx + a1000_Bu)

AICc(m1, m2, m3)

#going with % tx variables




