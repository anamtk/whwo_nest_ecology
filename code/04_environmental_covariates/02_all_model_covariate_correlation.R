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
                  "emmeans", 'ggcorrplot')


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
                pPIPO, 
                a1000_areamn2:a1000_RxBu, n_tx) %>%
  rename_with(str_sub, start = 7L, .cols = starts_with("a1000")) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.factor, as.numeric)

(init_pairs <- ggcorrplot(cor(init_corr, use = "complete.obs"), hc.order = FALSE, type = "lower",
                          lab = TRUE))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "init_env_corr.jpeg"),
       plot = init_pairs,
       width= 8,
       height =8,
       units = 'in')
#ntx correlated with both landscape tx variables
#will go with the %tx variables

init_corr2 <- init_data %>%
  dplyr::select(a1000_areamn2:a1000_proxmn2) %>%
  rename_with(str_sub, start = 7L, .cols = starts_with("a1000"))

#(init_pairs2 <- ggpairs(init_corr2))
(init_pairs2 <- ggcorrplot(cor(init_corr2), hc.order = TRUE, type = "lower",
           lab = TRUE))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "init_land_corr.jpeg"),
       plot = init_pairs2,
       width= 8,
       height =8,
       units = 'in')

init_clim <- init_data %>%
  dplyr::select(Tmax:Tmax_l10, PPT:PPT_l10)

(init_clim_pairs <- ggcorrplot(cor(init_clim), hc.order = TRUE, type = "lower",
                          lab = TRUE))
#these aren't really that correlated... HMM...

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "init_clim_corr.jpeg"),
       plot = init_clim_pairs,
       width= 10,
       height =10,
       units = 'in')

# Model selection ---------------------------------------------------------

m1 <- lm(log(Init_day) ~ a1000_areamn2,
              data = init_data)

m2 <- update(m1,  ~ -a1000_areamn2 + a1000_areaam2)

m3 <- update(m1,  ~ -a1000_areamn2 + a1000_pland2)

m4 <- update(m1,  ~ -a1000_areamn2 + a1000_pland1)

m5 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi1)

m6 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi2)

m7 <- update(m1,  ~ -a1000_areamn2 + a1000_np)

m8 <- update(m1,  ~ -a1000_areamn2 + a1000_np1)

m9 <- update(m1,  ~ -a1000_areamn2 + a1000_np2)

m10 <- update(m1,  ~ -a1000_areamn2 + a1000_proxmn2)


AICc(m1, m2, m3, m4,
     m5, m6, m7, m8,
     m9, m10)

# df      AICc
# m7   3 -851.6075 a1000_np 
# m9   3 -851.2034 a1000_np2 X
# m5   3 -850.6416 a1000_lpi1 
# m1   3 -850.1440 a1000_areamn2 
# m3   3 -850.0881 a1000_pland2 X
# m4   3 -850.0028 a1000_pland1 X
# m8   3 -849.8624 a1000_np1  X
# m10  3 -849.8277 a1000_proxmn2 X
# m6   3 -849.7885 a1000_lpi2 X 
# m2   3 -849.7806 a1000_areaam2 X

#highest while still uncorrelated:
# a1000_np 
# a1000_lpi1 
#a1000_areamn2

#uncorrelated:
#a1000_areacv2 #variation in forest patch sizes
#a1000_contag #some combo of patchiness
#a1000_lpi #largest patch index

#should I include all of them? Maybe not both patch indices

m11 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi)

AICc(m11, m5)

#lpi is better AIC

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
                pPIPO, a1000_areamn2:a1000_RxBu,
                n_tx, Init_day) %>%
  rename_with(str_sub, start = 7L, .cols = starts_with("a1000"))


(egg_pairs <- ggcorrplot(cor(egg_corr, use = "complete.obs"), hc.order = FALSE, type = "lower",
                          lab = TRUE))

#n_tx, and both area burned covariates are correlated
#going with %tx variables
ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "egg_env_corr.jpeg"),
       plot = egg_pairs,
       width= 8,
       height =8,
       units = 'in')


egg_corr2 <- egg_data %>%
  dplyr::select(a1000_areamn2:a1000_proxmn2) %>%
  rename_with(str_sub, start = 7L, .cols = starts_with("a1000"))


(egg_pairs2 <- ggcorrplot(cor(egg_corr2), hc.order = TRUE, type = "lower",
                          lab = TRUE))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "egg_land_corr.jpeg"),
       plot = egg_pairs2,
       width= 8,
       height =8,
       units = 'in')

#climate variables now
egg_clim <- egg_data %>%
  dplyr::select(Tmax:Tmax_l10, PPT:PPT_l10)

(egg_clim_pairs <- ggcorrplot(cor(egg_clim), hc.order = TRUE, type = "lower",
                          lab = TRUE))
#not super highly correlated... hmmm

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "egg_clim_corr.jpeg"),
       plot = egg_clim_pairs,
       width= 10,
       height =10,
       units = 'in')

# Model selection ---------------------------------------------------------

m1 <- glm(No_eggs ~ a1000_areamn2,
          data = egg_data,
          family = "poisson")

m2 <- update(m1,  ~ -a1000_areamn2 + a1000_areaam2)

m3 <- update(m1,  ~ -a1000_areamn2 + a1000_pland2)

m4 <- update(m1,  ~ -a1000_areamn2 + a1000_pland1)

m5 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi1)

m6 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi2)

m7 <- update(m1,  ~ -a1000_areamn2 + a1000_np)

m8 <- update(m1,  ~ -a1000_areamn2 + a1000_np1)

m9 <- update(m1,  ~ -a1000_areamn2 + a1000_np2)

m10 <- update(m1,  ~ -a1000_areamn2 + a1000_proxmn2)


AICc(m1, m2, m3, m4,
     m5, m6, m7, m8,
     m9, m10)

# df     AICc
# m2   2 665.5855 a1000_areaam2
# m6   2 665.7663 a1000_lpi2 X
# m7   2 665.8271 a1000_np
# m10  2 665.9239 a1000_proxmn2 X
# m3   2 666.0564 a1000_pland2 X
# m1   2 666.2167 a1000_areamn2 X
# m9   2 666.4315 a1000_np2 X
# m8   2 666.7359 a1000_np1 
# m5   2 666.7709 a1000_lpi1 X
# m4   2 666.9088 a1000_pland1 X

#highest while still uncorrelated
#a1000_areaam2 (top 5)
#a1000_np (top 5)
#a1000_np1

#uncorrelated 
#a1000_areacv2
#a1000_contag
#a1000_lpi

#according to cushman paper:
#at class level, areacv2 and areaam2 are redundant

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
                pPIPO, Tmax_eg, PPT_eg, a1000_areamn2:a1000_RxBu)


(eggs_pairs <- ggcorrplot(cor(eggs_corr, use = "complete.obs"), hc.order = FALSE, type = "lower",
                         lab = TRUE))

#ntx and % treated are correlated
#going with % tx variables
ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "eggs_env_corr.jpeg"),
       plot = eggs_pairs,
       width= 10,
       height =10,
       units = 'in')


eggs_corr2 <- eggnest_data %>%
  dplyr::select(a1000_areamn2:a1000_proxmn2) %>%
  rename_with(str_sub, start = 7L, .cols = starts_with("a1000"))


(eggs_pairs2 <- ggcorrplot(cor(eggs_corr2), hc.order = TRUE, type = "lower",
                          lab = TRUE))
# 
ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "covariates",
            "eggs_land_corr.jpeg"),
       plot = egg_pairs2,
       width= 8,
       height =8,
       units = 'in')

# Model selection ---------------------------------------------------------

eggs_data <- eggnest_data %>% 
  filter(No_eggs >= No_young) %>%
  rowwise() %>%
  mutate(No_noyoung = No_eggs - No_young)

m1 <- glm(cbind(No_young, No_noyoung) ~ a1000_areamn2,
          data = eggs_data,
          family = "binomial")

m2 <- update(m1,  ~ -a1000_areamn2 + a1000_areaam2)

m3 <- update(m1,  ~ -a1000_areamn2 + a1000_pland2)

m4 <- update(m1,  ~ -a1000_areamn2 + a1000_pland1)

m5 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi1)

m6 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi2)

m7 <- update(m1,  ~ -a1000_areamn2 + a1000_np)

m8 <- update(m1,  ~ -a1000_areamn2 + a1000_np1)

m9 <- update(m1,  ~ -a1000_areamn2 + a1000_np2)

m10 <- update(m1,  ~ -a1000_areamn2 + a1000_proxmn2)


AICc(m1, m2, m3, m4,
     m5, m6, m7, m8,
     m9, m10)

# df     AICc
# m3   2 499.0544 a1000_pland2
# m5   2 500.1760 a1000_lpi1 
# m6   2 500.5669 a1000_lpi2 X
# m10  2 500.9621 a1000_proxmn2 X
# m2   2 501.2450 a1000_areaam2 X
# m9   2 501.4476 a1000_np2 
# m8   2 501.9562 a1000_np1 X
# m4   2 502.7736 a1000_pland1 X
# m1   2 502.9929 a1000_areamn2 X
# m7   2 503.1166 a1000_np X

#highest correlated
#pland2 (top 5, 2+AIC higher than loewst model)
#lpi1 (top 5, 2+AIC higher than lowest model)
#np2 (2+AIC higher than lowest model)

#uncorrelated
#a1000_aracv2
#a1000_contag
#a1000_lpi



# Nestling survival -------------------------------------------------------

# Get variables to check for correlation ----------------------------------

#these will all be exactly the same as for egg above,
#but the ones that are important may nto be consistent 
#between them

# Model selection ---------------------------------------------------------

nestl_data <- eggnest_data %>% 
  filter(No_young >= NoFL_uncert) %>%
  filter(No_young > 0) %>%
  rowwise() %>%
  mutate(No_nofl = No_young - NoFL_uncert)

m1 <- glm(cbind(NoFL_uncert, No_nofl) ~ a1000_areamn2,
          data = nestl_data,
          family = "binomial")

m2 <- update(m1,  ~ -a1000_areamn2 + a1000_areaam2)

m3 <- update(m1,  ~ -a1000_areamn2 + a1000_pland2)

m4 <- update(m1,  ~ -a1000_areamn2 + a1000_pland1)

m5 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi1)

m6 <- update(m1,  ~ -a1000_areamn2 + a1000_lpi2)

m7 <- update(m1,  ~ -a1000_areamn2 + a1000_np)

m8 <- update(m1,  ~ -a1000_areamn2 + a1000_np1)

m9 <- update(m1,  ~ -a1000_areamn2 + a1000_np2)

m10 <- update(m1,  ~ -a1000_areamn2 + a1000_proxmn2)


AICc(m1, m2, m3, m4,
     m5, m6, m7, m8,
     m9, m10)

# df     AICc
# m3   2 720.7707 a1000_pland2
# m6   2 724.7134 a1000_lpi2 X
# m2   2 725.7940 a1000_areaam2 X
# m1   2 726.1598 a1000_areamn2 X
# m10  2 728.9566 a1000_proxmn2 X
# m5   2 731.1329  a1000_lpi1
# m8   2 731.8880 a1000_np1 X
# m9   2 731.9304 a1000_np2
# m7   2 731.9459 a1000_np X
# m4   2 731.9481 a1000_pland1 X

#uncorrelated
# a1000_areacv2
# a1000_contag
# a1000_lpi

#highest without correlations
#a1000_pland2 (top 5, 2+ AIC higher than lowest model)
#a1000_lpi1
#a1000_np2


# Across models -----------------------------------------------------------

#initiation 
#highest while still uncorrelated:
#np
#lpi1
#areamn2

#uncorrelated:
#a1000_areacv2 #variation in forest patch sizes
#a1000_contag #some combo of patchiness
#a1000_lpi #largest patch index

#egg num
#highest while still uncorrelated
#a1000_areaam2 (top 5)
#a1000_np (top 5)
#a1000_np1

#uncorrelated 
#a1000_areacv2
#a1000_contag
#a1000_lpi

#egg S
#highest correlated
#pland2 (top 5, 2+AIC higher than loewst model)
#lpi1 (top 5, 2+AIC higher than lowest model)
#np2 (2+AIC higher than lowest model)

#uncorrelated
#a1000_aracv2
#a1000_contag
#a1000_lpi

#nestling S
#uncorrelated
# a1000_areacv2
# a1000_contag
# a1000_lpi

#highest without correlations
#a1000_pland2 (top 5, 2+ AIC higher than lowest model)
#a1000_lpi1
#a1000_np2

#In general thoughts:
#I don't think it makes sense to have variables that
#are somewhat redundant - such as lpi of all patches
#vs lpi of only open patches - this seems sort of 
#redundant to me- but honestly, so many of the metrics
#feel redundant to me.

#it seems to make sense to sort of make things consistent
#across models - to the extent that we can. For example,
#it seems that there is high overlap in the predictors for
#landscape predictors for egg and nestling survival. IT seems
#to make sense to just use the overlapping variables as a 
#way to link them to consistent drivers. especially because
#there are such clearly consistent variables for the most 
#part in these two sets of models

#from cushman paper:
#LPI and AREA_AM are redundant at class level
#LPI and Area_CV are redundant at landscape level


# All models: landscape variables -----------------------------------------

#initiation
#np
#areamn2
#areacv2
#contag
#lpi

#initiation and egg number:
#a1000_areacv2 #variation in forest patch sizes
#a1000_contag #some combo of patchiness
#a1000_lpi #largest patch index
##NOT INCLUDED: a1000_areaam2 (top5) #weighted mean of forest patches (redundant with areacv)
# a1000_np1 (top5) #number open patches

#egg and nestling survival
#a1000_areacv2
#a1000_contag
#a1000_lpi
#a1000_pland2 
#a1000_lpi1 #not going to iniclude becuase feels redundant of LPI
#a1000_np2
