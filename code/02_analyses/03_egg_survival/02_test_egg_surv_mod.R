# Running the nest survival model
# Ana Miller-ter Kuile
# November 4, 2021

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "jagsUI",
                  "R2jags", #jags wrapper
                  'mcmcplots',
                  "coda",
                  "bayesplot") #mcmc output


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#Sys.setenv(JAGS_HOME="C:/Program Files/JAGS/JAGS-4.3.0")
#library(R2jags)

# Fix parallels -----------------------------------------------------------

#hopefully the parallels issue gets fixed, but for now this if statement works
# to set system preferences for jags to run with parallel
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}


# Load Data ---------------------------------------------------------------

#load the formatted data for the JAGS model
source(here("code", 
            "02_analyses",
            "03_egg_survival",
            "01_egg_surv_data_prep.R"))


# Compile data ------------------------------------------------------------

data <- list(#overall values for likelihood loops
                n.nests = n.nests,
                #Random variables
                Transect.num = Transect.num,
                Year.num = Year.num,
                Forest.num = Forest.num,
                #Treatment covariate
                TreatmentID = TreatmentID,
                TrtTime = TrtTime,
                NTrt = NTrt,
                #Nest-level
                NestHt = NestHt,
                CosOrientation = CosOrientation,
                SpeciesID = SpeciesID,
                InitDay = InitDay,
                #local-level
                Trees50 = Trees50,
                Trees2550 = Trees2550,
                PercPonderosa = PercPonderosa,
                #landscape-level
                Tmax = Tmax,
                PPT = PPT,
                LandHa = LandHa,
                LandBu = LandBu,
                PForest = PForest,
                NPatches = NPatches,
                #Data
                y = y,
                N.eggs = N.eggs,
                N.nestlings = N.nestlings,
                #numbers for prior distribution loops
                n.transects = n.transects,
                n.years = n.years,
                n.trt = n.trt,
                n.species = n.species,
                n.forests = n.forests,
                n.times = n.times)


# Parameters to save ------------------------------------------------------


params <- c(#Random covariate betas
            'b0.transect',
            'b0.year',
            'b0',
            #Variance/precision
            'sig.transect',
            'sig.year',
            'b1TrtID',
            'b2SpeciesID',
            'b',
            'z.b1',
            'z.b2',
            'z'
               )


# JAGS model --------------------------------------------------------------


model <- here("code", 
              "02_analyses",
              "03_egg_survival",
              "jags", 
              "model_egg_survival_pvalue.R")

egg_surv_mod <- jagsUI::jags(data = data,
                            inits = NULL,
                            model.file = model,
                            parameters.to.save = params,
                            parallel = TRUE,
                            n.chains = 3,
                            n.iter = 4000,
                            DIC = TRUE)


# Check convergence and raftery -------------------------------------------

parms <- c(#Random covariate betas
            'b0.transect',
            'b0.year',
            'b0',
            #Variance/precision
            'sig.transect',
            'sig.year',
            'b1TrtID',
            'b2SpeciesID',
            'b',
            'deviance'
          )


raf_egg_s <- raftery.diag(egg_surv_mod$samples)

names <- rownames(raf_egg_s[[1]]$resmatrix)
ch1 <- raf_egg_s[[1]]$resmatrix[,2]
ch2 <- raf_egg_s[[2]]$resmatrix[,2]
ch3 <- raf_egg_s[[3]]$resmatrix[,2]

raf_all <- as.data.frame(cbind(names, 
                               ch1, ch2, ch3)) %>%
  mutate(ch1 = as.numeric(ch1),
         ch2 = as.numeric(ch2),
         ch3 = as.numeric(ch3)) %>%
  filter(!str_detect(names, "z")) %>%
  pivot_longer(ch1:ch3,
               names_to = "chain",
               values_to = 'iterations') 

ggplot(raf_all, aes(x = iterations/3)) +
  geom_histogram() 

raf_all %>% 
  summarise(iterations_90 = quantile(iterations, 
                                     probs = 0.9, 
                                     na.rm = T)/3,
            iterations_95 = quantile(iterations,
                                     probs = 0.95,
                                     na.rm = T)/3,
            iterations_max = max(iterations, na.rm = T)/3)

# A tibble: 1 × 3
# iterations_90 iterations_95 iterations_max
# <dbl>         <dbl>          <dbl>
#   1          7191          9020         23124.

bu1 <- raf_egg_s[[1]]$resmatrix[,1]
bu2 <- raf_egg_s[[2]]$resmatrix[,1]
bu3 <- raf_egg_s[[3]]$resmatrix[,1]

burn_all <- as.data.frame(cbind(names, 
                                bu1, bu2, bu3)) %>%
  mutate(bu1 = as.numeric(bu1),
         bu2 = as.numeric(bu2),
         bu3 = as.numeric(bu3)) %>%
  filter(!str_detect(names, "z")) %>%
  pivot_longer(bu1:bu3,
               names_to = "chain",
               values_to = 'iterations') 

burn_all %>%
  summarise(max_iterations = max(iterations, na.rm = T))

#64

# Get initials ------------------------------------------------------------
b0.transect <- egg_surv_mod$mean$b0.transect
b0.year <- c(egg_surv_mod$mean$b0.year[1:9], NA)
b0 <- egg_surv_mod$mean$b0
sig.transect <- egg_surv_mod$mean$sig.transect
sig.year <- egg_surv_mod$mean$sig.year
b1TrtID <- c(NA, egg_surv_mod$mean$b1TrtID[2:length(egg_surv_mod$mean$b1TrtID)])
b2SpeciesID <- c(NA, egg_surv_mod$mean$b2SpeciesID[2: length(egg_surv_mod$mean$b2SpeciesID)])
b <- egg_surv_mod$mean$b

inits <- list(list(b0.transect = b0.transect,
                   b0.year = b0.year,
                   b0 = b0,
                   sig.transect = sig.transect,
                   sig.year = sig.year,
                   b1TrtID = b1TrtID,
                   b2SpeciesID = b2SpeciesID,
                   b = b),
              list(b0.transect = b0.transect +runif(length(b0.transect)),
                   b0.year = b0.year +runif(b0.year),
                   b0 = b0 + runif(length(b0)),
                   sig.transect = sig.transect + runif(length(sig.transect)),
                   sig.year = sig.year + runif(length(sig.year)),
                   b1TrtID = b1TrtID + runif(length(b1TrtID)),
                   b2SpeciesID = b2SpeciesID + runif(length(b2SpeciesID)),
                   b = b + runif(length(b))),
              list(b0.transect = b0.transect -runif(length(b0.transect)),
                   b0.year = b0.year -runif(b0.year),
                   b0 = b0 - runif(length(b0)),
                   sig.transect = sig.transect + runif(length(sig.transect)),
                   sig.year = sig.year + runif(length(sig.year)),
                   b1TrtID = b1TrtID - runif(length(b1TrtID)),
                   b2SpeciesID = b2SpeciesID - runif(length(b2SpeciesID)),
                   b = b -runif(length(b))))

saveRDS(inits,
        file = (here("data_outputs",
                     "02_monsoon",
                     "model_input_data",
                     "egg_s_inits_list.RDS")))



