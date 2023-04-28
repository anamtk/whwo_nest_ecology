# Running the nest survival model
# Ana Miller-ter Kuile
# November 4, 2021

# Load packages ---------------------------------------------------------------
Sys.time()
# Load packages
package.list <- c("jagsUI", 'coda') 

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load Data ---------------------------------------------------------------

data <- readRDS("/scratch/atm234/nests_10_19/egg_survival/inputs/egg_survival_JAGS_input_data.RDS")

# Compile data ------------------------------------------------------------

data.list <- list(#overall values for likelihood loops
                  n.nests = data$n.nests,
                  #Random variables
                  Transect.num = data$Transect.num,
                  Year.num = data$Year.num,
                  Forest.num = data$Forest.num,
                  #Treatment covariate
                  TreatmentID = data$TreatmentID,
                  TrtTime = data$TrtTime,
                  NTrt = data$NTrt,
                  #Nest-level
                  NestHt = data$NestHt,
                  CosOrientation = data$CosOrientation,
                  SpeciesID = data$SpeciesID,
                  InitDay = data$InitDay,
                  #local-level
                  Trees50 = data$Trees50,
                  Trees2550 = data$Trees2550,
                  PercPonderosa = data$PercPonderosa,
                  #landscape-level
                  Tmax = data$Tmax,
                  PPT = data$PPT,
                  LandHa = data$LandHa,
                  LandBu = data$LandBu,
                  PForest = data$PForest,
                  NPatches = data$NPatches,
                  #Data
                  y = data$y,
                  N.eggs = data$N.eggs,
                  N.nestlings = data$N.nestlings,
                  #numbers for prior distribution loops
                  n.transects = data$n.transects,
                  n.years = data$n.years,
                  n.trt = data$n.trt,
                  n.species = data$n.species,
                  n.forests = data$n.forests,
                  n.times = data$n.times)

# Parameters to save ------------------------------------------------------

params <- c(#Random covariate betas
            'b0.transect',
            'b0.year',
            'b0',
            #Variance/precision
            'sig.transect',
            'sig.year',
            #covariates
            'b1TrtID',
            'b2SpeciesID',
            'b',
            'z.b1',
            'z.b2',
            'z'
)

# Initial z values --------------------------------------------------------

inits <- readRDS("/scratch/atm234/nests_10_19/egg_survival/inputs/egg_s_inits_list.RDS")
# JAGS model --------------------------------------------------------------

egg_surv_mod <- jagsUI::jags(data = data.list,
                            #inits = NULL,
                            inits = inits,
                            model.file = "/scratch/atm234/nests_10_19/egg_survival/inputs/model_egg_survival.R",
                            parameters.to.save = params,
                            parallel = TRUE,
                            n.chains = 3,
                            n.burnin = 1000,
                            n.iter = 23000,
                            DIC = TRUE)


#save coda and model output
saveRDS(egg_surv_mod,
        file = "/scratch/atm234/nests_10_19/egg_survival/outputs/egg_survival_model_10_19.RDS")

Sys.time()






