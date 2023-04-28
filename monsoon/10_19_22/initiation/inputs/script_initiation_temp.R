# Running the nest survival model
# Ana Miller-ter Kuile
# November 4, 2021

# Load packages ---------------------------------------------------------------
print("start")

Sys.time()


# Load packages
package.list <- c("jagsUI", "coda") 


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load Data ---------------------------------------------------------------

#load the formatted data for the JAGS model
data <- readRDS("/scratch/atm234/nests_10_19/initiation/inputs/initday_JAGS_input_data.RDS")

# Compile data ------------------------------------------------------------
data_list <- list(#overall values for likelihood loops
                  n.nests = data$n.nests,
                  #Random effects things
                  Transect.num = data$Transect.num,
                  Year.num = data$Year.num,
                  n.transects = data$n.transects,
                  n.years = data$n.years,
                  #Treatment covariate
                  TrtID = data$TrtID,
                  TrtTime = data$TrtTime,
                  NTrt = data$NTrt,
                  #local-level
                  Trees50 = data$Trees50,
                  Trees2550 = data$Trees2550,
                  PercPonderosa = data$PercPonderosa,
                  Year = data$Year,
                  #Temp data
                  Tmax = data$Tmax,
                  PPT = data$PPT,
                  #landscape-level
                  LandHa = data$LandHa,
                  LandBu = data$LandBu,
                  ForestCV = data$ForestCV,
                  ForestProx = data$ForestProx,
                  Contag = data$Contag,
                  #Data
                  y = data$y,
                  #numbers for prior distribution loops
                  n.trt = data$n.trt,
                  n.forests = data$n.forests,
                  n.lag = data$n.lag,
                  n.times = data$n.times,
                  Forest.num = data$Forest.num)

# Parameters to save ------------------------------------------------------

params <- c(#Environmental covariates
            'b',
            #Random covariate betas
            'b0',
            'b0.transect',
            'b0.year',
            'sig.transect',
            'sig.year',
            #overall variance
            'sig',
            "s1",
            "s0",
            #weights for antecedent climate
            'wA',
            #p-value objects
            'z')
          
          

# INits -------------------------------------------------------------------

inits <- readRDS("/scratch/atm234/nests_10_19/initiation/inputs/init_init_list.RDS")

# JAGS model --------------------------------------------------------------

egg_mod <- jagsUI::jags(data = data_list,
                        inits = inits,
                        #inits = NULL,
                        model.file = "/scratch/atm234/nests_10_19/initiation/inputs/model_initiation_temp.R",
                        parameters.to.save = params,
                        parallel = TRUE,
                        n.chains = 3,
                        n.burnin = 1000,
                        n.iter = 3E5,
                        DIC = TRUE)

#save as an R data object
saveRDS(egg_mod, 
        file = "/scratch/atm234/nests_10_19/initiation/outputs/initiation_temp_JAGS_model_10_19.RDS")

Sys.time()
