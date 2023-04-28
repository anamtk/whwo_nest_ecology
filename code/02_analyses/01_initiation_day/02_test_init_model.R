# Running the nest initiation
# Ana Miller-ter Kuile
# November 4, 2021

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "jagsUI",
                  'rjags',
                  'mcmcplots',
                  "coda") #mcmc output


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#Sys.setenv(JAGS_HOME="C:/Program Files/JAGS/JAGS-4.3.0")
#library(R2jags)

source(here("code", 
            "00_functions",
            "plot_functions.R"))

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
            "01_initiation_day",
            "01_initiation_data_prep.R"))


# Compile data ------------------------------------------------------------

data <- list(#overall values for likelihood loops
  n.nests = n.nests,
  #Random effects things
  Transect.num = Transect.num,
  Year.num = Year.num,
  n.transects = n.transects,
  n.years = n.years,
  #Treatment covariate
  TrtID = TrtID,
  TrtTime = TrtTime,
  NTrt = NTrt,
  Year = Year,
  #local-level
  Trees50 = Trees50,
  Trees2550 = Trees2550,
  PercPonderosa = PercPonderosa,
  #Temp data
  PPT = PPT,
  Tmax = Tmax,
  #landscape-level
  LandHa = LandHa,
  LandBu = LandBu,
  ForestCV = ForestCV,
  ForestProx = ForestProx,
  Contag = Contag,
  #Data
  y = y,
  #numbers for prior distribution loops
  n.trt = n.trt,
  n.forests = n.forests,
  n.lag = n.lag,
  n.times = n.times,
  Forest.num = Forest.num)


# Parameters to save ------------------------------------------------------


params <- c("b",
            #'b1',
            "b0.transect",
            "b0.year",
            'sig.transect',
            'sig.year',
            "b0",
            's1',
            's0',
            'wA',
            'z')
            #'z.b1')


# JAGS model --------------------------------------------------------------

model <- here("code", 
              "02_analyses",
              "01_initiation_day",
              "jags", 
              "model_initiation_temp.R")


init_mod <- jagsUI::jags(data = data,
                        inits = NULL,
                        model.file = model,
                        parameters.to.save = params,
                        parallel = TRUE,
                        n.chains = 3,
                        n.iter = 3746,
                        DIC = TRUE)

# Check convergence -------------------------------------------------------

parms <- c(#"b1", 
           "b", 
           "b0", 
           "sig.transect", 
           "sig.year", 
           "wA", 
           "deviance")

mcmcplot(init_mod$samples, parms = parms)


# Raftery -----------------------------------------------------------------

raf_init <- raftery.diag(init_mod$samples)

names <- rownames(raf_init[[1]]$resmatrix)
ch1 <- raf_init[[1]]$resmatrix[,2]
ch2 <- raf_init[[2]]$resmatrix[,2]
ch3 <- raf_init[[3]]$resmatrix[,2]

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
            max = max(iterations, 
                      na.rm = T)/3)
# iterations_90 iterations_95   max
# <dbl>         <dbl> <dbl>
#   1        15018.        20408. 268883.

bu1 <- raf_init[[1]]$resmatrix[,1]
bu2 <- raf_init[[2]]$resmatrix[,1]
bu3 <- raf_init[[3]]$resmatrix[,1]

burn <- as.data.frame(cbind(names, bu1, bu2, bu3)) %>%
  mutate(bu1 = as.numeric(bu1),
         bu2 = as.numeric(bu2),
         bu3 = as.numeric(bu3)) %>%
  filter(!str_detect(names, "z")) %>%
  pivot_longer(bu1:bu3,
               names_to = "chain",
               values_to = 'iterations') 

burn %>%
  summarise(max(iterations, na.rm = T))
#130
#864


# Initials ----------------------------------------------------------------

b0.transect <- init_mod$mean$b0.transect
sig.transect <- init_mod$mean$sig.transect
b0.year <- c(init_mod$mean$b0.year[1:9], NA)
sig.year <- init_mod$mean$sig.year
b0 <- init_mod$mean$b0
#b1 <- c(NA, init_mod$mean$b1[2:length(init_mod$mean$b1)])
b <- init_mod$mean$b
#s0 <- init_mod$mean$s0
#s1 <- init_mod$mean$s1

# Set initials ------------------------------------------------------------

inits <- list(list(b0.transect = b0.transect,
                   sig.transect = sig.transect,
                   b0.year = b0.year,
                   sig.year = sig.year,
                   b0 = b0,
                   b = b),
              list(b0.transect = b0.transect +runif(length(b0.transect)),
                   sig.transect = sig.transect +runif(length(sig.transect)),
                   b0.year = b0.year + runif(length(b0.year)),
                   sig.year = sig.year + runif(length(sig.year)),
                   b0 = b0 + runif(length(b0)),
                   b = b + runif(length(b))),
              list(b0.transect = b0.transect -runif(length(b0.transect)),
                   sig.transect = sig.transect +runif(length(sig.transect)),
                   b0.year = b0.year - runif(length(b0.year)),
                   sig.year = sig.year + runif(length(sig.year)),
                   b0 = b0 - runif(length(b0)), 
                   b = b - runif(length(b))))


saveRDS(inits, 
        file = here("data_outputs", 
                    "02_monsoon",
                    "model_input_data",
                    "init_init_list.RDS"))

