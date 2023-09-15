# Assess model convergence and posteriors
# Ana Miller-ter Kuile
# MY 6 ,2022

#this script looks at the outputs of the fledge production models

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  'coda', #get posterior MCMC outputs
                  "bayesplot", #plot bayesian stuff
                  "tidybayes",#more bayesian plotting stuff
                  "mcmcplots", #posterior graphing
                  "patchwork",
                  "rjags")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#get rdRDS_files custom functoin
source(here("code",
            "00_functions",
            "tidy_functions.R"))

#get model diagnostic plotting functions
source(here("code",
            "00_functions",
            "plot_functions.R"))

# Load model output -------------------------------------------------------

Rhat <- readRDS(here("monsoon",
                       "8_24_23",
                       "nestling",
                       "outputs",
                       "nestling_survival_Rhat.RDS"))


# Gelman-Rubin Statistics graphs ------------------------------------------
parms <- c("b1TrtID", "b2SpeciesID", "b", 
           "b0", "b0.transect", "b0.year",
           "sig.transect", "sig.year",
           "deviance")

Rhat <- Rhat[which(names(Rhat) %in% parms)]

(rhat_nestling <- rhat_graph_fun2(Rhat) +
  labs(title = "Nestling survival model Gelman-Rubin statistic distribution") +
    scale_x_continuous(breaks = c(1, 1.02, 1.04, 1.06, 1.08, 1.1)))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "convergence",
            "nestling_sur_rhat.jpeg"),
       plot = rhat_nestling,
       units = "in",
       height = 5,
       width = 7)
#to look at potentially problematic parameters
rhat_graph_fun(Rhat)
