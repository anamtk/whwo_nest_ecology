# Assess model convergence and posteriors
# Ana Miller-ter Kuile
# MY 6 ,2022

#this script looks at the outputs of the nest initiation

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  'coda', #get posterior MCMC outputs
                  "mcmcplots", #posterior graphing
                  "rjags")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#get model diagnostic plotting functions
source(here("code",
            "00_functions",
            "plot_functions.R"))

# Load model output -------------------------------------------------------

Rhat <- readRDS(here("monsoon", 
                     "10_19_22",
                     "initiation",
                     "outputs",
                     "initiation_Rhat.RDS"))



# Gelman-Rubin plots ------------------------------------------------------

parms <- c("b1", "b", "b0", 
           "sig.transect", "sig.year", 
           'sig', 'b0.year', 'b0.transect',
           "wA",  "deviance")


Rhat <- Rhat[which(names(Rhat) %in% parms)]
(rhat_init <- rhat_graph_fun2(Rhat) +
  labs(title = "Nest initation day model Gelman-Rubin statistic distribution")+
    scale_x_continuous(breaks = c(1, 1.02, 1.04, 1.06, 1.08, 1.1)))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "convergence",
            "init_day_rhat.jpeg"),
       plot = rhat_init,
       units = "in",
       height = 5,
      width = 7)

#to look at potentially problematic parameters:
rhat_graph_fun(Rhat)




