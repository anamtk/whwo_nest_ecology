# Assess model convergence and posteriors
# Ana Miller-ter Kuile
# MY 6 ,2022

#this script looks at the outputs of the egg production models

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
                           "8_24_23",
                           "egg_num",
                           "outputs",
                           "egg_num_Rhat.RDS"))

# Posterior distributions and trace plots ---------------------------------
parms <- c("b1", "b2", "b", "b0", "sig.transect", "sig.year",
           "wA", "deviance")

# Gelman-Rubin plots ------------------------------------------------------

Rhat <- Rhat[which(names(Rhat) %in% parms)]

(rhat_egg <- rhat_graph_fun2(Rhat) +
  labs(title = "Egg production model Gelman-Rubin statistic distribution") +
    scale_x_continuous(breaks = c(1, 1.02, 1.04, 1.06, 1.08, 1.1)))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "convergence",
            "egg_num_rhat.jpeg"),
       plot = rhat_egg,
       units = "in",
       height = 5,
       width = 7)
#to look at potentially problematic parameters:
rhat_graph_fun(Rhat)
