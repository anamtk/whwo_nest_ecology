#model selection

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "coda", "bayesplot",
                  "jagsUI")


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


# Fix parallels -----------------------------------------------------------

#hopefully the parallels issue gets fixed, but for now this if statement works
# to set system preferences for jags to run with parallel
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}


# Load model output -------------------------------------------------------

model_init_t <- readRDS(here("monsoon", 
                           "10_19_22",
                           "initiation",
                           "outputs",
                           "initiation_temp_JAGS_model_10_19.RDS"))

model_init_p <- readRDS(here("monsoon", 
                             "10_19_22",
                             "initiation",
                             "outputs",
                             "initiation_ppt_JAGS_model_10_19.RDS"))



# Update to check model selection objects ---------------------------------

#temp needs 6370/chain
#ppt needs 2898/chain for deviance

parms <- c("lpd", "pd", "Dsum")

t_selection <- update(model_init_t,
                      parameters.to.save = parms,
                      n.iter = 500)

p_selection <- update(model_init_p,
                      parameters.to.save = parms,
                      n.iter= 500)


# WAIC --------------------------------------------------------------------

t_sum <- summary(t_selection$samples)
p_sum <- summary(p_selection$samples)

#get pd rows
lppd_t <- sum(log(t_sum$statistics[331:660, 1]))
lppd_p <- sum(log(p_sum$statistics[331:660,1]))

#get lpd columsn
pwaic_t <- sum(t_sum$statistics[1:330, 2]^2)
pwaic_p <- sum(p_sum$statistics[1:330, 2]^2)

(waic_t <- lppd_t + pwaic_t)
(waic_p <- lppd_p + pwaic_p)

# DIC ---------------------------------------------------------------------

model_init_t$DIC
model_init_p$DIC

# Dinf --------------------------------------------------------------------

t_selection$mean$Dsum
p_selection$mean$Dsum
