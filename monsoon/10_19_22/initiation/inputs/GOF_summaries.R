
# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("jagsUI", "coda",
                  "dplyr") #mcmc output


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Import model ------------------------------------------------------------

jags <- readRDS(file = "/scratch/atm234/nests_10_19/initiation/outputs/initiation_temp_JAGS_model_10_19.RDS")


# Update for goodness of fit ----------------------------------------------

parms <- c("yrep", 'resid')

jags.update <- update(jags,
                      parameters.to.save = parms,
                      n.iter = 500,
                      codaOnly = TRUE)

saveRDS(jags.update, 
        file= "/scratch/atm234/nests_10_19/initiation/outputs/initiation_temp_JAGS_model_GOF_10_19.RDS")


# Output summary stats and coda samples -----------------------------------

samples <- jags$samples


#pull all three chains out of the mcmc object
a <- as.matrix(samples[[1]])
b <- as.matrix(samples[[2]])
c <- as.matrix(samples[[3]])

#make this a dataframe bound by rows
samples_all <- as.data.frame(rbind(a,b,c))

#subset just a few for now because this is a LOT - can change the n 
# later if we want - I would probably aim for ~1000+ for any final
# analyses
samples_all2 <- slice_sample(samples_all, n = 4000)

saveRDS(samples_all2, 
        file= "/scratch/atm234/nests_10_19/initiation/outputs/initiation_temp_JAGS_model_samples_10_19.RDS")

summary <- summary(jags$samples)

saveRDS(summary, 
        file= "/scratch/atm234/nests_10_19/initiation/outputs/initiation_temp_JAGS_model_summary_10_19.RDS")


