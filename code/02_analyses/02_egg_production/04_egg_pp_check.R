# Graphical posterior predictive check
# April 1, 2022
# Ana Miller-ter Kuile

# this is a hack of the bayesplot functionality to generate posterior
# predictive check graphs - specifically to assess - is the model family and link
# function I've selected appropriate for the data I have

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "coda", "bayesplot",
                  "jagsUI",
                  "reshape2")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())

source(here("code",
            "00_functions",
            "plot_functions.R"))

# Fix parallels -----------------------------------------------------------

#hopefully the parallels issue gets fixed, but for now this if statement works
# # to set system preferences for jags to run with parallel
# if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
#     Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
#   parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
# }
# 

# Load GOF model ----------------------------------------------------------

model <- readRDS(here("monsoon",
                      "8_24_23",
                      "egg_num",
                      "outputs",
                      "egg_number_model_GOF_8_24.RDS"))

# Load data ---------------------------------------------------------------

#and we also need our original y data
#it is called "egg" here
source(here("code",
            "02_analyses",
            "02_egg_production",
            "01_egg_data_prep.R"))

samples <- readRDS(here("monsoon",
                        "8_24_23",
                        "egg_num",
                        "outputs",
                        "egg_number_model_samples_8_24.RDS"))

# Extract observed data from DF -------------------------------------------

#we need to extract our observed data from our dataframe
y1 <- egg %>%
  dplyr::select(No_eggs) %>%
  #make this type "observed"
  mutate(type = "Observed")

# Get yrep into DF format for graphing ------------------------------------

#extract the yreps, which for this model, which is an array of 
# iterations, nests, visits to nests, or a 3-D matrix
yreps <- model$sims.list$yrep

yrep <- as.data.frame(yreps) %>%
  pivot_longer(cols = c('V1':'V140'),
               names_to = "Nest_ID",
               values_to = "No_eggs") %>%
  group_by(Nest_ID) %>%
  mutate(Iteration = n()) %>%
mutate(type = "Predicted") %>%
  ungroup()


# Graph observed versus simulated -----------------------------------------

#posterior predictive check graphical observation
(pp_check_eggnum <- ggplot() +
  #graph the simulated data
  geom_density(data = yrep, aes(x = No_eggs, group = Iteration, fill = type), 
               alpha = 0.2) +
  geom_density(data = y1, aes(x = No_eggs, fill = type), alpha = 0.5))

# Another observed vs. predicted  graph -----------------------------------

y2 <- y1 %>%
  rename('observed' = 'No_eggs') %>%
  dplyr::select(observed)

predicted <- (model$mean$yrep) 
predicted_ll <- (model$q2.5$yrep)
predicted_ul <- (model$q97.5$yrep)

mean <- y2 %>%
  cbind(predicted) %>%
  cbind(predicted_ll) %>%
  cbind(predicted_ul)

summary(lm(mean$predicted ~ mean$observed))

lb1 <- paste("R^2 == 0.21")

(ob_pre <- ggplot(mean, aes(x = observed, y = predicted)) +
    geom_abline(slope = 1, linetype = 2) +
    geom_abline(slope = 0.23, intercept = 3.03) +
    geom_linerange(aes(ymin = predicted_ll, ymax = predicted_ul)) +
    geom_point() +
    annotate(geom = "text", 
             x = 2.5, 
             y = 11,
             label = lb1,
             parse = T))


(eggn_GOF <- pp_check_eggnum + ob_pre +
  plot_annotation(tag_levels = "A",
                  title = "Egg number model GOF"))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "GOF",
            "GOF_eggn.jpeg"),
       plot = eggn_GOF,
       units = "in",
       height = 4,
       width = 8)


# Residuals explorations --------------------------------------------------

Row_num <- 1:length(y)

egg_data <- egg %>%
  mutate(Row_num = 1:n())

resid <- model$mean$resid %>%
  as.data.frame() %>%
  cbind(Row_num) %>%
  rename("residual" = ".") %>%
  filter(!is.na(residual)) %>%
  left_join(egg_data, by = c("Row_num"))

m1 <- lm(residual ~ Project_ID,
         data = resid)
summary(m1)

siter <- paste("R^2 == 0.01")
(resid_site <- ggplot(resid, aes(x = Project_ID, y = residual)) +
    geom_boxplot() +
    #geom_jitter(height = 0) +
    geom_hline(yintercept = 0, linetype = 2) +
    annotate(geom = "text", 
             x = 3, 
             y = 2.5,
             label = siter,
             parse = T))

# Patterns in transect-level b0s ------------------------------------------

#this looks at whether nests on transects at different sites
# have consistently different b0s

names <- egg %>%
  distinct(Transect_ID2, Project_ID) %>%
  mutate(transect = 1:n())

b0 <- samples %>%
  dplyr::select(b0) %>%
  as_vector()

b0.transect <- samples %>%
  dplyr::select("b0.transect[1]":"b0.transect[67]") %>%
  as.matrix()

b0.transect.t <- b0.transect - b0

b0.transect.df <- as.data.frame(b0.transect.t) %>%
  pivot_longer(cols = "b0.transect[1]":"b0.transect[67]",
               names_to  = "Transect.num",
               values_to = "b0") %>%
  mutate(Transect.num = 
           str_sub(Transect.num, start = 13, end = str_length(Transect.num)-1)) %>%
  mutate(Transect.num = as.integer(Transect.num)) %>%
  left_join(names, by = c("Transect.num" = "transect")) %>%
  group_by(Transect.num, Project_ID, Transect_ID2) %>%
  summarise(median = median(b0, na.rm = T),
            median.LCI = quantile(b0, prob = 0.025, type = 8, na.rm = T),
            median.UCI = quantile(b0, prob = 0.975, type = 8, na.rm = T))  

(transect_errors <- ggplot(b0.transect.df, aes(x = Transect_ID2, y = median, color = Project_ID)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point() +
  geom_errorbar(aes(ymin = median.LCI, ymax = median.UCI), width = 0.2) +
  theme(axis.text.x = element_blank())  +
    labs(y = "median transect-level error"))

#COMBINE TO EXPORT

(resid_eggn <- resid_site + transect_errors +
  plot_annotation(tag_levels = "A",
                  title = "Egg number model residuals and errors by forest"))

ggsave(here("pictures", 
            "Rfigures",
            "supplement",
            "residuals",
            "resid_eggn.jpeg"),
       plot = resid_eggn,
       units = "in",
       height = 4,
       width = 8)
