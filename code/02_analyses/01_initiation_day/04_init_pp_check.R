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
# to set system preferences for jags to run with parallel
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}



# Load GOF model ----------------------------------------------------------
model <- readRDS(here("monsoon",
                      "10_19_22",
                      "initiation",
                      "outputs",
                      "initiation_temp_JAGS_model_GOF_10_19.RDS"))

# Load data ---------------------------------------------------------------

#and we also need our original y data
#load the formatted data for the JAGS model
source(here("code", 
            "02_analyses",
            "01_initiation_day",
            "01_initiation_data_prep.R"))

samples <- readRDS(here("monsoon",
                        "10_19_22",
                        "initiation",
                        "outputs",
                        "initiation_temp_JAGS_model_samples_10_19.RDS"))



# Extract observed data from DF -------------------------------------------

#we need to extract our observed data from our dataframe
y1 <- init %>%
  dplyr::select(Init_day, Project_ID, Year_located) %>%
  #make this type "observed"
  mutate(Init_day = log(Init_day)) %>%
  mutate(type = "Observed")

# Get yrep into DF format for graphing ------------------------------------

#extract the yreps, which for this model, which is an array of 
# iterations, nests, visits to nests, or a 3-D matrix
yreps <- model$sims.list$yrep

yrep <- as.data.frame(yreps) %>%
  pivot_longer(cols = c('V1':'V294'),
               names_to = "Nest_ID",
               values_to = "Init_day") %>%
  group_by(Nest_ID) %>%
  mutate(Iteration = n()) %>%
  mutate(type = "Predicted") %>%
  ungroup()


# Graph observed versus simulated -----------------------------------------

#posterior predictive check graphical observation
(pp_check_init <- ggplot() +
  #graph the simulated data
  geom_density(data = yrep, aes(x = Init_day, group = Iteration, fill = type), 
               alpha = 0.2) +
  geom_density(data = y1, aes(x = Init_day, fill = type), alpha = 0.5))


# Another observed vs. predicted  graph -----------------------------------

y2 <- y1 %>%
  rename('observed' = 'Init_day') %>%
  dplyr::select(observed, Project_ID, Year_located) 

predicted <- (model$mean$yrep) 
predicted_ll <- (model$q2.5$yrep)
predicted_ul <- (model$q97.5$yrep)

mean <- y2 %>%
  cbind(predicted) %>%
  cbind(predicted_ll) %>%
  cbind(predicted_ul)

summary(lm(mean$predicted ~ mean$observed))

lb1 <- paste("R^2 == 0.26")

(ob_pre <- ggplot(mean, aes(x = observed, y = predicted)) +
    geom_abline(slope = 1, linetype = 2) +
    geom_abline(slope = 0.242, intercept = 3.79) +
    geom_linerange(aes(ymin = predicted_ll, ymax = predicted_ul)) +
    geom_point() +
    annotate(geom = "text", 
             x = 5.15, 
             y = 4.85,
             label = lb1,
             parse = T))

#COMBINE TO EXPORT

(GOF_init <- pp_check_init + ob_pre +
  plot_annotation(tag_levels = "A",
                  title = "Nest initiation day model GOF"))

ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "GOF",
            "GOF_init.jpeg"),
       plot = GOF_init,
       units = "in",
       height = 4,
       width = 8)
# Residuals explorations --------------------------------------------------

Row_num <- 1:length(y)

init_data <- init %>%
  mutate(Row_num = 1:n())

resid <- model$mean$resid %>%
  as.data.frame() %>%
  cbind(Row_num) %>%
  rename("residual" = ".") %>%
  filter(!is.na(residual)) %>%
  left_join(init_data, by = c("Row_num"))

m1 <- lm(residual ~ Project_ID,
         data = resid)
summary(m1)

siter <- paste("R^2 == 0.003")
(resid_site <- ggplot(resid, aes(x = Project_ID, y = residual)) +
    geom_boxplot() +
    #geom_jitter(height = 0) +
    geom_hline(yintercept = 0, linetype = 2) +
    annotate(geom = "text", 
             x = 3, 
             y = 0.25,
             label = siter,
             parse = T))

# Patterns in transect-level b0s ------------------------------------------

#this looks at whether nests on transects at different sites
# have consistently different b0s

names <- init %>%
  distinct(Transect_ID2, Project_ID) %>%
  mutate(transect = 1:n())

b0 <- samples %>%
  dplyr::select(b0) %>%
  as_vector()

b0.transect <- samples %>%
  dplyr::select("b0.transect[1]":"b0.transect[107]") %>%
  as.matrix()

b0.transect.t <- b0.transect - b0

b0.transect.df <- as.data.frame(b0.transect.t) %>%
  pivot_longer(cols = 'b0.transect[1]':'b0.transect[107]',
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

(transects_plot <- ggplot(b0.transect.df, aes(x = Transect_ID2, y = median, color = Project_ID)) +
  geom_point() +
  geom_errorbar(aes(ymin = median.LCI, ymax = median.UCI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_blank())  +
  labs(y = "median transect-level error"))


#COMBINE FOR EXPORT
(resid_init <- resid_site + transects_plot +
  plot_annotation(tag_levels = "A",
                  title = "Nest initiation day model residuals and errors by forest"))

ggsave(here("pictures", 
            "Rfigures",
            "supplement",
            "residuals",
            "resid_init.jpeg"),
       plot = resid_init,
       units = "in",
       height = 4,
       width = 8)

# Yrep vs Residuals -------------------------------------------------------

resid_yrep <- as.data.frame(cbind(yrep = model$mean$yrep,
                           residuals = model$mean$resid,
                           y = y,
                           Forest.num = Forest.num,
                           Year.num = Year.num))

ggplot(resid_yrep, aes(x = yrep, 
                       y = residuals, 
                       color = as.factor(Forest.num))) +
  geom_point()

ggplot(resid_yrep, aes(x = as.factor(Forest.num), 
                       y = residuals)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

ggplot(resid_yrep, aes(x = as.factor(Year.num), 
                       y = residuals)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

ggplot(resid_yrep, aes(x = yrep, 
                      y = residuals)) +
  geom_point()

ggplot(resid_yrep, aes(x = yrep, 
                       y = residuals, 
                       color = as.factor(Year.num))) +
  geom_point()

ggplot(resid_yrep, aes(x = y, 
                       y = residuals)) +
  geom_point()

ggplot(resid_yrep, aes(x = y, 
                       y = residuals, 
                       color = as.factor(Forest.num))) +
  geom_point()

ggplot(resid_yrep, aes(x = y, 
                       y = residuals, 
                       color = as.factor(Year.num))) +
  geom_point()


