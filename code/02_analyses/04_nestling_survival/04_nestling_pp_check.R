# Graphical posterior predictive check
# April 1, 2022
# Ana Miller-ter Kuile

# this is a hack of the bayesplot functionality to generate posterior
# predictive check graphs - specifically to assess - is the model family and link
# function I've selected appropriate for the data I have, or do I need to consider
# a different link or distribution (e.g. logit vs. cloglog link for binomial data; 
# poisson vs. negative binomial distribution for count data)

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


# GOF model ---------------------------------------------------------------

model <- readRDS(here("monsoon",
                      "10_19_22",
                      "nestling_survival",
                      "outputs",
                      "nestling_survival_model_GOF_10_19.RDS"))

# Load data ---------------------------------------------------------------

#and  we also need our original y data
source(here("code",
            "02_analyses",
            "04_nestling_survival",
            "01_nestling_data_prep.R"))

samples <- readRDS(here("monsoon",
                        "10_19_22",
                        "nestling_survival",
                        "outputs",
                        "nestling_survival_model_samples_10_19.RDS"))

# Extract observed data from DF -------------------------------------------

#we need to extract our observed data from our dataframe
y1 <- prod1 %>%
  dplyr::select(NoFL_uncert, Nest_ID) %>%
  #make this type "observed"
  mutate(type = "Observed") %>%
  mutate(Nest_ID = 1:n()) %>%
  mutate(Nest_ID = paste0("V", Nest_ID))


# Get yrep into DF format for graphing ------------------------------------


#extract the yreps, which for this model, which is an array of 
# iterations, nests, visits to nests, or a 3-D matrix
yreps <- model$sims.list$yrep

yrep <- as.data.frame(yreps) %>%
  pivot_longer(cols = c('V1':'V271'),
               names_to = "Nest_ID",
               values_to = "NoFL_uncert") %>%
  group_by(Nest_ID) %>%
  mutate(Iteration = n()) %>%
  mutate(type = "Simulated") %>%
  ungroup()


# Graph observed versus simulated -----------------------------------------


#posterior predictive check graphical observation
(pp_check_nestl <- ggplot() +
  #graph the simulated data
  geom_density(data = yrep, aes(x = NoFL_uncert, group = Iteration, fill = type), 
               alpha = 0.2) +
  geom_density(data = y1, aes(x = NoFL_uncert, fill = type), alpha = 0.5))


# Observed vs. predicted  --------------------------------------
y2 <- y1 %>%
  rename('observed' = 'NoFL_uncert') %>%
  dplyr::select(observed)

predicted <- (model$mean$yrep) 
predicted_ll <- (model$q2.5$yrep)
predicted_ul <- (model$q97.5$yrep)

mean <- y2 %>%
  cbind(predicted) %>%
  cbind(predicted_ll) %>%
  cbind(predicted_ul)

summary(lm(mean$predicted ~ mean$observed))

lb1 <- paste("R^2 == 0.75")

mean$density <- get_density(mean$observed, mean$predicted, n = 100)

(ob_pre <- ggplot(mean, aes(x = observed, y = predicted)) +
    geom_abline(slope =1, intercept = 0, linetype = 2) +
    geom_abline(slope = 0.73, intercept = 0.57) + 
    geom_linerange(aes(ymin = predicted_ll, ymax = predicted_ul)) +
    geom_point(aes(color = density)) +
    scale_color_viridis_c() +
    annotate(geom = "text", 
             x = 1, 
             y = 5,
             label = lb1,
             parse = T))
# 


(nstls_GOF <- pp_check_nestl + ob_pre +
    plot_annotation(tag_levels = "A",
                    title = "Nestling survival model GOF"))
#export this
ggsave(here("pictures",
            "Rfigures",
            "supplement",
            "GOF",
            "GOF_nstls.jpeg"),
       plot = nstls_GOF,
       units = "in",
       height = 4,
       width = 8)


# Residuals explorations --------------------------------------------------

Row_num <- 1:length(y)

nestling_data <- prod1 %>%
  mutate(Row_num = 1:n())

resid <- model$mean$resid %>%
  as.data.frame() %>%
  cbind(Row_num) %>%
  rename("residual" = ".") %>%
  filter(!is.na(residual)) %>%
  left_join(nestling_data, by = c("Row_num"))

m1 <- lm(residual ~ Project_ID,
         data = resid)
summary(m1)

siter <- paste("R^2 == 0.002")
(resid_site <- ggplot(resid, aes(x = Project_ID, y = residual)) +
    geom_boxplot() +
    #geom_jitter(height = 0) +
    geom_hline(yintercept = 0, linetype = 2) +
    annotate(geom = "text", 
             x = 3, 
             y = 3.75,
             label = siter,
             parse = T))


# Patterns in transect-level b0s ------------------------------------------

#this looks at whether nests on transects at different sites
# have consistently different b0s

names <- prod1 %>%
  distinct(Transect_ID2, Project_ID) %>%
  mutate(transect = 1:n())

b0 <- samples %>%
  dplyr::select(b0) %>%
  as_vector()

b0.transect <- samples %>% 
  dplyr::select('b0.transect[1]':'b0.transect[102]') %>%
  as.matrix()

b0.transect.t <- b0.transect - b0

b0.transect.df <- as.data.frame(b0.transect.t) %>%
  pivot_longer(cols = 'b0.transect[1]':'b0.transect[102]',
               names_to  = "Transect.num",
               values_to = "b0") %>%
  mutate(Transect.num = 
           str_sub(Transect.num, 
                   start = 13, end = 
                     str_length(Transect.num)-1)) %>%
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
  theme(axis.text.x = element_blank()) +
  labs(y = "median transect-level error") )


mod <- glm(median ~ Project_ID, 
           data = b0.transect.df)

summary(mod)

#COMBINE TO EXPORT
resid_site + transect_errors

(resid_nstls <- resid_site + transect_errors +
    plot_annotation(tag_levels = "A",
                    title = "Nestling survival model residuals and errors by forest"))

ggsave(here("pictures", 
            "Rfigures",
            "supplement",
            "residuals",
            "resid_nstls.jpeg"),
       plot = resid_nstls,
       units = "in",
       height = 4,
       width = 8)
