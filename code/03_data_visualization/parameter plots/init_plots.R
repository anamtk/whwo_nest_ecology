#Initiation Day Plots
# Ana Miller-ter Kuile
# September 26, 2022

# this script brings in the model output as well as a 
# summaried 95% CI DF for the initiation day model
# and makes a all-parameters plot with p-values as well as a 
# predicted plto for each significant parameter

# Load packages -----------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "patchwork")


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

source(here::here("code", 
                  "00_functions",
                  "plot_functions.R"))

source(here::here("code",
                  "00_functions",
                  "tidy_functions.R"))


# Load model -------------------------------------------------------------

init_sum <- readRDS(here("monsoon",
                        "10_19_22",
                        "initiation",
                        "outputs",
                        "initiation_temp_JAGS_model_summary_10_19.RDS"))

init_samp <- readRDS(here("monsoon",
                     "10_19_22",
                     "initiation",
                     "outputs",
                     "initiation_temp_JAGS_model_samples_10_19.RDS"))

# Load data summary -------------------------------------------------------

init_d <- read.csv(here("data_outputs",
                       "03_model_results",
                       "init_parameter_mediansCIs.csv"))



# Data --------------------------------------------------------------------

init_data <- read.csv(here("data_outputs",
                           "01_cleaning",
                           "03b_nest_initiation",
                           "Nest_initiation_data.csv"))

# Data summary plot -------------------------------------------------------

init_d2 <- init_d %>%
  mutate(category = factor(category, levels = c("Landscape management (314 ha)",
                                                "Local habitat (0.4 ha)",
                                                "Landscape habitat (314 ha)",
                                                "Climate (27 ha)",
                                                "Variable interactions"))) %>%
  group_by(category) %>%
  arrange(category) %>%
  mutate(parameter = factor(parameter, levels=unique(parameter))) %>%
  mutate(p_levels = case_when((p >=0 & p <0.01) ~ "p < 0.01",
                              (p >= 0.01 & p<0.05) ~ "p < 0.05",
                              p == 0.05 ~ "p = 0.05",
                              TRUE ~ "ns"))%>%
  mutate(p_cat = case_when(p <= 0.05 ~ "s",
                           TRUE ~ "ns"))

(overall_init <- ggplot(init_d2, 
                         aes(x = parameter, y = beta_50, color = p_cat)) +
    geom_point(size =2) +
    geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), size = 1, width = 0.2) +
    geom_hline(yintercept = 0, linetype = 2) +
    scale_color_manual(values = c("black", "#BAE4BC")) +
    labs(y = "Covariate estimate \n (posterior median and 95% BCI)",
         x = "Covariate")+
    coord_flip() +
    theme(strip.text.y.left = element_text(angle = 0)) +
    facet_grid(category ~ ., 
               scales = "free_y", 
               space = "free_y") +
    theme(strip.text.y = element_text(angle = 0),
          legend.position = 'none',
          strip.background = element_rect(fill = "white")))


ggsave(plot = overall_init,
       filename = 'init_all_plot.pdf',
       path = here("pictures", "Rfigures", "initiation"),
       width = 8, height = 4,
       units = "in")


# Individual parameters ---------------------------------------------------
#b5-Tmax Ant, b9 - LandHarvested


b0.year <- as.data.frame(init_mod$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "b0.year")) 

b0.year %>%
  mutate(parameter = factor(parameter, levels = c("b0.year[1]", "b0.year[2]", "b0.year[3]",
                                        "b0.year[4]", "b0.year[5]","b0.year[6]",
                                        "b0.year[7]", "b0.year[8]", "b0.year[9]",
                                        "b0.year[10]"))) %>%
  ggplot(aes(x = parameter, y =`50%`)) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  geom_hline(yintercept = 0)


#b4-Tmax Ant, b8 - LandHarvested, 
# Landscape harvested -----------------------------------------------------
#b8 - LandHarvested
#need to add CI to these plots
t <- scale_df(x = init_data$a1000_Ha,
              length = 20,
              name = "Harvest")

lharvest_a <- lognormal_predicted(mod = init_sum,
                 beta = init_samp$`b[8]`,
                 metadf = t,
                 varS = varS,
                 var = "Harvest",
                 color = "#BAE4BC")


lharvest_b <- ggplot(init_data, aes(x = a1000_Ha)) +
  geom_boxplot(fill =  "#BAE4BC") +
  labs(x = "Percent landscape harvested") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(init_lharvest_plot <- lharvest_a/lharvest_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = init_lharvest_plot,
       filename = 'init_lharvest.pdf',
       path = here("pictures", "Rfigures", "initiation"),
       width = 3.5, height = 3.5,
       units = "in")

# Temperature -------------------------------------------------------------

temp_temp <- init_data %>%
  pivot_longer(Tmax:Tmax_l9,
               names_to = "lag",
               values_to = "Tmax") 


temp <- scale_df(temp_temp$Tmax,
                 name = "Temperature",
                 length = 20)


temp_a <- lognormal_predicted(mod = init_sum,
                               beta = init_samp$`b[4]`,
                               metadf = temp,
                               varS = varS,
                               var = 'Temperature',
                           color =  "#BAE4BC")


temp_b <- init_data %>%
  pivot_longer(Tmax:Tmax_l9,
               names_to = "lag",
               values_to = "Tmax") %>%
  ggplot( aes(x = Tmax)) +
  geom_boxplot(fill =  "#BAE4BC") +
  labs(x = "Maximum temperature") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(init_temp_plot <- temp_a/temp_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = init_temp_plot,
       filename = 'init_temp_all.pdf',
       path = here("pictures", "Rfigures", "initiation"),
       width = 3.5, height = 3.5,
       units = "in")

weight <- as.data.frame(init_sum$statistics) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "wA")) %>%
  dplyr::select(Mean)

weight_low <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "wA")) %>%
  dplyr::select(`2.5%`)

weight_high <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "wA")) %>%
  dplyr::select(`97.5%`)

weight_med <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "wA")) %>%
  dplyr::select(`50%`)

weights <- as.data.frame(cbind(lag = months <- c(1:10),
weight = weight,
weight_l = weight_low,
weight_h = weight_high,
weight_m = weight_med,
months = c("May", "April", "March", "February",
           "January", "December", "November", "October",
           "September", "August"))) 

(temp_weights <- ggplot(weights, aes(x = reorder(months, -lag), y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2) +
  labs(x = "Month",
       y = "Relative effect on nest initiation day") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

ggsave(plot = temp_weights,
       filename = 'temp_weights.pdf',
       path = here("pictures", "Rfigures", "initiation"),
       width = 3.25, height = 3.35,
       units = "in")

# Parameter values for text in paper --------------------------------------

#b5 = temp
#b9 = $harvest
#b1[2] = BURN
#get parameter summaries:
init_d2 %>%
  dplyr::select(-Mean) %>%
  filter(p_cat == "s") 

#get weights summaries:
weights


#b0 intercept
as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b0") %>%
  dplyr::select(`2.5%`, `50%`, `97.5%`) %>%
  mutate(across(is.numeric, ~ exp(.)))

#varinace terms
as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "sig")) %>%
  dplyr::select(parameter, `2.5%`, `50%`, `97.5%`)

