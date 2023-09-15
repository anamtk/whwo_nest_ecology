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
                        "8_24_23",
                        "init",
                        "outputs",
                        "init_model_summary_8_24.RDS"))

init_samp <- readRDS(here("monsoon",
                     "8_24_23",
                     "init",
                     "outputs",
                     "init_model_samples_8_24.RDS"))

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

init_exp <- init_d2 %>%
  dplyr::select(parameter, beta_50, beta_lower, beta_upper, p, category)

write.csv(init_exp, 
          file = here('data_outputs', '04_paper_tables',
                      'init_summary.csv'),
          row.names = F)


# Individual parameters ---------------------------------------------------
#b4 TmaxAnt, b11
 
# Landscape harvested -----------------------------------------------------
#b11 - LandHarvested
#need to add CI to these plots
ha <- scale_df(x = init_data$a1000_Ha,
              length = 20,
              name = "Harvest")

b0 <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm == "b0") %>%
  dplyr::select(`50%`) %>%
  as_vector()

blHa <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[11]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

regHa <- ha %>%
  mutate(reg = b0 + blHa*varS,
         exp_reg = exp(reg))

(lharvest_a <- ggplot() +
  geom_point(data = init_data, aes(x = a1000_Ha, y = Init_day),
             fill = "#BAE4BC", color = "black", shape = 21, alpha = 0.5) +
  geom_line(data = regHa, aes(x = Harvest, y = exp_reg),
            linewidth = 1.5, color = "black") +
  geom_line(data = regHa, aes(x = Harvest, y = exp_reg), 
            color = "#BAE4BC", linewidth = 1) +
  labs(x = "Percent landscape harvested",
       y = "Nest initation date (Julian date)"))


# ggsave(plot = lharvest_a,
#        filename = 'init_lharvest.pdf',
#        path = here("pictures", "Rfigures", "initiation"),
#        width = 3.5, height = 3.5,
#        units = "in")

# Temperature -------------------------------------------------------------

#pull out medians of weights as vector
#make climate a matrix of temp lags
#multiply each by the weight and take the sum for each row (observation)
#it would be a "monthly weighted z-score"
#beta for temp
blT <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[4]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

#get temparutres on scaled scale
temp_temp <- init_data %>%
  dplyr::select(Nest_ID, Tmax:Tmax_l9) %>% #adjust if needed
  pivot_longer(Tmax:Tmax_l9,
               names_to = "lag",
               values_to = "temp") %>%
  mutate(temp = scale(temp)) %>%
  pivot_wider(names_from = "lag",
              values_from = "temp") %>%
  dplyr::select(-Nest_ID) %>%
  as.matrix()

#make scaled data long format to get mean and sd
tmaxscale <- init_data %>%
  dplyr::select(Nest_ID, Tmax:Tmax_l9) %>% #adjust if needed
  pivot_longer(Tmax:Tmax_l9,
               names_to = "lag",
               values_to = "temp") 

#get mean and SD of OG data to back-transform stuff
mean <- mean(tmaxscale$temp)
sd <- sd(tmaxscale$temp)

#get weights per month
t_wt <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "wA")) %>%
  dplyr::select(`50%`) %>%
  as_vector()

#get tmax dataset
regT <- init_data %>%
  dplyr::select(Nest_ID, Init_day, Tmax:Tmax_l9)

#multiply months by their weights
regT$TAnt <- apply(temp_temp, MARGIN = 1, FUN = function(x){sum(x*t_wt)})

#revert Tmax to OG data scale
regT <- regT %>%
  dplyr::select(TAnt, Init_day) %>%
  mutate(Tmax = TAnt*sd + mean)

#regression prediction for Temperature
regT <- regT %>%
  mutate(reg = b0 + blT*TAnt,
         exp_reg = exp(reg))
# xscaled = (x – x̄) / s
# xscaled*sd + mean = x
xlab <-  expression("Weighted maximum temperature " ( degree*C))

(temp_a <- ggplot(regT) +
    geom_point(aes(x = Tmax, y = Init_day),
               fill = "#BAE4BC", color = "black", shape = 21, alpha = 0.5) +
    geom_line(aes(x = Tmax, y = exp_reg),
              linewidth = 1.5, color = "black") +
    geom_line(aes(x = Tmax, y = exp_reg), 
              color = "#BAE4BC", linewidth = 1) +
  labs(x = xlab,
       y = "Nest initation date (Julian date)"))

# ggsave(plot = temp_a,
#        filename = 'init_temperature.pdf',
#        path = here("pictures", "Rfigures", "initiation"),
#        width = 3.5, height = 3.5,
#        units = "in")
# Temp weights ------------------------------------------------------------


weight <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "wA")) %>%
  dplyr::select(`50%`)

weight_low <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "wA")) %>%
  dplyr::select(`2.5%`)

weight_high <- as.data.frame(init_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "wA")) %>%
  dplyr::select(`97.5%`)


weights <- as.data.frame(cbind(lag = months <- c(1:10),
weight = weight,
weight_l = weight_low,
weight_h = weight_high,
months = c("(May) 1", "(Apr) 2", "(Mar) 3", "(Feb) 4",
           "(Jan) 5", "(Dec) 6", "(Nov) 7", "(Oct) 8",
           "(Sep) 9", "(Aug) 10"))) 

(temp_weights <- ggplot(weights, aes(x = reorder(months, lag), y = `50%`)) +
  geom_hline(yintercept = 0.1, linetype = 2) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2) +
  labs(x = "Months before nesting season",
       y = "Temperature importance weight") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# ggsave(plot = temp_weights,
#        filename = 'temp_weights.pdf',
#        path = here("pictures", "Rfigures", "initiation"),
#        width = 3.25, height = 3.35,
#        units = "in")


# Patchwork? --------------------------------------------------------------

initplots <- lharvest_a + temp_a + temp_weights +
  plot_annotation(tag_levels = "A")

ggsave(plot = initplots,
       filename = 'init_plots.pdf',
       path = here("pictures", "Rfigures", "initiation"),
       width = 10, 
       height = 3.35,
       units = "in")

ggsave(plot = initplots,
       filename = 'init_plots.jpeg',
       path = here("pictures", "Rfigures", "initiation"),
       width = 10, 
       height = 3.35,
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

