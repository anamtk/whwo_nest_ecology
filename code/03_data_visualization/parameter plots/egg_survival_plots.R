# Egg survival plots
# Ana Miller-ter Kuile
# September 26, 2022


# this script brings in the model output as well as a 
# summaried 95% CI DF for the egg survival model
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

egg_s_sum <- readRDS(here("monsoon",
                          "8_24_23",
                          "egg_s",
                          "outputs",
                          "egg_survival_model_summary_8_24.RDS"))

egg_s_samp <- readRDS(here("monsoon",
                          "8_24_23",
                          "egg_s",
                          "outputs",
                          "egg_survival_model_samples_8_24.RDS"))


# Load data summary -------------------------------------------------------

egg_s <- read.csv(here("data_outputs",
                       "03_model_results",
                       "egg_survival_parameter_mediansCIs.csv"))


# Load OG data ------------------------------------------------------------

#and we also need our original y data
egg_s_data <- read.csv(here("data_outputs",
                       "01_cleaning",
                       "03_nestling_survival",
                       "Egg_Nestling_survival_data.csv"))


egg_s_data <- egg_s_data %>%
  filter(Type == "egg") %>%
  filter(No_eggs >= No_young)

# Summary plot ------------------------------------------------------------

egg_s2 <- egg_s %>%
  mutate(category = factor(category, levels = c("Nest management (2.25 ha)",
                                                "Landscape management (314 ha)",
                                                "Nest habitat",
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
                              TRUE ~ "ns")) %>%
  mutate(p_cat = case_when(p <= 0.05 ~ "s",
                           TRUE ~ "ns"))

eggs_exp <- egg_s2 %>%
  dplyr::select(parameter, beta_50, beta_lower, beta_upper, p, category)

write.csv(eggs_exp, 
          file = here('data_outputs', '04_paper_tables',
                      'eggs_summary.csv'),
          row.names = F)

# (egg_s_all <- ggplot(egg_s2, 
#        aes(x = parameter, y = beta_50, color = p_cat)) +
#   geom_point(size =2) +
#   geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), size = 1, width = 0.2) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   scale_color_manual(values = c("black", "#43A2CA"
#                                 )) +
#     labs(y = "Covariate estimate \n (posterior median and 95% BCI)",
#          x = "Covariate")+
#   coord_flip() +
#   theme(strip.text.y.left = element_text(angle = 0)) +
#   facet_grid(category ~ ., 
#              scales = "free_y", 
#              space = "free_y") +
#     theme(strip.text.y = element_text(angle = 0),
#           legend.position = 'none',
#           strip.background = element_rect(fill = "white")))

# ggsave(plot = egg_s_all,
#        filename = 'egg_s_all.pdf',
#        path = here("pictures", "Rfigures", "egg_s"),
#        width = 8, height = 5,
#        units = "in")

# Per parameter plots -----------------------------------------------------
#b1TrtID
#b[15] ForestCV
#b[11] PPT
#b[9] Tmax

# Treatment category ------------------------------------------------------
#Burn
egg_s2 <- egg_s_data %>%
  mutate(prop = No_young/No_eggs) 

b0 <- as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b0") %>%
  dplyr::select(`50%`) %>%
  as_vector()
  

tx_vals <- as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, 'b1')) %>%
  filter(!str_detect(parameter, "z")) %>%
  mutate(parameter = case_when(parameter == 'b1TrtID[1]' ~ "U",
                               parameter == "b1TrtID[2]" ~ "H",
                               parameter == "b1TrtID[3]" ~ "B",
                               parameter == "b1TrtID[4]" ~ "HB",
                               TRUE ~ NA_character_)) %>%
  mutate(median = plogis(`50%` + b0))

(tx <- ggplot() +
  geom_jitter(data = egg_s2, aes(x = Trt_cat, y = prop),
              height = 0, width = 0.2,
              fill = "#43A2CA", color = "black", shape = 21, alpha = 0.3) +
  #geom_boxplot(data = egg_s2, aes(x = Trt_cat, y = prop)) +
  geom_point(data = tx_vals, aes(x = parameter, y = median), size = 5, 
             shape = 21, fill = "#43A2CA", color = "black") +
  labs(x = "Stand treatment category",
       y = "Egg survival probability") +
  scale_x_discrete(labels = c("Burn", "Harvest", "Harv&Burn", "Untreated")) +
  scale_y_continuous(limits = c(0, 1.1),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(geom = "text",
           x = 1, y = 1.05,
           label = "*",
           size = 10) +
  annotate(geom = "text",
           x = 4, y = 1.05,
           label = "*",
           size = 10))
  
# ggsave(plot = tx_predicted,
#        filename = 'egg_s_tx_plot.pdf',
#        path = here("pictures", "Rfigures", "egg_s"),
#        width = 3.25, height = 3.35,
#        units = "in")
# 


# Forest CV ---------------------------------------------------------------
#b[15] ForestCV

fcv <- scale_df(x = egg_s_data$a1000_areacv2,
                length = 20,
                name = "fcv")

bfcv <- as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[15]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

regFCV <- fcv %>%
  mutate(reg = b0 + bfcv*varS,
         plogis_reg = plogis(reg))

(fcv_a <- ggplot() +
    geom_point(data = egg_s_data, aes(x = a1000_areacv2, y = No_young/No_eggs),
               fill = "#43A2CA", color = "black", shape = 21, alpha = 0.5) +
    geom_line(data = regFCV, aes(x = fcv, y = plogis_reg),
              linewidth = 1.5, color = "black") +
    geom_line(data = regFCV, aes(x = fcv, y = plogis_reg), 
              color = "#43A2CA", linewidth = 1) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    labs(x = "Forest patch coefficient of variation",
         y = "Egg survival probability") +
    theme(axis.title.y = element_blank()))


# # Temp --------------------------------------------------------------------
# #bTemp
#b[9] Tmax
#egg_s_data

tmp <- scale_df(x = egg_s_data$Tmax_eg,
               length = 20,
               name = "Tmax")

bTmp <- as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[9]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

regTmp <- tmp %>%
  mutate(reg = b0 + bTmp*varS,
         plogis_reg = plogis(reg))

xlab <-  expression("Maximum temperature " ( degree*C))


(temp_a <- ggplot() +
    geom_point(data = egg_s_data, aes(x = Tmax_eg, y = No_young/No_eggs),
               fill = "#43A2CA", color = "black", shape = 21, alpha = 0.5) +
    geom_line(data = regTmp, aes(x = Tmax, y = plogis_reg),
              linewidth = 1.5, color = "black") +
    geom_line(data = regTmp, aes(x = Tmax, y = plogis_reg), 
              color = "#43A2CA", linewidth = 1) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  labs(x = xlab,
       y = "Egg survival probability"))
# 
# 
# ggsave(plot = egg_s_temp_plot,
#        filename = 'egg_s_temp.pdf',
#        path = here("pictures", "Rfigures", "egg_s"),
#        width = 3.5, height = 3.5,
#        units = "in")
# 
# 
# # Precip ------------------------------------------------------------------
#b[11] PPT

ppt <- scale_df(x = egg_s_data$PPT_eg,
                length = 20,
                name = "PPT")

bPPT <- as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[11]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

regPPT <- ppt %>%
  mutate(reg = b0 + bPPT*varS,
         plogis_reg = plogis(reg))

(ppt_a <- ggplot() +
    geom_point(data = egg_s_data, aes(x = PPT_eg, y = No_young/No_eggs),
               fill = "#43A2CA", color = "black", shape = 21, alpha = 0.5) +
    geom_line(data = regPPT, aes(x = PPT, y = plogis_reg),
              linewidth = 1.5, color = "black") +
    geom_line(data = regPPT, aes(x = PPT, y = plogis_reg), 
              color = "#43A2CA", linewidth = 1) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  labs(x = "Cumulative precipitation (mm)",
       y = "Egg survival probability") +
    theme(axis.title.y = element_blank()))

# ggsave(plot = egg_s_ppt_plot,
#        filename = 'egg_s_ppt.pdf',
#        path = here("pictures", "Rfigures", "egg_s"),
#        width = 3.5, height = 3.5,
#        units = "in")
# 

# All plots patchwork -----------------------------------------------------

(eggsplots <- tx + fcv_a + temp_a + ppt_a +
   plot_annotation(tag_levels = "A") )

ggsave(plot = eggsplots,
       filename = 'eggs_plots.pdf',
       path = here("pictures", "Rfigures", "egg_s"),
       width = 8, 
       height = 6.5,
       units = "in")

ggsave(plot = eggsplots,
       filename = 'eggs_plots.jpeg',
       path = here("pictures", "Rfigures", "egg_s"),
       width = 8, 
       height = 6.5,
       units = "in")
# Parameter summaries -----------------------------------------------------

#b0 intercept
as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b0") %>%
  dplyr::select(`2.5%`, `50%`, `97.5%`) %>%
  mutate(across(is.numeric, ~ plogis(.)))

#varinace terms
as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "sig")) %>%
  dplyr::select(parameter, `2.5%`, `50%`, `97.5%`)

# Trees x PIPO another way - sensitivity ----------------------------------

#from Kiona:
#mu[i] = b0 + b1X[i] + b2Z[i] + b3X[i]Z[i]
#mu[i] = b0 + bLgTree*LgTree[i] + bPPIPO*PIPO[i] +
# bTreePIPO*LgTree[i]*PIPO[i]

#"Sensitivity of mu" (e.g.., logit-scale survival): 
#dmu/dX = b1 + b3Z[i]
#dmu/dTree = bLgTree + bTreePIPO*PIPO[i]

#OG with interaction: showing that this is driven by extreme values
egg_s_samp <- readRDS(here("monsoon",
                           "8_24_23",
                           "egg_s",
                           "outputs",
                           "egg_survival_model_samples1_8_24.RDS"))

colnames(egg_s_samp)

bTreePIPO <- egg_s_samp %>%
  dplyr::select(`b[21]`) %>%
  as_vector()

bLgTree <- egg_s_samp %>%
  dplyr::select(`b[6]`) %>%
  as_vector()
# 
pipo <- scale_df(x = egg_s_data$pPIPO,
                  length = 10,
                  name = "pPIPO") %>%
   rename("pipoS" = "varS")
# 
interaction <- bTreePIPO %*% t(pipo$pipoS)
# 
response <- bLgTree + interaction
# 
response1 <- as.data.frame(response) %>%
   pivot_longer(cols = 'V1':'V10',
               names_to = "level",
               values_to = "dmudTreeDensity") %>%
  mutate(level = str_sub(level, -1, length(level))) %>%
  mutate(level = as.integer(level)) %>%
  left_join(pipo, by = "level")
# 
response_sum <- response1 %>%
  group_by(level, pPIPO) %>%
  summarise(median.dmudTreeDensity = median(dmudTreeDensity),
            mean.dmudX = mean(dmudTreeDensity),
            LCI = quantile(dmudTreeDensity, prob = 0.025, type = 8),
            UCI = quantile(dmudTreeDensity, prob = 0.975, type = 8))

lab <- paste("Sensitivity (", expression(frac(dy,dx)), ")")
upper <- paste("d", expression(mu))
lower <- paste("d Large tree density")
lab <- expression(frac(upper, lower))
# 
(treepipo_a <- ggplot(response_sum, aes(x = pPIPO, y= mean.dmudX)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), fill = "#43A2CA", alpha = 0.2) +
  geom_line(size = 1, color = "#43A2CA") +
  labs(y = "Sensitivity",
       x = "Percent ponderosa") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()))
# 
# #Where you create a vector of Z values (Z_level) 
# #that likely span the range of observed Z values 
# #(e.g., on the order of 10-100 values). 
# #Then, you can get the posterior mean and CI for 
# #dmudX at each level (or value) of Z and can plot
# # this as a function of Z.
treepipo_b <- ggplot(egg_s_data, aes(x = pPIPO)) +
  geom_boxplot(fill = "#43A2CA") +
  labs(x = "Percent ponderosa") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(treepipo_sensitivity <- treepipo_a/treepipo_b +
  plot_layout(heights = c(4, 1)))
# 
