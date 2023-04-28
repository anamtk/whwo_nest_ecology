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
                          "10_19_22",
                          "egg_survival",
                          "outputs",
                          "egg_survival_model_summary_10_19.RDS"))

egg_s_samp <- readRDS(here("monsoon",
                          "10_19_22",
                          "egg_survival",
                          "outputs",
                          "egg_survival_model_samples_10_19.RDS"))


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

(egg_s_all <- ggplot(egg_s2, 
       aes(x = parameter, y = beta_50, color = p_cat)) +
  geom_point(size =2) +
  geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), size = 1, width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = c("black", "#43A2CA"
                                )) +
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

ggsave(plot = egg_s_all,
       filename = 'egg_s_all.pdf',
       path = here("pictures", "Rfigures", "egg_s"),
       width = 8, height = 5,
       units = "in")

# Per parameter plots -----------------------------------------------------
#treatment type - harvest
# percent landscape ahrvested
#PPT, PPT2
#Temperature

# Treatment category ------------------------------------------------------
#Harvest

b0 <- as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b0") %>%
  dplyr::select(`2.5%`, `50%`, `97.5%`)
  
tx_vals <- as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, 'b1')) %>%
  filter(!str_detect(parameter, "z")) %>%
  mutate(parameter = case_when(parameter == 'b1TrtID[1]' ~ "Untreated",
                               parameter == "b1TrtID[2]" ~ "Harvest",
                               parameter == "b1TrtID[3]" ~ "Burn",
                               parameter == "b1TrtID[4]" ~ "Harvest_Burn",
                               TRUE ~ NA_character_)) %>%
  mutate(median = plogis(`50%` + b0$`50%`),
         lci = plogis(`2.5%` + b0$`2.5%`),
         uci = plogis(`97.5%` + b0$`97.5%`))

egg_s_data %>%
  group_by(Trt_cat) %>%
  tally()

(tx_predicted <- tx_vals %>%
    ggplot(aes(x = parameter, y = median)) +
    geom_point(color = "#43A2CA", size = 2) +
    labs(x = "Treatment group",
         y = "Predicted egg survival") +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci),
                  width = .1,
                  size = 1,
                  color = "#43A2CA")  +
    annotate(geom = "text",
             x = 1, y = .15,
             label = "n=11") +
    annotate(geom = "text",
             x = 2, y = .15,
             label = "n=42") +
    annotate(geom = "text",
             x = 3,
             y = .15,
             label = "n=20") +
    annotate(geom = "text",
             x = 4, y = .15,
             label = "n=67") +
    annotate(geom = "text",
             x = 2, y = 0.99,
             label = "*",
             size = 8) +
    annotate(geom = "text",
             x = 4, y = 0.99,
             label = "*",
             size = 8) +
    theme(plot.background = element_blank(),
          axis.text.x = element_text( angle = 45, 
                                     hjust = 1)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
    ylim(0, 1))

ggsave(plot = tx_predicted,
       filename = 'egg_s_tx_plot.pdf',
       path = here("pictures", "Rfigures", "egg_s"),
       width = 3.25, height = 3.35,
       units = "in")

# Temp --------------------------------------------------------------------
#bTemp

#get a scaled df of temp for egg survival df
temp <- scale_df(x = egg_s_data$Tmax_eg,
                length = 20, 
                name = "Maximum temperature")

temp_a <- logistic_predicted(sum_mod = egg_s_sum,
                   beta = egg_s_samp$`b[9]`,
                   metadf = temp,
                   varS = varS,
                   var = "Maximum temperature",
                   color = "#43A2CA")
# 
temp_a <- temp_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted egg survival") +
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)
# 
# 
temp_b <- ggplot(egg_s_data, aes(x = Tmax_eg)) +
  geom_boxplot(fill = "#43A2CA") +
  labs(x = "Maximum Temperature") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(egg_s_temp_plot <- temp_a/temp_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = egg_s_temp_plot,
       filename = 'egg_s_temp.pdf',
       path = here("pictures", "Rfigures", "egg_s"),
       width = 3.5, height = 3.5,
       units = "in")


# Precip ------------------------------------------------------------------

#bPPT
ppt <- scale_df(x = egg_s_data$PPT_eg,
                 length = 20,
                 name = "Precipitation")

ppt_a <- logistic2_predicted(sum_mod = egg_s_sum,
                    beta = egg_s_samp$`b[11]`,
                    beta2 = egg_s_samp$`b[12]`,
                    metadf = ppt,
                    varS = varS,
                    var = "Precipitation",
                    color = "#43A2CA")

ppt_a <- ppt_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted egg survival") +
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)

ppt_b <- ggplot(egg_s_data, aes(x = PPT_eg)) +
  geom_boxplot(fill = "#43A2CA") +
  labs(x = "Precipitation") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(egg_s_ppt_plot <- ppt_a/ppt_b +
    plot_layout(heights = c(4, 1)))


ggsave(plot = egg_s_ppt_plot,
       filename = 'egg_s_ppt.pdf',
       path = here("pictures", "Rfigures", "egg_s"),
       width = 3.5, height = 3.5,
       units = "in")



# LandscapeHa -------------------------------------------------------------
#bLaHarvested <- egg_s_mod$sims.list$b[,16]

#get a scaled df ofr egg survival df
lharvest <- scale_df(x = egg_s_data$a1000_Ha,
                 length = 20, 
                 name = "Percent landscape harvested")

lharvest_a <- logistic_predicted(sum_mod = egg_s_sum,
                                 beta = egg_s_samp$`b[16]`,
                                 metadf = lharvest,
                                 varS = varS,
                                 var = "Percent landscape harvested",
                                 color = "#43A2CA") 
# 
lharvest_a <- lharvest_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted egg survival") +
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)
# 
# 
lharvest_b <- ggplot(egg_s_data, aes(x = a1000_Ha)) +
  geom_boxplot(fill = "#43A2CA") +
  labs(x = "Percent landscape harvested") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(egg_s_lharvest_plot <- lharvest_a/lharvest_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = egg_s_lharvest_plot,
       filename = 'egg_s_lharvest.pdf',
       path = here("pictures", "Rfigures", "egg_s"),
       width = 3.5, height = 3.5,
       units = "in")


# Parameter summaries -----------------------------------------------------

egg_s2 %>%
  dplyr::select(-Mean) %>%
  filter(p_cat == "s")

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

#max precip
a <- as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b[12]") %>%
  dplyr::select(`50%`) %>%
  as_vector()
b <- as.data.frame(egg_s_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b[11]") %>%
  dplyr::select(`50%`) %>%
  as_vector()
x <- -b/2*a
x*sd(egg_s_data$PPT_ne, na.rm = T) + 
  mean(egg_s_data$PPT_ne, na.rm = T)
# Trees x PIPO another way - sensitivity ----------------------------------

#from Kiona:
#mu[i] = b0 + b1X[i] + b2Z[i] + b3X[i]Z[i]
#mu[i] = b0 + bLgTree*LgTree[i] + bPPIPO*PIPO[i] +
# bTreePIPO*LgTree[i]*PIPO[i]

#"Sensitivity of mu" (e.g.., logit-scale survival): 
#dmu/dX = b1 + b3Z[i]
#dmu/dTree = bLgTree + bTreePIPO*PIPO[i]
# 
# pipo <- scale_df(x = egg_s_data$pPIPO,
#                  length = 10,
#                  name = "pPIPO") %>%
#   rename("pipoS" = "varS")
# 
# interaction <- bTreePIPO %*% t(pipo$pipoS)
# 
# response <- bLgTree + interaction
# 
# response1 <- as.data.frame(response) %>%
#   pivot_longer(cols = 'V1':'V10',
#                names_to = "level",
#                values_to = "dmudTreeDensity") %>%
#   mutate(level = str_sub(level, -1, length(level))) %>%
#   mutate(level = as.integer(level)) %>%
#   left_join(pipo, by = "level")
# 
# response_sum <- response1 %>%
#   group_by(level, pPIPO) %>%
#   summarise(median.dmudTreeDensity = median(dmudTreeDensity),
#             mean.dmudX = mean(dmudTreeDensity),
#             LCI = quantile(dmudTreeDensity, prob = 0.025, type = 8),
#             UCI = quantile(dmudTreeDensity, prob = 0.975, type = 8))
# 
# lab <- paste("Sensitivity (", expression(frac(dy,dx)), ")")
# upper <- paste("d", expression(mu))
# lower <- paste("d Large tree density")
# lab <- expression(frac(upper, lower))
# 
# treepipo_a <- ggplot(response_sum, aes(x = pPIPO, y= mean.dmudX)) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   geom_ribbon(aes(ymin = LCI, ymax = UCI), fill = "#43A2CA", alpha = 0.2) +
#   geom_line(size = 1, color = "#43A2CA") +
#   labs(y = "Sensitivity",
#        x = "Percent ponderosa") +
#   theme(axis.text.x = element_blank(),
#         axis.title.x = element_blank())
# 
# #Where you create a vector of Z values (Z_level) 
# #that likely span the range of observed Z values 
# #(e.g., on the order of 10-100 values). 
# #Then, you can get the posterior mean and CI for 
# #dmudX at each level (or value) of Z and can plot
# # this as a function of Z.
# treepipo_b <- ggplot(egg_s_data, aes(x = pPIPO)) +
#   geom_boxplot(fill = "#43A2CA") +
#   labs(x = "Percent ponderosa") +
#   theme(axis.title.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank())
# 
# (treepipo_sensitivity <- treepipo_a/treepipo_b +
#   plot_layout(heights = c(4, 1)))
# 
# ggsave(plot = treepipo_sensitivity,
#        filename = 'treexpipo_sensitivity.pdf',
#        path = here("pictures", "Rfigures", "egg_s"),
#        width = 3.5, height = 3.5,
#        units = "in")
# 
# # Trees x PIPO a third way ------------------------------------------------
# 
# 
# #trees
# trees <- scale_df(x = egg_s_data$Trees_50,
#                   length = 10,
#                   name = "Lg_trees") %>%
#   rename("treesS" = "varS")
# 
# ## Wave by Depth interaction
# 
# pipo2 <- scale_df(x = egg_s_data$pPIPO,
#                  length = 10,
#                  name = "pPIPO") %>%
#   rename("pipoS" = "varS")
# 
# 
# trees2 <- trees %>%
#   bind_rows(trees, trees, trees, trees,
#             trees, trees, trees, trees, trees)
# 
# pipo3 <- rep(pipo2$pipoS, length.out = (nrow(trees2))) %>%
#   as.data.frame() %>%
#   rename("pipoS" = ".") %>%
#   left_join(pipo2, by = "pipoS") %>%
#   rename("pipo_level" = "level") %>%
#   arrange(pipo_level) 
# 
# 
# trees3 <- trees2 %>%
#   cbind(pipo3)
# 
# tree.response <- bLgTree %*% t(trees3$treesS)
# pipo.response <- bPPIPO %*% t(trees3$pipoS)
# treepipo.response <- bTreePIPO %*% t(trees3$treesS * trees3$pipoS)
# 
# logit_response_tp <- b0 + tree.response + pipo.response + 
#   treepipo.response
# 
# transformed_tp <- plogis(logit_response_tp)
# 
# df_tp <- transformed_tp %>%
#   as.data.frame() %>%
#   pivot_longer(cols = c("V1":"V100"),
#                names_to = "level",
#                values_to = "p") %>%
#   mutate(level = str_sub(level, 2, length(level))) %>%
#   mutate(level = as.numeric(level)) %>%
#   mutate(pipo_level = case_when(level %in% c(1:10) ~ 1,
#                                 level %in% c(11:20) ~ 2, 
#                                 level %in% c(21:30) ~ 3,
#                                 level %in% c(31:40) ~ 4,
#                                 level %in% c(41:50) ~ 5,
#                                 level %in% c(51:60) ~ 6,
#                                 level %in% c(61:70) ~ 7,
#                                 level %in% c(71:80) ~ 8,
#                                 level %in% c(81:90) ~ 9,
#                               level %in% c(91:100)  ~ 10)) %>%
#   mutate(level = rep(1:10, length.out = 7200000)) %>%
#   left_join(trees3, by = c("level", "pipo_level" ))
# 
# df_tp_sum <- df_tp %>%
#   group_by(Lg_trees, pPIPO) %>%
#   summarise(median.p = median(p),
#             LCI = quantile(p, prob = 0.025, type = 8),
#             UCI = quantile(p, prob = 0.975, type = 8)) 
# 
# (pipo_treea <- ggplot(df_tp_sum, aes(x = Lg_trees, y = pPIPO)) +
#   geom_tile(aes(fill = median.p)) +
#   scale_fill_viridis_c())
# 
# pipo_treea <- ggplot(df_tp_sum, aes(x = pPIPO, y = Lg_trees, z = median.p)) +
#   geom_contour_filled()
# 
# pipo_treeb <- ggplot(egg_s_data, aes(x = Trees_50)) +
#   geom_boxplot()+
#   coord_flip()
# pipo_treec <- ggplot(egg_s_data, aes(x = pPIPO)) + 
#   geom_boxplot() 
# 
# pipo_treeb + pipo_treea + 
#   plot_spacer() + pipo_treec +
#   plot_layout(widths = c(1, 4), heights = c(4,1))

 
