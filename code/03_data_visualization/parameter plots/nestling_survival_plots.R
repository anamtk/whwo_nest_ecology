# Nestling survival plots
# Ana Miller-ter Kuile
# September 27, 2022

# this script brings in the model output as well as a 
# summaried 95% CI DF for the nestling survival model
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

model_n_sum <- readRDS(here("monsoon",
                        "10_19_22",
                        "nestling_survival",
                        "outputs",
                        "nestling_survival_model_summary_10_19.RDS"))

model_n_samp <- readRDS(here("monsoon",
                             "10_19_22",
                             "nestling_survival",
                             "outputs",
                             "nestling_survival_model_samples_10_19.RDS"))

# Load data summary -------------------------------------------------------

nestling_s <- read.csv(here("data_outputs",
                            "03_model_results",
                            "nestling_survival_parameter_mediansCIs.csv"))

# Load OG data ------------------------------------------------------------

#and we also need our original y data
data_nl <- read.csv(here("data_outputs",
                         "01_cleaning",
                         "03_nestling_survival",
                         "Egg_Nestling_survival_data.csv"))

data_nl <- data_nl %>%
  filter(No_young >= NoFL_uncert) %>%
  filter(No_young > 0)

# Summary plot ------------------------------------------------------------

nestling_s2 <- nestling_s %>%
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

(overall_nestling <- ggplot(nestling_s2, 
       aes(x = parameter, y = beta_50, color = p_cat)) +
  geom_point(size =2) +
  geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), size = 1, width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = c("black", '#0868AC'
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

ggsave(plot = overall_nestling,
       filename = 'nestling_s_all_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 8, height = 5,
       units = "in")

# Per parameter plots -----------------------------------------------------

#Treatment type = harvest_burn, burn

#Nest initation day, Nest height
#nest tree species - fir, juniper, aspen

#small tree density

#percent landscape burned
#percent forested landscape

#precip 2
#max temp + max temp 2

# Treatment ---------------------------------------------------------------
# "U", "H", "B", "HB"


b0 <- as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b0") %>%
  dplyr::select(`2.5%`, `50%`, `97.5%`)

tx_vals <- as.data.frame(model_n_sum$quantiles) %>%
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

data_nl %>%
  group_by(Trt_cat) %>%
  tally()

(tx_predicted <- tx_vals %>%
    ggplot(aes(x = parameter, y = median)) +
    geom_point(color = '#0868AC', size = 2) +
    labs(x = "Treatment group",
         y = "Predicted nestling survival") +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci),
                  width = .1,
                  size = 1,
                  color = '#0868AC')  +
    annotate(geom = "text",
             x = 1, y = .15,
             label = "n=18") +
    annotate(geom = "text",
             x = 2, y = .15,
             label = "n=65") +
    annotate(geom = "text",
             x = 3,
             y = .15,
             label = "n=40") +
    annotate(geom = "text",
             x = 4, y = .15,
             label = "n=148") +
    annotate(geom = "text",
             x = 1, y = 1.0,
             label = "*",
             size = 8) +
    annotate(geom = "text",
             x = 3, y = 1.0,
             label = "*",
             size = 8) +
    annotate(geom = "text",
             x = 4, y = 1.0,
             label = "*",
             size = 8) +
    theme(plot.background = element_blank(),
          axis.text.x = element_text( angle = 45, 
                                      hjust = 1)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
    ylim(0, 1.02))


ggsave(plot = tx_predicted,
       filename = 'nestling_tx_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 3.25, height = 3.35,
       units = "in")

# Nest initiation day ------------------------------------------------------
#bInitDay
init <- scale_df(x = data_nl$Init_day,
                 length = 20,
                 name = "Nest initiation day")

init_a <- logistic_predicted(sum_mod = model_n_sum,
                             beta = model_n_samp$`b[5]`,
                             metadf = init,
                             varS = varS,
                             var = "Nest initiation day",
                             color = '#0868AC')

init_a <- init_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted nestling survival")+
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)

init_b <- ggplot(data_nl, aes(x = Init_day)) +
  geom_boxplot(fill = '#0868AC') +
  labs(x = "Nest initiation day") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(nestling_init_plot <- init_a/init_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = nestling_init_plot,
       filename = 'nestling_init_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 3.5, height = 3.5,
       units = "in")
# Nest orientation --------------------------------------------------------
#bOrientation
# orientation <- scale_df(x = data_nl$cosOrientation,
#                  length = 20,
#                  name = "Nest orientation")
# #1 = north, -1 = south
# orientation_a <- logistic_predicted(sum_mod = model_n_sum,
#                                     beta = model_n_samp$`b[4]`,
#                                     metadf = orientation,
#                                     varS = varS,
#                                     var = "Nest orientation",
#                                     color = '#0868AC')
# 
# 
# orientation_a <- orientation_a +
#   theme(axis.title.x = element_blank()) +
#   labs(y = "Predicted nestling survival") +
#   scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
#   ylim(0, 1.2)
# 
# orientation_b <- ggplot(data_nl, aes(x = cosOrientation)) +
#   geom_boxplot(fill = '#0868AC') +
#   labs(x = "Nest orientation") +
#   theme(axis.title.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank())
# #-1 = south, 1 = north
# (nestling_orient_plot <- orientation_a/orientation_b +
#     plot_layout(heights = c(4, 1)))
# 
# ggsave(plot = nestling_orient_plot,
#        filename = 'nestling_orient_plot.pdf',
#        path = here("pictures", "Rfigures", "nestling_s"),
#        width = 3.5, height = 3.5,
#        units = "in")
# Nest Height -------------------------------------------------------------
#bNestHt
height <- scale_df(x = data_nl$Nest_Ht,
                        length = 20,
                        name = "Nest height")

height_a <- logistic_predicted(sum_mod = model_n_sum,
                               beta = model_n_samp$`b[3]`,
                               metadf = height,
                               varS = varS,
                               var = "Nest height", 
                               color = '#0868AC')

height_a <- height_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted nestling survival") +
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)

height_b <- ggplot(data_nl, aes(x = Nest_Ht)) +
  geom_boxplot(fill = '#0868AC') +
  labs(x = "Nest height") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(nestling_height_plot <- height_a/height_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = nestling_height_plot,
       filename = 'nestling_height_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 3.5, height = 3.5,
       units = "in")


# Nest tree species -------------------------------------------------------
#bTreeSP

#"PIPO", "POTR5", 
#"JUOC", "PSME", "Abies"


species_vals <- as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, 'b2SpeciesID')) %>%
  mutate(parameter = case_when(parameter == 'b2SpeciesID[1]' ~ "Ponderosa",
                               parameter == "b2SpeciesID[2]" ~ "Aspen",
                               parameter == "b2SpeciesID[3]" ~ "Juniper",
                               parameter == "b2SpeciesID[4]" ~ "Douglas Fir",
                               parameter == "b2SpeciesID[5]" ~ "Fir",
                               TRUE ~ NA_character_)) %>%
  mutate(median = plogis(`50%` + b0$`50%`),
         lci = plogis(`2.5%` + b0$`2.5%`),
         uci = plogis(`97.5%` + b0$`97.5%`))

data_nl %>%
  group_by(Tree_sp) %>%
  tally()

(species_predicted <- species_vals %>%
    ggplot(aes(x = parameter, y = median)) +
    geom_point(color = '#0868AC', size = 2) +
    labs(x = "Nest tree species",
         y = "Predicted nestling survival") +
    geom_errorbar(aes(ymin = lci,
                      ymax = uci),
                  width = .1,
                  size = 1,
                  color = '#0868AC')  +
    annotate(geom = "text",
             x = 1, y = 0,
             label = "n=35") +
    annotate(geom = "text",
             x = 2, y = 0,
             label = "n=11") +
    annotate(geom = "text",
             x = 3,
             y = 0,
             label = "n=2") +
    annotate(geom = "text",
             x = 4, y = 0,
             label = "n=39") +
    annotate(geom = "text",
             x = 5, y = 0,
             label = "n=184") +
    annotate(geom = "text",
             x = 1, y = 1.0,
             label = "*",
             size = 8) +
    annotate(geom = "text",
             x = 3, y = 1.0,
             label = "*",
             size = 8) +
    annotate(geom = "text",
             x = 4, y = 1.0,
             label = "*",
             size = 8) +
    annotate(geom = "text",
             x = 5, y = 1.0,
             label = "*",
             size = 8) +
    theme(plot.background = element_blank(),
          axis.text.x = element_text( angle = 45, 
                                      hjust = 1)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
    ylim(0, 1.02))

ggsave(plot = species_predicted,
       filename = 'nestling_treesp_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 3.25, height = 3.35,
       units = "in")


# Small tree density ------------------------------------------------------

smtree <- scale_df(x = data_nl$Trees_2550,
                   length = 20,
                   name = "Small tree density")

smtree_a <- logistic_predicted(sum_mod = model_n_sum,
                               beta = model_n_samp$`b[7]`,
                               metadf = smtree,
                               varS = varS,
                               var = "Small tree density",
                               color = '#0868AC')

smtree_a <- smtree_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted nestling survival") +
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)

smtree_b <- ggplot(data_nl, aes(x = Trees_2550)) +
  geom_boxplot(fill = '#0868AC') +
  labs(x = "Small tree density") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(nestling_smtree_plot <- smtree_a/smtree_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = nestling_smtree_plot,
       filename = 'nestling_smtree_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 3.5, height = 3.5,
       units = "in")


# Percent landscape burned ------------------------------------------------
#bLaBurn
lburn <- scale_df(x = data_nl$a1000_RxBu,
                   length = 20,
                   name = "Percent landscape burned")

lburn_a <- logistic_predicted(sum_mod = model_n_sum,
                              beta = model_n_samp$`b[15]`,
                              metadf = lburn,
                              varS = varS,
                              var = "Percent landscape burned",
                              color= '#0868AC')


lburn_a <- lburn_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted nestling survival") +
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)

lburn_b <- ggplot(data_nl, aes(x = a1000_RxBu)) +
  geom_boxplot(fill = '#0868AC') +
  labs(x = "Percent landscape burned (1000 m radius)") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(nestling_lburn_plot <- lburn_a/lburn_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = nestling_lburn_plot,
       filename = 'nestling_lburn_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 3.5, height = 3.5,
       units = "in")


# Percent forested --------------------------------------------------------

#bPForest
lforest <- scale_df(x = data_nl$a1000_pland2,
                  length = 20,
                  name = "Percent landscape forested")

lforest_a <- logistic_predicted(sum_mod = model_n_sum,
                                beta = model_n_samp$`b[13]`,
                                metadf = lforest,
                                varS = varS,
                                var = "Percent landscape forested",
                                color = '#0868AC')


lforest_a <- lforest_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted nestling survival") +
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)

lforest_b <- ggplot(data_nl, aes(x = a1000_pland2)) +
  geom_boxplot(fill = '#0868AC') +
  labs(x = "Percent landscape forested (1000 m radius)") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(nestling_lforest_plot <- lforest_a/lforest_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = nestling_lforest_plot,
       filename = 'nestling_lforest_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 3.5, height = 3.5,
       units = "in")

# Maximum temperature -----------------------------------------------------
#bTemp
#bTemp2
temp <- scale_df(x = data_nl$Tmax_ne,
                  length = 20,
                  name = "Maximum temperature")

temp_a <- logistic2_predicted(sum_mod = model_n_sum,
                              beta = model_n_samp$`b[9]`,
                              beta2 = model_n_samp$`b[10]`,
                              metadf = temp,
                              varS = varS,
                              var = "Maximum temperature",
                              color = '#0868AC')

temp_a <- temp_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted nestling survival") +
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)

temp_b <- ggplot(data_nl, aes(x = Tmax_ne)) +
  geom_boxplot(fill = '#0868AC') +
  labs(x = "Maximum temperature") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(nestling_temp_plot <- temp_a/temp_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = nestling_temp_plot,
       filename = 'nestling_temp_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 3.5, height = 3.5,
       units = "in")

# PPT ---------------------------------------------------------------------

ppt <- scale_df(x = data_nl$PPT_ne,
                 length = 20,
                 name = "Precipitation")

ppt_a <- logistic2_predicted(sum_mod = model_n_sum,
                             beta = model_n_samp$`b[11]`,
                             beta2 = model_n_samp$`b[12]`,
                             metadf = ppt,
                             varS = varS,
                             var = "Precipitation",
                             color = '#0868AC')
ppt_a <- ppt_a +
  theme(axis.title.x = element_blank()) +
  labs(y = "Predicted nestling survival") + 
  scale_y_continuous(breaks = c(0, 0.25, 0.75, 1)) +
  ylim(0, 1)

ppt_b <- ggplot(data_nl, aes(x = PPT_ne)) +
  geom_boxplot(fill = '#0868AC') +
  labs(x = "Precipitation") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(nestling_ppt_plot <- ppt_a/ppt_b +
    plot_layout(heights = c(4, 1)))

ggsave(plot = nestling_ppt_plot,
       filename = 'nestling_ppt_plot.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 3.5, height = 3.5,
       units = "in")

# Summary stats -----------------------------------------------------------
t <- nestling_s2 %>%
  filter(p_cat == "s") %>%
  arrange(desc(abs(beta_50))) %>%
  dplyr::select(-Mean)

#b0 intercept
as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b0") %>%
  dplyr::select(`2.5%`, `50%`, `97.5%`) %>%
  mutate(across(is.numeric, ~ plogis(.)))

#varinace terms
#b0 intercept
as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "sig")) %>%
  dplyr::select(parameter, `2.5%`, `50%`, `97.5%`)

#"maximum temp"
#-b/2a = x
a <- as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b[10]") %>%
  dplyr::select(`50%`) %>%
  as_vector()
b <- as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b[9]") %>%
  dplyr::select(`50%`) %>%
  as_vector()
x <- -b/2*a
x*sd(data_nl$Tmax_ne, na.rm = T) + 
  mean(data_nl$Tmax_ne, na.rm = T)

#max precip
a <- as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b[12]") %>%
  dplyr::select(`50%`) %>%
  as_vector()
b <- as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b[11]") %>%
  dplyr::select(`50%`) %>%
  as_vector()
x <- -b/2*a
x*sd(data_nl$PPT_ne, na.rm = T) + 
  mean(data_nl$PPT_ne, na.rm = T)
