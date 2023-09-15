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
                        "8_24_23",
                        "nestling",
                        "outputs",
                        "nestling_survival_model_summary_8_24.RDS"))

model_n_samp <- readRDS(here("monsoon",
                             "8_24_23",
                             "nestling",
                             "outputs",
                             "nestling_survival_model_samples_8_24.RDS"))

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


nestl_exp <- nestling_s2 %>%
  dplyr::select(category, parameter, beta_50, beta_lower, beta_upper, p)

write.csv(nestl_exp, 
          file = here('data_outputs', '04_paper_tables',
                      'nestl_summary.csv'),
          row.names = F)
# (overall_nestling <- ggplot(nestling_s2, 
#        aes(x = parameter, y = beta_50, color = p_cat)) +
#   geom_point(size =2) +
#   geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), size = 1, width = 0.2) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   scale_color_manual(values = c("black", '#0868AC'
#   )) +
#     labs(y = "Covariate estimate \n (posterior median and 95% BCI)",
#          x = "Covariate")+
#   coord_flip() +
#   theme(strip.text.y.left = element_text(angle = 0)) +
#   facet_grid(category ~ ., 
#              scales = "free_y", 
#              space = "free_y") +
#   theme(strip.text.y = element_text(angle = 0),
#         legend.position = 'none',
#         strip.background = element_rect(fill = "white")))
# 
# ggsave(plot = overall_nestling,
#        filename = 'nestling_s_all_plot.pdf',
#        path = here("pictures", "Rfigures", "nestling_s"),
#        width = 8, height = 5,
#        units = "in")

# Per parameter plots -----------------------------------------------------

#Treatment type = harvest_burn

#% landscape burned

#Nest initation day

# Nest height

#nest tree species - fir, juniper

#precip

#max temp + max temp 2

data_nl2 <- data_nl %>%
  mutate(prop = NoFL_uncert/No_young) 

# Treatment ---------------------------------------------------------------
# "U", "H", "B", "HB"


b0 <- as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(parameter == "b0") %>%
  dplyr::select(`50%`) %>%
  as_vector()

tx_vals <- as.data.frame(model_n_sum$quantiles) %>%
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
    geom_jitter(data = data_nl2, aes(x = Trt_cat, y = prop),
                height = 0, width = 0.2,
                fill = "#0868AC", color = "black", shape = 21, alpha = 0.3) +
    #geom_boxplot(data = egg_s2, aes(x = Trt_cat, y = prop)) +
    geom_point(data = tx_vals, aes(x = parameter, y = median), size = 5, 
               shape = 21, fill = "#0868AC", color = "black") +
    labs(x = "Stand treatment category",
         y = "Nestling survival probability") +
    scale_x_discrete(labels = c("Burn", "Harvest", "Harv&Burn", "Untreated")) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    annotate(geom = "text",
             x = 3, y = 1.05,
             label = "*",
             size = 10) +
    annotate(geom = "text",
             x = 4, y = 1.05,
             label = "*",
             size = 10))


# Percent landscape burned ------------------------------------------------
#bLaBurn
lburn <- scale_df(x = data_nl$a1000_RxBu,
                  length = 20,
                  name =  "a1000_RxBu")

bBurn <- as.data.frame(model_n_sum$quantiles) %>%  
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[18]") %>%
  dplyr::select(`50%`) %>%
  as_vector()
  
regBurn <- lburn %>%
  mutate(reg = b0 + bBurn*varS,
         plogis_reg = plogis(reg))

(burn_a <- ggplot() +
    geom_point(data = data_nl2, aes(x = a1000_RxBu, y = prop),
               fill = "#0868AC", color = "black", shape = 21, alpha = 0.5) +
    geom_line(data = regBurn, aes(x = a1000_RxBu, y = plogis_reg),
              linewidth = 1.5, color = "black") +
    geom_line(data = regBurn, aes(x = a1000_RxBu, y = plogis_reg), 
              color = "#0868AC", linewidth = 1) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    labs(x = "Percent landscape burned",
         y = "Nestling survival probability") +
    theme(axis.title.y = element_blank()))


# Nest tree species -------------------------------------------------------
#bTreeSP

#"PIPO", "POTR5", 
#"JUOC", "PSME", "Abies"


species_vals <- as.data.frame(model_n_sum$quantiles) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, 'b2SpeciesID')) %>%
  mutate(parameter = case_when(parameter == 'b2SpeciesID[1]' ~ "PIPO",
                               parameter == "b2SpeciesID[2]" ~ "POTR5",
                               parameter == "b2SpeciesID[3]" ~ "JUOC",
                               parameter == "b2SpeciesID[4]" ~ "PSME",
                               parameter == "b2SpeciesID[5]" ~ "Abies",
                               TRUE ~ NA_character_)) %>%
  mutate(median = plogis(`50%` + b0))

level_order <- c('POTR5', 'JUOC', 'PSME',
                 'Abies', "PIPO")

(spp <- ggplot() +
    geom_jitter(data = data_nl2, aes(x = Tree_sp, y = prop),
                height = 0, width = 0.2,
                fill = "#0868AC", color = "black", shape = 21, alpha = 0.3) +
    #geom_boxplot(data = egg_s2, aes(x = Trt_cat, y = prop)) +
    geom_point(data = species_vals, aes(x = parameter, y = median), size = 5, 
               shape = 21, fill = "#0868AC", color = "black") +
    labs(x = "Nest tree species",
         y = "Nestling survival probability") +
    scale_x_discrete(labels = c("Aspen", "Juniper", "Douglas-fir",
                                "Fir", "Ponderosa"),
                     limits = level_order) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    annotate(geom = "text",
             x = 2, y = 1.05,
             label = "*",
             size = 10) +
    annotate(geom = "text",
             x = 4, y = 1.05,
             label = "*",
             size = 10)+
    annotate(geom = "text",
             x = 5, y = 1.05,
             label = "*",
             size = 10)+
    theme(axis.title.y = element_blank()))

# Nest Height -------------------------------------------------------------
#bNestHt
height <- scale_df(x = data_nl$Nest_Ht,
                   length = 20,
                   name = "Nest_Ht")

bHt <- as.data.frame(model_n_sum$quantiles) %>%  
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[3]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

regHt <- height %>%
  mutate(reg = b0 + bHt*varS,
         plogis_reg = plogis(reg))

(ht <- ggplot() +
    geom_point(data = data_nl2, aes(x = Nest_Ht, y = prop),
               fill = "#0868AC", color = "black", shape = 21, alpha = 0.5) +
    geom_line(data = regHt, aes(x = Nest_Ht, y = plogis_reg),
              linewidth = 1.5, color = "black") +
    geom_line(data = regHt, aes(x = Nest_Ht, y = plogis_reg), 
              color = "#0868AC", linewidth = 1) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    labs(x = "Nest height (m)",
         y = "Nestling survival probability"))


# Nest initiation day ------------------------------------------------------
#bInitDay
init <- scale_df(x = data_nl$Init_day,
                 length = 20,
                 name = "Init_day")

bInit <- as.data.frame(model_n_sum$quantiles) %>%  
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[5]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

regInit <- init %>%
  mutate(reg = b0 + bInit*varS,
         plogis_reg = plogis(reg))

(init_nst <- ggplot() +
    geom_point(data = data_nl2, aes(x = Init_day, y = prop),
               fill = "#0868AC", color = "black", shape = 21, alpha = 0.5) +
    geom_line(data = regInit, aes(x = Init_day, y = plogis_reg),
              linewidth = 1.5, color = "black") +
    geom_line(data = regInit, aes(x = Init_day, y = plogis_reg), 
              color = "#0868AC", linewidth = 1) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    labs(x = "Nest initation date (Julian date)",
         y = "Nestling survival probability")+
    theme(axis.title.y = element_blank()))


# Maximum temperature -----------------------------------------------------
#bTemp
#bTemp2
temp <- scale_df(x = data_nl$Tmax_ne,
                  length = 20,
                  name = "Tmax_ne")

bTmp <- as.data.frame(model_n_sum$quantiles) %>%  
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[9]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

bTmp2 <- as.data.frame(model_n_sum$quantiles) %>%  
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[10]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

regTmp <- temp %>%
  mutate(reg = b0 + bTmp*varS + bTmp2*varS^2,
         plogis_reg = plogis(reg))

xlab <-  expression("Maximum temperature " ( degree*C))

(tmp_nst <- ggplot() +
    geom_point(data = data_nl2, aes(x = Tmax_ne, y = prop),
               fill = "#0868AC", color = "black", shape = 21, alpha = 0.5) +
    geom_line(data = regTmp, aes(x = Tmax_ne, y = plogis_reg),
              linewidth = 1.5, color = "black") +
    geom_line(data = regTmp, aes(x = Tmax_ne, y = plogis_reg), 
              color = "#0868AC", linewidth = 1) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    labs(x = xlab,
         y = "Nestling survival probability")+
    theme(axis.title.y = element_blank()))

#-b/2a
#y = ax^2 + bx + c
scaled_opt <- -bTmp/(2*bTmp2)

#get mean and SD of OG data to back-transform stuff
mean <- mean(data_nl$Tmax_ne, na.rm = T)
sd <- sd(data_nl$Tmax_ne, na.rm = T)
# xscaled = (x – x̄) / s
# xscaled*sd + mean = x
scaled_opt*sd + mean
# PPT ---------------------------------------------------------------------

ppt <- scale_df(x = data_nl$PPT_ne,
                 length = 20,
                 name = "PPT_ne")

bPpt <- as.data.frame(model_n_sum$quantiles) %>%  
  rownames_to_column(var = "parm") %>%
  filter(parm == "b[11]") %>%
  dplyr::select(`50%`) %>%
  as_vector()

regPpt <- ppt %>%
  mutate(reg = b0 + bPpt*varS,
         plogis_reg = plogis(reg))

(ppt_nst <- ggplot() +
    geom_point(data = data_nl2, aes(x = PPT_ne, y = prop),
               fill = "#0868AC", color = "black", shape = 21, alpha = 0.5) +
    geom_line(data = regPpt, aes(x = PPT_ne, y = plogis_reg),
              linewidth = 1.5, color = "black") +
    geom_line(data = regPpt, aes(x = PPT_ne, y = plogis_reg), 
              color = "#0868AC", linewidth = 1) +
    scale_y_continuous(limits = c(0, 1.1),
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    labs(x = "Cumulative precipitation (mm)",
         y = "Nestling survival probability"))


# Patchwork plots ---------------------------------------------------------

(nstlsplots <- tx + burn_a + spp + ht + init_nst + tmp_nst + plot_spacer() + ppt_nst +
  plot_annotation(tag_levels = "A"))

ggsave(plot = nstlsplots,
       filename = 'nstls_plots.pdf',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 10.5, 
       height = 9,
       units = "in")

ggsave(plot = nstlsplots,
       filename = 'nstls_plots.jpeg',
       path = here("pictures", "Rfigures", "nestling_s"),
       width = 10.5, 
       height = 9,
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


# Interaction -------------------------------------------------------------

#theres an interction bw sm trees and temp
#is it driven by extremes? let's find out
#OG with interaction: showing that this is driven by extreme values
model_n_samp <- readRDS(here("monsoon",
                             "8_24_23",
                             "nestling",
                             "outputs",
                             "nestling_survival_model_samples1_8_24.RDS"))

colnames(model_n_samp)

bTreeTemp <- model_n_samp %>%
  dplyr::select(`b[23]`) %>%
  as_vector()

bSmTree <- model_n_samp %>%
  dplyr::select(`b[7]`) %>%
  as_vector()
# 
temp <- scale_df(x = data_nl$Tmax_ne,
                 length = 10,
                 name = "Tmax") %>%
  rename("tmaxS" = "varS")
# 
interaction <- bTreeTemp %*% t(temp$tmaxS)
# 
response <- bSmTree + interaction
# 
response1 <- as.data.frame(response) %>%
  pivot_longer(cols = 'V1':'V10',
               names_to = "level",
               values_to = "dmudTreeDensity") %>%
  mutate(level = str_sub(level, -1, length(level))) %>%
  mutate(level = as.integer(level)) %>%
  left_join(temp, by = "level")
# 
response_sum <- response1 %>%
  group_by(level, Tmax) %>%
  summarise(median.dmudTreeDensity = median(dmudTreeDensity),
            mean.dmudX = mean(dmudTreeDensity),
            LCI = quantile(dmudTreeDensity, prob = 0.025, type = 8),
            UCI = quantile(dmudTreeDensity, prob = 0.975, type = 8))

lab <- paste("Sensitivity (", expression(frac(dy,dx)), ")")
upper <- paste("d", expression(mu))
lower <- paste("d Small tree density")
lab <- expression(frac(upper, lower))
# 
(int_a <- ggplot(response_sum, aes(x = Tmax, y= mean.dmudX)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_ribbon(aes(ymin = LCI, ymax = UCI), fill = "#43A2CA", alpha = 0.2) +
    geom_line(size = 1, color = "#43A2CA") +
    labs(y = "Sensitivity",
         x = "Temperature") +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()))
# 
# #Where you create a vector of Z values (Z_level) 
# #that likely span the range of observed Z values 
# #(e.g., on the order of 10-100 values). 
# #Then, you can get the posterior mean and CI for 
# #dmudX at each level (or value) of Z and can plot
# # this as a function of Z.
int_b <- ggplot(data_nl, aes(x = Tmax_ne)) +
  geom_boxplot(fill = "#43A2CA") +
  labs(x = "Percent ponderosa") +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# 
(sensitivity <- int_a/int_b +
    plot_layout(heights = c(4, 1)))
# 
# ggsave(plot = treepipo_sensitivity,
#        filename = 'treexpipo_sensitivity.pdf',
#        path = here("pictures", "Rfigures", "egg_s"),
#        width = 3.5, height = 3.5,
#        units = "in")
