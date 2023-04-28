# Prep landscape variables for nests
# Ana Miller-ter Kuile
# June 6, 2022

# this script preps the landscape variables for nests, 
# at all spatial scales

#This was an initial script that I ended up dropping and 
# going with just the 1000m radius size because of past
# research and because this is the range size of birds
# from telemetry research

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "readxl")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#custom functions 
source(here("code", 
            "00_functions", 
            "tidy_functions.R"))


# Load data ---------------------------------------------------------------

emfwor <- read.csv(here("data_raw",
                      "landscape_data",
                      "EMFWOR_LF-based_IPM_NestSurv_data_n68_nests.csv"))

emmaor <- read.csv(here("data_raw",
                        "landscape_data", 
                        "EMMAOR_LF-based_IPM_NestSurv_data_n149_nests.csv"))

empaid <- read.csv(here("data_raw",
                        "landscape_data", 
                        "EMPAID_LF-based_IPM_NestSurv_data_n146_nests.csv"))


#get all these together in one dataframe
land <- emfwor %>%
  rbind(emmaor) %>%
  rbind(empaid)
# Select variables --------------------------------------------------------

colnames(land)
#this dataframe is SCARY because it is ALL the Fragstats beware!

#taking just closed canopy forest variables from that set of variables
forest <- land %>%
  dplyr::select(FID, Nest_ID, Year_located, Date_located, Project_ID,
                Transect_ID, Point_ID, Pt_Trt,
                X3fxareaam2, X3fxareacv2, X3fxareamn2, X3fxpland2,
                X3fxnp, X3fxproxam2, X3fxproxmn2,
                X3f5areaam2, X3f5areacv2, X3f5areamn2, X3f5pland2,
                X3f5np, X3f5proxam2, X3f5proxmn2,
                X3f7areaam2, X3f7areacv2, X3f7areamn2, X3f7pland2,
                X3f7np, X3f7proxam2, X3f7proxmn2,
                X3f1areaam2, X3f1areacv2, X3f1areamn2, X3f1pland2,
                X3f1np, X3f1proxam2, X3f1proxmn2,
                X9fxareaam2,X9fxareacv2, X9fxareamn2, X9fxpland2,
                X9fxnp, X9fxproxam2, X9fxproxmn2,
                X9f5areaam2, X9f5areacv2, X9f5areamn2, X9f5pland2,
                X9f5np, X9f5proxam2, X9f5proxmn2,
                X9f7areaam2, X9f7areacv2, X9f7areamn2, X9f7pland2,
                X9f7np, X9f7proxam2, X9f7proxmn2,
                X9f1areaam2, X9f1areacv2, X9f1areamn2, X9f1pland2,
                X9f1np, X9f1proxam2, X9f1proxmn2,
                Ha11_5x5,  Ha11_500, Ha11_750,Ha11_1000 ,
                Ha12_5x5 ,Ha12_500, Ha12_750,Ha12_1000,
                Ha13_5x5 ,Ha13_500,Ha13_750, Ha13_1000,
                Ha14_5x5 ,Ha14_500 ,Ha14_750, Ha14_1000,
                Ha15_5x5, Ha15_500, Ha15_750, Ha15_1000,
                Ha16_5x5,  Ha16_500, Ha16_750, Ha16_1000,
                Ha17_5x5,Ha17_500,Ha17_750 ,Ha17_1000,
                Ha18_5x5,Ha18_500,Ha18_750,Ha18_1000,
                Ha19_5x5,Ha19_500,Ha19_750,Ha19_1000,
                Ha20_5x5, Ha20_500, Ha20_750,  Ha20_1000,
                Ha21_5x5,Ha21_500, Ha21_750,Ha21_1000 ,
                Bu11_5x5, Bu11_500, Bu11_750, Bu11_1000,
                Bu12_5x5, Bu12_500,Bu12_750, Bu12_1000,
                Bu13_5x5, Bu13_500, Bu13_750, Bu13_1000,
                Bu14_5x5,Bu14_500, Bu14_750, Bu14_1000,
                Bu15_5x5,Bu15_500 ,Bu15_750, Bu15_1000,
                Bu16_5x5 ,Bu16_500, Bu16_750, Bu16_1000,
                Bu17_5x5, Bu17_500, Bu17_750, Bu17_1000 ,
                Bu18_5x5 ,Bu18_500, Bu18_750, Bu18_1000,
                Bu19_5x5, Bu19_500, Bu19_750, Bu19_1000,
                Bu20_5x5, Bu20_500, Bu20_750, Bu20_1000,
                Bu21_5x5, Bu21_500, Bu21_750, Bu21_1000) %>%
  rename_at(vars(contains("areaam2")), funs(str_replace(., "areaam2", "areawm"))) %>%
  rename_at(vars(contains('areacv2')), funs(str_replace(., "areacv2", 'areacv'))) %>%
  rename_at(vars(contains("areamn2")), funs(str_replace(., "areamn2", 'areamn'))) %>%
  #rename_at(vars(contains("np2")), funs(str_replace(., "np2", 'np'))) %>%
  rename_at(vars(contains("proxam2")), funs(str_replace(., "proxam2", 'proxam'))) %>%
  rename_at(vars(contains("proxmn2")), funs(str_replace(., "proxmn2", 'proxmn'))) %>%
  rename_at(vars(contains("pland2")), funs(str_replace(., "pland2", "pland"))) %>%
  rename_at(vars(contains("X3fx")), funs(str_replace(., "X3fx", "a5x5_2013_"))) %>%
  rename_at(vars(contains("X3f5")), funs(str_replace(., "X3f5", 'a500_2013_'))) %>%
  rename_at(vars(contains("X3f7")), funs(str_replace(., "X3f7", "a750_2013_"))) %>%
  rename_at(vars(contains("X3f1")), funs(str_replace(., "X3f1", "a1000_2013_"))) %>%
  rename_at(vars(contains("X9fx")), funs(str_replace(., "X9fx", "a5x5_2019_"))) %>%
  rename_at(vars(contains("X9f5")), funs(str_replace(., "X9f5", "a500_2019_"))) %>%
  rename_at(vars(contains("X9f7")), funs(str_replace(., "X9f7", "a750_2019_"))) %>%
  rename_at(vars(contains("X9f1")), funs(str_replace(., "X9f1", "a1000_2019_"))) 


forest2 <- forest %>%
  mutate_if(., is.integer, as.numeric) %>%
  #weighted mean forest patch size
  #think about what makes sense given treatments, etc, for these conditionals... ask Jon
  mutate(a5x5_areawm = case_when(Year_located <= 2013 ~ a5x5_2013_areawm,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a5x5_2019_areawm,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a5x5_2013_areawm,
                                 TRUE ~ NA_real_)) %>%
  mutate(a500_areawm = case_when(Year_located <= 2013 ~ a500_2013_areawm,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a500_2019_areawm,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a500_2013_areawm,
                                 TRUE ~ NA_real_)) %>%
  mutate(a750_areawm = case_when(Year_located <= 2013 ~ a750_2013_areawm,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a750_2019_areawm,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a750_2013_areawm,
                                 TRUE ~ NA_real_)) %>%
  mutate(a1000_areawm = case_when(Year_located <= 2013 ~ a1000_2013_areawm,
                                  Year_located > 2013 & Pt_Trt != "U" ~ a1000_2019_areawm,
                                  Year_located > 2013 & Pt_Trt == "U" ~ a1000_2013_areawm,
                                 TRUE ~ NA_real_)) %>%
  #coefficient of variation patch size of forests
  mutate(a5x5_areacv = case_when(Year_located <= 2013 ~ a5x5_2013_areacv,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a5x5_2019_areacv,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a5x5_2013_areacv, 
                                 TRUE ~ NA_real_)) %>%
  mutate(a500_areacv = case_when(Year_located <= 2013 ~ a500_2013_areacv,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a500_2019_areacv,
                                 Year_located > 2013 & Pt_Trt == "U" ~a500_2013_areacv, 
                                 TRUE ~ NA_real_)) %>%
  mutate(a750_areacv = case_when(Year_located <= 2013 ~ a750_2013_areacv,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a750_2019_areacv,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a750_2013_areacv,
                                 TRUE ~ NA_real_)) %>%
  mutate(a1000_areacv = case_when(Year_located <= 2013 ~ a1000_2013_areacv,
                                  Year_located > 2013 & Pt_Trt != "U" ~ a1000_2019_areacv,
                                  Year_located > 2013 & Pt_Trt == "U" ~ a1000_2013_areacv,
                                  TRUE ~ NA_real_)) %>%
  #mean patch size of forest
  mutate(a5x5_areamn = case_when(Year_located <= 2013 ~ a5x5_2013_areamn,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a5x5_2019_areamn,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a5x5_2013_areamn,
                                 TRUE ~ NA_real_)) %>%
  mutate(a500_areamn = case_when(Year_located <= 2013 ~ a500_2013_areamn,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a500_2019_areamn,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a500_2013_areamn,
                                 TRUE ~ NA_real_)) %>%
  mutate(a750_areamn = case_when(Year_located <= 2013 ~ a750_2013_areamn,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a750_2019_areamn,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a750_2013_areamn,
                                 TRUE ~ NA_real_)) %>%
  mutate(a1000_areamn = case_when(Year_located <= 2013 ~ a1000_2013_areamn,
                                  Year_located > 2013 & Pt_Trt != "U" ~ a1000_2019_areamn,
                                  Year_located > 2013 & Pt_Trt == "U" ~ a1000_2013_areamn, 
                                  TRUE ~ NA_real_)) %>%
  #percent cover by forests
  mutate(a5x5_pland = case_when(Year_located <= 2013 ~ a5x5_2013_pland,
                                Year_located > 2013 & Pt_Trt != "U" ~ a5x5_2019_pland,
                                Year_located > 2013 & Pt_Trt == "U" ~ a5x5_2013_pland)) %>%
  mutate(a500_pland = case_when(Year_located <= 2013 ~ a500_2013_pland,
                                Year_located > 2013 & Pt_Trt != "U" ~ a500_2019_pland,
                                Year_located > 2013 & Pt_Trt == "U" ~ a500_2013_pland)) %>% 
  mutate(a750_pland = case_when(Year_located <= 2013 ~ a750_2013_pland,
                                Year_located > 2013 & Pt_Trt != "U" ~ a750_2019_pland,
                                Year_located > 2013 & Pt_Trt == "U" ~ a750_2013_pland)) %>%
  mutate(a1000_pland = case_when(Year_located <= 2013 ~ a1000_2013_pland,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a1000_2019_pland,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a1000_2013_pland)) %>%
  #number of patches
  mutate(a5x5_np = case_when(Year_located <= 2013 ~ a5x5_2013_np,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a5x5_2019_np,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a5x5_2013_np,
                                 TRUE ~ NA_real_)) %>%
  mutate(a500_np = case_when(Year_located <= 2013 ~ a500_2013_np,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a500_2019_np,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a500_2013_np,
                                 TRUE ~ NA_real_)) %>%
  mutate(a750_np = case_when(Year_located <= 2013 ~ a750_2013_np,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a750_2019_np,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a750_2013_np,
                                 TRUE ~ NA_real_)) %>%
  mutate(a1000_np = case_when(Year_located <= 2013 ~ a1000_2013_np,
                                  Year_located > 2013 & Pt_Trt != "U" ~ a1000_2019_np,
                                  Year_located > 2013 & Pt_Trt == "U" ~ a1000_2013_np, 
                                  TRUE ~ NA_real_)) %>%
  #proximity - weighted mean
  mutate(a5x5_proxam = case_when(Year_located <= 2013 ~ a5x5_2013_proxam,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a5x5_2019_proxam,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a5x5_2013_proxam,
                                 TRUE ~ NA_real_)) %>%
  mutate(a500_proxam = case_when(Year_located <= 2013 ~ a500_2013_proxam,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a500_2019_proxam,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a500_2013_proxam,
                                 TRUE ~ NA_real_)) %>%
  mutate(a750_proxam = case_when(Year_located <= 2013 ~ a750_2013_proxam,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a750_2019_proxam,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a750_2013_proxam,
                                 TRUE ~ NA_real_)) %>%
  mutate(a1000_proxam = case_when(Year_located <= 2013 ~ a1000_2013_proxam,
                                  Year_located > 2013 & Pt_Trt != "U" ~ a1000_2019_proxam,
                                  Year_located > 2013 & Pt_Trt == "U" ~ a1000_2013_proxam, 
                                  TRUE ~ NA_real_)) %>%
  #proximity - mean
  mutate(a5x5_proxmn = case_when(Year_located <= 2013 ~ a5x5_2013_proxmn,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a5x5_2019_proxmn,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a5x5_2013_proxmn,
                                 TRUE ~ NA_real_)) %>%
  mutate(a500_proxmn = case_when(Year_located <= 2013 ~ a500_2013_proxmn,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a500_2019_proxmn,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a500_2013_proxmn,
                                 TRUE ~ NA_real_)) %>%
  mutate(a750_proxmn = case_when(Year_located <= 2013 ~ a750_2013_proxmn,
                                 Year_located > 2013 & Pt_Trt != "U" ~ a750_2019_proxmn,
                                 Year_located > 2013 & Pt_Trt == "U" ~ a750_2013_proxmn,
                                 TRUE ~ NA_real_)) %>%
  mutate(a1000_proxmn = case_when(Year_located <= 2013 ~ a1000_2013_proxmn,
                                  Year_located > 2013 & Pt_Trt != "U" ~ a1000_2019_proxmn,
                                  Year_located > 2013 & Pt_Trt == "U" ~ a1000_2013_proxmn, 
                                  TRUE ~ NA_real_)) %>%
  mutate_at(vars(matches("Ha")), as.numeric) %>%
  mutate_at(vars(matches("Bu")), as.numeric) %>%
  #harvested area
  mutate(a5x5_Ha = case_when(Year_located == 2012 ~ Ha12_5x5,
                             Year_located == 2013 ~ Ha13_5x5,
                             Year_located == 2014 ~ Ha14_5x5,
                             Year_located == 2015 ~ Ha15_5x5,
                             Year_located == 2016 ~ Ha16_5x5,
                             Year_located == 2017 ~ Ha17_5x5,
                             Year_located == 2018 ~ Ha18_5x5,
                             Year_located == 2019 ~ Ha19_5x5,
                             Year_located == 2020 ~ Ha20_5x5,
                             Year_located == 2021 ~ Ha21_5x5,
                             TRUE ~ NA_real_),
         a500_Ha = case_when(Year_located == 2012 ~ Ha12_500,
                             Year_located == 2013 ~ Ha13_500,
                             Year_located == 2014 ~ Ha14_500,
                             Year_located == 2015 ~ Ha15_500,
                             Year_located == 2016 ~ Ha16_500,
                             Year_located == 2017 ~ Ha17_500,
                             Year_located == 2018 ~ Ha18_500,
                             Year_located == 2019 ~ Ha19_500,
                             Year_located == 2020 ~ Ha20_500,
                             Year_located == 2021 ~ Ha21_500,
                             TRUE ~ NA_real_), 
         a750_Ha = case_when(Year_located == 2012 ~ Ha12_750,
                             Year_located == 2013 ~ Ha13_750,
                             Year_located == 2014 ~ Ha14_750,
                             Year_located == 2015 ~ Ha15_750,
                             Year_located == 2016 ~ Ha16_750,
                             Year_located == 2017 ~ Ha17_750,
                             Year_located == 2018 ~ Ha18_750,
                             Year_located == 2019 ~ Ha19_750,
                             Year_located == 2020 ~ Ha20_750,
                             Year_located == 2021 ~ Ha21_750,
                             TRUE ~ NA_real_),
         a1000_Ha = case_when(Year_located == 2012 ~ Ha12_1000,
                             Year_located == 2013 ~ Ha13_1000,
                             Year_located == 2014 ~ Ha14_1000,
                             Year_located == 2015 ~ Ha15_1000,
                             Year_located == 2016 ~ Ha16_1000,
                             Year_located == 2017 ~ Ha17_1000,
                             Year_located == 2018 ~ Ha18_1000,
                             Year_located == 2019 ~ Ha19_1000,
                             Year_located == 2020 ~ Ha20_1000,
                             Year_located == 2021 ~ Ha21_1000,
                             TRUE ~ NA_real_)) %>%
  #burned area
  mutate(a5x5_Bu = case_when(Year_located == 2012 ~ Bu12_5x5,
                             Year_located == 2013 ~ Bu13_5x5,
                             Year_located == 2014 ~ Bu14_5x5,
                             Year_located == 2015 ~ Bu15_5x5,
                             Year_located == 2016 ~ Bu16_5x5,
                             Year_located == 2017 ~ Bu17_5x5,
                             Year_located == 2018 ~ Bu18_5x5,
                             Year_located == 2019 ~ Bu19_5x5,
                             Year_located == 2020 ~ Bu20_5x5,
                             Year_located == 2021 ~ Bu21_5x5,
                             TRUE ~ NA_real_),
         a500_Bu = case_when(Year_located == 2012 ~ Bu12_500,
                             Year_located == 2013 ~ Bu13_500,
                             Year_located == 2014 ~ Bu14_500,
                             Year_located == 2015 ~ Bu15_500,
                             Year_located == 2016 ~ Bu16_500,
                             Year_located == 2017 ~ Bu17_500,
                             Year_located == 2018 ~ Bu18_500,
                             Year_located == 2019 ~ Bu19_500,
                             Year_located == 2020 ~ Bu20_500,
                             Year_located == 2021 ~ Bu21_500,
                             TRUE ~ NA_real_), 
         a750_Bu = case_when(Year_located == 2012 ~ Bu12_750,
                             Year_located == 2013 ~ Bu13_750,
                             Year_located == 2014 ~ Bu14_750,
                             Year_located == 2015 ~ Bu15_750,
                             Year_located == 2016 ~ Bu16_750,
                             Year_located == 2017 ~ Bu17_750,
                             Year_located == 2018 ~ Bu18_750,
                             Year_located == 2019 ~ Bu19_750,
                             Year_located == 2020 ~ Bu20_750,
                             Year_located == 2021 ~ Bu21_750,
                             TRUE ~ NA_real_),
         a1000_Bu = case_when(Year_located == 2012 ~ Bu12_1000,
                              Year_located == 2013 ~ Bu13_1000,
                              Year_located == 2014 ~ Bu14_1000,
                              Year_located == 2015 ~ Bu15_1000,
                              Year_located == 2016 ~ Bu16_1000,
                              Year_located == 2017 ~ Bu17_1000,
                              Year_located == 2018 ~ Bu18_1000,
                              Year_located == 2019 ~ Bu19_1000,
                              Year_located == 2020 ~ Bu20_1000,
                              Year_located == 2021 ~ Bu21_1000,
                              TRUE ~ NA_real_)) %>%
  #select all the final variables
  dplyr::select(Nest_ID, 
                a5x5_areawm, a500_areawm, a750_areawm, a1000_areawm,
                a5x5_areamn, a500_areamn, a750_areamn, a1000_areamn,
                a5x5_areacv,  a500_areacv, a750_areacv, a1000_areacv,
                a5x5_proxmn, a500_proxmn, a750_proxmn, a1000_proxmn,
                a5x5_proxam, a500_proxam, a750_proxam, a1000_proxam,
                a5x5_pland, a500_pland, a750_pland, a1000_pland,
                a5x5_np, a500_np, a750_np, a1000_np,
                a5x5_Ha, a500_Ha, a750_Ha, a1000_Ha ,
                a5x5_Bu, a500_Bu, a750_Bu, a1000_Bu)

# Export ------------------------------------------------------------------

write.csv(forest2, here("data_outputs",
                        "01_cleaning",
                        "02_climate_trees",
                        "nest_landscape_covariates.csv"))

#END SCRIPT
