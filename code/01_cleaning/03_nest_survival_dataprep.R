# Nest survival data prep
# Ana Miller-ter Kuile
# October 28, 2021

# this script prepares data from nests where fate 
# was tracked to do survival analysis, exploring
# factors that influence the survival of nests.

# the resulting dataframe will also be slimmed down
# for the stage-specific models for egg production
# survival, and nestling survival


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "readxl", "lubridate")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#temp and precip functions
source(here("code",
            "00_functions",
            "tidy_functions.R"))

# Load data ---------------------------------------------------------------

#get the dataframe with all the treatment info for each nest
trt_covs <- read.csv(here("data_outputs",
                          "01_cleaning",
                          "01_trt_covs",
                          "01_Nest_trt_covs.csv"))

#tree data compilation, including abundance and sizes of trees and snags
tree_data <- read.csv(here("data_outputs",
                                "01_cleaning",
                                "02_climate_trees",
                                "nest_tree_densities.csv"))

#for monthly_clim dataframe that includes climate variables at monthly values,
# may also want yrly_clim too. not sure
climate <- read.csv(here("data_outputs",
                         "01_cleaning",
                         "02_climate_trees",
                         "nest_climate_data.csv"))

#landscape variables
land <- read.csv(here("data_outputs",
                      "01_cleaning",
                      "02_climate_trees",
                      "nest_landscape_1000_covariates.csv"))

#nest locations DF
fates <- read_xlsx(here("data_raw",
                        "bird_data",
                        "Birds03_nest_fates.xlsx"))

#Individual nest visits DF
visits <- read_xlsx(here("data_raw",
                         "bird_data",
                         "Birds02_nest_visits.xlsx"))
 

# Combine nest location,tx, and tree data -------------------------------------

nests <- trt_covs %>%
  dplyr::select(-Point_ID) %>%
  left_join(tree_data, by = c("Nest_ID" = "Measurement_ID")) %>%
  #group Tx by something larger than just the OG numbers
  mutate(Time_groups = case_when(Tm_since_tx %in% c("0", "1", 
                                                    "2", "3", "4", "5") ~ "0-5",
                                 Tm_since_tx %in% c("6", "7",
                                                    "8", "9", "10") ~ "6-10",
                                 Tm_since_tx %in% c("11", "12", "13", "14") ~ "11-14",
                                 TRUE ~ Tm_since_tx))

#how many nests are in this df?
nests %>%
  tally()
#363

# Combine fates with nest and treatment data ------------------------------

nests2 <- nests %>%
  left_join(fates, by = c("Nest_ID", 'Date_located')) %>%
  #cateogrize into success/failure, and unknown
  mutate(Fate_cat = case_when(Fate == 1 ~ "success",
                              Fate == 11 ~ "unknown",
                              TRUE ~ "failure")) %>%
  #remove unknown for future analyses
  filter(Fate_cat != "unknown") 
#This may remove nests that had initial egg counts in them -
#I wonder if it is worth keeping those in?? not sure

nests2 %>%
  tally()
#330 nests
# Clean and combine with visits dataset -----------------------------------

#get nest IDs so we can filter the visits dataset for just nests
nest_IDs <- nests2$Nest_ID

#Get visits dataset to be just nest visits
visits2 <- visits %>%
  # find visits that correspond to a Nest ID
  filter(Nest_ID %in% c(nest_IDs)) %>%
  #select variables of interest
  dplyr::select(Nest_ID, Visit_date, No_eggs, No_young, Stage, 
                Peeped, St_time, End_time) %>%
  #figure out the time spent at each nest during each visit
  mutate(interval = St_time %--% End_time,
         #convert to a duration in minutes
         Duration = as.duration(interval)/dminutes(1)) %>%
  # fix typos to be NA values for now 
  mutate(Duration = case_when(Duration < 0 ~ NA_real_, 
                              TRUE ~ Duration))

# Select variables of interest, convert to Julian days and tiem si --------

nests3 <- visits2 %>%
  #join the visits dataset with all the nest metatdata from nests
  left_join(nests2, by = "Nest_ID") %>%
  #select variables of interest
  dplyr::select(Nest_ID, Visit_date, No_eggs, No_young,
                Stage, Year_located, Project_ID, 
                Transect_ID, Trt_cat, Fate,
                UTM_datum_zone, UTM_N, UTM_E, Nest_Ht,
                Decay_class, Tree_Snag_Log,
                Tree_sp, Tree_ht, DBH, Orientation, 
                Aspect,
                Slope, Initiation_date, 
                NoFL_uncert, NoFL_cert, Trees_2550,  Trees_50,
                pPIPO, PIPO_2550, PIPO_50,  Fate_cat,
                Peeped, Duration,
                n_burn, n_harvest, n_tx, Tm_since_b,
                Tm_since_h, Tm_since_tx, Time_groups) %>%
  # get Julian Days for each start and end
  mutate(Julian_end = as.Date(Visit_date)) %>%
  mutate(Julian_end = format(Julian_end, "%j")) %>%
  group_by(Nest_ID) %>%
  #arrange visits in order by Julian Start to calculate end (from next visit)
  arrange(Julian_end, .by_group = TRUE) %>%
  #get end date based on the next visit period
  mutate(Julian_start = lag(Julian_end, 1)) %>%
  mutate(Initiation_date = ymd(Initiation_date)) %>%
  mutate(Init_day = yday(Initiation_date)) %>%
  ungroup() %>%
  #remove unknown stage nests 
  filter(!Stage %in% c("9")) %>%
  mutate(Orientation = case_when(Orientation == 999 ~ NA_integer_, 
                                 TRUE ~ Orientation)) %>%
  #set orientation to be 1 if N, -1 if South
  mutate(cosOrientation = cos(Orientation * (pi/180)))

# Combine Climate data ----------------------------------------------------

#select only climate variables of interest, which are nest period level
climate <- climate %>%
  dplyr::select(MeasurementID, TransID, ClimateYear,
                Tmax04, Tmax05, Tmax06,
                Tmax07, Tmax08,
                PPT04, PPT05, PPT06,
                PPT07, PPT08, 
                Tmax_activeinsects, PPT_activeinsects, Tmax_dormantinsects,
                PPT_dormantinsects) 

#get a value for monthly temperature for each transect and year
monthly_t <- climate %>%
  dplyr::select(MeasurementID, TransID, ClimateYear, 
                Tmax04, Tmax05, Tmax06,
                Tmax07, Tmax08) %>%
  #make the temps long format to be temp and month values
  pivot_longer(cols = Tmax04:Tmax08,
               values_to = "Tmax",
               names_to  = "Month") %>%
  #make month a more logical value than "TmaxNum"
  mutate(Month = str_sub(Month, -2, length(Month))) %>%
  #make a new variable called month_year that is combo of those 
  # two variables
  unite('Month_year',
        c("Month", "ClimateYear"),
        sep = "_",
        remove = F) %>%
  #make that into a date format
  mutate(Month_year = format(lubridate::parse_date_time(Month_year, 
                                                        orders = c("m_Y")), 
                             "%d-%m-%Y")) %>%
  #get a transect_year variable in a similar way
  unite(Trans_Year,
        c("TransID", "ClimateYear"),
        sep = "_",
        remove = F) 

#do all the same steps for precip
monthly_p <- climate %>%
  dplyr::select(TransID, MeasurementID, ClimateYear, 
                PPT04, PPT05, PPT06,
                PPT07, PPT08) %>%
  pivot_longer(cols = PPT04:PPT08,
               values_to = "PPT",
               names_to  = "Month") %>%
  mutate(Month = str_sub(Month, -2, length(Month))) %>%
  unite('Month_year',
        c("Month", "ClimateYear"),
        sep = "_",
        remove = F) %>%
  mutate(Month_year = format(lubridate::parse_date_time(Month_year, 
                                                        orders = c("m_Y")), 
                             "%d-%m-%Y")) %>%
  unite(Trans_Year,
        c("TransID", "ClimateYear"),
        sep = "_",
        remove = F)

#now get the intervals for each nest in order
# to get each time period's temp and precip
nests4 <- nests3 %>%
  #get year month and day columns from teh visit date
  separate(Visit_date,
           into = c("Year", "Month", "Day"),
           sep = "-",
           remove = F) %>%
  #get when the end month for the nest visit was 
  mutate(end_date = Month) %>%
  #group by each nest
  group_by(Nest_ID) %>%
  #get the start date for nest visit based on previous month
  mutate(start_date = lag(Month)) %>%
  #set start date to end date when it's NA
  mutate(start_date = case_when(is.na(start_date) ~ end_date,
                                TRUE ~ start_date)) %>%
  ungroup()

#fill Temp and precip based on these start and end date values now
# output vector y which will be filled in by the for loop
nests4$Tmax <- rep(NA, length(nests4$Nest_ID)) # create an empty vector of NAs

#loop through the t_max function in custom function list
# to get a moving window temp for each visit to each nest
for(i in 1:length(nests4$Nest_ID)) {
  nests4$Tmax[i] <- t_max(pointID = nests4$Nest_ID[i], 
                          start_date = nests4$start_date[i],
                          end_date = nests4$end_date[i]) 
}

# #do the same steps for precip
 nests4$PPT <- rep(NA, length(nests4$Nest_ID)) # create an empty vector of NAs
# 
 for(i in 1:length(nests4$Nest_ID)) {
   nests4$PPT[i] <- ppt_sum(pointID = nests4$Nest_ID[i], 
                            start_date = nests4$start_date[i],
                            end_date = nests4$start_date[i]) 
 }

#then set some -Inf/NA/0 values to be clear
nests5 <- nests4 %>%
  mutate(Tmax = case_when(Tmax == -Inf ~ NA_real_,
                          TRUE ~ Tmax),
         PPT = case_when(PPT == 0 ~ NA_integer_,
                         TRUE ~ PPT))

# Remove multiple visits per day ------------------------------------------

#some nests were visited twice in one day and the number of eggs/young
# counted on the second visit. We want ot delete the duplicate
# with the missing info

nests6 <- nests5 %>%
  group_by(Nest_ID, Visit_date) %>%
  #get nests where visits are greater than 1
  mutate(sum = n()) %>%
  # then delete the ones where they were visited twice ANd eggs weren't counted
  filter(!(sum > 1 & No_eggs == 999)) %>%
  #filter(Trt_150 != "B") #remove one nest where treatment is burn
  ungroup()

# final total of 330 nests
nests6 %>%
  distinct(Nest_ID) %>%
  tally()

# Combine landscape variables ---------------------------------------------

nests7 <- nests6 %>%
  left_join(land, by = "Nest_ID")

# Recategorize nest tree IDs ----------------------------------------------
#POTR5 - Populus tremuloides
#PIPO - Ponderosa pine
#JUOC - Juniper occidentalis
#PSME/PSMEG = doug fir
#ABGR/ABCO = firs, depending on location

#PSMEG and PSME ==
#ABCO and AB... ==

nests7 <- nests7 %>%
  mutate(Tree_sp = case_when(Tree_sp %in% c("PSMEG", "PSME") ~ "PSME",
                             Tree_sp %in% c("ABCO", "ABGR") ~ "Abies",
                             TRUE ~ Tree_sp))



# Add a new transect ID for incidental transects --------------------------

#in order for the model to not lump all "incidental" 
# nest spatially with the hierarchical effect - setting
#each "incidental" nest it's own "transectID"

transect_ids <- nests7 %>%
  distinct(Nest_ID, Transect_ID) %>%
  mutate(num = as.character(1:n())) %>%
  mutate(Transect_ID2 = case_when(Transect_ID == "Incidental" ~ num,
                                  TRUE ~ Transect_ID)) %>%
  dplyr:: select(-num, - Transect_ID)

nests7 <- nests7 %>%
  left_join(transect_ids, by = "Nest_ID")


# Clean up columns in datafram --------------------------------------------

colnames(nests7)

nests8 <- nests7 %>%
  dplyr::select(Nest_ID, Visit_date, Month, No_eggs, No_young,
                Stage, Year_located, Project_ID, Transect_ID,
                Transect_ID2,
                Trt_cat, Fate, UTM_datum_zone, UTM_N, UTM_E,
                Nest_Ht, Tree_sp, Orientation, cosOrientation,
                Initiation_date,Init_day,
                NoFL_uncert, NoFL_cert, Trees_2550, Trees_50,
                pPIPO, Fate_cat, Peeped, Duration, Julian_end,
                Julian_start,  start_date, end_date, 
                Tmax, PPT,
                a1000_areamn2, a1000_areaam2,
                a1000_areacv2, a1000_contag,
                a1000_pland2,
                a1000_pland1,
                a1000_lpi, a1000_lpi1, a1000_lpi2,
                a1000_np, a1000_np1, a1000_np2,
                a1000_proxmn2,a1000_Ha, a1000_RxBu,
                a1000_WFBu, a1000_PBu,
                n_burn, n_harvest, n_tx, Tm_since_h,
                Tm_since_b, Tm_since_tx, Time_groups)

#filter out wildfire nests
nests8 <- nests8 %>%
  filter(Trt_cat != "W")

# Double check all variables of interest ----------------------------------

data_check <- nests8 %>%
  dplyr::select(Nest_Ht,Orientation, Init_day,
                Trees_2550, Trees_50, pPIPO,
                Tmax, a1000_areamn2:a1000_RxBu,
                PPT
                )


ggplot(gather(data_check), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free')
# Export ------------------------------------------------------------------

write.csv(nests8, 
          here('data_outputs', 
               "01_cleaning", 
               "03_nest_survival",
               "Nest_survival_data.csv"))


# END SCRIPT ##

