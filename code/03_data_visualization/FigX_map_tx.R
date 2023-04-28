# Figure 1 for paper: overview and treatments
# October 5, 2022
# Ana Miller-ter Kuile

# this script makes the first figure that is a facet of a map, 
# number of nests per treatment type, and the differences
# in tree densities between treatments

# this script explores the tree data that has just been re-generated


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "readxl", "lubridate",
                  "DHARMa", 'patchwork',
                  'sf')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())


# Load data ---------------------------------------------------------------
#TREE GRAPHS
trees <- read.csv(here('data_outputs', 
                       "01_cleaning", 
                       "03_nest_survival",
                       "Nest_survival_data.csv"))

#MAPS
#survival data 
survival <- read.csv(here('data_outputs', 
                          "01_cleaning", 
                          "03_nest_survival",
                          "Nest_survival_data.csv"))

#FS boundaries
fs <- read_sf(here("data_raw",
                   "range_maps",
                   "S_USA.RangerDistrict",
                   "S_USA.RangerDistrict.shp"))

forests <- fs %>%
  filter(FORESTNAME %in% c("Payette National Forest",
                           "Fremont-Winema National Forest",
                           "Malheur National Forest")) 

us <- read_sf(here("data_raw",
                   "range_maps",
                   "tl_2012_us_state",
                   "tl_2012_us_state.shp"))

states <- us %>%
  filter(NAME %in% c("Idaho", "Washington", "Oregon", "California"))

range <- st_read(here("data_raw",
                      "range_maps",
                      "whhwoo-range-2020.gpkg",
                      "whhwoo-range-lr-2020.gpkg"))

#plot(range)

# Prep data ---------------------------------------------------------------

trees <- trees %>%
  distinct(Nest_ID, Trt_cat, Year_located,
           Project_ID, UTM_datum_zone, 
           UTM_N, UTM_E, Trees_2550,
           Trees_50, pPIPO) %>%
  mutate(Trt_cat2 = case_when(Trt_cat %in% c("B", "H", "HB") ~ "Treated",
                              Trt_cat == "U" ~ "Untreated",
                              TRUE ~ NA_character_)) %>%
  mutate(Trt_cat= factor(Trt_cat, 
                         levels = c("U", "H", "B", "HB")))


survival1 <- survival %>%
  ungroup() %>%
  distinct(Nest_ID, 
           Year_located, Project_ID,
           Transect_ID, Transect_ID2,
           Trt_cat, Fate, UTM_datum_zone,
           UTM_N, UTM_E, Fate_cat)

# Transform projects ------------------------------------------------------

#Lat long projection for all
newcrs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

#NESTS
#set the CRS for each set
crs10 <- '+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
crs11 <- '+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'


nests_10 <- survival1 %>%
  filter(UTM_datum_zone == "NAD83 10N") %>%
  st_as_sf(coords = c("UTM_E", "UTM_N"),
           crs = crs10) %>%
  st_transform(crs = newcrs) #transform to new CRS

nests_11 <- survival1 %>%
  filter(UTM_datum_zone == "NAD83 11N") %>%
  st_as_sf(coords = c("UTM_E", "UTM_N"),
           crs = crs11) %>%
  st_transform(crs = newcrs) #transform to new CRS

#recombine all data points
all_nests <- nests_10 %>%
  rbind(nests_11) %>%
  mutate(Year_located = as.factor(Year_located))

forests <- st_transform(forests, crs = newcrs)
states <- st_transform(states, crs = newcrs)
range <- st_transform(range, crs = newcrs)
us <- st_transform(us, crs = newcrs)

# Tree tx stats -----------------------------------------------------------

m1 <- lm(Trees_2550 ~ Trt_cat,
         data = trees)

simulateResiduals(m1, plot = T)
summary(m1)

m2 <- lm(Trees_50 ~ Trt_cat,
         data = trees)


simulateResiduals(m2, plot = T)
summary(m2)

m3 <- lm(pPIPO ~ Trt_cat,
         data = trees)

simulateResiduals(m3, plot = T)
summary(m3)

#on average, how different is tree density in treated/untreated
trees %>%
  ungroup() %>%
  group_by(Trt_cat) %>%
  summarise(total = n(),
            mean_2550 = mean(Trees_2550, na.rm = T),
            sd_2550 = sd(Trees_2550, na.rm = T),
            se_2550 = sd_2550/sqrt(total),
            mean_50 = mean(Trees_50, na.rm = T),
            sd_50 = sd(Trees_50, na.rm = T),
            se_50 = sd_50/sqrt(total),
            mean_pP = mean(pPIPO, na.rm = T),
            sd_pP = sd(pPIPO, na.rm = T),
            se_pP = sd_pP/sqrt(total))

# boxplots without facets -------------------------------------------------
trees <- trees %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("B", "H", "HB", "U")))


(a <- ggplot(trees, aes(x = Trt_cat, y = Trees_2550)) +
    geom_boxplot() +
    labs(x = "Treatment category",
         y = bquote("Small (25-50cm DBH) trees" ~ha^-1)) +
    scale_x_discrete(labels = c("U" = "Untreated",
                                "H" = "Harvest",
                                "B" = "Burn",
                                "HB" = "Harvest&Burn")) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    annotate(geom = "text",
             x = 2, 
             y = 200,
             label = "*",
             size = 10) +
    annotate(geom = "text",
             x = 3, 
             y = 200,
             label = "*",
             size = 10))


(b <- ggplot(trees, aes(x = Trt_cat, y = Trees_50)) +
    geom_boxplot() +
    labs(x = "Treatment category",
         y = bquote("Large (>50cm DBH) trees" ~ha^-1)) +
    scale_x_discrete(labels = c("U" = "Untreated",
                                "H" = "Harvest",
                                "B" = "Burn",
                                "HB" = "Harvest&Burn")) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    annotate(geom = "text",
             x = 2, 
             y = 70,
             label = "*",
             size = 10) +
    ylim(0, 75))

(c <- ggplot(trees, aes(x = Trt_cat, y = pPIPO)) +
    geom_boxplot() +
    labs(x = "Treatment category",
         y = "Percent ponderosa") +
    scale_x_discrete(labels = c("U" = "Untreated",
                                "H" = "Harvested",
                                "B" = "Burned",
                                "HB" = "Harvest&Burn")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    annotate(geom = "text",
             x = 2, 
             y = 1.1,
             label = "*",
             size = 10) +
    annotate(geom = "text",
             x = 1, 
             y = 1.1,
             label = "*",
             size = 10) +
    annotate(geom = "text",
             x = 3, 
             y = 1.1,
             label = "*",
             size = 10) +
    scale_y_continuous(breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
                       limits = c(0, 1.15)))

plots <- a + b + c +
  plot_layout(nrow = 3)


# Maps --------------------------------------------------------------------

#National map
(nation <- ggplot() +
  geom_sf(data = us, fill = "white")+
  geom_sf(data = range, alpha = 0.2, fill = "#beaed4") +
  geom_sf(data = all_nests))

ggsave(plot = nation,
       filename = here("pictures", "Rfigures", "nation_map.pdf"),
       width = 7, 
       height = 5,
       units = "in")

# Just states of interest -------------------------------------------------

idor <- states %>%
  filter(NAME %in% c("Oregon", "Idaho"))

(forests_map <- ggplot()+
  geom_sf(data = idor, fill = "white") +
  geom_sf(data = forests, fill = "#7fc97f", alpha = 0.2) +
  geom_sf(data = all_nests, shape = 1) )

ggsave(plot = forests_map,
       filename = here("pictures", "Rfigures", "forests_map.pdf"),
       width = 4, 
       height = 3,
       units = "in")

# Barplot of nests x treatment category -----------------------------------

all_nests_sum <- all_nests %>%
  group_by(Trt_cat) %>%
  tally()

(nest_tx <- ggplot(all_nests_sum, aes(x = Trt_cat, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Treatment category", y = "Number of nests") +
  scale_x_discrete(labels = c("B" = "Burned",
                              "H" = "Harvested",
                              "HB" = "Harvest&Burn",
                              "U" = "Untreated")) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1)))

ggsave(plot = nest_tx,
       filename = here("pictures", "Rfigures", "nest_tx_bar.pdf"),
       width = 4, 
       height = 3,
       units = "in")



# Facetted zoomed in map and barplot --------------------------------------

(facetted_map_tx <- forests_map/nest_tx)

ggsave(plot = facetted_map_tx,
       filename = here("pictures", "Rfigures", "location_tx_nests.pdf"),
       width = 7, 
       height = 5,
       units = "in")


# Try to put all together -------------------------------------------------

facetted_map_tx / plots

(fig1 <- (forests_map/nest_tx) | (a/b/c) )

ggsave(fig1,
       filename = here("pictures", "Rfigures", "fig1_map_tx.pdf"),
                       width = 8,
                       height = 6,
                       units = "in")
