# Some figures for lab meeting - March 31
# Ana Miller-ter Kuile
# March 21, 2022

# this script imports the survival and productivity datasets and summarises them


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "readxl", "lubridate",
                  "sf", 'patchwork')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())
# Load data ---------------------------------------------------------------

#survival data 
survival <- read.csv(here('data_outputs', 
                          "01_cleaning", 
                          "03_nest_survival",
                          "Nest_survival_data.csv"))

#FS boundaries
fs <- read_sf(here("data_raw",
                   "range_maps",
                   "USARangerDistrict",
                   "S_USA.RangerDistrict.shp"))

forests <- fs %>%
  filter(FORESTNAME %in% c("Payette National Forest",
                           "Fremont-Winema National Forest",
                           "Malheur National Forest")) 

us <- read_sf(here("data_raw",
                   "range_maps",
                   "US-Boundary-Layers",
                   "US-State-Boundaries-Census-2014.shp"))

states <- us %>%
  filter(NAME %in% c("Idaho", "Washington", "Oregon", "California"))

range <- st_read(here("data_raw",
                      "range_maps",
                      "whhwoo-range-2020.gpkg",
                      "whhwoo-range-lr-2020.gpkg"))

#plot(range)


# Select the distinct nests -----------------------------------------------

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

ggplot() +
  geom_sf(data = states, fill = "white") +
  geom_sf(data = range, alpha = 0.2, fill = "#beaed4") +
  geom_sf(data = all_nests)

nation <- ggplot() +
  geom_sf(data = us, fill = "white")+
  geom_sf(data = range, alpha = 0.2, fill = "#beaed4") +
  geom_sf(data = all_nests)

ggsave(plot = nation,
       filename = here("pictures", "Rfigures", "nation_map.pdf"),
       width = 7, 
       height = 5,
       units = "in")
# Just states of interest -------------------------------------------------

idor <- states %>%
  filter(NAME %in% c("Oregon", "Idaho"))

forests_map <- ggplot()+
  geom_sf(data = idor, fill = "white") +
  geom_sf(data = forests, fill = "#7fc97f", alpha = 0.2) +
  geom_sf(data = all_nests, shape = 1) 

ggsave(plot = forests_map,
       filename = here("pictures", "Rfigures", "forests_map.pdf"),
       width = 4, 
       height = 3,
       units = "in")

# Barplot of nests x treatment category -----------------------------------

all_nests_sum <- all_nests %>%
  group_by(Trt_cat) %>%
  tally()

nest_tx <- ggplot(all_nests_sum, aes(x = Trt_cat, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Treatment category", y = "Number of nests") +
  scale_x_discrete(labels = c("B" = "Burned",
                              "H" = "Harvested",
                              "HB" = "Harvest&Burn",
                              "U" = "Untreated")) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1))

ggsave(plot = nest_tx,
       filename = here("pictures", "Rfigures", "nest_tx_bar.pdf"),
       width = 4, 
       height = 3,
       units = "in")



# Facetted zoomed in map and barplot --------------------------------------

facetted_map_tx <- forests_map/nest_tx

ggsave(plot = facetted_map_tx,
       filename = here("pictures", "Rfigures", "location_tx_nests.pdf"),
       width = 7, 
       height = 5,
       units = "in")
