library(tidyverse)
library(sf)
library(tmap) # Loading tmap will change the random status!

# Change this value to your working directory.
DIR_WORK <- "D:/ATI/SIPHER"

setwd(DIR_WORK)

source("Config.R", echo = T)

source("Utility.R", echo = T)

LAD_MODE <- F

MY_LAD_ID <-
  "E09000031" # For the MSOA mode only: specify the LAD ID of interest (e.g. E09000031: Waltham Forest).

VIEW_MODE <- "plot"
# VIEW_MODE <- "view"
tmap_mode(VIEW_MODE) # View mode: "plot" for the static mode; "view" for the interactive mode.

# Create output directories.
if (!file.exists(DIR_OUTPUT))
  dir.create(DIR_OUTPUT)

if (!file.exists(DIR_OUTPUT_LAD))
  dir.create(DIR_OUTPUT_LAD)

if (!file.exists(DIR_OUTPUT_MSOA))
  dir.create(DIR_OUTPUT_MSOA)

ind <- read_csv("ind.csv")

pop <- readRDS("pop.rds")

pop <- inner_join(pop, ind, by = "pidp")

city <- "London"
gisCity <- st_read("GIS/London/ESRI", "MSOA_2011_London_gen_MHW")
gisCity$ZoneID <- gisCity$MSOA11CD

# city <- "Wales"
# gisCity <- st_read("GIS/Wales_MSOA", "msoa_wales_2011")
# gisCity$ZoneID <- gisCity$MSOA11_Cod

# city <- "England & Wales"

# city <- "Leeds"
# gisCity <- st_read("GIS/UK_MSOA", "infuse_msoa_lyr_2011")
# gisCity <- gisCity %>% filter(substr(gisCity$geo_label, 1, 5) == "Leeds") # Check again!!!
# gisCity$ZoneID <- gisCity$geo_code

pop <- pop %>% filter(ZoneID %in% gisCity$ZoneID)

pop$GHQ1[pop$GHQ1 < 0] <- NA
pop$GHQ2[pop$GHQ2 < 0] <- NA
pop$PCS[pop$PCS < 0] <- NA
pop$MCS[pop$MCS < 0] <- NA
pop$sat_health[pop$sat_health < 0] <- NA
pop$sat_income[pop$sat_income < 0] <- NA
pop$sat_life[pop$sat_life < 0] <- NA
pop$sat_job[pop$sat_job < 0] <- NA
pop$lonely[pop$lonely < 0] <- NA
pop$smoker[pop$smoker < 0] <- NA
# pop$finsec[pop$finsec < 0] <- NA
# pop$jobsec[pop$jobsec < 0] <- NA

# Reverse PCS and MCS.
# pop$PCS <- max(pop$PCS, na.rm = TRUE) - pop$PCS
# pop$MCS <- max(pop$MCS, na.rm = TRUE) - pop$MCS

agg_GHQ1 <-
  aggregate(pop$GHQ1,
            by = list(pop$ZoneID),
            mean,
            na.rm = TRUE)
colnames(agg_GHQ1) <- c(ZONE_ID, "GHQ1")

agg_PCS <-
  aggregate(pop$PCS,
            by = list(pop$ZoneID),
            mean,
            na.rm = TRUE)
colnames(agg_PCS) <- c(ZONE_ID, "PCS")

agg_MCS <-
  aggregate(pop$MCS,
            by = list(pop$ZoneID),
            mean,
            na.rm = TRUE)
colnames(agg_MCS) <- c(ZONE_ID, "MCS")

agg_health <-
  aggregate(pop$sat_health,
            by = list(pop$ZoneID),
            mean,
            na.rm = TRUE)
colnames(agg_health) <- c(ZONE_ID, "sat_health")

agg_income <-
  aggregate(pop$sat_income,
            by = list(pop$ZoneID),
            mean,
            na.rm = TRUE)
colnames(agg_income) <- c(ZONE_ID, "sat_income")

agg_life <-
  aggregate(pop$sat_life,
            by = list(pop$ZoneID),
            mean,
            na.rm = TRUE)
colnames(agg_life) <- c(ZONE_ID, "sat_life")

agg_job <-
  aggregate(pop$sat_job,
            by = list(pop$ZoneID),
            mean,
            na.rm = TRUE)
colnames(agg_job) <- c(ZONE_ID, "sat_job")

agg_lonely <-
  aggregate(pop$lonely,
            by = list(pop$ZoneID),
            mean,
            na.rm = TRUE)
colnames(agg_lonely) <- c(ZONE_ID, "lonely")

agg_smoker <-
  aggregate(pop$smoker,
            by = list(pop$ZoneID),
            mean,
            na.rm = TRUE)
colnames(agg_smoker) <- c(ZONE_ID, "smoker")

gisCity <-
  gisCity %>% merge(agg_GHQ1) %>% merge(agg_PCS) %>% merge(agg_MCS) %>% merge(agg_health) %>% merge(agg_income) %>% merge(agg_life) %>% merge(agg_job) %>% merge(agg_lonely) %>% merge(agg_smoker)

tm <-
  tm_shape(gisCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "GHQ1",
                                                                   palette = "Reds",
                                                                   # alpha = 0.7,
                                                                   # n = NUMBER_BINS,
                                                                   title = "General Health Questionnaire (subjective well-being):\n1 (the least distressed) to 32 (the most distressed)") + tm_layout(title = paste0(city), title.size = TITLE_SIZE)
saveMap(tm, paste0(city, "_", "GHQ1"), F)

tm <-
  tm_shape(gisCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "PCS",
                                                                   palette = "Purples",
                                                                   # alpha = 0.7,
                                                                   # n = NUMBER_BINS,
                                                                   title = "Physical component summary: the higher the better") + tm_layout(title = paste0(city), title.size = TITLE_SIZE)
saveMap(tm, paste0(city, "_", "PCS"), F)

tm <-
  tm_shape(gisCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "MCS",
                                                                   palette = "Purples",
                                                                   # alpha = 0.7,
                                                                   # n = NUMBER_BINS,
                                                                   title = "Mental component summary: the higher the better") + tm_layout(title = paste0(city), title.size = TITLE_SIZE)
saveMap(tm, paste0(city, "_", "MCS"), F)

tm <-
  tm_shape(gisCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "sat_health",
                                                                   palette = "Oranges",
                                                                   # alpha = 0.7,
                                                                   # n = NUMBER_BINS,
                                                                   title = "Satisfaction with health:\n1 (completely dissatisfied) to 7 (completely satisfied)") + tm_layout(title = paste0(city), title.size = TITLE_SIZE)
saveMap(tm, paste0(city, "_", "Health"), F)

tm <-
  tm_shape(gisCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "sat_income",
                                                                   palette = "Oranges",
                                                                   # alpha = 0.7,
                                                                   # n = NUMBER_BINS,
                                                                   title = "Satisfaction with income:\n1 (completely dissatisfied) to 7 (completely satisfied)") + tm_layout(title = paste0(city), title.size = TITLE_SIZE)
saveMap(tm, paste0(city, "_", "Income"), F)

tm <-
  tm_shape(gisCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "sat_life",
                                                                   palette = "Oranges",
                                                                   # alpha = 0.7,
                                                                   # n = NUMBER_BINS,
                                                                   title = "Satisfaction with life:\n1 (completely dissatisfied) to 7 (completely satisfied)") + tm_layout(title = paste0(city), title.size = TITLE_SIZE)
saveMap(tm, paste0(city, "_", "Life"), F)

tm <-
  tm_shape(gisCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "lonely",
                                                                   palette = "Greens",
                                                                   # alpha = 0.7,
                                                                   # n = NUMBER_BINS,
                                                                   title = "Loneliness: 1 (hardly ever or never) to 3 (often)") + tm_layout(title = paste0(city), title.size = TITLE_SIZE)
saveMap(tm, paste0(city, "_", "Loneliness"), F)

tm <-
  tm_shape(gisCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "smoker",
                                                                   palette = "Blues",
                                                                   # alpha = 0.7,
                                                                   # n = NUMBER_BINS,
                                                                   title = "Smoker: 1 (yes), 2 (no)") + tm_layout(title = paste0(city), title.size = TITLE_SIZE)
saveMap(tm, paste0(city, "_", "Smoker"), F)
