library(tidyverse)
library(sf)
library(tmap) # Loading tmap will change the random number status!
library(readxl)

MSOA_MODE <- F

VALIDATION <- T

IPF_MODE <- T

CITY <- "London"
# CITY <- "Greater Manchester"
# CITY <- "Aberdeen"
# CITY <- "Glasgow"
# CITY <- "Wales"
# CITY <- "Cardiff"

# Change this value to your working directory.
DIR_WORK <- "D:/ATI/SIPHER"

setwd(DIR_WORK)

source("Config.R", echo = T)

source("Utility.R", echo = T)

VIEW_MODE <- "plot"
# VIEW_MODE <- "view"
tmap_mode(VIEW_MODE) # View mode: "plot" for the static mode; "view" for the interactive mode.

# Create output directories.
if (!file.exists(DIR_OUTPUT))
  dir.create(DIR_OUTPUT)

if (MSOA_MODE) {
  if (!file.exists(DIR_OUTPUT_MSOA))
    dir.create(DIR_OUTPUT_MSOA)
} else {
  if (!file.exists(DIR_OUTPUT_LSOA))
    dir.create(DIR_OUTPUT_LSOA)
}

ind <- read_csv("ind_drop_na.csv")

if (MSOA_MODE) {
  if (IPF_MODE) {
    pop <- read_rds("population_IPF_MSOA.rds")
  } else {
    pop <- read_csv("population_SA_MSOA.csv")
  }
} else{
  if (IPF_MODE) {
    pop <- read_rds("population_IPF_LSOA.rds")
  } else {
    pop <- read_csv("population_SA_LSOA.csv")
  }
}

if (IPF_MODE) {
  if (MSOA_MODE) {
    weight <- read_rds("weight_MSOA.rds")
    
    zones <- read_csv("cons_sex_age_MSOA.csv")
  } else {
    weight <- read_rds("weight_LSOA.rds")
    
    zones <- read_csv("cons_sex_age_LSOA.csv")
  }
  
  areas_no_people <- which(rowSums(zones[-1]) == 0)
  
  if (length(areas_no_people) != 0) {
    zones[areas_no_people, 1] # Show zone IDs.
    
    zones <- zones %>% slice(-areas_no_people)
  }
  
  colnames(weight) <- zones$ZoneID
}

pop <- pop %>% inner_join(ind, by = "pidp")

if (CITY == "London") {
  if (MSOA_MODE) {
    gisCity <- st_read("GIS/London/ESRI", "MSOA_2011_London_gen_MHW")
    gisCity$ZoneID <- gisCity$MSOA11CD
  } else {
    gisCity <- st_read("GIS/London/ESRI", "LSOA_2011_London_gen_MHW")
    gisCity$ZoneID <- gisCity$LSOA11CD
  }
} else if (CITY == "Greater Manchester") {
  lookup <-
    read_csv(
      "data/Output_Area_to_Lower_layer_Super_Output_Area_to_Middle_layer_Super_Output_Area_to_Local_Authority_District_(December_2011)_Lookup_in_England_and_Wales.csv"
    )
  
  lookup <-
    lookup %>% select(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM) %>% unique %>% arrange(LSOA11CD)
  
  gisCity <- st_read("GIS/UK_LSOA", "infuse_lsoa_lyr_2011")
  
  gisCity$ZoneID <- gisCity$geo_code
  
  gisCity <-
    gisCity %>% inner_join(lookup, by = c("ZoneID" = "LSOA11CD"))
  
  gisCity <- gisCity %>% filter((LAD11NM == "Bolton") |
                                  (LAD11NM == "Bury") |
                                  (LAD11NM == "Manchester") |
                                  (LAD11NM == "Oldham") |
                                  (LAD11NM == "Rochdale") |
                                  (LAD11NM == "Salford") |
                                  (LAD11NM == "Stockport") |
                                  (LAD11NM == "Tameside") |
                                  (LAD11NM == "Trafford") |
                                  (LAD11NM == "Wigan")
  )
} else if (CITY == "Aberdeen" | CITY == "Glasgow") {
  lookup <- read_csv("data/DataZone2011lookup_2022-05-31.csv")
  
  lookup <- lookup %>% select(1, 2, 7, 8)
  
  colnames(lookup) <-
    c("LSOA11CD", "LSOA11NM", "LAD11CD", "LAD11NM")
  
  lookup <-
    lookup %>% unique %>% filter(LAD11NM == paste0(CITY, " City")) %>% arrange(LSOA11CD)
  
  gisCity <-
    st_read("GIS/SG_DataZoneBdry_2011", "SG_DataZone_Bdry_2011")
  
  gisCity$ZoneID <- gisCity$DataZone
  
  gisCity <-
    gisCity %>% filter(ZoneID %in% lookup$LSOA11CD) %>% inner_join(lookup, by = c("ZoneID" = "LSOA11CD"))
} else if (CITY == "Wales" | CITY == "Cardiff") {
  ### Which lookup table to use???
  lookup <-
    read_xlsx(#"data/Output_Area_to_Lower_layer_Super_Output_Area_to_Middle_layer_Super_Output_Area_to_Local_Authority_District_(December_2011)_Lookup_in_England_and_Wales.csv"
      "data/Geography lookups - match LSOA to different geography groups.xlsx",
      sheet = 2)
  
  # lookup <- lookup %>% select(LSOA11CD, LSOA11NM, LAD11CD, LAD11NM) %>% unique %>% arrange(LSOA11CD)
  
  lookup <-
    lookup %>% select(1, 2, 3, 4)
  
  colnames(lookup) <-
    c("LSOA11CD", "LSOA11NM", "LAD11CD", "LAD11NM")
  
  lookup <- lookup %>% unique %>% arrange(LSOA11CD)
  
  gisCity <- st_read("GIS/Wales_LSOA", "lsoa_wales_2011Polygon")
  
  gisCity$ZoneID <- gisCity$LSOA11Code
  # gisCity$LSOA11CD <- gisCity$LSOA11Code
  
  gisCity <-
    gisCity %>% inner_join(lookup,
                           by = c("ZoneID" = "LSOA11CD"))
  
  if (CITY == "Cardiff")
    gisCity <- gisCity %>% filter(LAD11NM == CITY)
}

if (!all(st_is_valid(gisCity)))
  gisCity <- st_make_valid(gisCity)

pop <- pop %>% filter(ZoneID %in% gisCity$ZoneID)

pop$GHQ1[pop$GHQ1 < 0] <- NA
pop$GHQ2[pop$GHQ2 < 0] <- NA
pop$PCS[pop$PCS < 0] <- NA
pop$MCS[pop$MCS < 0] <- NA
pop$sat_health[pop$sat_health < 0] <- NA
pop$sat_income[pop$sat_income < 0] <- NA
pop$sat_life[pop$sat_life < 0] <- NA
pop$sat_job[pop$sat_job < 0] <- NA
pop$loneliness[pop$loneliness < 0] <- NA
pop$worthless[pop$worthless < 0] <- NA
pop$happiness[pop$happiness < 0] <- NA
pop$smoker[pop$smoker < 0] <- NA
# pop$finsec[pop$finsec < 0] <- NA
# pop$jobsec[pop$jobsec < 0] <- NA

# Reverse PCS and MCS.
# pop$PCS <- max(pop$PCS, na.rm = TRUE) - pop$PCS
# pop$MCS <- max(pop$MCS, na.rm = TRUE) - pop$MCS

# if (VALIDATION) {
#   gis <- gisCity %>% st_drop_geometry() %>% select(ZoneID, LAD11CD)
#
#   pop <- pop %>% inner_join(gis, by = "ZoneID")
# }

pop$sex <- as.factor(pop$sex)
pop$sex <- relevel(pop$sex, ref = 2)

pop$hiqual <- as.factor(pop$hiqual)
pop$hiqual <- relevel(pop$hiqual, ref = 4)

pop$ethnicity <- as.factor(pop$ethnicity)
pop$ethnicity <- relevel(pop$ethnicity, ref = 5)

pop$marstat <- as.factor(pop$marstat)
pop$marstat <- relevel(pop$marstat, ref = 4)

pop$jbstat <- as.factor(pop$jbstat)

pop$hhtype <- as.factor(pop$hhtype)
pop$hhtype <- relevel(pop$hhtype, ref = 2)

pop$htenure <- as.factor(pop$htenure)
pop$htenure <- relevel(pop$htenure, ref = 2)

pop$smoker <- as.factor(pop$smoker)
pop$smoker <- relevel(pop$smoker, ref = 2)

# Important!!!
ind$GHQ1[ind$GHQ1 < 0] <- NA
ind$GHQ2[ind$GHQ2 < 0] <- NA
ind$sat_health[ind$sat_health < 1] <- NA
ind$sat_income[ind$sat_income < 1] <- NA
ind$sat_time[ind$sat_time < 1] <- NA
ind$sat_life[ind$sat_life < 1] <- NA
ind$loneliness[ind$loneliness < 1] <- NA

if (VALIDATION &
    (CITY != "Aberdeen") &
    (CITY != "Glasgow") & (CITY != "Cardiff")) {
  gis <- gisCity %>% st_drop_geometry() %>% select(ZoneID, LAD11CD)
  
  pop <- pop %>% inner_join(gis, by = "ZoneID")
  
  if (IPF_MODE) {
    weight1 <- weight %>% as_tibble()
    
    weight1 <-
      weight1 %>% select(contains(gisCity$ZoneID)) # gisCity$ZoneID may not be ordered. weight1 has no n/a values.
    
    # ind$GHQ1[ind$GHQ1 < 0] <- NA
    
    ind_no_GHQ <- which(is.na(ind$GHQ1))
    
    if (length(ind_no_GHQ) != 0) {
      ind_remove_na <- ind %>% slice(-ind_no_GHQ)
      
      weight_remove_na <- weight1 %>% slice(-ind_no_GHQ)
      
      mental_health <- ind_remove_na$GHQ1 * weight_remove_na
      
      sum_GHQ <- colSums(mental_health)
      
      sum_pop <- colSums(weight_remove_na)
    } else {
      mental_health <- ind$GHQ1 * weight1
      
      sum_GHQ <- colSums(mental_health)
      
      sum_pop <- colSums(weight1)
    }
    
    GHQ_LSOA <-
      sum_GHQ %>% as.table() %>% as.data.frame() %>% as_tibble()
    
    pop_LSOA <-
      sum_pop %>% as.table() %>% as.data.frame() %>% as_tibble()
    
    colnames(GHQ_LSOA) <- c(ZONE_ID, "GHQ1")
    
    colnames(pop_LSOA) <- c(ZONE_ID, "pop")
    
    GHQ_LSOA <- GHQ_LSOA %>% inner_join(gis)
    
    pop_LSOA <- pop_LSOA %>% inner_join(gis)
    
    GHQ_LAD <- aggregate(GHQ_LSOA$GHQ1,
                         by = list(GHQ_LSOA$LAD11CD),
                         sum,
                         na.rm = TRUE)
    
    pop_LAD <- aggregate(pop_LSOA$pop,
                         by = list(pop_LSOA$LAD11CD),
                         sum,
                         na.rm = TRUE)
    
    colnames(GHQ_LAD) <- c("LAD11CD", "GHQ1")
    
    colnames(pop_LAD) <- c("LAD11CD", "pop")
    
    agg_GHQ1 <- GHQ_LAD
    
    if (identical(agg_GHQ1$LAD11CD, pop_LAD$LAD11CD))
      agg_GHQ1$GHQ1 <- agg_GHQ1$GHQ1 / pop_LAD$pop
  } else {
    agg_GHQ1 <-
      aggregate(pop$GHQ1,
                by = list(pop$LAD11CD),
                mean,
                na.rm = TRUE)
    
    colnames(agg_GHQ1) <- c("LAD11CD", "GHQ1")
  }
  
  agg_GHQ1 <- agg_GHQ1 %>% arrange(LAD11CD)
  
  wb <- read_excel(paste0("data/Well-being_", CITY, ".xlsx"))
  
  colnames(wb)[2] <- "LAD11CD"
  
  colnames(wb)[12] <- "Y2018"
  colnames(wb)[13] <- "Y2019"
  colnames(wb)[14] <- "Y2020"
  colnames(wb)[15] <- "Y2021"
  
  wb <- wb %>% arrange(LAD11CD)
  
  wb <- wb %>% filter(Estimate == "Average (mean)")
  
  wb_anxiety_2018 <-
    wb %>% filter(MeasureOfWellbeing == "Anxiety") %>% select(LAD11CD, Y2018)
  wb_anxiety_2019 <-
    wb %>% filter(MeasureOfWellbeing == "Anxiety") %>% select(LAD11CD, Y2019)
  wb_anxiety_2020 <-
    wb %>% filter(MeasureOfWellbeing == "Anxiety") %>% select(LAD11CD, Y2020)
  wb_anxiety_2021 <-
    wb %>% filter(MeasureOfWellbeing == "Anxiety") %>% select(LAD11CD, Y2021)
  
  wb_happiness_2018 <-
    wb %>% filter(MeasureOfWellbeing == "Happiness") %>% select(LAD11CD, Y2018)
  wb_happiness_2019 <-
    wb %>% filter(MeasureOfWellbeing == "Happiness") %>% select(LAD11CD, Y2019)
  wb_happiness_2020 <-
    wb %>% filter(MeasureOfWellbeing == "Happiness") %>% select(LAD11CD, Y2020)
  wb_happiness_2021 <-
    wb %>% filter(MeasureOfWellbeing == "Happiness") %>% select(LAD11CD, Y2021)
  
  wb_life_2018 <-
    wb %>% filter(MeasureOfWellbeing == "Life satisfaction") %>% select(LAD11CD, Y2018)
  wb_life_2019 <-
    wb %>% filter(MeasureOfWellbeing == "Life satisfaction") %>% select(LAD11CD, Y2019)
  wb_life_2020 <-
    wb %>% filter(MeasureOfWellbeing == "Life satisfaction") %>% select(LAD11CD, Y2020)
  wb_life_2021 <-
    wb %>% filter(MeasureOfWellbeing == "Life satisfaction") %>% select(LAD11CD, Y2021)
  
  wb_worthwhile_2018 <-
    wb %>% filter(MeasureOfWellbeing == "Worthwhile") %>% select(LAD11CD, Y2018)
  wb_worthwhile_2019 <-
    wb %>% filter(MeasureOfWellbeing == "Worthwhile") %>% select(LAD11CD, Y2019)
  wb_worthwhile_2020 <-
    wb %>% filter(MeasureOfWellbeing == "Worthwhile") %>% select(LAD11CD, Y2020)
  wb_worthwhile_2021 <-
    wb %>% filter(MeasureOfWellbeing == "Worthwhile") %>% select(LAD11CD, Y2021)
  
  wb_2018 <-
    (10 - wb_anxiety_2018[2]) + wb_happiness_2018[2] + wb_life_2018[2] + wb_worthwhile_2018[2]
  wb_2019 <-
    (10 - wb_anxiety_2019[2]) + wb_happiness_2019[2] + wb_life_2019[2] + wb_worthwhile_2019[2]
  wb_2020 <-
    (10 - wb_anxiety_2020[2]) + wb_happiness_2020[2] + wb_life_2020[2] + wb_worthwhile_2020[2]
  wb_2021 <-
    (10 - wb_anxiety_2021[2]) + wb_happiness_2021[2] + wb_life_2021[2] + wb_worthwhile_2021[2]
  
  wb_2018$LAD11CD <- wb_anxiety_2018$LAD11CD
  wb_2019$LAD11CD <- wb_anxiety_2019$LAD11CD
  wb_2020$LAD11CD <- wb_anxiety_2020$LAD11CD
  wb_2021$LAD11CD <- wb_anxiety_2021$LAD11CD
  
  if (CITY == "London") {
    myCity <- st_read("GIS/London/ESRI", "London_Borough_Excluding_MHW")
    
    if (!all(st_is_valid(myCity)))
      myCity <- st_make_valid(myCity)
    
    myCity$LAD11CD <- myCity$GSS_CODE
    
    myCity <-
      myCity %>% inner_join(agg_GHQ1, by = "LAD11CD") %>% inner_join(wb_2020, by = "LAD11CD")
    
    myCity$GHQ_R <- 36 - myCity$GHQ1
    
    tm <-
      tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "GHQ_R",
                                                                      palette = "Reds",
                                                                      title = "Reverse of GHQ:\n0 (the most distressed) to 36 (the least distressed)") + tm_layout(title = CITY, title.size = TITLE_SIZE) + tm_text("NAME", size = 0.5)
    saveMap(tm, paste0(CITY, "_", "SIM"), MSOA_MODE)
    
    tm <-
      tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "Y2020",
                                                                      palette = "Greens",
                                                                      title = "Subjective well-being:\n0 (the most distressed) to 40 (the least distressed)") + tm_layout(title = CITY, title.size = TITLE_SIZE) + tm_text("NAME", size = 0.5)
    saveMap(tm, paste0(CITY, "_", "ONS"), MSOA_MODE)
    
    # rank <- myCity %>% st_drop_geometry() %>% select(NAME, GHQ1)
    #
    # rank <- transform(rank, NAME = reorder(NAME, GHQ1))
    #
    # gg <- ggplot(data = rank,
    #              mapping = aes(x = NAME, y = GHQ1)) +
    #   geom_bar(stat = "identity", fill = "pink2") + coord_flip() +
    #   xlab("Zone") +
    #   ylab("Average GHQ") +
    #   ggtitle("Ranking of simulated mental distress by local authority districts in London") +
    #   theme(panel.grid.minor.x = element_blank())
    #
    # saveRank("London_Ranking_SIM", isLAD = T)
    #
    # rank <- myCity %>% st_drop_geometry() %>% select(NAME, Y2020)
    #
    # rank <- rank %>% drop_na()
    #
    # rank <- transform(rank, NAME = reorder(NAME, -Y2020))
    #
    # gg <- ggplot(data = rank,
    #              mapping = aes(x = NAME, y = Y2020)) +
    #   geom_bar(stat = "identity", fill = "palegreen3") + coord_flip() +
    #   xlab("Zone") +
    #   ylab("Average GHQ") +
    #   ggtitle("Ranking of ONS's subjective well-being by local authority districts in London") +
    #   theme(panel.grid.minor.x = element_blank())
    #
    # saveRank("London_Ranking_ONS", isLAD = T)
  }
  
  if (identical(agg_GHQ1$LAD11CD, wb_2020$LAD11CD)) {
    # cor(agg_GHQ1$GHQ1,
    #     wb_anxiety_2020$Y2020,
    #     method = "pearson",
    #     use = "complete.obs")
    #
    # cor(agg_GHQ1$GHQ1,
    #     wb_happiness_2020$Y2020,
    #     method = "pearson",
    #     use = "complete.obs")
    #
    # cor(agg_GHQ1$GHQ1,
    #     wb_life_2020$Y2020,
    #     method = "pearson",
    #     use = "complete.obs")
    #
    # cor(agg_GHQ1$GHQ1,
    #     wb_worthwhile_2020$Y2020,
    #     method = "pearson",
    #     use = "complete.obs")
    #
    # cor(agg_GHQ1$GHQ1,
    #     wb_2020$Y2020,
    #     method = "pearson",
    #     use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_anxiety_2019$Y2019,
        method = "spearman",
        use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_happiness_2019$Y2019,
        method = "spearman",
        use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_life_2019$Y2019,
        method = "spearman",
        use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_worthwhile_2019$Y2019,
        method = "spearman",
        use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_2019$Y2019,
        method = "spearman",
        use = "complete.obs")
    
    ###
    
    cor(agg_GHQ1$GHQ1,
        wb_2020$Y2020,
        method = "spearman",
        use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_anxiety_2020$Y2020,
        method = "spearman",
        use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_happiness_2020$Y2020,
        method = "spearman",
        use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_life_2020$Y2020,
        method = "spearman",
        use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_worthwhile_2020$Y2020,
        method = "spearman",
        use = "complete.obs")
    
    cor(agg_GHQ1$GHQ1,
        wb_2020$Y2020,
        method = "spearman",
        use = "complete.obs")
    
    ###
    
    cor(agg_GHQ1$GHQ1,
        wb_2018$Y2018,
        method = "spearman",
        use = "complete.obs")
    cor(agg_GHQ1$GHQ1,
        wb_2019$Y2019,
        method = "spearman",
        use = "complete.obs")
    cor(agg_GHQ1$GHQ1,
        wb_2020$Y2020,
        method = "spearman",
        use = "complete.obs")
    cor(agg_GHQ1$GHQ1,
        wb_2021$Y2021,
        method = "spearman",
        use = "complete.obs")
  }
}

weighting_LSOA <- function(ind, weight, variable, gisCity) {
  weight1 <- weight %>% as_tibble()
  
  weight1 <-
    weight1 %>% select(contains(gisCity$ZoneID)) # gisCity$ZoneID may not be ordered. weight1 has no n/a values.
  
  ind_no_var <- which(is.na(pull(ind, variable)))
  
  if (length(ind_no_var) != 0) {
    ind_remove_na <- ind %>% slice(-ind_no_var)
    
    weight_remove_na <- weight1 %>% slice(-ind_no_var)
    
    weighted_var <- pull(ind_remove_na, variable) * weight_remove_na
    
    sum_var <- colSums(weighted_var)
    
    sum_pop <- colSums(weight_remove_na)
  } else {
    weighted_var <- pull(ind, variable) * weight1
    
    sum_var <- colSums(weighted_var)
    
    sum_pop <- colSums(weight1)
  }
  
  agg <- sum_var / sum_pop
  
  agg_var <-
    agg %>% as.table() %>% as.data.frame() %>% as_tibble()
  
  colnames(agg_var) <- c(ZONE_ID, variable)
  
  agg_var$ZoneID <- as.character(agg_var$ZoneID)
  
  agg_var <- agg_var %>% arrange(ZoneID)
  
  agg_var
}

weighting_MSOA <- function(ind, weight, variable, gisCity) {
  weight1 <- weight %>% as_tibble()
  
  weight1 <-
    weight1 %>% select(contains(gisCity$ZoneID)) # gisCity$ZoneID may not be ordered. weight1 has no n/a values.
  
  ind_no_var <- which(is.na(pull(ind, variable)))
  
  if (length(ind_no_var) != 0) {
    ind_remove_na <- ind %>% slice(-ind_no_var)
    
    weight_remove_na <- weight1 %>% slice(-ind_no_var)
    
    weighted_var <- pull(ind_remove_na, variable) * weight_remove_na
    
    sum_var <- colSums(weighted_var)
    
    sum_pop <- colSums(weight_remove_na)
  } else {
    weighted_var <- pull(ind, variable) * weight1
    
    sum_var <- colSums(weighted_var)
    
    sum_pop <- colSums(weight1)
  }
  
  lookup <-
    read_csv(
      "data/Output_Area_to_Lower_layer_Super_Output_Area_to_Middle_layer_Super_Output_Area_to_Local_Authority_District_(December_2011)_Lookup_in_England_and_Wales.csv"
    )
  
  lookup <-
    lookup %>% select(LSOA11CD, MSOA11CD) %>% unique %>% arrange(LSOA11CD)
  
  var_LSOA <-
    sum_var %>% as.table() %>% as.data.frame() %>% as_tibble()
  
  pop_LSOA <-
    sum_pop %>% as.table() %>% as.data.frame() %>% as_tibble()
  
  colnames(var_LSOA) <- c(ZONE_ID, variable)
  
  colnames(pop_LSOA) <- c(ZONE_ID, "pop")
  
  var_LSOA <-
    var_LSOA %>% inner_join(lookup, by = c("ZoneID" = "LSOA11CD"))
  
  pop_LSOA <-
    pop_LSOA %>% inner_join(lookup, by = c("ZoneID" = "LSOA11CD"))
  
  var_MSOA <- aggregate(
    pull(var_LSOA, variable),
    by = list(var_LSOA$MSOA11CD),
    sum,
    na.rm = TRUE
  )
  
  pop_MSOA <- aggregate(pop_LSOA$pop,
                        by = list(pop_LSOA$MSOA11CD),
                        sum,
                        na.rm = TRUE)
  
  colnames(var_MSOA) <- c("MSOA11CD", variable)
  
  colnames(pop_MSOA) <- c("MSOA11CD", "pop")
  
  agg_var_MSOA <- var_MSOA
  
  if (identical(agg_var_MSOA$MSOA11CD, pop_MSOA$MSOA11CD))
    agg_var_MSOA[variable] <-
    agg_var_MSOA[variable] / pop_MSOA$pop
  
  agg_var_MSOA <- agg_var_MSOA %>% arrange(MSOA11CD)
  
  agg_var_MSOA
}

if (IPF_MODE) {
  if (MSOA_MODE) {
    agg_GHQ1 <- weighting_LSOA(ind, weight, "GHQ1", gisCity)
    
    agg_GHQ2 <- weighting_LSOA(ind, weight, "GHQ2", gisCity)
    
    agg_sat_life <- weighting_LSOA(ind, weight, "sat_life", gisCity)
    
    agg_sat_health <-
      weighting_LSOA(ind, weight, "sat_health", gisCity)
    
    agg_loneliness <-
      weighting_LSOA(ind, weight, "loneliness", gisCity)
  }
  else {
    agg_GHQ1 <- weighting_LSOA(ind, weight, "GHQ1", gisCity)
    
    agg_GHQ2 <- weighting_LSOA(ind, weight, "GHQ2", gisCity)
    
    agg_sat_life <- weighting_LSOA(ind, weight, "sat_life", gisCity)
    
    agg_sat_health <-
      weighting_LSOA(ind, weight, "sat_health", gisCity)
    
    agg_loneliness <-
      weighting_LSOA(ind, weight, "loneliness", gisCity)
    
    agg_GHQ1_MSOA <- weighting_MSOA(ind, weight, "GHQ1", gisCity)
  }
} else {
  agg_GHQ1 <-
    aggregate(pop$GHQ1,
              by = list(pop$ZoneID),
              mean,
              na.rm = TRUE)
  
  colnames(agg_GHQ1) <- c(ZONE_ID, "GHQ1")
  
  # Add agg_GHQ_MSOA here.
}

if (CITY == "London" | CITY == "Greater Manchester") {
  imd <-
    read_excel("data/IMD2019_Index_of_Multiple_Deprivation.xlsx", sheet = 2) %>% select(1, 5, 6)
  
  colnames(imd) <- c("LSOA11CD", "rank", "dec")
  
  imd <-
    imd %>% subset(LSOA11CD %in% pull(agg_GHQ1, ZONE_ID)) %>% arrange(LSOA11CD)
} else if (CITY == "Aberdeen" | CITY == "Glasgow") {
  imd <-
    read_excel("data/SIMD+2020v2+-+ranks.xlsx", sheet = 2) %>% select(1, 6)
  
  colnames(imd) <- c("LSOA11CD", "rank")
  
  imd <-
    imd %>% subset(LSOA11CD %in% pull(agg_GHQ1, ZONE_ID)) %>% arrange(LSOA11CD)
} else if (CITY == "Wales" | CITY == "Cardiff") {
  lookup_Wales <-
    read_xlsx("data/Geography lookups - match LSOA to different geography groups.xlsx",
              sheet = 2)
  
  lookup_Wales <-
    lookup_Wales %>% select(1, 2, 3, 4)
  
  colnames(lookup_Wales) <-
    c("LSOA11CD", "LSOA11NM", "LAD11CD", "LAD11NM")
  
  lookup_Wales <- lookup_Wales %>% unique %>% arrange(LSOA11CD)
  
  imd <-
    read_csv("data/WIMD.csv") # Because of "Felin-fâch", this file should be UTF-8 csv.
  
  imd <-
    lookup_Wales %>% inner_join(imd, by = c("LSOA11NM" = "LSOA"))
  
  mh <-
    read_csv("data/MentalHealth_Wales.csv") # Because of "Felin-fâch", this file should be UTF-8 csv.
  
  mh <-
    lookup_Wales %>% inner_join(mh, by = c("LSOA11NM" = "LSOA"))
  
  if (CITY == "Cardiff") {
    imd <- imd %>% filter(LAD11NM == CITY)
    
    mh <- mh %>% filter(LAD11NM == CITY)
  }
}

if (!MSOA_MODE & identical(agg_GHQ1$ZoneID, imd$LSOA11CD)) {
  cor(agg_GHQ1$GHQ1, imd$rank, method = "pearson")
  
  cor(agg_GHQ1$GHQ1, imd$rank, method = "spearman")
  
  cor(agg_GHQ2$GHQ2, imd$rank, method = "pearson")
  
  cor(agg_GHQ2$GHQ2, imd$rank, method = "spearman")
  
  cor(agg_sat_life$sat_life, imd$rank, method = "spearman")
  
  # if (CITY == "Wales" | CITY == "Cardiff") {
  #   cor(agg_GHQ1$GHQ1, mh$MH, method = "pearson")
  # }
}

if (MSOA_MODE) {
  myCity <-
    gisCity %>% inner_join(agg_GHQ1, by = ZONE_ID) %>% inner_join(agg_GHQ2, by = ZONE_ID) %>% inner_join(agg_sat_life, by = ZONE_ID) %>% inner_join(agg_sat_health, by = ZONE_ID) %>% inner_join(agg_loneliness, by = ZONE_ID)
} else {
  myCity <-
    gisCity %>% inner_join(agg_GHQ1, by = ZONE_ID) %>% inner_join(agg_GHQ2, by = ZONE_ID) %>% inner_join(agg_sat_life, by = ZONE_ID) %>% inner_join(agg_sat_health, by = ZONE_ID) %>% inner_join(agg_loneliness, by = ZONE_ID) %>% inner_join(imd, by = c("ZoneID" = "LSOA11CD"))
}

myCity$GHQ1_R <- 36 - myCity$GHQ1




tm <-
  tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "GHQ1",
                                                                  palette = "Reds",
                                                                  title = "GHQ (Likert):\n0 (the least distressed) to 36 (the most distressed)") + tm_layout(title = CITY, title.size = TITLE_SIZE)
saveMap(tm, paste0(CITY, "_", "GHQ1"), MSOA_MODE)

tm <-
  tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "GHQ1_R",
                                                                  palette = "Reds",
                                                                  title = "Reverse of GHQ (Likert):\n0 (the most distressed) to 36 (the least distressed)") + tm_layout(title = CITY, title.size = TITLE_SIZE)
saveMap(tm, paste0(CITY, "_", "GHQ1_R"), MSOA_MODE)

tm <-
  tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "GHQ2",
                                                                  palette = "Reds",
                                                                  title = "GHQ (caseness):\n0 (the least distressed) to 12 (the most distressed)") + tm_layout(title = CITY, title.size = TITLE_SIZE)
saveMap(tm, paste0(CITY, "_", "GHQ2"), MSOA_MODE)

tm <-
  tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "sat_life",
                                                                  palette = "Greens",
                                                                  title = "Satisfaction with life:\n1 (completely dissatisfied) to 7 (completely satisfied)") + tm_layout(title = CITY, title.size = TITLE_SIZE)
saveMap(tm, paste0(CITY, "_", "Sat_Life"), MSOA_MODE)

tm <-
  tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "sat_health",
                                                                  palette = "Blues",
                                                                  title = "Satisfaction with health:\n1 (completely dissatisfied) to 36 (completely satisfied)") + tm_layout(title = CITY, title.size = TITLE_SIZE)
saveMap(tm, paste0(CITY, "_", "Sat_Health"), MSOA_MODE)

tm <-
  tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "loneliness",
                                                                  palette = "Purples",
                                                                  title = "Loneliness:\n1 (hardly ever or never) to 3 (often)") + tm_layout(title = CITY, title.size = TITLE_SIZE)
saveMap(tm, paste0(CITY, "_", "Loneliness"), MSOA_MODE)


if (!MSOA_MODE) {
  tm <-
    tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "rank",
                                                                    palette = "Greens",
                                                                    title = "Rank of subjective well-being:\nmin (the most distressed) to max (the least distressed)") + tm_layout(title = CITY, title.size = TITLE_SIZE)
  saveMap(tm, paste0(CITY, "_", "SWB"), MSOA_MODE)
}

if (!MSOA_MODE & CITY == "London") {
  myCity <-
    st_read("GIS/London/ESRI", "MSOA_2011_London_gen_MHW") %>% inner_join(agg_GHQ1_MSOA)
  
  myCity$GHQ1_R <- 36 - myCity$GHQ1
  
  tm <-
    tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "GHQ1",
                                                                    palette = "Reds",
                                                                    title = "GHQ:\n0 (the least distressed) to 36 (the most distressed)") + tm_layout(title = CITY, title.size = TITLE_SIZE)
  saveMap(tm, paste0(CITY, "_", "GHQ1_MSOA"), MSOA_MODE)
  
  tm <-
    tm_shape(myCity) + tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "GHQ1_R",
                                                                    palette = "Reds",
                                                                    title = "Reverse of GHQ:\n0 (the most distressed) to 36 (the least distressed)") + tm_layout(title = CITY, title.size = TITLE_SIZE)
  saveMap(tm, paste0(CITY, "_", "GHQ1_R_MSOA"), MSOA_MODE)
}
