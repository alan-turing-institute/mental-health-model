library(tidyverse)
library(readxl)

# True: MSOA; False: LSOA.
MSOA_MODE <- F

# Change this value to your working directory.
DIR_WORK <- "D:/ATI/SIPHER"

setwd(DIR_WORK)

source("Config.R", echo = T)

# Read the individual data (adults).
ind_orig <-
  read_delim("data/US/k_indresp.tab",
             delim = "\t")

# Read the household data.
hh_orig <-
  read_delim("data/US/k_hhresp.tab",
             delim = "\t")

# Join the individual and household data.
ind_orig <-
  ind_orig %>% inner_join(hh_orig %>% select(k_hidp, k_tenure_dv, k_fihhmnnet1_dv), by = "k_hidp")

# Select useful variables.
ind <-
  ind_orig %>% select(
    pidp,
    k_hidp,
    k_indinui_xw,
    # Main interview survey weight.
    k_sex,
    k_dvage,
    # dvage or age_dv???
    k_hiqual_dv,
    k_racel_dv,
    k_marstat,
    k_jbstat,
    # k_jbnssec8_dv, # Eight class NS-SEC (cf. occupation: https://www.nomisweb.co.uk/census/2011/qs606uk).
    k_hhtype_dv,
    k_tenure_dv,
    k_fimnnet_dv,
    # Total net personal income - no deductions.
    k_fihhmnnet1_dv,
    # Total household net income - no deductions.
    k_urban_dv,
    # Urban or rural area.
    k_scghq1_dv,
    # GHQ based on Likert.
    k_scghq2_dv,
    # GHQ based on caseness.
    k_sf12pcs_dv,
    # SF-12 Physical Component Summary (PCS).
    k_sf12mcs_dv,
    # SF-12 Mental Component Summary (MCS).
    k_sclfsat1,
    # Satisfaction with health.
    k_sclfsat2,
    # Satisfaction with income.
    # k_sclfsat3,
    # Satisfaction with house/flat.
    k_sclfsat7,
    # Satisfaction with amount of leisure time.
    k_sclfsato,
    # Satisfaction with life overall.
    k_jbsat,
    # Job satisfaction (only for employeed people).
    k_sclonely,
    # How often feels lonely.
    k_sclackcom,
    # How often feels lack of companionship.
    k_scleftout,
    # How often feels left out.
    k_scisolate,
    # How often feels isolated from others.
    k_scghqk,
    # GHQ: believe worthless.
    k_scghql,
    # GHQ: general happiness.
    k_smoker,
    # Smoker.
    k_frdrg # Frequency of drug use.
  )

colnames(ind) <-
  c(
    "pidp",
    "hidp",
    "weight",
    "sex0",
    "age0",
    "hiqual0",
    "ethnicity0",
    "marstat0",
    "jbstat0",
    # "jbnssec0",
    "hhtype0",
    "htenure0",
    "pincome",
    "hincome",
    "urban_rural",
    "GHQ1",
    "GHQ2",
    "PCS",
    "MCS",
    "sat_health",
    "sat_income",
    # "sat_house",
    "sat_time",
    "sat_life",
    "sat_job",
    "loneliness",
    "lackcom",
    "leftout",
    "isolated",
    "worthless",
    "happiness",
    "smoker",
    "freq_drug"
  )

ind <- ind %>% arrange(pidp)

head(ind)

# ind_orig <- ind

# Rename sex indicators.
# If none of the cases match, NA is used.
ind <- ind %>% mutate(sex = case_when(sex0 == 1 ~ "M",
                                      sex0 == 2 ~ "F"))

ind <- ind %>% mutate(age = case_when(age0 >= 0 ~ age0))

ind <- ind %>% mutate(age_group = age)

brks <- c(15, 24, 34, 49, 64, 74, Inf)

labs <-
  c("16_24", "25_34", "35_49", "50_64", "65_74", "75_up")

ind$age_group <-
  cut(ind$age_group, breaks = brks, labels = labs) # Categorise age.

ind$sex_age <-
  paste(ind$sex, ind$age_group, sep = "_") # Generate the sex & age category.

ind <-
  ind %>% mutate(
    hiqual = case_when(
      hiqual0 == 1 ~ "level_4_up",
      hiqual0 == 2 ~ "level_4_up",
      hiqual0 == 3 ~ "level_3",
      hiqual0 == 4 ~ "level_1_2",
      hiqual0 == 5 ~ "other",
      hiqual0 == 9 ~ "none"
    )
  )

ind <-
  ind %>% mutate(
    ethnicity = case_when(
      ethnicity0 >= 1 &
        ethnicity0 <= 4 ~ "white",
      ethnicity0 >= 5 &
        ethnicity0 <= 8 ~ "mixed",
      ethnicity0 >= 9 &
        ethnicity0 <= 13 ~ "Asian",
      ethnicity0 >= 14 &
        ethnicity0 <= 16 ~ "black",
      ethnicity0 == 17 |
        ethnicity0 == 97 ~ "other",
    )
  )

ind <-
  ind %>% mutate(
    marstat = case_when(
      marstat0 == 1 ~ "single",
      marstat0 == 2 ~ "married_civil_partner",
      marstat0 == 3 ~ "married_civil_partner",
      # Since 2019, opposite-sex couples have been allowed to have a civil union.
      marstat0 == 4 ~ "separated",
      marstat0 == 5 ~ "divorced",
      marstat0 == 6 ~ "widowed",
      marstat0 == 7 ~ "separated",
      marstat0 == 8 ~ "divorced",
      marstat0 == 9 ~ "widowed"
    )
  )

ind <-
  ind %>% mutate(
    jbstat = case_when(
      jbstat0 == 1 ~ "self_employed",
      jbstat0 == 2 ~ "employee",
      jbstat0 == 3 ~ "unemployed",
      jbstat0 == 4 ~ "retired",
      jbstat0 == 5 ~ "other",
      jbstat0 == 6 ~ "looking_after_home",
      jbstat0 == 7 ~ "student",
      jbstat0 == 8 ~ "long_term_sick",
      jbstat0 == 9 |
        jbstat0 == 10 |
        jbstat0 == 11 |
        jbstat0 == 12 | jbstat0 == 13 | jbstat0 == 97 ~ "other",
    )
  )

ind <-
  ind %>% mutate(
    hhtype = case_when(
      hhtype0 == 1 ~ "one_adu_no_child",
      hhtype0 == 2 ~ "one_adu_no_child",
      hhtype0 == 3 ~ "one_adu_no_child",
      hhtype0 == 4 ~ "one_adu_child",
      hhtype0 == 5 ~ "one_adu_child",
      hhtype0 == 6 ~ "one_couple_no_child",
      hhtype0 == 8 ~ "one_couple_no_child",
      hhtype0 == 10 ~ "one_couple_child",
      hhtype0 == 11 ~ "one_couple_child",
      hhtype0 == 12 ~ "one_couple_child",
      hhtype0 == 16 ~ "others_no_child",
      hhtype0 == 17 ~ "others_no_child",
      hhtype0 == 18 ~ "others_child",
      hhtype0 == 19 ~ "others_no_child",
      hhtype0 == 20 ~ "others_child",
      hhtype0 == 21 ~ "others_child",
      hhtype0 == 22 ~ "others_no_child",
      hhtype0 == 23 ~ "others_child"
    )
  )

ind <-
  ind %>% mutate(
    htenure = case_when(
      htenure0 == 1 ~ "owned_outright",
      htenure0 == 2 ~ "owned_mortgage",
      htenure0 == 3 ~ "social_rented",
      htenure0 == 4 ~ "social_rented",
      htenure0 == 5 ~ "private_rented",
      htenure0 == 6 ~ "private_rented",
      htenure0 == 7 ~ "private_rented",
      htenure0 == 8 ~ "private_rented" # Private_rented or other??? https://www.nomisweb.co.uk/census/2011/lc3409ew
    )
  )

write_csv(ind, "ind.csv")

# Drop individuals with missing socioeconomic characteristics.
ind_drop_na <-
  ind %>% drop_na(sex, age, hiqual, ethnicity, marstat, jbstat, hhtype, htenure)

# For spatial microsimulation.
write_csv(ind_drop_na, "ind_drop_na.csv")

# Constraint: sex & age for England and Wales.

if (MSOA_MODE) {
  sex_age_EW_M <-
    read_excel("data/ONS/SAPE23DT4_SexAge_EW.xlsx", sheet = "Mid-2020 Males_ATI") %>% arrange(`MSOA Code`)
  
  sex_age_EW_F <-
    read_excel("data/ONS/SAPE23DT4_SexAge_EW.xlsx", sheet = "Mid-2020 Females_ATI") %>% arrange(`MSOA Code`)
} else {
  sex_age_EW_M <-
    read_excel("data/ONS/SAPE23DT2_SexAge_EW.xlsx", sheet = "Mid-2020 Males_ATI") %>% arrange(`LSOA Code`)
  
  sex_age_EW_F <-
    read_excel("data/ONS/SAPE23DT2_SexAge_EW.xlsx", sheet = "Mid-2020 Females_ATI") %>% arrange(`LSOA Code`)
}

pop_EW <- sex_age_EW_M$`All Ages` + sex_age_EW_F$`All Ages`

if (!identical(colnames(sex_age_EW_M), colnames(sex_age_EW_F)))
  myStop("The data structures of the two populations are different.")

if (MSOA_MODE) {
  if (!identical(sex_age_EW_M$`MSOA Code`, sex_age_EW_F$`MSOA Code`))
    myStop("The zones of the two populations are different.")
} else {
  if (!identical(sex_age_EW_M$`LSOA Code`, sex_age_EW_F$`LSOA Code`))
    myStop("The zones of the two populations are different.")
}

sex_age_EW <- bind_cols(sex_age_EW_M, sex_age_EW_F[8:98])

age_names <-
  c(
    "M_0_4",
    "M_5_11",
    "M_12_15",
    "M_16_24",
    "M_25_34",
    "M_35_49",
    "M_50_64",
    "M_65_74",
    "M_75_up",
    "F_0_4",
    "F_5_11",
    "F_12_15",
    "F_16_24",
    "F_25_34",
    "F_35_49",
    "F_50_64",
    "F_65_74",
    "F_75_up"
  )

sex_age_EW <- sex_age_EW %>% select(-(2:7))

table_sex_age <-
  matrix(nrow = nrow(sex_age_EW), ncol = length(age_names)) %>% as_tibble()

colnames(table_sex_age) <- age_names

table_sex_age[1] <- rowSums(sex_age_EW[2:6])

table_sex_age[2] <- rowSums(sex_age_EW[7:13])

table_sex_age[3] <- rowSums(sex_age_EW[14:17])

table_sex_age[4] <- rowSums(sex_age_EW[18:26])

table_sex_age[5] <- rowSums(sex_age_EW[27:36])

table_sex_age[6] <- rowSums(sex_age_EW[37:51])

table_sex_age[7] <- rowSums(sex_age_EW[52:66])

table_sex_age[8] <- rowSums(sex_age_EW[67:76])

table_sex_age[9] <- rowSums(sex_age_EW[77:92])

table_sex_age[10] <- rowSums(sex_age_EW[93:97])

table_sex_age[11] <- rowSums(sex_age_EW[98:104])

table_sex_age[12] <- rowSums(sex_age_EW[105:108])

table_sex_age[13] <- rowSums(sex_age_EW[109:117])

table_sex_age[14] <- rowSums(sex_age_EW[118:127])

table_sex_age[15] <- rowSums(sex_age_EW[128:142])

table_sex_age[16] <- rowSums(sex_age_EW[143:157])

table_sex_age[17] <- rowSums(sex_age_EW[158:167])

table_sex_age[18] <- rowSums(sex_age_EW[168:183])

# View(table_sex_age)

zones_EW <- sex_age_EW[1]

colnames(zones_EW) <- ZONE_ID

if (is.unsorted(zones_EW$ZoneID))
  myStop("The zones of England and Wales are not sorted.")

cons_sex_age_EW <- bind_cols(zones_EW, table_sex_age)

cons_sex_age_EW <- cons_sex_age_EW %>% arrange(ZoneID)

# View(cons_sex_age_EW)

if (!identical(pop_EW, rowSums(cons_sex_age_EW[-1])))
  myStop("The total population is incorrect.")

if (!identical(zones_EW$ZoneID, cons_sex_age_EW$ZoneID))
  myStop("Zones are inconsistent.")

if (MSOA_MODE) {
  write_csv(cons_sex_age_EW, "cons_sex_age_EW_MSOA.csv")
} else {
  write_csv(cons_sex_age_EW, "cons_sex_age_EW_LSOA.csv")
}

cons_sex_age_adult_EW <-
  cons_sex_age_EW[c(1, 5:10, 14:19)] # Select the adult population for England and Wales.

if (MSOA_MODE) {
  write_csv(cons_sex_age_adult_EW, "cons_sex_age_adult_EW_MSOA.csv")
} else{
  write_csv(cons_sex_age_adult_EW, "cons_sex_age_adult_EW_LSOA.csv")
}

cons_sex_age_child_EW <-
  cons_sex_age_EW[c(1, 2:4, 11:13)] # Select the child population for England and Wales.

if (MSOA_MODE) {
  write_csv(cons_sex_age_child_EW, "cons_sex_age_child_EW_MSOA.csv")
} else {
  write_csv(cons_sex_age_child_EW, "cons_sex_age_child_EW_LSOA.csv")
}

# Constraint: highest qualification for England and Wales.

if (MSOA_MODE) {
  cons_hiqual_EW <-
    read_csv("data/Census_2011/QS501EW_Highest_Qualification_EW_MSOA.csv") %>% arrange(`geography code`)
} else {
  cons_hiqual_EW <-
    read_csv("data/Census_2011/QS501EW_Highest_Qualification_EW.csv") %>% arrange(`geography code`)
}

pop_hiqual_EW <-
  cons_hiqual_EW$`Qualification: All categories: Highest level of qualification; measures: Value`

cons_hiqual_EW <- cons_hiqual_EW %>% select(3, 6:12)

colnames(cons_hiqual_EW)[1] <- ZONE_ID

colnames(cons_hiqual_EW)[2] <- "none"

cons_hiqual_EW$level_1_2 <- rowSums(cons_hiqual_EW[3:4])

cons_hiqual_EW$other <- rowSums(cons_hiqual_EW[c(5, 8)])

colnames(cons_hiqual_EW)[6] <- "level_3"

colnames(cons_hiqual_EW)[7] <- "level_4_up"

cons_hiqual_EW <- cons_hiqual_EW %>% select(1, 2, 9, 6, 7, 10)

cons_hiqual_EW <- cons_hiqual_EW %>% arrange(ZoneID)

if (!identical(pop_hiqual_EW, rowSums(cons_hiqual_EW[-1])))
  myStop("The total population (hiqual) is incorrect.")

if (!identical(zones_EW$ZoneID, cons_hiqual_EW$ZoneID))
  myStop("Zones are inconsistent.")

# Constraint: ethnicity for England and Wales.

if (MSOA_MODE) {
  cons_ethnicity_EW_orig <-
    read_csv("data/Census_2011/LC2109EWLS_Ethnicity_EW_MSOA.csv") %>% arrange(`geography code`)
} else {
  cons_ethnicity_EW_orig <-
    read_csv("data/Census_2011/LC2109EWLS_Ethnicity_EW.csv") %>% arrange(`geography code`)
}

pop_ethnicity_EW <-
  cons_ethnicity_EW_orig$`Age: All categories: Age; Ethnic Group: All categories: Ethnic group; measures: Value`

pop_ethnicity_child_EW <-
  rowSums(cons_ethnicity_EW_orig[c(28, 52, 76, 100, 124)])

cons_ethnicity_EW <-
  cons_ethnicity_EW_orig %>% select(3, 5, 10, 15, 21, 25)

# colnames(cons_ethnicity_EW)

colnames(cons_ethnicity_EW) <-
  c(ZONE_ID, "white", "mixed", "Asian", "black", "other")

# Disregard children's ethnicity.
cons_ethnicity_EW[2] <-
  cons_ethnicity_EW[2] - rowSums(cons_ethnicity_EW_orig[c(29, 53, 77, 101, 125)])
cons_ethnicity_EW[3] <-
  cons_ethnicity_EW[3] - rowSums(cons_ethnicity_EW_orig[c(34, 58, 82, 106, 130)])
cons_ethnicity_EW[4] <-
  cons_ethnicity_EW[4] - rowSums(cons_ethnicity_EW_orig[c(39, 63, 87, 111, 135)])
cons_ethnicity_EW[5] <-
  cons_ethnicity_EW[5] - rowSums(cons_ethnicity_EW_orig[c(45, 69, 93, 117, 141)])
cons_ethnicity_EW[6] <-
  cons_ethnicity_EW[6] - rowSums(cons_ethnicity_EW_orig[c(49, 73, 97, 121, 145)])

cons_ethnicity_EW <- cons_ethnicity_EW %>% arrange(ZoneID)

if (!identical(pop_ethnicity_EW,
               rowSums(cons_ethnicity_EW[-1]) + pop_ethnicity_child_EW))
  myStop("The total population (ethnicity) is incorrect.")

if (!identical(zones_EW$ZoneID, cons_ethnicity_EW$ZoneID))
  myStop("Zones are inconsistent.")

# Constraint: marital status for England and Wales.

if (MSOA_MODE) {
  cons_marstat_EW <-
    read_csv("data/Census_2011/KS103EW_Marital_Status_EW_MSOA.csv") %>% arrange(`geography code`)
} else {
  cons_marstat_EW <-
    read_csv("data/Census_2011/KS103EW_Marital_Status_EW.csv") %>% arrange(`geography code`)
}

pop_marstat_EW <-
  cons_marstat_EW$`Marital Status: All usual residents aged 16+; measures: Value`

cons_marstat_EW$`Marital Status: Married; measures: Value` <-
  cons_marstat_EW$`Marital Status: Married; measures: Value` + cons_marstat_EW$`Marital Status: In a registered same-sex civil partnership; measures: Value`

cons_marstat_EW <- cons_marstat_EW %>% select(-c(1, 2, 4, 5, 8))

colnames(cons_marstat_EW) <-
  c(ZONE_ID,
    "single",
    "married_civil_partner",
    "separated",
    "divorced",
    "widowed")

cons_marstat_EW <- cons_marstat_EW %>% arrange(ZoneID)

if (!identical(pop_marstat_EW, rowSums(cons_marstat_EW[-1])))
  myStop("The total population (marstat) is incorrect.")

if (!identical(zones_EW$ZoneID, cons_marstat_EW$ZoneID))
  myStop("Zones are inconsistent.")

# Constraint: job status for England and Wales.

if (MSOA_MODE) {
  cons_jbstat_EW <-
    read_csv("data/Census_2011/QS601EW_Job_Status_EW_MSOA.csv") %>% arrange(`geography code`)
} else {
  cons_jbstat_EW <-
    read_csv("data/Census_2011/QS601EW_Job_Status_EW.csv") %>% arrange(`geography code`)
}

pop_jbstat_EW <-
  cons_jbstat_EW$`Economic Activity: All categories: Economic activity; measures: Value`

cons_jbstat_EW <-
  cons_jbstat_EW %>% select(3, 7:8, 9:12, 13, 14, 17, 16, 18, 19, 20)

colnames(cons_jbstat_EW)[1] <- ZONE_ID

cons_jbstat_EW$employee <- rowSums(cons_jbstat_EW[2:3])

cons_jbstat_EW$self_employed <- rowSums(cons_jbstat_EW[4:7])

cons_jbstat_EW$unemployed <- rowSums(cons_jbstat_EW[8])

cons_jbstat_EW$student <- rowSums(cons_jbstat_EW[c(9, 10)])

cons_jbstat_EW$retired <- rowSums(cons_jbstat_EW[11])

cons_jbstat_EW$looking_after_home <- rowSums(cons_jbstat_EW[12])

cons_jbstat_EW$long_term_sick <- rowSums(cons_jbstat_EW[13])

cons_jbstat_EW$other <- rowSums(cons_jbstat_EW[14])

cons_jbstat_EW <-
  cons_jbstat_EW %>% select(1, 15:22) # Some column names are strange if no rowSums().

cons_jbstat_EW <- cons_jbstat_EW %>% arrange(ZoneID)

if (!identical(pop_jbstat_EW, rowSums(cons_jbstat_EW[-1])))
  myStop("The total population (jbstat) is incorrect.")

if (!identical(zones_EW$ZoneID, cons_jbstat_EW$ZoneID))
  myStop("Zones are inconsistent.")

# Constraint: household composition for England and Wales.

if (MSOA_MODE) {
  cons_hhtype_EW <-
    read_csv("data/Census_2011/LC1109EW_Household_Composition_EW_MSOA.csv") %>% arrange(`geography code`)
} else {
  cons_hhtype_EW <-
    read_csv("data/Census_2011/LC1109EW_Household_Composition_EW.csv") %>% arrange(`geography code`)
}

pop_hhtype_EW <-
  cons_hhtype_EW$`Sex: All persons; Age: All categories: Age; Household Composition: All categories: Household composition; measures: Value`

colnames(cons_hhtype_EW)[3] <- ZONE_ID

cons_hhtype_EW$one_adu_no_child <- rowSums(cons_hhtype_EW[5])

cons_hhtype_EW$one_adu_child <- rowSums(cons_hhtype_EW[18])

cons_hhtype_EW$one_couple_no_child <-
  rowSums(cons_hhtype_EW[c(9, 11, 15)])

cons_hhtype_EW$one_couple_child <-
  rowSums(cons_hhtype_EW[c(12, 13, 16, 17)])

cons_hhtype_EW$others_no_child <- rowSums(cons_hhtype_EW[23])

cons_hhtype_EW$others_child <- rowSums(cons_hhtype_EW[22])

# Row sums of children's household composition. (adult column number + 20)
cons_hhtype_EW$one_couple_no_child_for_children <-
  rowSums(cons_hhtype_EW[c(29, 31, 35)]) # Column 35 refers to very young couples.

cons_hhtype_EW$one_couple_child_for_children <-
  rowSums(cons_hhtype_EW[c(32, 33, 36, 37)])

# Disregard children's household composition.
cons_hhtype_child_EW <-
  cons_hhtype_EW[c(25, 38, 370, 371, 43, 42)]

cons_hhtype_child_all_EW <-
  cons_hhtype_EW[24]

if (!identical(rowSums(cons_hhtype_child_all_EW),
               rowSums(cons_hhtype_child_EW)))
  myStop("The children populations are inconsistent.")

cons_hhtype_EW[364:369] <-
  cons_hhtype_EW[364:369] - cons_hhtype_child_EW

cons_hhtype_EW <-
  cons_hhtype_EW %>% select(3, 364:369) # Some column names are strange if no rowSums().

cons_hhtype_EW <- cons_hhtype_EW %>% arrange(ZoneID)

if (!identical(pop_hhtype_EW,
               rowSums(cons_hhtype_EW[-1] + cons_hhtype_child_EW)))
  myStop("The total population (hhtype) is incorrect.")

if (!identical(zones_EW$ZoneID, cons_hhtype_EW$ZoneID))
  myStop("Zones are inconsistent.")

# Constraint: housing tenure for England and Wales.

if (MSOA_MODE) {
  cons_htenure_EW <-
    read_csv("data/Census_2011/LC3409EW_Housing_Tenure_EW_MSOA.csv") %>% arrange(`geography code`)
} else {
  cons_htenure_EW <-
    read_csv("data/Census_2011/LC3409EW_Housing_Tenure_EW.csv") %>% arrange(`geography code`)
}

pop_htenure_EW <-
  cons_htenure_EW$`Tenure: All categories: Tenure; Age: All categories: Age; General Health: All categories: General health; measures: Value`

cons_htenure_child_EW <-
  cons_htenure_EW %>% select(48, 68, 108, 128)

cons_htenure_child_all_EW <-
  cons_htenure_EW[8]

if (!identical(rowSums(cons_htenure_child_all_EW),
               rowSums(cons_htenure_child_EW)))
  myStop("The children populations are inconsistent.")

# Owned or shared ownership = owned outright + owned with a mortgage or loan or shared ownership.
cons_htenure_EW <-
  cons_htenure_EW %>% select(3, 44, 64, 104, 124)

colnames(cons_htenure_EW) <-
  c(ZONE_ID,
    "owned_outright",
    "owned_mortgage",
    "social_rented",
    "private_rented")

# Disregard children's housing tenure.
cons_htenure_EW[c(2, 3, 4, 5)] <-
  cons_htenure_EW[c(2, 3, 4, 5)] - cons_htenure_child_EW

cons_htenure_EW <- cons_htenure_EW %>% arrange(ZoneID)

if (!identical(pop_htenure_EW,
               rowSums(cons_htenure_EW[-1] + cons_htenure_child_EW)))
  myStop("The total population (htenure) is incorrect.")

if (!identical(zones_EW$ZoneID, cons_htenure_EW$ZoneID))
  myStop("Zones are inconsistent.")

# Scotland.

if (MSOA_MODE) {
  zone_lookup_S <-
    read_csv("data/Scotland/DataZone2011lookup_2022-05-31.csv") %>% arrange(DZ2011_Code, IZ2011_Code) %>% select(DZ2011_Code, IZ2011_Code)
  
  iz <-
    zone_lookup_S %>% select(IZ2011_Code) %>% unique() %>% arrange(IZ2011_Code)
}

# Constraint: sex & age for Scotland.
sex_age_S_M <-
  read_excel("data/NRS/sape-2020-males.xlsx", sheet = "Table 1b Males (2021)_ATI") %>% arrange(`Area code`)

sex_age_S_F <-
  read_excel("data/NRS/sape-2020-females.xlsx", sheet = "Table 1c Females (2021)_ATI") %>% arrange(`Area code`)

pop_S <- sex_age_S_M$`All ages` + sex_age_S_F$`All ages`

if (!identical(colnames(sex_age_S_M), colnames(sex_age_S_F)))
  myStop("The data structures of the two populations are different.")

if (!identical(sex_age_S_M$`Area code`, sex_age_S_F$`Area code`))
  myStop("The zones of the two populations are different.")

sex_age_S <- bind_cols(sex_age_S_M, sex_age_S_F[5:95])

sex_age_S <- sex_age_S %>% select(-(2:4))

table_sex_age <-
  matrix(nrow = nrow(sex_age_S), ncol = length(age_names)) %>% as_tibble()

colnames(table_sex_age) <- age_names

table_sex_age[1] <- rowSums(sex_age_S[2:6])

table_sex_age[2] <- rowSums(sex_age_S[7:13])

table_sex_age[3] <- rowSums(sex_age_S[14:17])

table_sex_age[4] <- rowSums(sex_age_S[18:26])

table_sex_age[5] <- rowSums(sex_age_S[27:36])

table_sex_age[6] <- rowSums(sex_age_S[37:51])

table_sex_age[7] <- rowSums(sex_age_S[52:66])

table_sex_age[8] <- rowSums(sex_age_S[67:76])

table_sex_age[9] <- rowSums(sex_age_S[77:92])

table_sex_age[10] <- rowSums(sex_age_S[93:97])

table_sex_age[11] <- rowSums(sex_age_S[98:104])

table_sex_age[12] <- rowSums(sex_age_S[105:108])

table_sex_age[13] <- rowSums(sex_age_S[109:117])

table_sex_age[14] <- rowSums(sex_age_S[118:127])

table_sex_age[15] <- rowSums(sex_age_S[128:142])

table_sex_age[16] <- rowSums(sex_age_S[143:157])

table_sex_age[17] <- rowSums(sex_age_S[158:167])

table_sex_age[18] <- rowSums(sex_age_S[168:183])

# View(table_sex_age)

data_zones <- sex_age_S[1]

colnames(data_zones) <- ZONE_ID

if (is.unsorted(data_zones$ZoneID))
  myStop("The data zones of Scotland are not sorted.")

cons_sex_age_S <- bind_cols(data_zones, table_sex_age)

cons_sex_age_S <- cons_sex_age_S %>% arrange(ZoneID)

# View(cons_sex_age_S)

if (!identical(pop_S, rowSums(cons_sex_age_S[-1])))
  myStop("The total population is incorrect.")

if (!identical(data_zones$ZoneID, cons_sex_age_S$ZoneID))
  myStop("Data zones are inconsistent.")

if (MSOA_MODE) {
  cons_sex_age_S <-
    cons_sex_age_S %>% inner_join(zone_lookup_S, by = c("ZoneID" = "DZ2011_Code"))
  
  cons_sex_age_S <-
    aggregate(
      cbind(
        M_0_4,
        M_5_11,
        M_12_15,
        M_16_24,
        M_25_34,
        M_35_49,
        M_50_64,
        M_65_74,
        M_75_up,
        F_0_4,
        F_5_11,
        F_12_15,
        F_16_24,
        F_25_34,
        F_35_49,
        F_50_64,
        F_65_74,
        F_75_up
      ) ~ IZ2011_Code,
      data = cons_sex_age_S,
      FUN = sum,
      na.rm = F
    )
  
  colnames(cons_sex_age_S)[1] <- ZONE_ID
  
  cons_sex_age_S <- cons_sex_age_S %>% arrange(ZoneID)
  
  if (!identical(iz$IZ2011_Code, cons_sex_age_S$ZoneID))
    myStop("Intermediate Zones are inconsistent.")
}

if (MSOA_MODE) {
  write_csv(cons_sex_age_S, "cons_sex_age_S_MSOA.csv")
} else {
  write_csv(cons_sex_age_S, "cons_sex_age_S_LSOA.csv")
}

cons_sex_age_adult_S <-
  cons_sex_age_S[c(1, 5:10, 14:19)] # Select the adult population for Scotland.

if (MSOA_MODE) {
  write_csv(cons_sex_age_adult_S, "cons_sex_age_adult_S_MSOA.csv")
} else {
  write_csv(cons_sex_age_adult_S, "cons_sex_age_adult_S_LSOA.csv")
}

cons_sex_age_child_S <-
  cons_sex_age_S[c(1, 2:4, 11:13)] # Select the child population for Scotland.

if (MSOA_MODE) {
  write_csv(cons_sex_age_child_S, "cons_sex_age_child_S_MSOA.csv")
} else {
  write_csv(cons_sex_age_child_S, "cons_sex_age_child_S_LSOA.csv")
}

# Constraint: highest qualification for Scotland.
cons_hiqual_S <-
  read_csv("data/Scotland/SNS Data Zone 2011 blk/QS501SC.csv") # No the "other" group!!!

colnames(cons_hiqual_S)[1] <- ZONE_ID

cons_hiqual_S <- cons_hiqual_S %>% slice(-(1:2)) %>% arrange(ZoneID)

pop_hiqual_S <- cons_hiqual_S$`All people aged 16 and over`

cons_hiqual_S <- cons_hiqual_S %>% select(-2)

colnames(cons_hiqual_S)[2] <- "none"

cons_hiqual_S$level_1_2 <- rowSums(cons_hiqual_S[3:4])

colnames(cons_hiqual_S)[5] <- "level_3"

colnames(cons_hiqual_S)[6] <- "level_4_up"

cons_hiqual_S <- cons_hiqual_S %>% select(1, 2, 7, 5, 6)

# Add a new column!!!
cons_hiqual_S$other <- 0

cons_hiqual_S <- cons_hiqual_S %>% arrange(ZoneID)

if (!identical(pop_hiqual_S,
               rowSums(cons_hiqual_S[-1])))
  myStop("The total population (hiqual) is incorrect.")

if (!identical(data_zones$ZoneID, cons_hiqual_S$ZoneID))
  myStop("Data zones are inconsistent.")

if (MSOA_MODE) {
  cons_hiqual_S <-
    cons_hiqual_S %>% inner_join(zone_lookup_S, by = c("ZoneID" = "DZ2011_Code"))
  
  cons_hiqual_S <-
    aggregate(
      cbind(none,
            level_1_2,
            level_3,
            level_4_up,
            other) ~ IZ2011_Code,
      data = cons_hiqual_S,
      FUN = sum,
      na.rm = F
    )
  
  colnames(cons_hiqual_S)[1] <- ZONE_ID
  
  cons_hiqual_S <- cons_hiqual_S %>% arrange(ZoneID)
  
  if (!identical(iz$IZ2011_Code, cons_hiqual_S$ZoneID))
    myStop("Intermediate Zones are inconsistent.")
}

# Constraint: ethnicity for Scotland.
cons_ethnicity_S <-
  read_csv("data/Scotland/SNS Data Zone 2011 blk/LC2101SC.csv")

colnames(cons_ethnicity_S)[1] <- ZONE_ID

colnames(cons_ethnicity_S)[2] <- "age"

colnames(cons_ethnicity_S)[3] <- "all"

cons_ethnicity_S <-
  cons_ethnicity_S %>% filter((age == "All people") |
                                (age == "0 to 15")) %>% slice(-(1:2)) %>% arrange(ZoneID)
cons_ethnicity_S <-
  cons_ethnicity_S %>% select(1, 2, 3, 4, 11, 12, 18, 21, 25)

cons_ethnicity_S$black <-
  cons_ethnicity_S$`African: Total` + cons_ethnicity_S$`Caribbean or Black: Total`

cons_ethnicity_S <-
  cons_ethnicity_S %>% select(1, 2, 3, 4, 5, 6, 10, 9)

colnames(cons_ethnicity_S) <-
  c(ZONE_ID,
    "age",
    "all",
    "white",
    "mixed",
    "Asian",
    "black",
    "other")

# Disregard children's ethnicity.
for (i in 1:nrow(cons_ethnicity_S)) {
  # print(i)
  
  if (i %% 2 == 1) {
    if ((cons_ethnicity_S[i, 2] != "All people") |
        (cons_ethnicity_S[i + 1, 2] != "0 to 15"))
      myStop("cons_ethnicity_S is incorrect.")
    
    cons_ethnicity_S[i, 3:8] <-
      cons_ethnicity_S[i, 3:8] - cons_ethnicity_S[i + 1, 3:8]
  }
}

# Only adults.
pop_ethnicity_S <-
  cons_ethnicity_S %>% filter(age == "All people") %>% select(all) %>% unlist() %>% as.numeric()

cons_ethnicity_S <-
  cons_ethnicity_S %>% filter(age == "All people") %>% select(-age) %>% select(-all) %>% arrange(ZoneID)

if (!identical(pop_ethnicity_S,
               rowSums(cons_ethnicity_S[-1])))
  myStop("The total population (ethnicity) is incorrect.")

if (!identical(data_zones$ZoneID, cons_ethnicity_S$ZoneID))
  myStop("Data zones are inconsistent.")

if (MSOA_MODE) {
  cons_ethnicity_S <-
    cons_ethnicity_S %>% inner_join(zone_lookup_S, by = c("ZoneID" = "DZ2011_Code"))
  
  cons_ethnicity_S <-
    aggregate(
      cbind(white,
            mixed,
            Asian,
            black,
            other) ~ IZ2011_Code,
      data = cons_ethnicity_S,
      FUN = sum,
      na.rm = F
    )
  
  colnames(cons_ethnicity_S)[1] <- ZONE_ID
  
  cons_ethnicity_S <- cons_ethnicity_S %>% arrange(ZoneID)
  
  if (!identical(iz$IZ2011_Code, cons_ethnicity_S$ZoneID))
    myStop("Intermediate Zones are inconsistent.")
}

# Constraint: marital status for Scotland.
cons_marstat_S <-
  read_csv("data/Scotland/SNS Data Zone 2011 blk/KS103SC.csv")

colnames(cons_marstat_S)[1] <- ZONE_ID

cons_marstat_S <- cons_marstat_S %>% slice(-1) %>% arrange(ZoneID)

pop_marstat_S <- cons_marstat_S$`All people aged 16 and over`

cons_marstat_S$Married <-
  cons_marstat_S$Married + cons_marstat_S$`In a registered same-sex civil partnership`

cons_marstat_S <- cons_marstat_S %>% select(-c(2, 5))

colnames(cons_marstat_S) <-
  c(ZONE_ID,
    "single",
    "married_civil_partner",
    "separated",
    "divorced",
    "widowed")

cons_marstat_S <- cons_marstat_S %>% arrange(ZoneID)

if (!identical(pop_marstat_S,
               rowSums(cons_marstat_S[-1])))
  myStop("The total population (marstat) is incorrect.")

if (!identical(data_zones$ZoneID, cons_marstat_S$ZoneID))
  myStop("Data zones are inconsistent.")

if (MSOA_MODE) {
  cons_marstat_S <-
    cons_marstat_S %>% inner_join(zone_lookup_S, by = c("ZoneID" = "DZ2011_Code"))
  
  cons_marstat_S <-
    aggregate(
      cbind(single,
            married_civil_partner,
            separated,
            divorced,
            widowed) ~ IZ2011_Code,
      data = cons_marstat_S,
      FUN = sum,
      na.rm = F
    )
  
  colnames(cons_marstat_S)[1] <- ZONE_ID
  
  cons_marstat_S <- cons_marstat_S %>% arrange(ZoneID)
  
  if (!identical(iz$IZ2011_Code, cons_marstat_S$ZoneID))
    myStop("Intermediate Zones are inconsistent.")
}

# Constraint: job status for Scotland.
cons_jbstat_S <-
  read_csv("data/Scotland/SNS Data Zone 2011 blk/QS601SC.csv")

colnames(cons_jbstat_S)[1] <- ZONE_ID

cons_jbstat_S <- cons_jbstat_S %>% slice(-1) %>% arrange(ZoneID)

pop_jbstat_S <- cons_jbstat_S$`All people aged 16 to 74`

cons_jbstat_S <- cons_jbstat_S %>% select(-2)

cons_jbstat_S$employee <- rowSums(cons_jbstat_S[3:4])

cons_jbstat_S$self_employed <- rowSums(cons_jbstat_S[5:8])

cons_jbstat_S$unemployed <- rowSums(cons_jbstat_S[9])

cons_jbstat_S$student <- rowSums(cons_jbstat_S[c(10, 13)])

cons_jbstat_S$retired <- rowSums(cons_jbstat_S[12])

cons_jbstat_S$looking_after_home <- rowSums(cons_jbstat_S[14])

cons_jbstat_S$long_term_sick <- rowSums(cons_jbstat_S[15])

cons_jbstat_S$other <- rowSums(cons_jbstat_S[16])

cons_jbstat_S <-
  cons_jbstat_S %>% select(1, 17:24) # Some column names are strange if no rowSums().

cons_jbstat_S <- cons_jbstat_S %>% arrange(ZoneID)

if (!identical(pop_jbstat_S,
               rowSums(cons_jbstat_S[-1])))
  myStop("The total population (jbstat) is incorrect.")

if (!identical(data_zones$ZoneID, cons_jbstat_S$ZoneID))
  myStop("Data zones are inconsistent.")

if (MSOA_MODE) {
  cons_jbstat_S <-
    cons_jbstat_S %>% inner_join(zone_lookup_S, by = c("ZoneID" = "DZ2011_Code"))
  
  cons_jbstat_S <-
    aggregate(
      cbind(
        employee,
        self_employed,
        unemployed,
        student,
        retired,
        looking_after_home,
        long_term_sick,
        other
      ) ~ IZ2011_Code,
      data = cons_jbstat_S,
      FUN = sum,
      na.rm = F
    )
  
  colnames(cons_jbstat_S)[1] <- ZONE_ID
  
  cons_jbstat_S <- cons_jbstat_S %>% arrange(ZoneID)
  
  if (!identical(iz$IZ2011_Code, cons_jbstat_S$ZoneID))
    myStop("Intermediate Zones are inconsistent.")
}

# Constraint: household composition for Scotland.
cons_hhtype_S <-
  read_csv("data/Scotland/SNS Data Zone 2011 blk/LC1109SC.csv")

colnames(cons_hhtype_S)[1] <- ZONE_ID

colnames(cons_hhtype_S)[2] <- "age"

colnames(cons_hhtype_S)[3] <- "all"

cons_hhtype_S <-
  cons_hhtype_S %>% filter((age == "All people in households") |
                             (age == "0 to 15")) %>% slice(-(1:2)) %>% arrange(ZoneID)

cons_hhtype_S$one_adu_no_child <- rowSums(cons_hhtype_S[4])

cons_hhtype_S$one_adu_child <- rowSums(cons_hhtype_S[18])

cons_hhtype_S$one_couple_no_child <-
  rowSums(cons_hhtype_S[c(8, 10, 13, 15)])

cons_hhtype_S$one_couple_child <-
  rowSums(cons_hhtype_S[c(11, 12, 16, 17)])

cons_hhtype_S$others_no_child <- rowSums(cons_hhtype_S[23:25])

cons_hhtype_S$others_child <- rowSums(cons_hhtype_S[22])

cons_hhtype_S <-
  cons_hhtype_S %>% select(1, 2, 3, 26:31) %>% arrange(ZoneID)

# Disregard children's household composition.
for (i in 1:nrow(cons_hhtype_S)) {
  # print(i)
  
  if (i %% 2 == 1) {
    if ((cons_hhtype_S[i, 2] != "All people in households") |
        (cons_hhtype_S[i + 1, 2] != "0 to 15"))
      myStop("cons_hhtype_S is incorrect.")
    
    cons_hhtype_S[i, 3:9] <-
      cons_hhtype_S[i, 3:9] - cons_hhtype_S[i + 1, 3:9]
  }
}

pop_hhtype_S <-
  cons_hhtype_S %>% filter(age == "All people in households") %>% select(all) %>% unlist() %>% as.numeric()

cons_hhtype_S <-
  cons_hhtype_S %>% filter(age == "All people in households") %>% select(-age) %>% select(-all) %>% arrange(ZoneID)

if (!identical(pop_hhtype_S,
               rowSums(cons_hhtype_S[-1])))
  myStop("The total population (hhtype) is incorrect.")

if (!identical(data_zones$ZoneID, cons_hhtype_S$ZoneID))
  myStop("Data zones are inconsistent.")

if (MSOA_MODE) {
  cons_hhtype_S <-
    cons_hhtype_S %>% inner_join(zone_lookup_S, by = c("ZoneID" = "DZ2011_Code"))
  
  cons_hhtype_S <-
    aggregate(
      cbind(
        one_adu_no_child,
        one_adu_child,
        one_couple_no_child,
        one_couple_child,
        others_no_child,
        others_child
      ) ~ IZ2011_Code,
      data = cons_hhtype_S,
      FUN = sum,
      na.rm = F
    )
  
  colnames(cons_hhtype_S)[1] <- ZONE_ID
  
  cons_hhtype_S <- cons_hhtype_S %>% arrange(ZoneID)
  
  if (!identical(iz$IZ2011_Code, cons_hhtype_S$ZoneID))
    myStop("Intermediate Zones are inconsistent.")
}

# Constraint: housing tenure for Scotland.
cons_htenure_S <-
  read_csv("data/Scotland/SNS Data Zone 2011 blk/QS403SC.csv")

colnames(cons_htenure_S)[1] <- ZONE_ID

colnames(cons_htenure_S)[2] <- "all"

cons_htenure_S <- cons_htenure_S %>% slice(-1) %>% arrange(ZoneID)

pop_htenure_S <- cons_htenure_S$all

cons_htenure_S <-
  cons_htenure_S %>% select(1, 4, 5, 6, 7, 10, 15)

colnames(cons_htenure_S) <-
  c(
    ZONE_ID,
    "owned_outright",
    "owned_mortgage_1",
    "owned_mortgage_2",
    "social_rented",
    "private_rented_1",
    "private_rented_2"
  )

cons_htenure_S$owned_mortgage <-
  cons_htenure_S$owned_mortgage_1 + cons_htenure_S$owned_mortgage_2

cons_htenure_S$private_rented <-
  cons_htenure_S$private_rented_1 + cons_htenure_S$private_rented_2

# Disregard children's housing tenure. (omitted)
# QS403SC considers all people.

cons_htenure_S <-
  cons_htenure_S %>% select(1, 2, 8, 5, 9) %>% arrange(ZoneID)

if (!identical(pop_htenure_S,
               rowSums(cons_htenure_S[-1])))
  myStop("The total population (htenure) is incorrect.")

if (!identical(data_zones$ZoneID, cons_htenure_S$ZoneID))
  myStop("Data zones are inconsistent.")

if (MSOA_MODE) {
  cons_htenure_S <-
    cons_htenure_S %>% inner_join(zone_lookup_S, by = c("ZoneID" = "DZ2011_Code"))
  
  cons_htenure_S <-
    aggregate(
      cbind(
        owned_outright,
        owned_mortgage,
        social_rented,
        private_rented
      ) ~ IZ2011_Code,
      data = cons_htenure_S,
      FUN = sum,
      na.rm = F
    )
  
  colnames(cons_htenure_S)[1] <- ZONE_ID
  
  cons_htenure_S <- cons_htenure_S %>% arrange(ZoneID)
  
  if (!identical(iz$IZ2011_Code, cons_htenure_S$ZoneID))
    myStop("Intermediate Zones are inconsistent.")
}

# Check if data structures are the same.
identical(colnames(cons_sex_age_EW), colnames(cons_sex_age_S))

identical(colnames(cons_sex_age_adult_EW),
          colnames(cons_sex_age_adult_S))

identical(colnames(cons_sex_age_child_EW),
          colnames(cons_sex_age_child_S))

identical(colnames(cons_hiqual_EW), colnames(cons_hiqual_S))

identical(colnames(cons_ethnicity_EW), colnames(cons_ethnicity_S))

identical(colnames(cons_marstat_EW), colnames(cons_marstat_S))

identical(colnames(cons_jbstat_EW), colnames(cons_jbstat_S))

identical(colnames(cons_hhtype_EW), colnames(cons_hhtype_S))

identical(colnames(cons_htenure_EW), colnames(cons_htenure_S))

identical(cons_sex_age_EW$ZoneID, zones_EW$ZoneID)
identical(cons_sex_age_adult_EW$ZoneID, zones_EW$ZoneID)
identical(cons_sex_age_child_EW$ZoneID, zones_EW$ZoneID)
identical(cons_hiqual_EW$ZoneID, zones_EW$ZoneID)
identical(cons_ethnicity_EW$ZoneID, zones_EW$ZoneID)
identical(cons_marstat_EW$ZoneID, zones_EW$ZoneID)
identical(cons_jbstat_EW$ZoneID, zones_EW$ZoneID)
identical(cons_hhtype_EW$ZoneID, zones_EW$ZoneID)
identical(cons_htenure_EW$ZoneID, zones_EW$ZoneID)

if (MSOA_MODE) {
  identical(cons_sex_age_S$ZoneID, iz$IZ2011_Code)
  identical(cons_sex_age_adult_S$ZoneID, iz$IZ2011_Code)
  identical(cons_sex_age_child_S$ZoneID, iz$IZ2011_Code)
  identical(cons_hiqual_S$ZoneID, iz$IZ2011_Code)
  identical(cons_ethnicity_S$ZoneID, iz$IZ2011_Code)
  identical(cons_marstat_S$ZoneID, iz$IZ2011_Code)
  identical(cons_jbstat_S$ZoneID, iz$IZ2011_Code)
  identical(cons_hhtype_S$ZoneID, iz$IZ2011_Code)
  identical(cons_htenure_S$ZoneID, iz$IZ2011_Code)
} else {
  identical(cons_sex_age_S$ZoneID, data_zones$ZoneID)
  identical(cons_sex_age_adult_S$ZoneID, data_zones$ZoneID)
  identical(cons_sex_age_child_S$ZoneID, data_zones$ZoneID)
  identical(cons_hiqual_S$ZoneID, data_zones$ZoneID)
  identical(cons_ethnicity_S$ZoneID, data_zones$ZoneID)
  identical(cons_marstat_S$ZoneID, data_zones$ZoneID)
  identical(cons_jbstat_S$ZoneID, data_zones$ZoneID)
  identical(cons_hhtype_S$ZoneID, data_zones$ZoneID)
  identical(cons_htenure_S$ZoneID, data_zones$ZoneID)
}

# Combine the constraints of adults.
cons_sex_age <-
  bind_rows(cons_sex_age_adult_EW, cons_sex_age_adult_S)

cons_hiqual <- bind_rows(cons_hiqual_EW, cons_hiqual_S)

cons_ethnicity <- bind_rows(cons_ethnicity_EW, cons_ethnicity_S)

cons_marstat <- bind_rows(cons_marstat_EW, cons_marstat_S)

cons_jbstat <- bind_rows(cons_jbstat_EW, cons_jbstat_S)

cons_hhtype <- bind_rows(cons_hhtype_EW, cons_hhtype_S)

cons_htenure <- bind_rows(cons_htenure_EW, cons_htenure_S)

identical(cons_sex_age$ZoneID, cons_hiqual$ZoneID)
identical(cons_hiqual$ZoneID, cons_ethnicity$ZoneID)
identical(cons_ethnicity$ZoneID, cons_marstat$ZoneID)
identical(cons_marstat$ZoneID, cons_jbstat$ZoneID)
identical(cons_jbstat$ZoneID, cons_hhtype$ZoneID)
identical(cons_hhtype$ZoneID, cons_htenure$ZoneID)

sum(cons_sex_age_adult_EW[-1])
sum(cons_hiqual_EW[-1])
sum(cons_ethnicity_EW[-1])
sum(cons_marstat_EW[-1])
sum(cons_jbstat_EW[-1])
sum(cons_hhtype_EW[-1])
sum(cons_htenure_EW[-1])

sum(cons_sex_age_adult_S[-1])
sum(cons_hiqual_S[-1])
sum(cons_ethnicity_S[-1])
sum(cons_marstat_S[-1])
sum(cons_jbstat_S[-1])
sum(cons_hhtype_S[-1])
sum(cons_htenure_S[-1])

sum(cons_sex_age[-1])
sum(cons_hiqual[-1])
sum(cons_ethnicity[-1])
sum(cons_marstat[-1])
sum(cons_jbstat[-1])
sum(cons_hhtype[-1])
sum(cons_htenure[-1])

c(
  sum(cons_sex_age[-1]),
  sum(cons_hiqual[-1]),
  sum(cons_ethnicity[-1]),
  sum(cons_marstat[-1]),
  sum(cons_jbstat[-1]),
  sum(cons_hhtype[-1]),
  sum(cons_htenure[-1])
) / sum(cons_sex_age[-1]) # Examine how much the values deviate from the baseline population.

cons_pop <- rowSums(cons_sex_age[-1])

# Scale the other constraints to the baseline population. Need round()???
cons_hiqual[-1] <-
  round(cons_hiqual[-1] * cons_pop / rowSums(cons_hiqual[-1]))

cons_ethnicity[-1] <-
  round(cons_ethnicity[-1] * cons_pop / rowSums(cons_ethnicity[-1]))

cons_marstat[-1] <-
  round(cons_marstat[-1] * cons_pop / rowSums(cons_marstat[-1]))

cons_jbstat[-1] <-
  round(cons_jbstat[-1] * cons_pop / rowSums(cons_jbstat[-1]))

cons_hhtype[-1] <-
  round(cons_hhtype[-1] * cons_pop / rowSums(cons_hhtype[-1]))

cons_htenure[-1] <-
  round(cons_htenure[-1] * cons_pop / rowSums(cons_htenure[-1]))

# Ensure that all populations are equal.
sum(cons_sex_age[-1])
sum(cons_hiqual[-1])
sum(cons_ethnicity[-1])
sum(cons_marstat[-1])
sum(cons_jbstat[-1])
sum(cons_hhtype[-1])
sum(cons_htenure[-1])

if (MSOA_MODE) {
  write_csv(cons_sex_age, "cons_sex_age_MSOA.csv")
  write_csv(cons_hiqual, "cons_hiqual_MSOA.csv")
  write_csv(cons_ethnicity, "cons_ethnicity_MSOA.csv")
  write_csv(cons_marstat, "cons_marstat_MSOA.csv")
  write_csv(cons_jbstat, "cons_jbstat_MSOA.csv")
  write_csv(cons_hhtype, "cons_hhtype_MSOA.csv")
  write_csv(cons_htenure, "cons_htenure_MSOA.csv")
} else {
  write_csv(cons_sex_age, "cons_sex_age_LSOA.csv")
  write_csv(cons_hiqual, "cons_hiqual_LSOA.csv")
  write_csv(cons_ethnicity, "cons_ethnicity_LSOA.csv")
  write_csv(cons_marstat, "cons_marstat_LSOA.csv")
  write_csv(cons_jbstat, "cons_jbstat_LSOA.csv")
  write_csv(cons_hhtype, "cons_hhtype_LSOA.csv")
  write_csv(cons_htenure, "cons_htenure_LSOA.csv")
}

# Use https://github.com/MassAtLeeds/FMF/releases to generate synthetic populations.

# Or use IPF.
# source("IPF.R", echo = T)
