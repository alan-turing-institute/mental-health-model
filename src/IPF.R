library(ipfp)

ind <- ind_drop_na

cons <-
  bind_cols(
    cons_sex_age[-1],
    cons_hiqual[-1],
    cons_ethnicity[-1],
    cons_marstat[-1],
    cons_jbstat[-1],
    cons_hhtype[-1],
    cons_htenure[-1]
  )

areas_no_people <- which(rowSums(cons_sex_age[-1]) == 0)

if (length(areas_no_people) != 0) {
  cons_sex_age[areas_no_people, 1] # Show zone IDs.
  
  cons <- cons %>% slice(-areas_no_people)
}

# View(cons)

cat_lab <- colnames(cons) # Categorical names.

# Create binary dummy variables for each category.
source("Categorise.R", echo = T)

# Check if the number in each category is correct.
if (sum(ind_cat[1:ncol(cons_sex_age[-1])]) != nrow(ind))
  myStop("ind_cat's sum for cons1 is incorrect.")

if (sum(ind_cat[ncol(cons_sex_age[-1]) + (1:ncol(cons_hiqual[-1]))]) != nrow(ind))
  myStop("ind_cat's sum for cons2 is incorrect.")

if (sum(ind_cat[ncol(cons_sex_age[-1]) + ncol(cons_hiqual[-1]) + (1:ncol(cons_ethnicity[-1]))]) != nrow(ind))
  myStop("ind_cat's sum for cons3 is incorrect.")

if (sum(ind_cat[, ncol(cons_sex_age[-1]) + ncol(cons_hiqual[-1]) + ncol(cons_ethnicity[-1]) + (1:ncol(cons_marstat[-1]))]) != nrow(ind))
  myStop("ind_cat's sum for cons4 is incorrect.")

if (sum(ind_cat[, ncol(cons_sex_age[-1]) + ncol(cons_hiqual[-1]) + ncol(cons_ethnicity[-1]) + ncol(cons_marstat[-1]) + (1:ncol(cons_jbstat[-1]))]) != nrow(ind))
  myStop("ind_cat's sum for cons5 is incorrect.")

if (sum(ind_cat[, ncol(cons_sex_age[-1]) + ncol(cons_hiqual[-1]) + ncol(cons_ethnicity[-1]) + ncol(cons_marstat[-1]) + ncol(cons_jbstat[-1]) + (1:ncol(cons_hhtype[-1]))]) != nrow(ind))
  myStop("ind_cat's sum for cons6 is incorrect.")

if (sum(ind_cat[, ncol(cons_sex_age[-1]) + ncol(cons_hiqual[-1]) + ncol(cons_ethnicity[-1]) + ncol(cons_marstat[-1]) + ncol(cons_jbstat[-1]) + ncol(cons_hhtype[-1]) + (1:ncol(cons_htenure[-1]))]) != nrow(ind))
  myStop("ind_cat's sum for cons7 is incorrect.")

# if (length(which(cons <= 0)) > 0)
#   myStop("A constraint number cannot be zero or negative.") # Because it will cause NaN in weights.

# (14+8+5+1):8
# 14+8+5+1:8 ==> 14+8+5+(1:8)

cons <-
  apply(cons, MARGIN = 2, FUN = as.numeric) # Convert the integer constraints to a numeric matrix.

ind_cat_t <- t(ind_cat) # Transpose the dummy variables for ipfp.

x0 <- rep(1, nrow(ind)) # Set the initial weights.

# The following command will take a long time (around 1 hour).
weight <-
  apply(
    cons,
    MARGIN = 1,
    FUN = function(x)
      # ipfp(x, ind_cat_t, x0) #, maxit = 1000, verbose = T)
      ipfp(x, ind_cat_t, x0, maxit = 100)
  )

if (MSOA_MODE) {
  write_rds(weight, "weight_MSOA.rds")
} else{
  write_rds(weight, "weight_LSOA.rds")
}

# weight <- read_rds("weight_LSOA.rds")

# Convert back to aggregates.
# x (weight): matrix.
# ind_cat: tibble.
ind_agg <- t(apply(
  weight,
  MARGIN = 2,
  FUN = function(x)
    colSums(x * ind_cat)
))

# colSums((person, zone) * (person, category)).
# (zone i has n individuals) * (each individual accounts for what categories): sum up each category by zones.

# if (!identical(colnames(ind_agg), colnames(cons)))
#   myStop("ind_agg is incorrect.")

# View(ind_agg)

# Test the results for the first row.
ind_agg[1,] - cons[1,] # should be zero or close to zero.
ind_agg[2,] - cons[2,] # should be zero or close to zero.
ind_agg[3,] - cons[3,] # should be zero or close to zero.
ind_agg[4,] - cons[4,] # should be zero or close to zero.
ind_agg[5,] - cons[5,] # should be zero or close to zero.
ind_agg[6,] - cons[6,] # should be zero or close to zero.
ind_agg[7,] - cons[7,] # should be zero or close to zero.

cor(as.numeric(cons), as.numeric(ind_agg)) # Fit between constraints and estimates.

# head(cons, n = 10)
# head(ind_agg, n = 10)

which(abs(ind_agg - cons) == max(abs(ind_agg - cons)), arr.ind = T)

if (RANDOM_SEED_MODE)
  set.seed(RANDOM_SEED)

# Integerize individuals.
source("Integerise.R", echo = T)

ind_int <-
  ints_df %>% as_tibble() ### Change Integerise.R to tibble!

colnames(ind_int)[colnames(ind_int) == "id"] <- "pid"
colnames(ind_int)[colnames(ind_int) == "zone"] <- "zid"

# remove(ints_df)

if (length(areas_no_people) != 0) {
  zones <-
    cons_sex_age %>% slice(-areas_no_people) %>% select(ZoneID)
} else{
  zones <- cons_sex_age %>% select(ZoneID)
}

zones$zone_num <- 1:nrow(zones)

# Optional.
ind_int <- ind_int %>% arrange(zid, pid)

ind_int <- ind_int %>% inner_join(zones, by = c("zid" = "zone_num"))

# if(nrow(ind_int) != sum(cons_sex_age[-1]))
#   myStop("Populations mismatch.")

# write_rds(ind_int, "pop_all.rds")

# Note: ZoneID is not sorted. (Wales before Scotland)
pop <- ind_int %>% select(ZoneID, pidp)

if (MSOA_MODE) {
  write_rds(pop, "population_IPF_MSOA.rds")
} else{
  write_rds(pop, "population_IPF_LSOA.rds")
}
