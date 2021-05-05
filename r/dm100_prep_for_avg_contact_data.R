## Name: dm100_prep_for_avg_contact_data
## Description: Combine contact to be smoothed over two week periods 

## Input file: part.qs, contacts.qs
## Functions: 
## Output file: dt_1w and dt_2w

# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
# Source user written scripts ---------------------------------------------

# Load participant data ---------------------------------------------------
part <- qs::qread("../comix/data/part.qs")
pt <- qs::qread("../comix/data/part_min.qs")
ct <- qs::qread("../comix/data/contacts.qs")
part <- part[country == "uk"]
pt <- pt[country == "uk"]
ct <- ct[country == "uk"]

ct$cnt_inside <- as.numeric(ct$cnt_inside)
ct$cnt_outside <- as.numeric(ct$cnt_outside)

# Map objects for labels --------------------------------------------------
cnt_main_vars <- c(
  "cnt_home", 
  "cnt_household",
  "cnt_household",
  "cnt_work",
  "cnt_school",
  "cnt_other",
  "cnt_phys"
)

cnt_other_vars <- c(
  "cnt_inside", 
  "cnt_outside", 
  "cnt_sport", 
  #"cnt_outside_other",
  "cnt_other_place", 
  "cnt_other_house",
  "cnt_worship",
  "cnt_public_transport", 
  "cnt_supermarket",
  "cnt_shop",
  "cnt_bar_rest",
  "cnt_health_facility", 
  "cnt_salon",
  "cnt_public_market"
)

cnt_vars <- c(cnt_main_vars, cnt_other_vars)
all_vars <- c(cnt_vars, "part_wave_uid")
ct <- ct[, ..all_vars]

sumna <- function(x) sum(x, na.rm = TRUE)

ct[, cnt_non_household := ifelse(cnt_household == 0, 1, 0)]

cp_n_cnts <- ct[, .(
  n_cnt = .N,
  n_cnt_home             = sumna(cnt_home),
  n_cnt_household        = sumna(cnt_household),
  n_cnt_non_household    = sumna(cnt_non_household),
  n_cnt_work             = sumna(cnt_work),
  n_cnt_school           = sumna(cnt_school),
  n_cnt_other            = sumna(cnt_other),
  n_cnt_phys             = sumna(cnt_phys),
  n_cnt_inside           = sumna(cnt_inside),
  n_cnt_outside          = sumna(cnt_outside),
  n_cnt_sport            = sumna(cnt_sport),
  #n_cnt_outside_other    = sumna(cnt_outside_other),
  n_cnt_other_place      = sumna(cnt_other_place),
  n_cnt_other_house      = sumna(cnt_other_house),
  n_cnt_worship          = sumna(cnt_worship),
  n_cnt_public_transport = sumna(cnt_public_transport),
  n_cnt_supermarket      = sumna(cnt_supermarket),
  n_cnt_shop             = sumna(cnt_shop),
  n_cnt_bar_rest         = sumna(cnt_bar_rest),
  n_cnt_health_facility  = sumna(cnt_health_facility),
  n_cnt_salon            = sumna(cnt_salon)
),
by = part_wave_uid]

pt_cnt = merge(part[,.(part_wave_uid)], cp_n_cnts, by = c("part_wave_uid"), all.x = TRUE)

var_list <- names(cp_n_cnts)
for (j in var_list){
  set(pt_cnt,which(is.na(pt_cnt[[j]])),j,0)
}



# Add on contacts ---------------------------------------------------------
dt <- merge(part, pt_cnt, by = "part_wave_uid", all.x = TRUE)


# Subset to UK ------------------------------------------------------------
dt <- dt[country == "uk"]

## Remove round 6 and 7
dt <- dt[!survey_round %in% 6:7]

# Change names of areas ---------------------------------------------------
table(dt$area_3_name)
dt[, area := area_3_name]
dt[area_3_name %in% c("Yorkshire and The Humber", "North East"), area := "North East and Yorkshire"]
dt[area_3_name %in% c("East Midlands", "West Midlands"), area := "Midlands"]

## Created an employed variables -------------------------------------------
dt[part_employstatus=="employed full-time (34 hours or more)", part_employed := "Full time"]
dt[part_employstatus=="employed part-time (less than 34 hours)", part_employed := "Part time"]
dt[part_employstatus=="self employed", part_employed := "Self employed"]

dt[, table(survey_round, part_employstatus)]
dt[, table(survey_round, part_employed)]
## Created a workplace variables ------------------------------------------
dt[, table(survey_round, part_work_closed)]
dt[part_work_closed == "no", part_work_place := "open"]
dt[part_work_closed == "yes", part_work_place := "closed"]

dt[, table(survey_round, part_work_place)]


# Income ------------------------------------------------------------------
dt[part_income=="under £5,000",      part_income := "<5,000"]
dt[part_income=="£5,000 - £9,999",   part_income := "5,000-9,999"] 
dt[part_income=="£10,000 - £14,999", part_income := "10,000-14,999"]
dt[part_income=="£15,000 - £19,999", part_income := "15,000-19,999"]
dt[part_income=="£20,000 - £24,999", part_income := "20,000-24,999"]
dt[part_income=="£25,000 - £34,999", part_income := "25,000-34,999"]
dt[part_income=="£35,000 - £44,999", part_income := "35,000-44,999"]
dt[part_income=="£45,000 - £54,999", part_income := "45,000-54,999"]
dt[part_income=="£55,000 - £99,999", part_income := "55,000-99,999"]
dt[part_income=="£100,000 or more",  part_income := "100,000+"]


# Create a weekday weekend weight -----------------------------------------
dt[, dayweight := fifelse(weekday %in% c("Sun", "Sat"), 2/7, 5/7)]


# Restrict the contacts to 50 max ----------------------------------------

dt[, n_cnt := pmin(n_cnt, 50)]
dt[, n_cnt_home := pmin(n_cnt_home, 50)]
dt[, n_cnt_workschool := pmin(n_cnt_work + n_cnt_school, 50)]
dt[, n_cnt_other := pmin(n_cnt_other, 50)]



# type of  contacts ----------------------------------------------------------
dt[, n_cnt_inside := pmin(n_cnt_inside, 50)]
dt[, n_cnt_outside := pmin(n_cnt_outside, 50)]
dt[, n_cnt_supermarket := pmin(n_cnt_supermarket, 50)]
dt[, n_cnt_bar_rest := pmin(n_cnt_bar_rest, 50)]
dt[, n_cnt_other_house := pmin(n_cnt_other_house, 50)]
dt[, n_cnt_phys := pmin(n_cnt_phys, 50)]



## the idea here is that the data in the next week will also be present in 
## the previous week. Therefore there will be duplicates in the dataset.

## Include variables that we will want to subset by.
vars <- c(
  ## Identifiers
  "part_id",
  "survey_round",
  "date",
  "panel",
  "wave",
  "part_wave_uid",
  ## Contact setting main
  "n_cnt",
  "n_cnt_home",
  "n_cnt_household",
  "n_cnt_non_household",
  "n_cnt_workschool",
  "n_cnt_work" ,
  "n_cnt_school",
  "n_cnt_other",
  "n_cnt_phys",
  ## Other settings
  "n_cnt_inside", 
  "n_cnt_outside", 
  "n_cnt_other_house",
  "n_cnt_supermarket",
  "n_cnt_bar_rest",
  ## Subsets
  "country",
  "sample_type",
  "area",
  "part_age_group",
  "part_age_est_max",
  "part_age_est_min",
  "part_social_group",
  "part_gender",
  # Risk
  "part_high_risk",
  "part_att_spread",
  "part_att_likely",
  "part_att_serious",
  "part_face_mask",
  # SE
  "part_work_closed",
  "part_income",
  "part_employed",
  "part_work_place",
  ## Weighting
  "dayweight"
)


# Combine the data over two weeks for smoothing ---------------------------
## Original data
p1 <- dt[, ..vars] ## Will be used for one week version
p2 <-  dt[, ..vars]

qs::qsave(p1,file =  '../comix/data/part_cnts.qs')

## Move p2 back for one survey round
p2[, survey_round := survey_round -1]

## Remove round 0, round 7 for p2 will be single week, and round 5 for p1 would be a single week so remove. 
p2 <- p2[!survey_round %in% c(0,7)]
p12 <- p1[!survey_round %in% c(5)]

# Combine data double the amount for each round
pdt <- rbind(p12,p2)

# Create a new start, mid, and end date for the survey rounds
p1[, start_date := min(date), by = .(survey_round)]
p1[, end_date := max(date), by = .(survey_round)]
p1[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round)]

## Repeat for two week version
pdt[, start_date := min(date), by = .(survey_round)]
pdt[, end_date := max(date), by = .(survey_round)]
pdt[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round)]



# Save data ---------------------------------------------------------------

qs::qsave(p1, "data/dt_1w.qs")
qs::qsave(pdt, "data/dt_2w.qs")


