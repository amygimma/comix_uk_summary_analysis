## Name: dm100b_weights_for_avg_contact_data.R
## Description: Add population and age weights to participant data tables.

## Input file: dt_1w and dt_2w
## Output file: dt_2w

# Packages ----------------------------------------------------------------
library(data.table)
library(readxl)


# Load participant data ---------------------------------------------------
pdt <- qs::qread('data/dt_2w.qs')

pdt[is.na(part_gender), part_gender := "other"]

pdt <- pdt[!area %in% c("Scotland", "Northern Ireland", "Wales")]
# Load and shape population data -------------------------

popall <- as.data.table(readxl::read_xlsx(
  file.path("data", "WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx"),
  skip = 16))
popall[, part_gender := "other"]


popf <- as.data.table(readxl::read_xlsx(
    file.path("data", "WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"),
    skip = 16))
popf[, part_gender := "female"]
popm <- as.data.table(readxl::read_xlsx(
  file.path("data", "WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"),
  skip = 16))
popm[, part_gender := "male"]

pop <- rbindlist(list(popall, popf, popm))
 

setnames(pop, 
         old = c("Region, subregion, country or area *", 
                 "Reference date (as of 1 July)"), 
         new = c("location", "year"))

pop <- pop[location == "United Kingdom" & year == 2020]

pop2 <- melt(pop, id.vars = c("location", "year", "part_gender"), 
             measure.vars = as.character(0:100), 
             variable.name = "age",
             value.name = "estimate")

# age_breaks <- c(0, 4, 11, 17, 29, 39, 49, 59, 69, 120)
# cut(as.numeric(pop$age), age_breaks, right = TRUE)
pop2[, age := as.numeric(as.character(age))]
pop2[, estimate := as.numeric(as.character(estimate))]

pop2[between(age, 0, 4), part_age_group := "0-4"]
pop2[between(age, 5, 11), part_age_group := "5-11"]
pop2[between(age, 12, 17), part_age_group := "12-17"]
pop2[between(age, 18, 29), part_age_group := "18-29"]
pop2[between(age, 30, 39), part_age_group := "30-39"]
pop2[between(age, 40, 49), part_age_group := "40-49"]
pop2[between(age, 50, 59), part_age_group := "50-59"]
pop2[between(age, 60, 69), part_age_group := "60-69"]
pop2[between(age, 70, 120), part_age_group := "70-120"]

pop2[part_age_group %in% c("0-4", "5-11", "12-17"), sample_type := "child"]
pop2[part_age_group %in% c("18-29", "30-39", "40-49", "50-59", "60-69", "70-120"), 
     sample_type := "adult"]


table(pop2$part_age_group, pop2$part_gender, useNA = "always")
table(pop2$sample_type, pop2$part_gender, useNA = "always")

pop3 <- pop2[, .(pop_estimate = sum(estimate)), by = c("part_age_group", "part_gender", "sample_type")]
pop3 <- pop3[, pop_total := sum(pop_estimate), by = c("part_gender", "sample_type")]
pop3[, pop_proportion := pop_estimate / pop_total]
pw <- pop3[sample_type == "child"]

# Create a lookup for each sex-age(group) ------------------
# lookup should contain, for each sex-age(group) group, sample size and total population
# weight is calculated as pop/sample meaning each participant who belongs in each
# sex-age(group) group should represent x=pop/sample in the population
weightlookup <- pdt[!is.na(part_age_group) & part_gender != "other", 
                    .(sample = .N), by = .(country, mid_date, part_gender, part_age_group, sample_type)]
weightlookup[, sample_total := sum(sample), by = .(country, part_gender, mid_date, sample_type)]
weightlookup[, sample_proportion := sample / sample_total]

weightlookupage <- pdt[!is.na(part_age_group), 
                    .(sample = .N), by = .(country, mid_date, part_age_group, sample_type)]
weightlookupage[, sample_total := sum(sample), by = .(country, mid_date, sample_type)]
weightlookupage[, sample_proportion := sample / sample_total]
# Assign weights for "other/unknown" gender by age only
weightlookupage[, part_gender := "other"]

weightlookup <- rbind(weightlookup, weightlookupage)
tw <- weightlookup[mid_date == "2020-05-27" & sample_type == "child"]


## Plot population data to visually check -------------
# library(ggplot2)
# ggplot(pop3, aes(x = factor(part_age_group), y = pop_estimate, fill = part_gender)) +
#   geom_col(position = "dodge")
# ggplot(pop3, aes(x = factor(part_age_group), y = pop_proportion, fill = part_gender)) +
#   geom_col(position = "dodge")
# table(pop3$part_gender, useNA = "always")
# table(weightlookup$part_gender, useNA = "always")


weightlookup2 <- merge(weightlookup, pop3, 
                      by = c( "part_age_group", "part_gender", "sample_type"), all.x = TRUE)


weightlookup2[, genderageweight_raw := pop_estimate/sample]
weightlookup2[, genderageweight_proportion := pop_proportion / sample_proportion]

# # Tests --------------------
# tw2 <- weightlookup2[mid_date == "2020-05-27"]
# tw2[, tot := sum(sample_proportion), by = c("part_gender")]
# tw2[, weights_ratio2 := genderageweight_raw / genderageweight_proportion]
# tw2[, weights_ratio := genderageweight_proportion / genderageweight_raw]
# tw2[, prop_recalc := genderageweight_raw * sample_total / pop_total]
# tw2[, compare := round(prop_recalc - genderageweight_proportion, 2)]
# 
# summary(pdt$genderageweight_proportion)
# by(pdt$genderageweight_proportion,pdt$part_age_group,  summary)
# quantile(pdt$genderageweight_proportion, 0.05)
# quantile(pdt$genderageweight_proportion, 0.05)


# Merge weights to pdt
pdt <- merge(pdt, weightlookup2, by = c("country", "mid_date", "part_gender", "part_age_group"))


qs::qsave(pdt, file.path("data", "dt_2w_weighted.qs"))
message("Saved to: data/dt_2w_weighted.qs")

