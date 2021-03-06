## Name: dm101_calc_avg_contact_data
## Description: Calculate and save the mean contacts over time.

## Input file: dt_1w and dt_2w
## Functions: bs_group
## Output file: 2021-01-24_bs_means_2w.qs
set.seed(23032021)
# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
# Source user written scripts ---------------------------------------------

## Only works for the subset in this script will need adapting to do more
source('r/functions/bs_group.R')

# Load participant data ---------------------------------------------------
p1 <- qs::qread('data/dt_1w.qs')
pdt <- qs::qread('data/dt_2w_weighted.qs')


p1  <-  p1[!area %in% c("Scotland", "Northern Ireland", "Wales")]
pdt <- pdt[!area %in% c("Scotland", "Northern Ireland", "Wales")]

# Add attitude likert bins
map_likert2 <- c("Strongly agree" = "Agree",
                 "Tend to agree" = "Agree",
                 "Neither agree nor disagree" = "Neutral",
                 "Tend to disagree" = "Disagree", 
                 "Strongly disagree" = "Disagree"
)

pdt[, part_att_spread_bin := map_likert2[part_att_spread]]
pdt[, part_att_likely_bin := map_likert2[part_att_likely]]
pdt[, part_att_serious_bin := map_likert2[part_att_serious]]
# pdt[part_age_group %in% c("60-69", "70-120"), part_age_group := ""]
# pdt[, part_high_risk_bin := map_hr[part_high_risk]]


# Define boots ------------------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)
print(args)
if (length(args) == 1) boots <- as.numeric(args)
if (!exists("boots")) boots <- 1000
boots <- 500

dt_boot <- data.table()
message(paste("Running", boots, "bootstrapped samples"))


# Main analysis -----------------------------------------------------------

## Setting and risk by age
for(i in c("0-4", "5-17", "18-59", "60+")){
  print(i)
  # dt1 <- bs_group(pdt,  boots, prop = 1.0, age = i, area_ = "All", risk_group_ = "All" )
  dt2 <- bs_group(pdt,  boots, prop = 1.0, age = i, area_ = "All", risk_group_ = "yes")
  dt3 <- bs_group(pdt,  boots, prop = 1.0, age = i, area_ = "All", risk_group_ = "no")
  
  dt_boot <- rbind(dt_boot, dt1, dt2, dt3)
}


# Get for att_spread status age ---------------------------------------------
for(i in c("Agree", "Disagree", "Neutral")){
  for (j in c("18-59", "60+")) {
  # for (j in c("18-59", "60+", "All-adults")) { # analysis currently doesn't use All-adults group
    dt1 <- bs_group(pdt,  boots, prop = 1.0, att_spread_bin_ = i,  age_ = j)
    dt_boot <- rbind(dt_boot, dt1)
  }
}


# Get for att_likely status age ---------------------------------------------
for(i in c("Agree", "Disagree", "Neutral")){
  for (j in c("18-59", "60+")) {
  # for (j in c("18-59", "60+", "All-adults")) { # analysis currently doesn't use All-adults group
    dt1 <- bs_group(pdt,  boots, prop = 1.0, att_likely_bin_ = i,  age_ = j)
    # dt2 <- bs_group(pdt,  boots, prop = 1.0, att_likely_bin_ = i, age_ = "All")
    dt_boot <- rbind(dt_boot, dt1)
  }
}

# # Get for att_serious status ---------------------------------------------
# for(i in c("Agree", "Disagree", "Neutral")){
#   dt1 <- bs_group(pdt,  boots, prop = 1.0, att_serious_bin_ = i,  age_ = "All-adults")
#   # dt2 <- bs_group(pdt,  boots, prop = 1.0, att_serious_bin_ = i, age_ = "All")
#   dt_boot <- rbind(dt_boot, dt1)
# }

for(i in c("Agree", "Disagree", "Neutral")){
  for (j in c("18-59", "60+")) {
  # for (j in c("18-59", "60+", "All-adults")) { # analysis currently doesn't use All-adults group
    print(i)
    print(j)
    dt1 <- bs_group(pdt,  boots, prop = 1.0, att_serious_bin_ = i,  age_ = j)
    # dt2 <- bs_group(pdt,  boots, prop = 1.0, att_serious_bin_ = i, age_ = "All")
    dt_boot <- rbind(dt_boot, dt1)
  }
}




dt_boot[, n := round(median(N)), by = .(part_age_group, part_region, part_gender,
                                        part_social_group, part_high_risk, start_date, mid_date, end_date)]

mea_vars <- c("All", "Home", "Work", "Work/Educ", "Other",
              "All_genderage", "Home_genderage", "Work_genderage", "Work/Educ_genderage", "Other_genderage"
              # "Physical",
              # "Inside",
              # "Outside",
              # "Other house",
              # "Supermarket",
              # "Bar restaurant"
              )


l_dt <- melt(dt_boot, id.vars = c("part_age_group", "part_region", "part_gender", "part_work_place",
                                  "part_employed", "part_income", "part_social_group", 
                                  "part_high_risk", "start_date", "part_att_spread_bin", 
                                  "part_att_likely_bin", "part_att_serious_bin",
                                  "mid_date", "end_date", "survey_round", "n"), 
             measure.vars = mea_vars, variable.name = "setting", value  = "avg")

dts <- l_dt[, .(
  lci = quantile(avg, 0.025, na.rm = T),  
  mean = mean(avg, na.rm = T), 
  uci = quantile(avg, 0.975, na.rm = T), 
  boots = .N),
  by = .(part_age_group, part_region, part_gender, part_social_group, part_work_place,
         part_income, part_employed, part_high_risk, part_att_spread_bin, 
         part_att_likely_bin, part_att_serious_bin,
         start_date, mid_date, end_date, setting, n)]



# Save data ---------------------------------------------------------------
sys_date <- Sys.Date()
filename <- "bs_means_2w_risk.qs"
file_path <- file.path("data", paste(sys_date, boots, filename, sep = "_"))
qs::qsave(dts, file_path)
message(paste("saved to:", file_path))

file_path <- file.path("data", filename)
qs::qsave(dts, file_path)
message(paste("saved to:", file_path))


