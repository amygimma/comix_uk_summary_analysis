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
source('r/functions/bs_facemask_proportion.R')

# Load participant data ---------------------------------------------------
p1 <- qs::qread('data/dt_1w.qs')
pdt <- qs::qread('data/dt_2w.qs')

p1  <-  p1[!area %in% c("Scotland", "Northern Ireland", "Wales")]
pdt <- pdt[!area %in% c("Scotland", "Northern Ireland", "Wales")]


# non_household_cnt <- contacts[cnt_household == 0, .(total_non_hh_contacts = .N), by = c("part_wave_uid")]
# total_cnt <- contacts[, .(total_contacts = .N), by = c("part_wave_uid")]
# 
# part <- merge(part, non_household_cnt, by = "part_wave_uid", all.x = T)
# part[is.na(total_non_hh_contacts), total_non_hh_contacts := 0]
# part <- merge(part, total_cnt, by = "part_wave_uid", all.x = T)
# part[is.na(total_contacts), total_non_hh_contacts := 0]
# Define boots ------------------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)
print(args)
if (length(args) == 1) boots <- as.numeric(args)
if (!exists("groups")) boots <- 100

dt_boot <- data.table()
message(paste("Running", boots, "bootstrapped samples"))



# Analysis -----------------------------------------------------------


## Adults by non-household contact status (0 or >0)
dt_boot <- data.table()
for(i in c("18-59", "60+")){
  for(j in c(TRUE, FALSE)){
    print(j)
    print(i)
    dt1 <- bs_facemask_proportion(pdt,  boots, prop = 1.0, has_non_hh_contacts_ = j,  age_ = i)
    
    dt_boot <- rbind(dt_boot, dt1)
  }
}

dt_boot[, n := round(median(N)), by = .(part_age_group, part_gender,
                                        start_date, mid_date, end_date)]

mea_vars <- c("fm_per")


l_dt <- melt(dt_boot, id.vars = c("part_age_group", "part_gender", "has_non_hh_contacts", "start_date",
                                  "mid_date", "end_date", "survey_round", "n"), 
             measure.vars = mea_vars, variable.name = "measure", value  = "proportion")

dts <- l_dt[, .(
  lci = quantile(proportion, 0.025, na.rm = T),  
  mean = mean(proportion, na.rm = T), 
  uci = quantile(proportion, 0.975, na.rm = T), 
  boots = .N),
  by = .(part_age_group, part_gender,has_non_hh_contacts,
         start_date, mid_date, end_date, n)]



# Save data ---------------------------------------------------------------

sys_date <- Sys.Date()

file_path <- file.path("data", paste(sys_date, boots, "bs_proportions_2w.qs", sep = "_"))
qs::qsave(dts, file_path)
message(paste("saved to:", file_path))

file_path <- file.path("data", paste("bs_proportions_2w.qs", sep = "_"))
qs::qsave(dts, file_path)
message(paste("saved to:", file_path))

