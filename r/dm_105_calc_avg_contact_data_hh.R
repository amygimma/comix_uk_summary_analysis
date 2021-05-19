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
part <- qs::qread("../comix/data/part.qs")
# p1 <- qs::qread('data/dt_1w.qs')
pdt <- qs::qread('data/dt_2w.qs')

part <- part[country == "uk"]
part[, area := area_3_name]
part[area_3_name %in% c("Yorkshire and The Humber", "North East"), area := "North East and Yorkshire"]
part[area_3_name %in% c("East Midlands", "West Midlands"), area := "Midlands"]
part <-  part[!area %in% c("Scotland", "Northern Ireland", "Wales")]
part <- part[date >= as.Date("2020-03-23") & date <= as.Date("2021-03-26")]
part[, wave_id := paste0(panel, wave)]
pdt <- pdt[!area %in% c("Scotland", "Northern Ireland", "Wales")]
pdt <- pdt[date >= as.Date("2020-03-23") & date <= as.Date("2021-03-26")]

summary(part$hh_size)
quantile(part$hh_size, 0.25, na.rm = T)
mean(part_)

hh_summary <- part[!is.na(hh_size), .(
                       q_025 = quantile(hh_size, 0.025),
                       q_25  = quantile(hh_size, 0.25),
                       mean = mean(hh_size),
                       q_75 = quantile(hh_size, 0.75),
                       q_975 = quantile(hh_size, 0.975)
                       ),
     by = c("wave_id", "sample_type")]
amean_max <- max(hh_summary[sample_type == "adult"]$mean)
amean_min <- min(hh_summary[sample_type == "adult"]$mean)
aq25_max <- max(hh_summary[sample_type == "adult"]$q_25)
aq25_min <- min(hh_summary[sample_type == "adult"]$q_25)
aq75_max <- max(hh_summary[sample_type == "adult"]$q_75)
aq75_min <- min(hh_summary[sample_type == "adult"]$q_75)
hist(part[sample_type == "adult"]$hh_size)
asdm <- sd(hh_summary[sample_type == "adult"]$mean)
asd <- sd(part[sample_type == "adult"]$hh_size, na.rm = T)

cmean_max <- max(hh_summary[sample_type == "child"]$mean)
cmean_min <- min(hh_summary[sample_type == "child"]$mean)
cq25_max <- max(hh_summary[sample_type == "child"]$q_25)
cq25_min <- min(hh_summary[sample_type == "child"]$q_25)
cq75_max <- max(hh_summary[sample_type == "child"]$q_75)
cq75_min <- min(hh_summary[sample_type == "child"]$q_75)
hist(part[sample_type == "child"]$hh_size)
csdm <- sd(hh_summary[sample_type == "child"]$mean)
csd <- sd(part[sample_type == "child"]$hh_size, na.rm = T)


hh_size_levels <- c("1",  "2", "3-5", "6+", "Unknown")
p1[, hh_size_levels]

table()

pdt

# Define boots ------------------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)
print(args)
if (length(args) == 1) boots <- as.numeric(args)
if (!exists("boots")) boots <- 2

# dput(unique(pdt$part_social_group))


dt_boot <- data.table()
message(paste("Running", boots, "bootstrapped samples"))



# SE analysis -----------------------------------------------------------
## Adults by socio-economic status
for(i in c("0-4", "5-17", "18-59", "60+")){
  for(j in c(hh_size_levels)){
    if(!is.na(j)){
      print(i)
      print(j)
      dt1 <- bs_group(pdt, boots, prop = 1.0, soc_group_ = j,  age_ = i)
      
      dt_boot <- rbind(dt_boot, dt1)
    }
  }
}



dt_boot[, n := round(median(N)), by = .(part_age_group, part_region, part_gender, part_social_group, part_high_risk, start_date, mid_date, end_date)]

mea_vars <- c("All", "Home", "Work", "Work/Educ", "Other",
              "Physical",
              "Inside",
              "Outside",
              "Other house",
              "Supermarket",
              "Bar restaurant")


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
filename <- "bs_means_2w_sg2.qs"
file_path <- file.path("data", paste(sys_date, boots, filename, sep = "_"))
qs::qsave(dts, file_path)
message(paste("saved to:", file_path))

file_path <- file.path("data", filename)
qs::qsave(dts, file_path)
message(paste("saved to:", file_path))

