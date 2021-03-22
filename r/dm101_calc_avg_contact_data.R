## Name: dm101_calc_avg_contact_data
## Description: Calculate and save the mean contacts over time.

## Input file: dt_1w and dt_2w
## Functions: bs_group
## Output file: 2021-01-24_bs_means_2w.qs

# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
# Source user written scripts ---------------------------------------------

## Only works for the subset in this script will need adapting to do more
source('r/functions/bs_group.R')

# Load participant data ---------------------------------------------------
p1 <- qs::qread('data/dt_1w.qs')
pdt <- qs::qread('data/dt_2w.qs')


p1  <-  p1[!area %in% c("Scotland", "Northern Ireland", "Wales")]
pdt <- pdt[!area %in% c("Scotland", "Northern Ireland", "Wales")]

# Define boots ------------------------------------------------------------

boots <- 10
#boots <- 1000
dt_boot <- data.table()




# Main analysis -----------------------------------------------------------

## Setting and risk by age
for(i in c("0-4", "5-17", "18-59", "60+")){
  print(i)
    dt1 <- bs_group(pdt,  boots, prop = 1.0, age = i, area_ = "All" )
    dt2 <- bs_group(pdt,  boots, prop = 1.0, age = i, area_ = "All", risk_group_ = "yes")
    dt3 <- bs_group(pdt,  boots, prop = 1.0, age = i, area_ = "All", risk_group_ = "no")
    
    dt_boot <- rbind(dt_boot, dt1, dt2, dt3)
}


## Adults by socio-economic status
for(i in c("18-59", "60+")){
    for(ii in unique(pdt$part_social_group)){
      if(i %in% c("18-59", "60+")){
        print(ii)
        print(i)
        dt1 <- bs_group(pdt,  boots, prop = 1.0, soc_group_ = ii,  age_ = i)
        
        dt_boot <- rbind(dt_boot, dt1)
      }
    }
}

# Get regions -------------------------------------------------------------
for(i in c(unique(pdt$area), "England")){
  print(i)
  dt1 <- bs_group(pdt,  boots, prop = 1.0, area_ = i, age_ = "All-adults")
  dt2 <- bs_group(pdt,  boots, prop = 1.0, area_ = i, age_ = "All")
  dt_boot <- rbind(dt_boot, dt1, dt2)
}


# Get age groups ----------------------------------------------------------
for(i in c(unique(pdt$part_age_group), "All", "All-adults")){
  print(i)
  if(!is.na(i)){
    dt1 <- bs_group(pdt,  boots, prop = 1.0, area_ = "All", age = i)
    dt_boot <- rbind(dt_boot, dt1)
  }
}


# Get for gender ----------------------------------------------------------
for(i in c("male", "female")){
  dt1 <- bs_group(pdt,  boots, prop = 1.0, gender_ = i,  age_ = "All-adults")
  dt2 <- bs_group(pdt,  boots, prop = 1.0, gender_ = i, age_ = "All")
  dt_boot <- rbind(dt_boot, dt1, dt2)
}


# Get for socioeconomic status ---------------------------------------------
for(i in unique(pdt$part_social_group)){
  dt1 <- bs_group(pdt,  boots, prop = 1.0, soc_group_ = i,  age_ = "All-adults")
  dt2 <- bs_group(pdt,  boots, prop = 1.0, soc_group_ = i, age_ = "All")
  dt_boot <- rbind(dt_boot, dt1, dt2)
}

# Get for socioeconomic status ---------------------------------------------
for(i in c("yes", "no")){
  dt1 <- bs_group(pdt,  boots, prop = 1.0, risk_group_ = i,  age_ = "All-adults")
  dt2 <- bs_group(pdt,  boots, prop = 1.0, risk_group_ = i, age_ = "All")
  dt_boot <- rbind(dt_boot, dt1, dt2)
}

dt_boot[, n := round(median(N)), by = .(part_age_group, part_region, part_gender, part_social_group, part_high_risk, start_date, mid_date, end_date)]

mea_vars <- c("All", "Home", "Work/Educ", "Other",
  "Physical",
  "Inside",
  "Outside",
  "Other house",
  "Supermarket",
  "Bar restaurant")


l_dt <- melt(dt_boot, id.vars = c("part_age_group", "part_region", "part_gender", "part_social_group", "part_high_risk", "start_date", "mid_date", "end_date", "survey_round", "n"), measure.vars = mea_vars, variable.name = "setting", value  = "avg")

dts <- l_dt[, .(lci = quantile(avg, 0.025, na.rm = T),  mean = mean(avg, na.rm = T), uci = quantile(avg, 0.975, na.rm = T), boots = .N),
            by = .(part_age_group, part_region, part_gender, part_social_group, part_high_risk, start_date, mid_date, end_date, setting, n)]



# Save data ---------------------------------------------------------------
qs::qsave(dts, "data/2021-03-05_bs_means_2w.qs")



