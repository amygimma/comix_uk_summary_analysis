library(data.table)
library(dplyr)
library(qs)
library(ggplot2)
library(ggthemr)
library(cowplot)
library(patchwork)
library(forcats)
library(stringr)
library(snakecase)
library(openxlsx)
source(file.path("r", "functions", "utility_functions.R"))

data_path <- "../comix/data"
part <- qread(file.path(data_path, "part.qs"))
contacts <- qread(file.path(data_path, "contacts.qs"))

hh <- qread(file.path(data_path, "households.qs"))

hh_totals <- hh[, .(hh_size = .N), by = c("country", "panel", "wave", "part_wave_uid")]
na_hhs <- hh[is.na(hh_size)]
message(paste("Assigning NA hh sizes: ", nrow(hh[is.na(hh_size)])))
part[is.na(hh_size), hh_size := 
       unlist(lapply(part_wave_uid, match_hh_size, hh_total_dt = hh_totals))]

# Filter for England only & rename area_3 ------------------
england_area_3 <- c("South East", "North West", "West Midlands", "East Midlands", 
                    "South West", "Greater London", "North East", 
                    "Yorkshire and The Humber", "East of England")
part <- part[country == "uk" & area_3_name %in% england_area_3]
part_reg <- part[,list(part_id, panel, wave,country,area_3_name)]
contacts <- merge(contacts, part_reg, by = c("part_id", "panel", "wave", "country"))
contacts <- contacts[country == "uk" & area_3_name %in% england_area_3]
# contacts <- contacts[country == "uk" & part_id %in% part$part_id]

setnames(part, "area_3_name", "nhs_region")
setnames(contacts, "area_3_name", "nhs_region")

part[nhs_region %in% c("East Midlands", "West Midlands"), nhs_region := "Midlands"]
part[nhs_region %in% c("Yorkshire and The Humber", "North East"), 
     nhs_region := "North East and Yorkshire"]


part <- add_england_col(part)
contacts <- add_england_col(contacts)

# contacts <- add_england_col(contacts)

# Lock to wave E/F 12 and before ------------------

end_date <- "2021-03-29"
part <- part[date <= as.Date(end_date)]
contacts <- contacts[date <= as.Date(end_date)]


# Add survey phase column ------------------
part[, survey_phase := fifelse(
  panel %in% c("A", "B", "C", "D"), "Phase 1", "Phase 2")]
part[, survey_phase := fifelse(
  panel %in% c("A", "B", "C", "D"), "Phase 1", "Phase 2")]
# contacts[, survey_phase := 
#            fifelse(panel %in% c("A", "B", "C", "D"), "Phase 1", "Phase 2")]

# Add study period column ------------------
if (!exists("study_period_type")) study_period_type <- "table"
if (study_period_type == "figure") {
part[between(date, as.Date("2020-03-29"), as.Date("2020-06-03")),
     study_period := "Lockdown 1"]
part[between(date, as.Date("2020-07-30"), as.Date("2020-09-03")),
     study_period := "Reduced restrictions"]
part[between(date, as.Date("2020-11-05"), as.Date("2020-12-02")),
     study_period := "Lockdown 2"]
part[between(date, as.Date("2020-12-19"), as.Date("2021-01-02")),
     study_period := "Christmas"]
part[between(date, as.Date("2021-01-05"), as.Date(end_date)),
     study_period := "Lockdown 3"]
} else if (study_period_type == "table") {
  # table(part[wave == "1" & panel %in% c("A", "B")]$date, part[wave == "1" & panel %in% c("A", "B")]$panel)
  # table(part[wave == "1" & panel %in% c("E", "F")]$date, part[wave == "1" & panel %in% c("E", "F")]$panel)
  # part[order(wave), wave_number := seq_along(wave), by = c("part_id") ]
  # table(part[wave_number == 1]$wave, part[wave_number == 1]$panel, part[wave_number == 1]$sample_type)
  # table(part[wave == 9 & panel %in% c("E", "F")]$date, part[wave == 9 & panel %in% c("E", "F")]$panel)
  # table(part[wave %in% c(8, 9) & panel %in% c("E", "F")]$date, part[wave %in% c(8, 9) & panel %in% c("E", "F")]$panel)
  # table(part[wave == 15]$date, part[wave == 15]$panel)
  
  part[between(date, as.Date("2020-03-23"), as.Date("2020-08-08")),
       study_period := "Initial recruitment"]
  part[between(date, as.Date("2020-08-09"), as.Date("2021-01-01")), 
       study_period := "Second recruitment"]
  part[between(date, as.Date("2021-01-02"), as.Date(end_date)), 
       study_period := "New year"]
}

# Relabel part and contact ages ------------------
table(part$part_age_group, useNA = "always")
part[, part_age_group := fifelse(part_age_group == "70-120", "70+", part_age_group)]
part[, part_age_group := fifelse(is.na(part_age_group), "Unknown age", part_age_group)]

# Remove participants from children's survey with adult ages 
# part <- part[sample_type == "adult" | 
#                (sample_type == "child" & 
#                   part_age_group %in% c("0-4", "5-11", "12-17", "18-29"))]

# part[, part_age_group := fifelse(part_age_group == "70-120", "70+", part_age_group)]
# 
# contacts[, cnt_age_group := fifelse(cnt_age_group == "70-120", "70+", cnt_age_group)]
# contacts[, cnt_age_group := fifelse(cnt_age_group == "60-120", "60+", cnt_age_group)]
# contacts[, cnt_age_group := fifelse(cnt_age_group == "0-120", "Unknown age", cnt_age_group)]

# Group household sizes ------------------
part[, hh_size_group := fcase(
  as.numeric(hh_size) == 1, "1",
  as.numeric(hh_size) == 2, "2",
  as.numeric(hh_size) %in% 3:5, "3-5",
  as.numeric(hh_size) >= 6, "6+",
  is.na(hh_size), "Unknown"
)]

# Relabel gender ------------------
table(part[panel == "E" & wave == 11]$part_gender, useNA = "always")

part[, part_gender := str_to_sentence(part_gender)]
part[is.na(part_gender), part_gender := "Other"]

# Set unknown age groups in parent panels

table(part[sample_type == "child"]$part_age_group)
child_ages <- c("0-4", "5-11", "12-17")
part[sample_type == "child" & !part_age_group %in% child_ages, part_age_group := "Unknown age"]


# Set contact cnt_setting -----------------
contacts[, cnt_setting := case_when(
  cnt_home == 1 ~ "Home",
  cnt_work == 1 ~ "Work",
  cnt_school == 1 ~ "Education",
  TRUE ~ "Other"
)]
contacts[!is.na(cnt_setting), cnt_any := 1]
contacts[, cnt_setting := factor(cnt_setting, levels = c("Home", "Work", "Education", "Other"))]

