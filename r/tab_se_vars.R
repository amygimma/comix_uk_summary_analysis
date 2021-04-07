


## Average contacts overtime
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemr)
ggthemr('dust')

# Load participant data ---------------------------------------------------
part <- qs::qread("../comix/data/part.qs")
table(part$country)
part[, table(area_3_name)]

# Get total contacts ------------------------------------------------------
p_cnts <- qs::qread('../comix/data/part_cnts.qs')


# Add on contacts ---------------------------------------------------------
dt <- merge(part, p_cnts, by = "part_wave_uid", all.x = TRUE, no.dups = TRUE)
dt <- part

# Change names of areas ---------------------------------------------------
table(dt$area_3_name)
dt[, area := area_3_name]
dt[area_3_name %in% c("Yorkshire and The Humber", "North East"), area := "North East and Yorkshire"]
dt[area_3_name %in% c("East Midlands", "West Midlands"), area := "Midlands"]


# Subset to only UK -------------------------------------------------------
dt <- dt[country == "uk"]
## Remove round 6 and 7
dt <- dt[!survey_round %in% 6:7]


## Created an employed variables
dt[part_employstatus=="employed full-time (34 hours or more)", part_employed := "Full time"]
dt[part_employstatus=="employed part-time (less than 34 hours)", part_employed := "Part time"]
dt[part_employstatus=="self employed", part_employed := "Self employed"]

dt[, table(survey_round, part_employstatus)]
dt[, table(survey_round, part_employed)]
## Created a workplace variables
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


#tabulate by age_group
library(janitor)
dt[, first := min(survey_round), by = .(panel, wave, part_id)]
#regroup income in 3 categories
dt[part_income=="<5,000",        income := "<£20k"]
dt[part_income=="5,000-9,999",   income := "<£20k"]
dt[part_income=="10,000-14,999", income := "<£20k"]
dt[part_income=="15,000-19,999", income := "<£20k"]
dt[part_income=="20,000-24,999", income := "20k-44.9k"]
dt[part_income=="25,000-34,999", income := "20k-44.9k"]
dt[part_income=="35,000-44,999", income := "20k-44.9k"]
dt[part_income=="45,000-54,999", income := "45k+"]
dt[part_income=="55,000-99,999", income := "45k+"]
dt[part_income=="100,000+",      income := "45k+"]

#only tabulate participants' age as they reported at their first round
sum <- dt[first==survey_round] 

# n and % in diff income groups
income <- setDT(sum)[part_work_place == "open" & !is.na(part_employed) & !is.na(income), 
                     .(count = tabulate(.N)), by = .(income)]
income <- dcast(income, income ~ ., value.var = "count")
income <- income %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()

#n and % in diff employment group
employ <- setDT(sum)[part_work_place == "open" & !is.na(part_employed) & !is.na(income), 
                     .(count = tabulate(.N)), by = .(part_employed)]
employ <- dcast(employ, part_employed ~ ., value.var = "count")
employ <- employ %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns()

#income x age
income_age <- setDT(sum)[part_work_place == "open" & !is.na(part_employed) & !is.na(income), 
                         .(count = tabulate(.N)), by = .(income, part_age_group)]
income_age <- dcast(income_age, income ~ part_age_group, value.var = "count")
income_age <- income_age %>%
  adorn_percentages("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns() 
income_age <- cbind(income, income_age[,-1])

#employ x age
employ_age <- setDT(sum)[part_work_place == "open" & !is.na(part_employed) & !is.na(income), 
                         .(count = tabulate(.N)), by = .(part_employed, part_age_group)]
employ_age <- dcast(employ_age, part_employed ~ part_age_group, value.var = "count")
employ_age <- employ_age %>%
  adorn_percentages("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns() 
employ_age <- cbind(employ, employ_age[,-1])

#employment status x income (n)
employ_income <- setDT(sum)[part_work_place == "open" & !is.na(part_employed) & !is.na(income), 
                            .(count = tabulate(.N)), by = .(income, part_employed)]
employ_income <- dcast(employ_income, part_employed ~ income, value.var = "count")

#employment status x income (n who is also attending school)
employ_income_student <- setDT(sum)[part_work_place == "open" & !is.na(part_employed) & !is.na(income) &
                                      part_employed_attends_education %in% c("further education, e.g. college",
                                                                             "higher education, e.g. university"), 
                                    .(count = tabulate(.N)), by = .(income, part_employed)]
employ_income_student <- dcast(employ_income_student, part_employed ~ income, value.var = "count")

#final tables needed:
income_age
employ_age
employ_income
employ_income_student

