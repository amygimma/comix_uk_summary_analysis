# source(file.path("r", "functions", "utility_functions.R"))
source(file.path("r", "functions", "table_utility_functions.R"))
source(file.path("r", "table_setup.R"))

source(file.path("r", "functions", "formatting.R"))

part <- part[!is.na(study_period)]

study_periods <- part[, .(min = format(min(date), "%d %b %y"), 
                          max = format(max(date), "%d %b %y")), 
                      by =  "study_period"]
study_periods <- transpose(study_periods, make.names = "study_period")
study_periods[, description := "Dates"]
study_periods[, sample_type := "All"]
study_periods[, value := c("Start", "End")]

expressions <- list(
  list(desc_name = "All", byvars = c("study_period")),
  list(desc_name = "All sample type", byvars = c("sample_type", "study_period")),
  list(desc_name = "Gender", byvars = c("part_gender", "study_period")),
  # list(desc_name = "Age Group", byvars = c("part_age_group", "study_period")),
  list(desc_name = "Age Group (Children)", pexp = expression(sample_type == "child"),
       byvars = c("part_age_group", "study_period")),
  list(desc_name = "Age Group (Adults)", pexp = expression(sample_type == "adult"),
       byvars = c("part_age_group", "study_period")),
  list(desc_name = "NHS Region", byvars = c("nhs_region", "study_period")),
  list(desc_name = "Household Size", byvars = c("hh_size_group", "study_period")),
  list(desc_name = "Social Group", byvars = c("part_social_group", "study_period"))
)

summ_list <- list()
i <- 0
study_periods_  <- unique(part$study_period)
for(study_period_ in study_periods_) {
  message(study_period_)
  
  for(exp in expressions){
    # if(exp$desc_name == "Age Group (Children)") browser()
    message(exp$desc_name)
    if (is.null(exp$pexp)) exp$pexp <- expression(TRUE)
    pexp <- part[eval(exp$pexp)]
    puids <- unique(pexp[study_period_ == study_period,
           .(puid = first(part_wave_uid)), by = "part_uid"]$puid)
    pexp <- pexp[part_wave_uid %in% puids]
  
    summ_exp <- pexp[, .(part_n = .N), by = c(exp$byvars)]
    summ_exp[, description := exp$desc_name]
    
    if (!exp$desc_name %in% c("All", "All sample type")) {
      key_byvar <- exp$byvars[1]
      summ_exp[, variable := key_byvar]
      setnames(summ_exp, old = key_byvar, new = "value")
    }
    
    i = i + 1
    summ_list[[paste(study_period_, i)]] <- summ_exp
    
  }
}

summary_dt <- rbindlist(summ_list , fill = TRUE)
all <- unique(summary_dt[description %in% c("All", "All sample type")])
summary_dt[grepl("Children", description), sample_type := "child"]
summary_dt[grepl("Adults", description), sample_type := "adult"]
setnames(all, "part_n", "total_n")
summary_dt <- merge(summary_dt, 
                    all[, list(sample_type, study_period, total_n)], 
                    by = c("study_period", "sample_type"), all = T)

# Only use known genders in percentages
summary_dt[grepl("Other", value) & variable == "part_gender_nb", remove_n := sum(part_n), 
           by = c("study_period", "sample_type")]
# Only use known ages in percentages
summary_dt[grepl("Unknown", value) & variable == "part_age_group", remove_n := sum(part_n), 
           by = c("study_period", "sample_type")]
summary_dt[, remove_n := max(remove_n, na.rm = T), by = c("study_period", "sample_type", "variable")]


summary_dt[!is.na(remove_n), total_n := total_n - remove_n]



summary_dt[is.na(sample_type), sample_type := "All"]


summary_dt[, sample_type := to_title_case(sample_type)]

summary_dt[!description %in% c("All", "All sample type") & !value %like% "Unknown age|Other",
           summary := paste0(part_n, " ", "(", 
                             (formatC(part_n/total_n * 100, digits = 1, format = "f")),
                             "%)")]
summary_dt[description %in% c("All", "All sample type") | value %like% "Unknown age|Other", 
           summary := part_n]
summary_dt[description == "All", sample_type := "All"]
summary_dt[description == "Age Group (Children)" & value == "Unknown age", 
           value := "Unknown age*"]

table_one_demo <- dcast(description + sample_type + value ~ study_period, data = summary_dt,
                        value.var = "summary")
table_one_demo[description %in% c("All", "All sample type"), value := "-"]
cols <- names(table_one_demo)

table_one_demo[, (cols) := lapply(.SD, replace_na), .SDcols = cols]
table_one_demo <- rbind(table_one_demo, study_periods)

table_one_demo[, description := factor(description, table_group_levels)]
table_one_demo[, value := factor(as.character(value), 
                                 levels = table_category_levels)]



table_one_demo <- table_one_demo[order(value)]

setnames(table_one_demo, 
         old = c("description", "value", "sample_type"),
         new = c("Group", "Value", "Sample type"))

# col_order <- c("Group", "Sample type", "Value", "Phase 1", "Phase 2A", "Phase 2B", 
#                "Christmas", "Latest wave")
# col_order <- c("Group", "Value", "Sample type", "Lockdown 1", "Reduced restrictions", 
  # "Lockdown 2", "Christmas", "Lockdown 3")
dput(names(table_one_demo))
col_order <- c("Group", "Sample type", "Value", "Initial recruitment", 
               "Second recruitment", "New year")
table_one_demo <- table_one_demo[, col_order, with = F]
table_one_demo
write.csv(format(table_one_demo),
          file.path("outputs", "table_one_demographics_study_periods.csv"),
          row.names = FALSE)
write.xlsx(format(table_one_demo), 
           file.path("outputs", "table_one_demographics_study_periods.xlsx"))

