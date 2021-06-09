## Name: gam_study_periods.R
## Description: Runs gam function with given formula on two-week weighted 
## contact means by study period

## Input file: dt_2w_weighted.qs
## Functions: gam_wrap, ci_gam
## Output file: "mod_cis_[MODEL_REFERENCE_NAME].qs"
set.seed(23032021)
# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
library(mgcv)
library(ggplot2)


# Load participant data ---------------------------------------------------
pdt <- qs::qread('data/dt_2w_weighted.qs')

pdt <- pdt[!area %in% c("Scotland", "Northern Ireland", "Wales")]

dates <- readxl::read_xlsx(file.path("data", "table1_dates.xlsx"), sheet = 2)
cols <- c("#17877b", "#D7402B", "#055a8c", "#daa520")


# Assign study periods and numbers ---------
pdt[, study_period := NA_character_]
pdt[, study_number := NA_integer_]

for(i in 1:9) {
  pdt[is.na(study_period), 
      study_period := ifelse(between(mid_date, dates$start_date[i], dates$end_date[i]),
                             dates$study_period[i], NA_character_)]
  pdt[is.na(study_number), 
      study_number := ifelse(between(mid_date, dates$start_date[i], dates$end_date[i]),
                             dates$number[i], NA_integer_)]
}
pdt[, study_period := factor(study_period, levels = dates$study_period)]
table(pdt$study_period, pdt$study_number, useNA = "always")

# study_age_groups -----------------
part_age_groups <- c("0-4", "5-17", "18-59", "60+")
part_age_labs <- part_age_groups 
pdt[, part_age_group := as.character(part_age_group)]
pdt[part_age_group %in% c("5-11", "12-17"), part_age_group := "5-17"]
pdt[part_age_group %in% c("18-29", "30-39", "40-49", "50-59"), part_age_group := "18-59"]
pdt[part_age_group %in% c("60-69", "70-120"), part_age_group := "60+"]
pdt[, part_age_group := factor(part_age_group, levels = c("0-4", "5-17", "18-59", "60+"))]
table(pdt$part_age_group, useNA = "always")

# Check data ---------
by( pdt$n_cnt, pdt$study_period, var)
by(pdt$n_cnt, pdt$study_period , summary)

# Functions ----------------------------
gam_wrap <- function(formula_, link = "log", data) {
      gam(formula_,
      data = data, 
      weights = genderageweight_proportion,
      family = nb(link = link))
}

ci_gam <- function(mod, level = 0.95) {
  msum <- mgcv:::summary.gam(mod)
  dt <- as.data.table(msum$p.table)
  dt <- dt[, 1:2]
  names(dt) <- c("est", "se")
  dt$coef <- row.names(msum$p.table)
  nu <- msum$residual.df
  
  dt[ , lci := est + se * qt(df = nu, p = (1 - level) / 2)]
  dt[ , uci := est + se * qt(df = nu, p = 1 - (1 - level) / 2)]
  dt
  return(dt)
}

# Formulas ------------------------------------------

formulas <- list(
  list(form = formula(n_cnt ~ study_period + s(part_id, bs = "re")),
       mod_ref = "study_period_re-part_id")
)

# Run models -----------------------------
for (j in 1:length(formulas)) {
  form_item <- formulas[[j]]
  # Set formula
  form <- form_item$form
  # set model reference for files
  mod_ref <- form_item$mod_ref
  message(mod_ref)
  
  # Run for each age group
  mods <- list()
  mod_raw <- list()
  for (i in 1:length(part_age_groups)){
    age_group <- part_age_groups[i]
    message(age_group)
    pdtage <- pdt[as.character(part_age_group) == age_group]
    mod_subset <- gam_wrap(form, link = "log", data = pdtage)
    mod_raw[[i]] <- mod_subset

    mod_ci <- ci_gam(mod_subset)
    mod_ci[, mod := age_group]
    mods[[i]] <- mod_ci
  }
  
  # Create output dt-----------
  cis <- rbindlist(mods)
  
  m_periods <- cis[grepl("period", coef),
                    .(
                      mod,
                      term = gsub("study_period", "", coef),
                      rr = exp(est),
                      lci = exp(lci),
                      uci = exp(uci)
                    )
  ]
  full <- data.frame(mod = m_periods$mod,
                     term = "Lockdown 1", rr = 1, lci = 1, uci = 1)
  m_periods <- rbind(m_periods, full)
  
  
  # Format output dt-----------------
  m_periods[, term := factor(m_periods$term)]
  m_periods[, mod := factor(mod, levels = part_age_groups, labels = part_age_labs)]
  m_periods[, term := factor(term, levels = dates$study_period)]
  m_periods[, sample_type := ifelse(as.character(mod) %in% c("0-4", "5-17"), "Children", "Adults")]
  m_periods[, sample_type := factor(sample_type, levels = c("Adults", "Children"))]
  m_periods[, mod := factor(mod, levels = part_age_groups)]

  mod_fname <- paste0("mod_raw_", mod_ref, ".qs")
  qs::qsave(mod_raw, file.path("data", mod_fname))
  message(mod_fname)

  cis_fname <- paste0("mod_cis_", mod_ref, ".qs")
  qs::qsave(cis, file.path("data", cis_fname))
  message(cis_fname)
}
