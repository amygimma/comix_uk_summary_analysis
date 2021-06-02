## Name: 
## Description:

## Input file: dt_1w and dt_2w
## Functions: 
## Output file: 
set.seed(23032021)
# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
library(mgcv)
library(ggplot2)


# Load participant data ---------------------------------------------------
p1 <- qs::qread('data/dt_1w.qs')
pdt <- qs::qread('data/dt_2w_weighted.qs')

p1  <-  p1[!area %in% c("Scotland", "Northern Ireland", "Wales")]
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
var(p1$n_cnt)
summary(p1$n_cnt)
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
  # list(form = formula(n_cnt ~ study_period),
  #      mod_ref = "study_period"),
  list(form = formula(n_cnt ~ study_period + s(part_id, bs = "re")),
       mod_ref = "study_period_re-part_id")
  # list(form = formula(n_cnt ~ study_period + s(part_id, by = study_period, bs = "re")),
  #      mod_ref = "study_period_re-part_id-by-study_period"),
  # list(form = formula(n_cnt ~ study_period + s(study_period, bs = "fs") + s(part_id, bs = "re")),
  #      mod_ref = "sp__s-study_period-fs__re-part_id-by-study_period"),
  # list(form = formula(n_cnt ~ study_period + s(study_period, bs = "fs")),
  #      mod_ref = "sp__s-study_period-fs"),
  # list(form = formula(n_cnt ~ study_period + s(study_period, bs = "fs")),
  #      mod_ref = "study_period__re-part_id-by-study_period")
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
    # browser()
    
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

# Check model

# plot(mod_raw[[3]], shade = TRUE, pages = 1, scale = 0)
# summary(mod_raw[[3]])
# gam.check(mod_raw[[3]])
# 
# # devtools::install_github("m-clark/visibly")
# library(visibly)
# mod_raw[[3]][[1]] 
# visibly::plot_gam(mod_raw[[3]], main_var = study_period)
# visibly::plot_gam_check(mod_raw_s[[3]])
# 
# Plot output ------------------
# avg_conts_rr2_facet <- ggplot(m_periods[],
#                               aes(x = term, y = rr, color = mod)) +
#   geom_hline(yintercept = 1, color = '#f79b57') +
#   geom_point(shape = 15, size  = 1.5, position = position_dodge(width = 0.3)) +
#   geom_errorbar(aes(ymin = lci,
#                     ymax = uci),
#                 width = 0.05,
#                 size  = 0.8,
#                 position = position_dodge(width = 0.3)) +
#   facet_grid(vars(sample_type), scales = "free_y") +
#   scale_color_manual(values = cols, aesthetics = "color") +
#   xlab("Study period") +
#   ylab("Relative difference") +
#   labs(color = "Participant age group") +
#   
#   theme_bw() +
#   theme(plot.title = element_text(size = 10, face = "bold"),
#         axis.title.x = element_text(size = 9),
#         axis.title.y = element_text(size = 9),
#         legend.title = element_text(size = 9),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
#   )
# 
# # Save files
# plot_fname <- paste0("cnt_rr_mod_", mod_ref, ".png")
# ggsave(avg_conts_rr2_facet, filename = file.path("outputs", plot_fname), 
#        width = 9, height = 8)
# message(plot_fname)