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

# Source user written scripts ---------------------------------------------

## Only works for the subset in this script will need adapting to do more
source('r/functions/bs_group.R')

# Load participant data ---------------------------------------------------
p1 <- qs::qread('data/dt_1w.qs')
pdt <- qs::qread('data/dt_2w_weighted.qs')
pdt[, wfp_final := genderageweight_raw * genderageweight_proportion]

p1  <-  p1[!area %in% c("Scotland", "Northern Ireland", "Wales")]
pdt <- pdt[!area %in% c("Scotland", "Northern Ireland", "Wales")]

dates <- readxl::read_xlsx(file.path("data", "table1_dates.xlsx"), sheet = 2)

pdt[, study_period := NA_character_]
pdt[, study_number := NA_integer_]
for(i in 1:9) {

  pdt[is.na(study_period), 
      study_period := ifelse(between(mid_date, dates$start_date[i], dates$end_date[i]), dates$study_period[i], NA_character_)]
  pdt[is.na(study_number), 
      study_number := ifelse(between(mid_date, dates$start_date[i], dates$end_date[i]), dates$number[i], NA_integer_)]
}

pdt[, study_period := factor(study_period, levels = dates$study_period)]
table(pdt$study_period, pdt$study_number, useNA = "always")


var(p1$n_cnt)
summary(p1$n_cnt)

by( pdt$n_cnt, pdt$study_period, var)

by(pdt$n_cnt, pdt$study_period , summary)



gam_wrap <- function(outcome, link = "log", data) {
  gam(get(outcome) ~ study_period + s(part_id, bs = "re"), data = data, 
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

# dput(sort(unique(pdt$part_age_group)))
part_age_groups <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", 
  "60-69", "70-120")
part_age_labs <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", 
                     "60-69", "70+")

mods <- list()

for (i in 1:length(part_age_groups)){
  # i <- 1
  age_group <- part_age_groups[i]
  pdtage <- pdt[as.character(part_age_group) == age_group]
  mod_subset <- gam_wrap("n_cnt", link = "log", data = pdtage)
  mod_ci <- ci_gam(mod_subset)
  mod_ci[, mod := age_group]
  mods[[i]] <- mod_ci
}

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

m_periods[, term := factor(m_periods$term)]
m_periods[, mod := factor(mod, levels = part_age_groups, labels = part_age_labs)]
table(m_periods$mod)
full <- data.frame(mod = m_periods$mod,
                   term = "Lockdown 1", rr = 1, lci = 1, uci = 1)


m_periods <- rbind(m_periods, full)
m_periods[, term := factor(term, levels = dates$study_period)]
ggthemr("grape")
cols <- c("#17877b", "#055a8c", "#D7402B", "#daa520", "#20bdcc", "#010f5b", "#d72638", "#03c04a", "#60100b")
scales::show_col(cols)

m_periods[, sample_type := ifelse(as.character(mod) %in% c("0-4", "5-11", "12-17"), "child", "adult")]
avg_conts_rr <- ggplot(m_periods[sample_type == "adult"],
                       aes(x = mod, y = rr, color = term)) +
  geom_hline(yintercept = 1, color = '#f79b57') +
  geom_point(shape = 15, size  = 1.5, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lci,
                    ymax = uci),
                width = 0.05,
                size  = 0.8,
                position = position_dodge(width = 0.9)) +
  scale_color_manual(values = cols, aesthetics = "color") +
  xlab("Participant age group") +
  ylab("Relative difference") +
  labs(color = "Lockdown phase") +
  
  ggtitle("A. Contacts trimmed to 100") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 9))

avg_conts_rr


m_periods[is.na(mod), mod := "70+"]
avg_conts_rr2 <- ggplot(m_periods[sample_type == "adult"],
                       aes(x = term, y = rr, color = mod)) +
  geom_hline(yintercept = 1, color = '#f79b57') +
  geom_point(shape = 15, size  = 1.5, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lci,
                    ymax = uci),
                width = 0.05,
                size  = 0.8,
                position = position_dodge(width = 0.9)) +
  facet_grid(vars(mod)) +
  scale_color_manual(values = cols, aesthetics = "color") +
  xlab("Participant age group") +
  ylab("Relative difference") +
  labs(color = "Lockdown phase") +
  
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )

avg_conts_rr2

avg_conts_rr2_children <- ggplot(m_periods[sample_type == "child"],
                        aes(x = term, y = rr, color = mod)) +
  geom_hline(yintercept = 1, color = '#f79b57') +
  geom_point(shape = 15, size  = 1.5, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lci,
                    ymax = uci),
                width = 0.05,
                size  = 0.8,
                position = position_dodge(width = 0.9)) +
  facet_grid(vars(mod)) +
  scale_color_manual(values = cols, aesthetics = "color") +
  xlab("Participant age group") +
  ylab("Relative difference") +
  labs(color = "Lockdown phase") +
  
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )
avg_conts_rr2_children

dggsave(avg_conts_rr2, filename = file.path("outputs", "cnt_rr_by_age_adult.png"), 
       width = 9, height = 8)

ggsave(avg_conts_rr2_children, filename = file.path("outputs", "cnt_rr_by_age_children.png"), 
       width = 9, height = 5)
