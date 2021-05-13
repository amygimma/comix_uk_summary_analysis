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


# >Load model and CI dt ---------------------------------------------
grep("mod_raw", list.files("data"), value = T)

mod_raw <- qs::qread("data/mod_raw_factor_by_part_re.qs")
cis <- qs::qread("data/mod_cis_factor_by_part_re.qs" )

# Check model ----------------------------------

mod_raw[[1]]$formula
mod_raw[[1]]$method
gam.check(mod_raw[[3]])
mgcv::logLik.gam(mod_raw[[3]])
summary(mod_raw[[3]])
# mgcv:::summary.gam(mod_raw[[3]])
# summary(mod_raw[[3]])


# mgcv:::summary.gam(mod_raw[[3]])
# summary(mod_raw[[3]])

# devtools::install_github("m-clark/visibly")
# library(visibly)
# mod_raw[[3]][[1]]
# visibly::plot_gam(mod_raw[[3]], main_var = study_period)
# visibly::plot_gam_check(mod_raw[[3]])
# 
# plot(mod_raw[[2]], shade = TRUE, pages = 1, scale = 0)



# Create data.table for plot  -------------------------------------------------

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


# cols <- c("#17877b", "#055a8c", "#D7402B", "#daa520", "#20bdcc", "#010f5b", "#d72638", "grey", "darkgrey")
cols <- c("#17877b", "#D7402B", "#055a8c", "#daa520")

m_periods[, sample_type := ifelse(as.character(mod) %in% c("0-4", "5-17"), "Children", "Adults")]
m_periods[, sample_type := factor(sample_type, levels = c("Adults", "Children"))]
part_age_groups <- c("0-4", "5-17", "18-59", '60+')
m_periods[, mod := factor(mod, levels = part_age_groups)]

avg_conts_rr2_facet <- ggplot(m_periods,
                              aes(x = term, y = rr, color = mod)) +
  geom_hline(yintercept = 1, color = '#f79b57') +
  geom_point(shape = 15, size  = 1.5, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lci,
                    ymax = uci),
                width = 0.05,
                size  = 0.8,
                position = position_dodge(width = 0.9)) +
  facet_grid(vars(sample_type), scales = "free_y") +
  scale_color_manual(values = cols, aesthetics = "color") +
  xlab("Study period") +
  ylab("Relative difference") +
  labs(color = "Participant age group") +
  
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  )
avg_conts_rr2_facet


# ggsave(avg_conts_rr2, filename = file.path("outputs", "cnt_rr_by_age_adult.png"), 
#        width = 9, height = 8)
# 
# ggsave(avg_conts_rr2_children, filename = file.path("outputs", "cnt_rr_by_age_children.png"), 
#        width = 9, height = 5)
# ggsave(avg_conts_rr2_facet, filename = file.path("outputs", "cnt_rr_by_age_adult.png"), 
       # width = 9, height = 8)


# # Re-create ci dt (use with caution - confirm model and age groups)
# 
# part_age_groups <- c("0-4", "5-17", "18-59", "60+")
# mod_raw[[1]]$formula
# 
# 
# mod_ci <- list()
# for (i in 1:length(part_age_groups)){
#   # i <- 1
#   age_group <- part_age_groups[i]
#   message(age_group)
# 
#   mod_subset <- mod_raw[[i]]
# 
#   mod_ci <- ci_gam(mod_subset)
#   mod_ci[, mod := age_group]
#   mods[[i]] <- mod_ci
# }

