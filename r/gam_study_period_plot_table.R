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


# Load participant data and set variables -----------------------------------
# files <- list.files("data")
# files <- grep("mod_cis_", files, value = T)
cis_filename <- "mod_cis_study_period_re-part_id.qs" 
mod_filename <- "mod_raw_study_period_re-part_id.qs" 


cis <- qs::qread(file.path("data", cis_filename))
mod_raw <- qs::qread(file.path("data", mod_filename))

mod_raw[[3]]$formula

# Check model

# summary(mod_raw[[3]])
gam.check(mod_raw[[3]])
# 
# # devtools::install_github("m-clark/visibly")
# library(visibly)
# mod_raw[[3]][[1]] 
# visibly::plot_gam(mod_raw[[3]], main_var = study_period)
# visibly::plot_gam_check(mod_raw_s[[3]])
# 
  
  
m_periods <- cis[grepl("period", coef),
                 .(
                   mod,
                   term = gsub("study_period", "", coef),
                   rr = exp(est),
                   lci = exp(lci),
                   uci = exp(uci)
                 )
]
full <- data.frame(mod = unique(m_periods$mod),
                     term = "Lockdown 1", rr = 1, lci = 1, uci = 1)
# Format output dt-----------------
m_periods[, term := factor(m_periods$term)]
m_periods[, mod := factor(mod, levels = part_age_groups, labels = part_age_labs)]
m_periods <- rbind(m_periods, full)
m_periods[, term := factor(term, levels = dates$study_period)]
m_periods[, sample_type := ifelse(as.character(mod) %in% c("0-4", "5-17"), "Children", "Adults")]
m_periods[, sample_type := factor(sample_type, levels = c("Adults", "Children"))]
m_periods[, mod := factor(mod, levels = part_age_groups)]
  
  
# Plot output ------------------
avg_conts_rr2_facet <- ggplot(m_periods[],
                              aes(x = term, y = rr, color = mod)) +
  geom_hline(yintercept = 1, color = '#f79b57') +
  geom_point(shape = 15, size  = 1.5, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lci,
                    ymax = uci),
                width = 0.05,
                size  = 0.8,
                position = position_dodge(width = 0.3)) +
  facet_grid(vars(sample_type)) +
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


rd_table <- m_periods

rd_table[, val_summary := paste0(
  formatC(rr, digits = 2, format = "f"), 
  " (",
  formatC(lci, digits = 2, format = "f"), 
  " to ",
  formatC(uci, digits = 2, format = "f"),
  ")")
]
rd_table[term == "Lockdown 1", val_summary := "ref"]
rd_table[, mod := paste("Ages", mod, "years")]

rd_wide <- dcast(data = rd_table, term ~ mod)
setnames(rd_wide, c("term"), c("Study period"))
rd_wide


write.csv(rd_wide, "outputs/relative_difference_table.csv")

# 
# unique(m_periods$term)
# 
# sp <- m_periods[term == "Lockdown 2 easing"]
# 
# spbase <- sprintf("During %s the relative difference in contacts for %s ages %s years was %s (95%% CI %s to %s).", 
#         sp$term, 
#         tolower(sp$sample_type), 
#         sp$mod, 
#         formatC(sp$rr, digits = 2, format = "f"), 
#         formatC(sp$lci, digits = 2, format = "f"), 
#         formatC(sp$uci, digits = 2, format = "f")
# )
# 
# spbase
# 
# spbase2 <- sprintf("The relative difference in contacts during the %s period was %s (95%% CI %s to %s) for %s ages %s years.", 
#                 sp$term, 
#                 formatC(sp$rr, digits = 2, format = "f"), 
#                 formatC(sp$lci, digits = 2, format = "f"), 
#                 formatC(sp$uci, digits = 2, format = "f"),
#                 tolower(sp$sample_type), 
#                 sp$mod
# 
# )
#         
# spbase2
# 
