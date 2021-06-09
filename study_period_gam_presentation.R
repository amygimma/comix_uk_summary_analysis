## Name: 
## Description:

## Input file: dt_1w and dt_2w
## Functions: 
## Output file: 
set.seed(23032021)
# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
library(ggplot2)


# Load participant data and set variables -----------------------------------
# files <- list.files("data")
# files <- grep("mod_cis_", files, value = T)
cis_filename <- "mod_cis_study_period_re-part_id.qs" 
mod_filename <- "mod_raw_study_period_re-part_id.qs" 


cis <- qs::qread(file.path("data", cis_filename))
mod_raw <- qs::qread(file.path("data", mod_filename))
dates <- readxl::read_xlsx(file.path("data", "table1_dates.xlsx"), sheet = 2)
study_periods <- dates$study_period
part_age_groups <- c("0-4", "5-17", "18-59", "60+")

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
m_periods[, term := factor(term, levels = study_periods)]
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
  facet_grid(vars(sample_type), scales = "free_y") +
  scale_color_manual(values = cols, aesthetics = "color") +
  xlab("Study period") +
  ylab("Relative difference") +
  labs(color = "Participant age") +
  ylim(c(0.75, NA)) +
  theme(plot.title = element_text(size = 10, face = "bold"),
        legend.position = c(0.075, 0.85),
        legend.title = element_text(size = 10, face = ),
        panel.spacing.y =  unit(1, "lines"),
        panel.grid.major.x = element_line(color= "grey", size = 0.25),
        panel.grid.major.y = element_line(color= "grey", size = 0.25),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust=1),
        # axis.line.x = element_line(color = "blue", size = 2),
        panel.border = element_rect(color = "black", size = 0.75)
  )
avg_conts_rr2_facet

ggsave(avg_conts_rr2_facet, 
       filename = file.path("outputs", gsub("\\.qs", "_plot_presentation\\.png", cis_filename)),
       width = 10, height = 5.5)

