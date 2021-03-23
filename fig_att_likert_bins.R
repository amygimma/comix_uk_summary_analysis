## Name: fig1_timelines
## Description: Show when people responded to the survey

## Input file: dt_1w and dt_2w
## Functions: bs_group
## Output file: 2021-03-05_bs_means_2w.qs

# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
library(ggplot2)
library(patchwork)


theme_set(cowplot::theme_cowplot(font_size = 11) + theme(strip.background = element_blank()))

# Load participant data ---------------------------------------------------
sys_date <- Sys.Date() -1
file_path <- file.path("data", paste(sys_date, "bs_means_2w.qs", sep = "_"))
dt <- qs::qread(file_path)
message(paste("read from:", file_path))

likert_bin_levels <- c("All", "Agree", "Disagree", "Neutral")

dt[, part_att_spread_bin := factor(part_att_spread_bin, levels = likert_bin_levels)]
dt[, part_att_likely_bin := factor(part_att_likely_bin, levels = likert_bin_levels)]
dt[, part_att_serious_bin := factor(part_att_serious_bin, levels = likert_bin_levels)]

# dt_likely <- dt[part_att_likely_bin %in% likert_bin_levels]

expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))


# Create plotting function ------------------------------------------------
plot_mean_att_likert_bins <- function(dt, time_break = "2 month", upper_limit = 6){
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, group = part_att_likely_bin, fill = part_att_likely_bin), alpha = 0.3) +
    geom_line( aes(y = mean, linetype = part_att_likely_bin, col = part_att_likely_bin)) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.1,0.8)
    ) +
    scale_linetype_manual(name = "part_att_likely_bin", values = c(1,2,3, 4)) +
    # geom_vline(aes(xintercept = study_dates[1]), linetype = "dashed",  alpha = 0.3) +
    # geom_vline(aes(xintercept = study_dates[2]), linetype = "dashed",  alpha = 0.3) +
    # geom_vline(aes(xintercept = study_dates[3]), linetype = "dashed",  alpha = 0.3) +
    # geom_vline(aes(xintercept = study_dates[4]), linetype = "dashed",  alpha = 0.3) +
    # geom_vline(aes(xintercept = study_dates[5]), linetype = "dashed",  alpha = 0.3) +
    # geom_vline(aes(xintercept = study_dates[6]), linetype = "dashed",  alpha = 0.3) +
    # geom_vline(aes(xintercept = study_dates[7]), linetype = "dashed",  alpha = 0.3) +
    # geom_vline(aes(xintercept = study_dates[8]), linetype = "dashed",  alpha = 0.3) +
    # geom_vline(aes(xintercept = study_dates[9]), linetype = "dashed",  alpha = 0.3) +
    # geom_vline(aes(xintercept = study_dates[10]), linetype = "dashed", alpha = 0.3) +
    annotate("rect", 
             xmin = study_dates[1], xmax = study_dates[2],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[3], xmax = study_dates[4],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[5], xmax = study_dates[6],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[7], xmax = study_dates[8],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[9], xmax = study_dates[10],
             ymin = 0, ymax = upper_limit, alpha = .1)  
}


# Plot A overall -------------------------------------------------------------
all_likely <- dt[
  part_age_group == "All-adults" &
  part_social_group == "All" &
  part_high_risk == "All" &
  setting == "All" &
  part_att_spread_bin == "All"  &
  part_att_likely_bin != "All" &
    part_att_serious_bin == "All"
] 
table(all_likely$part_att_likely_bin)

study_dates <- as.Date(c(
  "2020-03-23",
  "2020-06-03",
  "2020-11-05",
  "2020-12-02",
  "2020-12-19",
  "2021-01-02",
  "2021-01-05",
  "2021-03-31", ## LD 3 ending may need updating
  "2020-07-30",
  "2020-09-02"
))

att_likely_p <- plot_mean_att_likert_bins(all_likely, time_break = "month", upper_limit = 20) +
  annotate("text", x = as.Date("2020-05-01"), y = 19.5, label = "Lockdown 1 (LD 1)") +
  annotate("text", x = as.Date("2020-11-15"), y = 19.5, label = "LD 2") +
  annotate("text", x = as.Date("2021-01-30"), y = 19.5, label = "LD 3") +
  annotate("text", x = as.Date("2020-12-22"), y = 19.5, label = "Christmas", angle = 0) +
  annotate("text", x = as.Date("2020-08-15"), y = 19.5, label = "Reduced\nrestrictions") + 
  ggtitle("A") +
  labs(subtitle = "Adults")

att_likely_p 

