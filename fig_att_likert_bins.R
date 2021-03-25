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
plot_mean_by_var <- function(dt, var, guide_lab, time_break = "2 month", upper_limit = 6){
  # browser()
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, group = get(var), fill = get(var)), alpha = 0.3) +
    geom_line( aes(y = mean, linetype = get(var), color = get(var))) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.025, 0.65)
    ) +
    scale_linetype_manual(name = guide_lab, values = c(1,2,3, 4)) +
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
             ymin = 0, ymax = upper_limit, alpha = .1) +
    labs(color = guide_lab, fill = guide_lab) 
    
}


# Plot  -------------------------------------------------------------


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

upper_lim <- 10
ylabel <- upper_lim - 0.75
timeline_size <- 3


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

likely_lab <- "I am likely to catch coronavirus"
att_likely_p <- 
  plot_mean_by_var(all_likely, 
                            var = "part_att_likely_bin", 
                            guide_lab = likely_lab,
                            time_break = "month", 
                            upper_limit = upper_lim) +
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) +
  annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) +
  annotate("text", x = as.Date("2020-08-15"), y = ylabel, label = "Reduced restrictions", size = timeline_size) + 
  ggtitle("A") 

att_likely_p 




all_spread <- dt[
  part_age_group == "All-adults" &
    part_social_group == "All" &
    part_high_risk == "All" &
    setting == "All" &
    part_att_spread_bin != "All"  &
    part_att_likely_bin == "All" &
    part_att_serious_bin == "All"
] 
spread_lab <- "I am worried that I might spread coronavirus to someone who is vulnerable"
att_spread_p <- 
  plot_mean_by_var(all_spread , 
                            var = "part_att_spread_bin", 
                            guide_lab = spread_lab,
                            time_break = "month", 
                            upper_limit = upper_lim) +
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) +
  annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) +
  annotate("text", x = as.Date("2020-08-15"), y = ylabel, label = "Reduced restrictions", size = timeline_size) + 
  ggtitle("B") 
att_spread_p 


all_serious <- dt[
  part_age_group == "All-adults" &
    part_social_group == "All" &
    part_high_risk == "All" &
    setting == "All" &
    part_att_spread_bin == "All"  &
    part_att_likely_bin == "All" &
    part_att_serious_bin != "All"
] 
serious_lab <- "Coronavirus would be a serious illness for me"
att_serious_p <- 
  plot_mean_by_var(all_serious , 
                            var = "part_att_serious_bin", 
                            guide_lab = serious_lab,
                            time_break = "month", 
                            upper_limit = upper_lim) +
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) +
  annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) +
  annotate("text", x = as.Date("2020-08-15"), y = ylabel, label = "Reduced restrictions", size = timeline_size) + 
  ggtitle("C")
att_serious_p 

likert_plots <- (att_likely_p / att_spread_p / att_serious_p) + patchwork::plot_layout()

ggsave(plot = likert_plots, filename = "outputs/likert_plots.png", 
       height = 9, width = 7)
  
  
