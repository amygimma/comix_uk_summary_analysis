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

cols <- c("#17877b", "#055a8c", "#D7402B", "#daa520", "#20bdcc", "#010f5b", "#d72638")

# Load participant data ---------------------------------------------------
file_path <- file.path("data", "bs_means_2w_risk.qs")
dt <- qs::qread(file_path)
dt[, part_age_group_lab := paste("Ages", part_age_group)]

dt <- dt[mid_date >= as.Date("2020-03-23") & mid_date <= as.Date("2021-03-26")]

message(paste("read from:", file_path))
table(dt$boots)
summary(dt[part_age_group %in% c("60+")]$n)
# hist(dt[part_age_group %in% c("60+")]$n)
# summary(dt[part_age_group %in% c("60+") & part_att_likely_bin == "Agree"]$n)
# summary(dt[part_age_group %in% c("60+") & part_att_serious_bin == "Disagree"]$n)

likert_bin_levels <- c("All", "Agree", "Neutral", "Disagree")

dt[, part_att_spread_bin := factor(part_att_spread_bin, levels = likert_bin_levels)]
dt[, part_att_likely_bin := factor(part_att_likely_bin, levels = likert_bin_levels)]
dt[, part_att_serious_bin := factor(part_att_serious_bin, levels = likert_bin_levels)]

# dt_likely <- dt[part_att_likely_bin %in% likert_bin_levels]

expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))
subplot_title_size <- 10


# Create plotting function ------------------------------------------------
plot_mean_by_var_age <- function(dt, var, guide_lab, time_break = "2 month", 
                             upper_limit = 6, cols_ = c("#d72638", "#055a8c", "#17877b")){
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, group = get(var), fill = get(var)), alpha = 0.3) +
    geom_line( aes(y = mean,   color = get(var))) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b '%y", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.775, 0.85),
      legend.title=element_text(size=9),
      legend.text=element_text(size=8),
      panel.grid.major = element_line(colour="grey", size=0.05),
      axis.text.x = element_text(size = 6.5)
    ) +
    scale_color_manual(values = cols_) +
    scale_fill_manual(values = cols_) +
    facet_grid(vars(part_age_group_lab)) +
    scale_linetype_manual(name = guide_lab, values = c(2,3, 4)) +
    annotate("rect", 
             xmin = study_dates[1], xmax = study_dates[2],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[3], xmax = study_dates[4],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    # annotate("rect", 
    #          xmin = study_dates[5], xmax = study_dates[6],
    #          ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[7], xmax = study_dates[8],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    # annotate("rect", 
    #          xmin = study_dates[9], xmax = study_dates[10],
    #          ymin = 0, ymax = upper_limit, alpha = .1) +
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

upper_lim <- 9
ylabel <- upper_lim - 0.35
timeline_size <- 2.5


all_likely <- dt[
  part_age_group %in% c("18-59", "60+") &
    part_social_group == "All" &
    part_high_risk == "All" &
    setting == "All_genderage" &
    part_att_spread_bin == "All"  &
    part_att_likely_bin != "All" &
    part_att_serious_bin == "All"
] 
# table(all_likely$part_att_likely_bin)

likely_lab <- "I am likely to catch coronavirus"
att_likely_p <- 
  plot_mean_by_var_age(all_likely, 
                   var = "part_att_likely_bin", 
                   guide_lab = "Response",
                   time_break = "month", 
                   upper_limit = upper_lim, 
                   cols_ = cols) +
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) +
  # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) +
  ggtitle(paste("A.", likely_lab)) +
  theme(plot.title = element_text(size = subplot_title_size))

att_likely_p 



# cols <- rev(c("#d72638", "#055a8c", "#17877b"))

all_spread <- dt[
  part_age_group %in% c("18-59", "60+") &
    part_social_group == "All" &
    part_high_risk == "All" &
    setting == "All_genderage" &
    part_att_spread_bin != "All"  &
    part_att_likely_bin == "All" &
    part_att_serious_bin == "All"
] 
spread_lab <- "I am worried that I might spread coronavirus to someone who is vulnerable"
att_spread_p <- 
  plot_mean_by_var_age(all_spread , 
                   var = "part_att_spread_bin", 
                   guide_lab = "Response",
                   time_break = "month", 
                   upper_limit = upper_lim, 
                   cols_ = cols
  ) +
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) +
  # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) +
  # annotate("text", x = as.Date("2020-08-15"), y = ylabel, label = "Reduced restrictions", size = timeline_size) + 
  ggtitle(paste("B.", spread_lab)) +
  theme(plot.title = element_text(size = subplot_title_size))
att_spread_p 


all_serious <- dt[
  part_age_group %in% c("18-59", "60+") &
    part_social_group == "All" &
    part_high_risk == "All" &
    setting == "All_genderage" &
    part_att_spread_bin == "All"  &
    part_att_likely_bin == "All" &
    part_att_serious_bin != "All"
] 
serious_lab <- "Coronavirus would be a serious illness for me"
att_serious_p <- 
  plot_mean_by_var_age(all_serious , 
                   var = "part_att_serious_bin", 
                   guide_lab = "Response",
                   time_break = "month", 
                   upper_limit = upper_lim, 
                   cols_ = cols) +
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) +
  # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) +
  # annotate("text", x = as.Date("2020-08-15"), y = ylabel, label = "Reduced restrictions", size = timeline_size) + 
  ggtitle(paste("C.", serious_lab)) +
  theme(plot.title = element_text(size = subplot_title_size))
att_serious_p 

likert_plots <- (att_likely_p / att_spread_p / att_serious_p) + patchwork::plot_layout()

likert_plots



ggsave(plot = likert_plots, filename = "outputs/likert_plots_age.png", 
       height = 9, width = 7)

plot_mean_age_by_var <- function(dt, var, guide_lab, time_break = "2 month", 
                                 upper_limit = 6, cols_ = c("#d72638", "#055a8c", "#17877b")){
  # browser()
  dt[, part_age_group_lab := paste("Ages", part_age_group)]
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, 
                    group = get(var), 
                    fill = get(var)), 
                alpha = 0.35) +
    geom_line( aes(y = mean,   
                   group = get(var),
                   color = get(var))) +
    facet_grid(rows = vars(part_age_group_lab)) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    scale_x_date(breaks = time_break, date_labels = "%b '%y", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.775, 0.85),
      legend.title = element_text(size=9),
      legend.text = element_text(size=8),
      panel.grid.major = element_line(colour="grey", size=0.05),
      axis.text.x = element_text(size = 6.5)
    ) +
    scale_color_manual(values = cols_) +
    scale_fill_manual(values = cols_) +
    annotate("rect", 
             xmin = study_dates[1], xmax = study_dates[2],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[3], xmax = study_dates[4],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    # annotate("rect", 
    #          xmin = study_dates[5], xmax = study_dates[6],
    #          ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[7], xmax = study_dates[8],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    labs(color = guide_lab, fill = guide_lab) 
  
}


hr_age <- dt[
  part_age_group %in% c("18-59", "60+") &
    part_high_risk != "All" &
    setting == "All_genderage" 
  
] 


upper_lim <- 9
ylabel <- upper_lim - 0.75
timeline_size <- 2.5

hr_age[, part_high_risk := 
         factor(part_high_risk, levels = c("yes", "no"), labels = c("Yes", "No"))]
hr_lab <- "High risk participant"
hr_p <- 
  plot_mean_age_by_var(hr_age , 
                       var = "part_high_risk", 
                       guide_lab = "Response",
                       time_break = "month", 
                       upper_limit = upper_lim,
                       cols_ = cols[c(2,3)]) +
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) +
  # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) +
  ggtitle(paste("D.", hr_lab)) +
  theme(plot.title = element_text(size = subplot_title_size))
hr_p

ggsave(plot = hr_p, filename = "outputs/hr_plots.png", 
       height = 5, width = 7)




risk_plots <- ((att_likely_p + att_spread_p) / (att_serious_p + hr_p)) + patchwork::plot_layout()
risk_plots

ggsave(risk_plots, filename = "outputs/risk_plots.png", height = 8, width = 12)

