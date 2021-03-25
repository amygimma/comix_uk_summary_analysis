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
library(RColorBrewer)

theme_set(cowplot::theme_cowplot(font_size = 11) + theme(strip.background = element_blank()))

# Load participant data ---------------------------------------------------
file_path <- file.path("data", paste("bs_proportions_2w.qs", sep = "_"))
dt <- qs::qread(file_path)
message(paste("read from:", file_path))


expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))


# Create plotting function ------------------------------------------------
plot_fm_prop <- function(dt, time_break = "2 month", upper_limit = 1){
  guide_lab <- "Age and non-household contacts"
  browser()
  cols <- c("#d72638", "#dc3a4b", "#17877b", "#1a9689")
  guide_labels <- c("Age 18-59 & All participants", "Age 18-59 & partcipants with contacts",
                    "Age 60+ & All participants", "Age 60+ & partcipants with contacts")
  col_manual <- cols
  names(col_manual) <- guide_labels
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, group = interaction(has_non_hh_contacts, part_age_group),
                    fill = interaction(has_non_hh_contacts, part_age_group)), alpha = 0.3) +
    geom_line( aes(y = mean, linetype = interaction(has_non_hh_contacts, part_age_group), 
                   color = interaction(has_non_hh_contacts, part_age_group))) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.025, 0.65)
    ) +
    scale_linetype_manual(name = guide_lab, values = c(1,2,3,4)) +
    scale_color_manual(name=guide_lab, values = guide_labels ) + 
    # scale_fill_brewer(palette = "Paired") +
    # scale_color_brewer(palette = "Paired") +
    scale_color_manual(values = cols) + 
    scale_fill_manual(values = cols) +
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
    labs(color = guide_lab, fill = guide_lab, linetype = guide_lab) 
  
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



fm_label <- "Face mask"
fm_plot <- 
  plot_fm_prop(dt,
               time_break = "month", 
               upper_limit = upper_lim) +
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) +
  annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) +
  # annotate("text", x = as.Date("2020-08-15"), y = ylabel, label = "Reduced restrictions", size = timeline_size) + 
  ggtitle("A") 

att_likely_p 

