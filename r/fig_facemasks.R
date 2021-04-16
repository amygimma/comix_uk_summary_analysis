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
file_path <- file.path("data", paste("bs_proportions_2w.qs", sep = "_"))
dt <- qs::qread(file_path)
message(paste("read from:", file_path))
dt[,participants := ifelse(has_non_hh_contacts == F, "All", "With non-household contacts")]

expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))


# Create plotting function ------------------------------------------------
plot_fm_prop <- function(dt, time_break = "2 month", upper_limit = 1){
  # browser()
  cols <- c("#d72638", "#055a8c", "#17877b")
 
  col_manual <- cols
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, 
                    group = interaction(participants, part_age_group),
                    fill = part_age_group), alpha = 0.3) +
    geom_line( aes(y = mean, linetype = participants, 
                   color = part_age_group)) +
    labs(title = "", y = "Proportion of participants", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.5, 0.25)
    ) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_linetype_manual(name = "Participants", values = c(2,1)) +
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
             # ymin = 0, ymax = upper_limit, alpha = .1) +
    labs(color = "Age", fill = "Age") 
  
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

upper_lim <- 1
ylabel <- upper_lim - 0.05
timeline_size <- 3



fm_label <- "Face mask"
fm_plot <- 
  plot_fm_prop(dt, time_break = "2 months", upper_limit = upper_lim) +
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) 
  # annotate("text", x = as.Date("2020-08-15"), y = ylabel, label = "Reduced restrictions", size = timeline_size) + 
  # ggtitle("A") 

fm_plot

ggsave(plot = fm_plot, filename = "outputs/face_mask_plot.png", width = 7, height = 4)


dtl <- dt[has_non_hh_contacts == TRUE & mid_date >= "2020-08-01"]
by(dtl$mean, dtl$part_age_group, summary)


