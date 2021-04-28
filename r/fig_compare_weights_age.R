## Name: fig1_timelines
## Description: Show when people responded to the survey

## Input file: bs_means_2w.qs
## Functions: none
## Output file: outputs/fig1v3.png

# Packages ----------------------------------------------------------------
library(data.table)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemr)
library(ggthemes)
# source("r/functions/ggthemr_workaround.R")

theme_set(cowplot::theme_cowplot(font_size = 11) + theme(strip.background = element_blank()))
cols <- c("#055a8c", "#d72638", "#17877b", "#daa520", "#20bdcc", "#010f5b")

# Load participant data ---------------------------------------------------
file_path <- file.path("data", "bs_means_2w.qs")
dt <- qs::qread(file_path)
message(paste("read from:", file_path))
# cases <- fread(file.path("data", "epiforecasts_cases_hospitilizations.csv"))

nrow(dt)
dt <- dt[part_age_group %in% c("0-4", "5-17", "18-59", "60+")]
table(dt$boots)


# Create folder for plots -------------------------------------------------
plotdate <- max(dt$end_date)
dir.create(paste0("outputs/", plotdate), showWarnings = FALSE, recursive = TRUE)


# create mapping ----------------------------------------------------------
dt[, table(part_age_group)]

# Recode factors to be in order -------------------------------------------
age_levs <- c("0-4", "5-17", "18-59", "60+")
age_labs <- c("0-4", "5-17", "18-59", "60+")
dt[, part_age_group := factor(part_age_group, levels = age_levs, labels = age_labs)]
dt[, setting := 
     factor(setting, 
            levels = 
              c("All", "Home", "Work", "Work/Educ", "Other",
                "All_genderage", "Home_genderage", "Work_genderage", 
                "Work/Educ_genderage", "Other_genderage"))]

# 
# dt[, part_social_group_lab := factor(part_social_group,
#                                      levels = c("All", 
#                                                 "A - Upper middle class", 
#                                                 "B - Middle class", 
#                                                 "C1 - Lower middle class", 
#                                                 "C2 - Skilled working class",
#                                                 "D - Working class",
#                                                 "E - Lower level of subsistence"),
#                                      labels = c("All", "A", "B", "C1", "C2", "D", "E"))]
# 

table(dt$part_att_serious_bin)

# Set plot parameters -----------------------------------------------------
expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))


# Create plotting function ------------------------------------------------
plot_mean <- function(dt, time_break = "2 month", upper_limit = 6, 
                      cols_ = c("#055a8c", "#d72638", "#17877b", "#daa520", "#20bdcc", "#010f5b")){
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, group = interaction(part_age_group, setting), fill = part_age_group), alpha = 0.2) +
    geom_line( aes(y = mean, linetype = setting, col = part_age_group)) +
    scale_color_manual(values = cols_) +
    scale_fill_manual(values = cols_) +
    # facet_grid(vars(setting)) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    # expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b '%y", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.1,0.7)
    ) +
    scale_linetype_manual(name = "Age (years)", values = c(1,2,3, 4)) +
    annotate("rect", 
             xmin = study_dates[1], xmax = study_dates[2],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[3], xmax = study_dates[4],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect", 
             xmin = study_dates[7], xmax = study_dates[8],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    labs(color = "Age (years)", fill = "Age (years)")
  
}


# Plot A overall -------------------------------------------------------------
all_age <- dt[
  part_age_group %in% c("0-4", "5-17", "18-59", "60+") &
  part_social_group == "All" &
  part_high_risk == "All" &
    setting %in% c("All", "All_genderage")
] 


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

second_recruitment_date <- "2020-08-09"

contacts_p <- plot_mean(all_age, time_break = "month", upper_limit = 20) 
# annotate("text", x = as.Date("2020-05-01"), y = 19.5, label = "Lockdown 1 (LD 1)") +
# annotate("text", x = as.Date("2020-11-15"), y = 19.5, label = "LD 2") +
# annotate("text", x = as.Date("2021-01-30"), y = 19.5, label = "LD 3") +

contacts_p



