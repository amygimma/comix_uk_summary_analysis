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

nrow(dt)
dt <- dt[part_age_group %in% c("0-4", "5-17", "18-59", "60+")]


# Create folder for plots -------------------------------------------------
plotdate <- max(dt$end_date)
dir.create(paste0("outputs/", plotdate), showWarnings = FALSE, recursive = TRUE)


# create mapping ----------------------------------------------------------
dt[, table(part_age_group)]

# Recode factors to be in order -------------------------------------------
age_levs <- c("0-4", "5-17", "18-59", "60+")
age_labs <- c("0-4", "5-17", "18-59", "60+")
dt[, part_age_group := factor(part_age_group, levels = age_levs, labels = age_labs)]
dt[, setting := factor(setting, levels = c("All", "Home", "Work/Educ", "Other"))]

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
    geom_ribbon(aes(ymin = lci, ymax = uci, group = part_age_group, fill = part_age_group), alpha = 0.2) +
    geom_line( aes(y = mean, linetype = part_age_group, col = part_age_group)) +
    scale_color_manual(values = cols_) +
    scale_fill_manual(values = cols_) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
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
    # part_age_group %in% c("18-59", "60+") &
    part_social_group == "All" & 
    part_high_risk == "All" &
    setting == "All"
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

contacts_p <- plot_mean(all_age, time_break = "month", upper_limit = 20) +
  annotate("text", x = as.Date("2020-05-01"), y = 19.5, label = "Lockdown 1 (LD 1)") +
  annotate("text", x = as.Date("2020-11-15"), y = 19.5, label = "LD 2") +
  annotate("text", x = as.Date("2021-01-30"), y = 19.5, label = "LD 3") +
  ggtitle("A") +
  labs(subtitle = "Adults") #+

contacts_p
# ggsave(contacts_p, filename = "outputs/fig_1A.png", width = 9, height = 4.5)


# Plot B: AdultContacts by setting ----------------------------------------------------
plot_mean_setting <- function(dt, time_break = "month", upper_limit = 6, 
                              cols_ = c("#055a8c", "#d72638", "#17877b", "#daa520", "#20bdcc", "#010f5b")){
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, group = part_age_group, fill = part_age_group), alpha = 0.3) +
    geom_line( aes(y = mean, linetype = part_age_group, col = part_age_group)) +
    scale_color_manual(values = cols_) +
    scale_fill_manual(values = cols_) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      # legend.position = c(0.1,0.9)
      legend.position = "top"
    ) +
    scale_linetype_manual(name = "Age (years)", values = c(1,2,3,4)) +
    annotate("rect",
             xmin = study_dates[1], xmax = study_dates[2],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect",
             xmin = study_dates[3], xmax = study_dates[4],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    annotate("rect",
             xmin = study_dates[7], xmax = study_dates[8],
             ymin = 0, ymax = upper_limit, alpha = .1) +
    facet_grid(setting ~ .,  scales = "free") +
    labs(color = "Age (years)", fill = "Age (years)")
}


# Plot B: All part age contacts by setting ----------------------------------------------------

all_setting <- dt[
  part_age_group %in% c("0-4", "5-17", "18-59", "60+") &
    part_gender == "All" &
    part_social_group == "All" & 
    part_high_risk == "All" &
    setting %in% c("Home", "Work/Educ", "Other")
] 

all_setting_p <- plot_mean_setting(all_setting, time_break = "month", upper_limit = 5) +
  ggtitle("B") 
# labs(subtitle = "Age (years)")
all_setting_p

# ggsave(all_setting_p, filename = "outputs/all_setting.png", height = 6, width = 9)

# Plot C: Count data ------------------------------------------------------

dt <- qs::qread('../comix/data/part_min.qs')
adults_id <- dt[#sample_type == "adult" &
  !area_3_name %in% c("Scotland", "Northern Ireland", "Wales") &
    country == "uk" &
    !survey_round %in% c(6,7),]$part_wave_uid
# 
sample_type_count <- dt[part_wave_uid %in% adults_id,
                        .(start_date = min(date), end_date = max(date), .N), by = .(sample_type, survey_round)]
sample_type_count[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round)]

mid_dates <- sample_type_count[, .(sample_type, survey_round, mid_date)]

sample_type_plot <- dcast(sample_type_count, formula = sample_type ~ mid_date, value.var = "N", fill = 0)

sample_type_count[, sample_type := ifelse(sample_type == "child", "Child", "Adult")]

sample_type_count[, sample_type := factor(sample_type)]


sample_type_count[, labmin := fifelse(mid_date %in% c(min(mid_date)),N, NA_integer_), by =(sample_type)]
sample_type_count[, labmax := fifelse(mid_date %in% c(max(mid_date)),N, NA_integer_), by =(sample_type)]
sample_type_count[, Recruitment := ifelse(survey_round < 20, "First", "Second")]
counts_all_p <- ggplot(sample_type_count) +
  geom_point(aes(y = sample_type, x = mid_date, size = N, color = Recruitment), alpha = 0.7) +
  geom_line(aes(y = sample_type, x = mid_date, color = Recruitment)) +
  geom_text(aes(y = sample_type, x = mid_date, label = labmin), nudge_x = - 10, nudge_y = 0.2, size = 3) +
  geom_text(aes(y = sample_type, x = mid_date, label = labmax), nudge_x = +10, nudge_y = 0.2,size = 3) +
  scale_color_manual(values = cols) +
  scale_size(range = c(1, 3.5), name = "Number of participants", breaks = c(1000, 1500, 2000)) +
  scale_x_date(breaks = "month", date_labels = "%b", name = "") +
  expand_limits(x = expand_dates)  +
  theme(
    legend.position = c(0.6, 0.9)
  ) + 
  annotate("rect",
           xmin = study_dates[1], xmax = study_dates[2],
           ymin = 0, ymax = 5, alpha = .1) +
  annotate("rect",
           xmin = study_dates[3], xmax = study_dates[4],
           ymin = 0, ymax = 5, alpha = .1) +
  annotate("rect",
           xmin = study_dates[7], xmax = study_dates[8],
           ymin = 0, ymax = 5, alpha = .1) +
  theme(legend.direction = "horizontal") +
  guides(size = guide_legend(title.position = "top", label.position = "top")) +
  labs(y = "Sample Type", subtitle = "") +
  ggtitle("C")

counts_all_p
# ggsave(counts_all_p, filename = "outputs/all_counts.png", width = 9, height  = 4)




layout <- "
AAAAAAAAAAAA
AAAAAAAAAAAA
BBBBBBBBBBBB
BBBBBBBBBBBB
BBBBBBBBBBBB
CCCCCCCCCCCC
"


fig1v3 <- (contacts_p / all_setting_p / counts_all_p) + plot_layout(design = layout)
fig1v3 

ggsave(fig1v3, filename = "outputs/fig1v3.png", width = 9 , height = 11)
