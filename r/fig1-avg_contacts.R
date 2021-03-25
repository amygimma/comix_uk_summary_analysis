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
file_path <- file.path("data", paste(sys_date, "bs_means_2w.qs", sep = "_"))
dt <- qs::qread(file_path)
message(paste("read from:", file_path))

nrow(dt)
dt <- dt[part_age_group %in% c("0-4", "5-17", "18-59", "60+")]

set(dt, j = c("part_region", "part_gender"), value = NULL)

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


dt[, part_social_group_lab := factor(part_social_group,
                                     levels = c("All", 
                                                "A - Upper middle class", 
                                                "B - Middle class", 
                                                "C1 - Lower middle class", 
                                                "C2 - Skilled working class",
                                                "D - Working class",
                                                "E - Lower level of subsistence"),
                                     labels = c("All", "A", "B", "C1", "C2", "D", "E"))]


table(dt$part_att_serious_bin)

# Set plot parameters -----------------------------------------------------
expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))


# Create plotting function ------------------------------------------------
plot_mean <- function(dt, time_break = "2 month", upper_limit = 6){
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, group = part_age_group, fill = part_age_group), alpha = 0.2) +
    geom_line( aes(y = mean, linetype = part_age_group, col = part_age_group)) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.1,0.8)
    ) +
    scale_linetype_manual(name = "Age (years)", values = c(1,2,3, 4)) +
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
all_age <- dt[
    #part_age_group %in% c("18-59", "60+") &
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
    annotate("text", x = as.Date("2020-12-22"), y = 19.5, label = "Christmas", angle = 0) #+
    # annotate("text", x = as.Date("2020-08-15"), y = 19.5, label = "Reduced\nrestrictions") + 
    ggtitle("A") +
    labs(subtitle = "Adults")

contacts_p



# Plot B: Contacts by setting ----------------------------------------------------
# Filter data -------------------------------------------------------------
plot_mean_setting <- function(dt, time_break = "month", upper_limit = 6){
  ggplot(dt, aes(x = mid_date)) +
    geom_ribbon(aes(ymin = lci, ymax = uci, group = part_age_group, fill = part_age_group), alpha = 0.2) +
    geom_line( aes(y = mean, linetype = part_age_group, col = part_age_group)) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.1,0.9)
    ) +
    scale_linetype_manual(name = "", values = c(4,1, 2,3)) +
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
             ymin = 0, ymax = upper_limit, alpha = .1)  +
    facet_grid(setting ~ .,  scales = "free_y")
}



# Create folder for plots -------------------------------------------------

all_setting <- dt[
  part_social_group == "All" & 
    part_high_risk == "All" &
    setting %in% c("Home", "Work/Educ", "Other")
] 



contacts_setting_p <- plot_mean_setting(all_setting, time_break = "month", upper_limit = 15) +
  ggtitle("B") +
  labs(subtitle = "Contact Setting")
contacts_setting_p


contacts_p / contacts_setting_p






# Plot B Kids -------------------------------------------------------------
all_kids <- dt[
  part_region %in% c("All") &
    part_age_group %in% c("0-4", "5-17") &
    part_gender == "All" &
    part_social_group == "All" & 
    part_high_risk == "All" &
    setting == "All"
] 



kids_p <- plot_mean(all_kids, time_break = "month", upper_limit = 20) +
  annotate("text", x = as.Date("2020-05-01"), y = 19.5, label = "Lockdown 1 (LD 1)") +
  annotate("text", x = as.Date("2020-11-15"), y = 19.5, label = "LD 2") +
  annotate("text", x = as.Date("2021-01-30"), y = 19.5, label = "LD 3") +
  annotate("text", x = as.Date("2020-12-22"), y = 18.5, label = "Christmas", angle = 0) +
  annotate("text", x = as.Date("2020-08-15"), y = 18.5, label = "Reduced\nrestrictions") + 
  ggtitle("B") +
  labs(subtitle = "Children")
  
kids_p




# Plot C: Count data ------------------------------------------------------
# # Figure 1 survey counts --------------------------------------------------
dt <- qs::qread('../comix/data/part_min.qs')
adults_id <- dt[sample_type == "adult" &
                  !area_3_name %in% c("Scotland", "Northern Ireland", "Wales") &
                  country == "uk" &
                  !survey_round %in% c(6,7),]$part_wave_uid
# 
panel_count <- dt[part_wave_uid %in% adults_id,
                  .(start_date = min(date), end_date = max(date), .N), by = .(panel, survey_round)]
panel_count[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round)]

mid_dates <- panel_count[, .(panel, survey_round, mid_date)]

panel_plot <- dcast(panel_count, formula = panel ~ mid_date, value.var = "N", fill = 0)

panel_count[, panel := factor(panel, levels = rev(LETTERS[1:6]))]


panel_count[, labmin := fifelse(mid_date %in% c(min(mid_date)),N, NA_integer_), by =(panel)]
panel_count[, labmax := fifelse(mid_date %in% c(max(mid_date)),N, NA_integer_), by =(panel)]
panel_count
counts_adults_p <- ggplot(panel_count) +
  geom_point(aes(y = panel, x = mid_date, size = N)) +
  geom_line(aes(y = panel, x = mid_date)) +
  geom_text(aes(y = panel, x = mid_date, label = labmin), nudge_x = - 10, nudge_y = 0.2, size = 3) +
  geom_text(aes(y = panel, x = mid_date, label = labmax), nudge_x = +10, nudge_y = 0.2,size = 3) +
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
           xmin = study_dates[5], xmax = study_dates[6],
           ymin = 0, ymax = 5, alpha = .1) +
  annotate("rect", 
           xmin = study_dates[7], xmax = study_dates[8],
           ymin = 0, ymax = 5, alpha = .1) +
  annotate("rect", 
           xmin = study_dates[9], xmax = study_dates[10],
           ymin = 0, ymax = 5, alpha = .1)  +
  theme(legend.direction = "horizontal") +
  guides(size = guide_legend(title.position = "top", label.position = "top")) +
  labs(y = "Panel", subtitle = "") +
  ggtitle("C")

counts_adults_p


# Plot D: Counts kids -----------------------------------------------------

# Figure 1 survey counts --------------------------------------------------
dt <- qs::qread('../comix/data/part_min.qs')
kids_id <- dt[sample_type == "child" &
                !area_3_name %in% c("Scotland", "Northern Ireland", "Wales") &
                country == "uk" &
                !survey_round %in% c(6,7),]$part_wave_uid

panel_count <- dt[part_wave_uid %in% kids_id, 
                  .(start_date = min(date), end_date = max(date), .N), by = .(panel, survey_round)]
panel_count[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round)]

panel_plot <- dcast(panel_count, formula = panel ~ mid_date, value.var = "N", fill = 0) 

panel_count[, panel := factor(panel, levels = rev(LETTERS[1:6]))]


panel_count[, labmin := fifelse(mid_date %in% c(min(mid_date)),N, NA_integer_), by =(panel)]
panel_count[, labmax := fifelse(mid_date %in% c(max(mid_date)),N, NA_integer_), by =(panel)]
panel_count
counts_kids_p <- ggplot(panel_count) +
  geom_point(aes(y = panel, x = mid_date, size = N)) +
  geom_line(aes(y = panel, x = mid_date)) +
  geom_text(aes(y = panel, x = mid_date, label = labmin), nudge_x = - 10, nudge_y = 0.2, size = 3) +
  geom_text(aes(y = panel, x = mid_date, label = labmax), nudge_x = +10, nudge_y = 0.2,size = 3) +
  scale_size(range = c(1, 3.5), name = "Number of participants", breaks = c(200, 400, 600)) +
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
           xmin = study_dates[5], xmax = study_dates[6],
           ymin = 0, ymax = 5, alpha = .1) +
  annotate("rect", 
           xmin = study_dates[7], xmax = study_dates[8],
           ymin = 0, ymax = 5, alpha = .1) +
  annotate("rect", 
           xmin = study_dates[9], xmax = study_dates[10],
           ymin = 0, ymax = 5, alpha = .1)  +
  theme(legend.direction = "horizontal") +
  guides(size = guide_legend(title.position = "top", label.position = "top")) +
  labs(y = "Panel", subtitle = "") +
  ggtitle("D")

counts_kids_p




# 
# part <- qs::qread("../comix/data/part.qs")
# part <- part[country == "uk" & sample_type == "adult"]
# part[, start_date := min(date), by = .(survey_round, country)]
# part[, end_date := max(date), by = .(survey_round, country)]
# part[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round, country)]
# 



# Plot Attendance --------------------------------------------------------------
pt <- qs::qread("../comix/data/part.qs")

pt <- pt[sample_type == "child" &
                !area_3_name %in% c("Scotland", "Northern Ireland", "Wales") &
                country == "uk" &
                !survey_round %in% c(6,7),]





pt[, weekday := weekdays(date)]
pt[, weekend := weekday %in% c("Saturday", "Sunday")]

map_school_attends <- c(
  c("closed" = "closed", 
    "closed - holiday" = "closed holiday",
    "Don’t know" = "unknown", 
    "no but open for my child" = "no - open for my child", 
    "No, but it was open for my child" = "no - open for my child", 
    "not answered" = "no answer", 
    "Not applicable as it was a weekend/holiday/day off" = "day off", 
    "Not applicable as it was closed" = "closed", 
    "Prefer not to answer" = "no answer",
    "yes" = "yes", 
    "Yes" = "yes")
  
)

pt[, part_attend_school_yesterday := map_school_attends[part_attend_school_yesterday]]


pt_school_all <- pt[part_attend_school_yesterday %in% c("closed", "no - open for my child", "yes")  & weekend == FALSE, 
                    .(
                      yes_n = sum(part_attend_school_yesterday == "yes"),
                      no_n = sum(part_attend_school_yesterday != "yes"),
                      n = .N,
                      yes = round(sum(part_attend_school_yesterday == "yes")/.N,2), 
                      no = round(sum(part_attend_school_yesterday != "yes")/.N,2), 
                      start_date = min(date), end_date = max(date)), by = survey_round]


pt_school_all[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round)]
pt_school_all[, lci := yes - 1.96*sqrt((yes*(1-yes)/n))]
pt_school_all[, uci := yes + 1.96*sqrt((yes*(1-yes)/n))]


attends_p <- ggplot(pt_school_all[], aes(x = mid_date)) +
  geom_line( aes(y = yes)) +
  geom_point( aes(y = yes)) +
  #geom_smooth( aes(y = yes), method = "gam") +
  #geom_point( aes(y = avg)) +
  labs(title = "", y = "", x = "") +
  scale_y_continuous(expand = expansion(0), labels = scales::percent, limits = c(0,1)) +
  scale_x_date(breaks = "month", date_labels = "%b") +
  expand_limits(x = expand_dates) + 
  annotate("rect", 
           xmin = study_dates[1], xmax = study_dates[2],
           ymin = 0, ymax = 1, alpha = .1) +
  annotate("rect", 
           xmin = study_dates[3], xmax = study_dates[4],
           ymin = 0, ymax = 1, alpha = .1) +
  annotate("rect", 
           xmin = study_dates[5], xmax = study_dates[6],
           ymin = 0, ymax = 1, alpha = .1) +
  annotate("rect", 
           xmin = study_dates[7], xmax = study_dates[8],
           ymin = 0, ymax = 1, alpha = .1) +
  annotate("rect", 
           xmin = study_dates[9], xmax = study_dates[10],
           ymin = 0, ymax = 1, alpha = .1)  +
  labs(y = "", x = "", subtitle = "School attendance") +
  ggtitle("F")

attends_p






layout <- "
AAAAAABBBBBB
AAAAAABBBBBB
AAAAAABBBBBB
AAAAAABBBBBB
CCCCCCDDDDDD 
EEEEEEFFFFFF
EEEEEEFFFFFF
GGGGGGHHHHHH
IIIIIIJJJJJJ
"

fig1 <- adults_p + kids_p + counts_adults_p + counts_kids_p + adults_setting_p + attends_p + dur_adults_p + dur_kids_p + freq_adults_p + freq_kids_p +   plot_layout(design = layout)
fig1

qs::qsave(fig1, "outputs/fig1.qs")
ggsave(fig1, filename = "outputs/fig1.png", height = 16, width = 17, dpi = 300)





