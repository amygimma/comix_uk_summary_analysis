## Create plots of average contacts

library(data.table) 
library(lubridate)
library(ggplot2)
library(patchwork)

# library(ggthemr)
# ggthemr::ggthemr('dust')
# ggthemr workaround for ggplot error: -----------------------------------
source(file.path("r", "functions", "ggthemr_workaround.R"))
theme_set(cowplot::theme_cowplot(font_size = 11) + theme(strip.background = element_blank()))


# Load data --------------------------------------------------------------

dts <- qs::qread("data/bs_means_2w_hh.qs")


age_levs <- c("All", "All-adults",  "0-4", "5-17", "18-59", "60+")
age_labs <- c("All", "All-adults",  "0-4", "5-17", "18-59", "60+")
dts[, part_age_group := factor(part_age_group, levels = age_levs, labels = age_labs)]

settings_ <- c(
  # "All", "Home", "Work/Educ", "Other",
  "All_genderage", "Home_genderage", "Work_genderage", 
  "Work/Educ_genderage", "Other_genderage", "Non household_genderage")
dts[, setting := factor(setting, levels = settings_)]
table(dts$setting)

settings_labs <- c("All", "Home", "Work/Educ", "Other")

#dts_rec[part_region == "North East and Yorkshire", part_region := "NE & Y"]

# Two plotting functions I think. 
## For a single setting and for all settings.


library(magrittr)



# Set plot parameters -----------------------------------------------------

plotdate <- max(as.Date("2021-03-23"))
expand_dates <- plotdate + 5
plot_width <- 12
plot_height <- 7


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

# Set the dates for the graphs --------------------------------------------
dts_rec <- dts[mid_date >= as.Date("2020-02-01") & mid_date <= as.Date("2021-03-23")]
time_break <- "2 month"

## For 2 weeks
date_labs = "%d-%b-"
# plotdate <- plotdate

## For 2 months
plotdate <- paste0(plotdate,"_all")
date_labs = "%d-%b-%y"
dir.create(paste0("outputs/", plotdate), showWarnings = FALSE)


cols <- c("#17877b", "#055a8c", "#D7402B", "#daa520", "#20bdcc", "#010f5b", "#d72638")



# Plot sc adults ------------------------------------------------------------

hh_dt <-  dts_rec[
  part_age_group %in% c("0-4", "5-17", "18-59", "60+") &
    setting %in% c("Home_genderage", "Non household_genderage") &
    hh_size_group %in% c("1", "2", "3-5", "6+")]

hh_dt[, setting := gsub("_genderage", "", setting)]
hh_dt[, setting := gsub("Non household", "Non-household", setting)]


upl <- max(hh_dt$uci)
upper_limit <- upl
ylabel <- upper_limit  - 1.5
timeline_size <- 1.75
hh_p <-  ggplot(hh_dt[part_age_group != "0-4"], aes(x = mid_date)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = hh_size_group, group = interaction(part_age_group, hh_size_group)), alpha = 0.3) +
  geom_line( aes(y = mean, color = hh_size_group)) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_grid(rows = vars(part_age_group), cols = vars(setting)) +
  scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
  scale_x_date(breaks = time_break, date_labels = "%b %y", name = "") +
  expand_limits(x = expand_dates) + 
  theme(
    panel.spacing.y =  unit(1, "lines"),
    legend.position = c(0.05, 0.9),
    legend.direction = "horizontal",
    # legend.title=element_text(size=12),
    # legend.text=element_text(size=10)
    panel.grid.major = element_line(colour="grey", size=0.05),
    axis.text.x = element_text(size = 6.5)
  ) +
  labs(title = "", y = "Mean non-household contacts", x = "") +
  guides(fill=guide_legend(title="Household size"), 
         color = guide_legend(title="Household size")) +
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
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) 

# annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) 
# ggtitle("A")
hh_p
hh_name <- "outputs/hh_contacts_all.png"
ggsave(hh_p, filename = hh_name, width = plot_width, height = 6)



upl <- max(hh_dt[setting %in% c("Non-household")]$uci)
upper_limit <- upl
ylabel <- upper_limit - 1
timeline_size <- 1.75
# Non household only -----------
nhh_hh_p <-  ggplot(
  hh_dt[setting %in% c("Non household_genderage") & part_age_group != "0-4"], 
  aes(x = mid_date)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = hh_size_group, group = interaction(part_age_group, hh_size_group)), alpha = 0.3) +
  geom_line( aes(y = mean, color = hh_size_group)) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_grid(rows = vars(part_age_group)) +
  scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
  scale_x_date(breaks = time_break, date_labels = "%b %y", name = "") +
  expand_limits(x = expand_dates) + 
  theme(
    panel.spacing.y =  unit(1, "lines"),
    legend.position = c(0.1, 0.925),
    legend.direction = "horizontal",
    # legend.title=element_text(size=12),
    # legend.text=element_text(size=10)
    panel.grid.major = element_line(colour="grey", size=0.05),
    axis.text.x = element_text(size = 6.5)
  ) +
  labs(title = "", y = "Mean non-household contacts", x = "") +
  guides(fill=guide_legend(title="Household size"), 
         color = guide_legend(title="Household size")) +
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
  annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
  annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
  annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) 

nhh_hh_p
nhh_name <- "outputs/hh_contacts_nhh.png"
ggsave(nhh_hh_p, filename = nhh_name, width = plot_width, height = 6)

