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

dts <- qs::qread("data/bs_means_2w_se.qs")

# dts[, table(setting)]
# dts[, table(part_age_group, part_gender)]
# dts[, table(part_region, part_age_group)]
# dts[, table(part_age_group, part_social_group)]
# dts[, table(part_income)]
# dts[, table(part_high_risk)]
# dts[, table(part_work_place)]
# dts[, table(part_employed)]


age_levs <- c("All", "All-adults",  "0-4", "5-11", "12-17", "18-29", "30-39", "40-49",  "50-59", 
              "60-69", "70-120", "18-59", "60+")
age_labs <- c("All", "All-adults",  "0-4", "5-11", "12-17", "18-29", "30-39", "40-49",  "50-59", 
              "60-69", "70+", "18-59", "60+")
settings_ <- c("All", "Home", "Work/Educ", "Other",
               "All_genderage", "Home_genderage", "Work_genderage", 
               "Work/Educ_genderage", "Other_genderage")
dt <- dt[mid_date >= as.Date("2020-03-23") & mid_date <= as.Date("2021-03-26")]
dts[, part_age_group := factor(part_age_group, levels = age_levs, labels = age_labs)]
dts[, setting := factor(setting, levels = settings_)]


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

# Age and income --------------------------------------------
# 
# inc_age_dt <-  dts[
#   part_region %in% c("All") &
#     part_age_group %in% c("18-59", "60+") &
#     part_gender == "All" &
#     part_social_group == "All" &
#     part_high_risk == "All" &
#     part_income != "All" &
#     part_work_place == "open" &
#     part_employed == "All" &
#     setting == "All_genderage"
# ]
# inc_age_dt
# #
# # emp_inc_dt$part_income <- as.factor(emp_inc_dt$part_income)
# upper_limit <- 15
# ylabel <- upper_limit - 2
# timeline_size <- 3.5
# 
# inc_age_p <- ggplot(inc_age_dt, aes(x = mid_date)) +
#   geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_income), alpha = 0.3) +
#   geom_line( aes(y = mean, color = part_income), alpha = 0.7) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = cols) +
#   facet_grid( rows = vars(part_age_group)) +
#   scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
#   scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
#   expand_limits(x = expand_dates) +
#   theme(
#     panel.spacing.y =  unit(1, "lines"),
#     legend.position = c(0.05, 0.9),
#     legend.title=element_text(size=12),
#     legend.text=element_text(size=10)
#   ) +
#   labs(title = "", y = "Mean contacts", x = "") +
#   guides(fill=guide_legend(title="Income"), color = guide_legend(title="Income")) +
#   annotate("rect",
#            xmin = study_dates[1], xmax = study_dates[2],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[3], xmax = study_dates[4],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   # annotate("rect",
#   #          xmin = study_dates[5], xmax = study_dates[6],
#   #          ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[7], xmax = study_dates[8],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
#   annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
#   annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) 
#   # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0)
# # ggtitle("A")
# 
# 
# inc_age_p
# inc_age_adults_name <- "outputs/inc_age_adults_work.png"
# ggsave(inc_age_p, filename = inc_age_adults_name , width = 9, height = 6)
# 
# 
# 
# # Age and employment
# emp_age_dt <-  dts[
#   part_region %in% c("All") &
#     part_age_group %in% c("18-59", "60+") &
#     part_gender == "All" &
#     part_social_group == "All" &
#     part_high_risk == "All" &
#     part_income == "All" &
#     part_work_place == "open" &
#     part_employed != "All" &
#     setting == "Work_genderage"
# ]
# #
# # emp_inc_dt$part_income <- as.factor(emp_inc_dt$part_income)
# upper_limit <- 40
# ylabel <- upper_limit - 2
# timeline_size <- 3.5
# 
# emp_age_p <- ggplot(emp_age_dt, aes(x = mid_date)) +
#   geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_employed), alpha = 0.3) +
#   geom_line( aes(y = mean, color = part_employed), alpha = 0.7) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = cols) +
#   facet_grid( rows = vars(part_age_group)) +
#   scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
#   scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
#   expand_limits(x = expand_dates) +
#   theme(
#     panel.spacing.y =  unit(1, "lines"),
#     legend.position = c(0.05, 0.9),
#     legend.title=element_text(size=12),
#     legend.text=element_text(size=10)
#   ) +
#   labs(title = "", y = "Mean contacts", x = "") +
#   guides(fill=guide_legend(title="Employment type"), color = guide_legend(title="Employment type")) +
#   annotate("rect",
#            xmin = study_dates[1], xmax = study_dates[2],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[3], xmax = study_dates[4],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   # annotate("rect",
#   #          xmin = study_dates[5], xmax = study_dates[6],
#   #          ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[7], xmax = study_dates[8],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
#   annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
#   annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size)
#   # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0)
# # ggtitle("B")
# 
# 
# emp_age_p
# emp_age_adults_name <- "outputs/emp_age_adults_work.png"
# ggsave(emp_age_p, filename = emp_age_adults_name , width = 9, height = 6)
# 
# 
# emp_inc_age_p <- ((emp_age_p + ggtitle("A") + 
#                      theme(legend.position = c(0.35, 0.85), legend.direction = "horizontal")) 
#   / (inc_age_p + ggtitle("B") + 
#        theme(legend.position = c(0.35, 0.85), legend.direction = "horizontal"))) +
#   plot_layout()
# 
# emp_inc_age_adults_name <- "outputs/emp_inc_age_adults_work.png"
# ggsave(emp_inc_age_p, filename = emp_inc_age_adults_name , width = 9, height = 9)



# # Income work open ---------------------------------------------------------------
# 
# 
# emp_inc_dt <-  dts_rec[
#   part_region %in% c("All") &
#   part_age_group %in% c("All-adults") &
#   part_gender == "All" &
#   part_social_group == "All" &
#   part_high_risk == "All" &
#   part_income != "All" &
#     part_work_place == "open" &
#     part_employed != "All" &
#     setting == "Work_genderage"
# ]
# 
# emp_inc_dt$part_income <- as.factor(emp_inc_dt$part_income)
# upper_limit <- 55
# ylabel <- upper_limit - 2
# timeline_size <- 3.5
# 
# emp_inc_p <- ggplot(emp_inc_dt, aes(x = mid_date)) +
#   geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_employed), alpha = 0.3) +
#   geom_line( aes(y = mean, color = part_employed), alpha = 0.7) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = cols) +
#   facet_grid( rows = vars(part_income)) +
#   scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
#   scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
#   expand_limits(x = expand_dates) + 
#   theme(
#     panel.spacing.y =  unit(1, "lines"),
#     legend.position = c(0.05, 0.9),
#     legend.title=element_text(size=12),
#     legend.text=element_text(size=10)
#   ) +
#   labs(title = "", y = "Mean contacts", x = "") +
#   guides(fill=guide_legend(title="Employment type"), color = guide_legend(title="Employment type")) +
#   annotate("rect",
#            xmin = study_dates[1], xmax = study_dates[2],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[3], xmax = study_dates[4],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   # annotate("rect",
#   #          xmin = study_dates[5], xmax = study_dates[6],
#   #          ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[7], xmax = study_dates[8],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
#   annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) 
#   # annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) 
#   # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) 
#   # ggtitle("A")
# 
# 
# emp_inc_p
# emp_inc_adults_name <- "outputs/emp_inc_adults_work.png"
# ggsave(emp_inc_p, filename = emp_inc_adults_name, width = 9, height = 6)


# Plot sc adults ------------------------------------------------------------

# sc_dt <-  dts_rec[
#     part_age_group %in% c("All-adults") &
#     part_social_group != "All" &
#     part_income == "All" & 
#     part_work_place == "All" & 
#     part_employed == "All" &
#     setting == "All_genderage"]
#  
# upper_limit <- 7.5
# ylabel <- upper_limit - 0.5
# timeline_size <- 1.75
# sc_p <-  ggplot(sc_dt, aes(x = mid_date)) +
#   geom_ribbon(aes(ymin = lci, ymax = uci), fill = cols[6], alpha = 0.3) +
#   geom_line( aes(y = mean), color = cols[6]) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = cols) +
#   facet_wrap(vars(part_social_group) , ncol = 3 ) +
#   scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
#   scale_x_date(breaks = time_break, date_labels = "%b %y", name = "") +
#   expand_limits(x = expand_dates) + 
#   theme(
#     panel.spacing.y =  unit(1, "lines"),
#     # legend.position = c(0.05, 0.9),
#     legend.position = "none",
#     # legend.title=element_text(size=12),
#     # legend.text=element_text(size=10)
#     panel.grid.major = element_line(colour="grey", size=0.05),
#     axis.text.x = element_text(size = 6.5)
#   ) +
#   labs(title = "", y = "Mean contacts", x = "") +
#   guides(fill=guide_legend(title="Socioeconomic group"), 
#          color = guide_legend(title="Socioeconomic group")) +
#   annotate("rect",
#            xmin = study_dates[1], xmax = study_dates[2],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[3], xmax = study_dates[4],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   # annotate("rect",
#   #          xmin = study_dates[5], xmax = study_dates[6],
#   #          ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[7], xmax = study_dates[8],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
#   annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
#   annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) 
#   # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) 
#   # ggtitle("A")
# sc_p
# sc_adults_name <- "outputs/sc_adults_all.png"
# ggsave(sc_p, filename = sc_adults_name, width = plot_width, height = 6)
# 
# 
# # Social group 2 
# sc_g2_dt <- qs::qread("data/bs_means_2w_sg2.qs")
# # Set the dates for the graphs --------------------------------------------
# sc_g2_dt[, part_age_group := factor(part_age_group, levels = age_levs, labels = age_labs)]
# sc_g2_dt[, setting := factor(setting, levels = c("All", "Home", "Work/Educ", "Other", "School", "Work"))]
# 
# 
# sc_g2_dt <- sc_g2_dt[mid_date >= as.Date("2020-02-01") & mid_date <= as.Date("2021-03-23")]
# 
# sc_g2_dt  <-  sc_g2_dt[
#   part_region %in% c("All") &
#     part_age_group %in% c("18-59", "60+") &
#     part_gender == "All" &
#     part_social_group != "All" &
#     part_high_risk == "All" & 
#     part_income == "All" & 
#     part_work_place == "All" & 
#     part_employed == "All" &
#     setting == "All_genderage"]
# 
# upper_limit <- 7.5
# ylabel <- upper_limit - 0.5
# timeline_size <- 3.5
# scg2_p <-  ggplot(sc_g2_dt , aes(x = mid_date)) +
#   geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_social_group), alpha = 0.3) +
#   geom_line( aes(y = mean, color = part_social_group)) +
#   scale_color_manual(values = cols[c(2,1)]) +
#   scale_fill_manual(values = cols[c(2,1)]) +
#   facet_grid(vars(part_age_group)) +
#   # theme_bw() +
#   scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
#   scale_x_date(breaks = time_break, date_labels = "%b %y", name = "") +
#   expand_limits(x = expand_dates) + 
#   expand_limits(y = 0) +
#   theme(
#     panel.spacing.y =  unit(1, "lines"),
#     # legend.background = 
#     # legend.position = "none",
#     # legend.title=element_text(size=12),
#     legend.position = c(0.55, 0.875),
#     legend.direction = "horizontal",
#     legend.text=element_text(size=12)
#   ) +
#   labs(title = "", y = "Mean contacts", x = "") +
#   guides(fill=guide_legend(title="Socioeconomic group"), 
#          color = guide_legend(title="Socioeconomic group")) +
#   annotate("rect",
#            xmin = study_dates[1], xmax = study_dates[2],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[3], xmax = study_dates[4],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   # annotate("rect",
#   #          xmin = study_dates[5], xmax = study_dates[6],
#   #          ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[7], xmax = study_dates[8],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
#   annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
#   annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size)
# # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) 
# # ggtitle("A")
# scg2_p 
# sc_adults_name <- "outputs/sc_sg2_adults.png"
# ggsave(scg2_p , filename = sc_adults_name, width = plot_width, height = 6)
# 
# # Income work open ---------------------------------------------------------------
# 
# 
# inc_dt <-  dts_rec[
#   part_region %in% c("All") &
#     part_age_group %in% c("All-adults") &
#     part_gender == "All" &
#     part_social_group == "All" &
#     part_high_risk == "All" & 
#     part_income != "All" & 
#     part_work_place == "open" & 
#     part_employed == "All" &
#     setting == "All_genderage"]
#  
# 
# upper_limit <- 10
# ylabel <- upper_limit - 1
# timeline_size <- 3.5
# inc_p <-  ggplot(inc_dt, aes(x = mid_date)) +
#   geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_income), alpha = 0.3) +
#   geom_line( aes(y = mean, col = part_income)) +
#   # facet_grid( rows = vars(part_income)) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = cols) +
#   labs(title = "", y = "Mean contacts", x = "") +
#   expand_limits(x = expand_dates) + 
#   scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
#   scale_x_date(breaks = time_break, date_labels = "%b '%y", name = "") +
#   expand_limits(x = expand_dates) + 
#   theme(
#     panel.spacing.y =  unit(1, "lines"),
#     legend.position = c(0.45, 0.80),
#     # legend.position = "none",
#     legend.title=element_text(size=12),
#     legend.text=element_text(size=10),
#     legend.direction = "horizontal",
#     panel.grid.major = element_line(colour="grey", size=0.05),
#     axis.text.x = element_text(size = 6.5)
#   ) +
#   labs(title = "", y = "Mean contacts", x = "") +
#   guides(fill=guide_legend(title="Income"), 
#          color = guide_legend(title="Income")) +
#   annotate("rect",
#            xmin = study_dates[1], xmax = study_dates[2],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[3], xmax = study_dates[4],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   # annotate("rect",
#   #          xmin = study_dates[5], xmax = study_dates[6],
#   #          ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[7], xmax = study_dates[8],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
#   annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
#   annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) 
#   # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) 
# # ggtitle("A")
# 
# inc_p
# inc_adults_name <- "outputs/inc_adults_work.png"
# ggsave(inc_p, filename = inc_adults_name, width = 9, height = 6)
# 
# # Income work open ---------------------------------------------------------------
# 
# 
# emp_dt <-  dts_rec[
#   part_region %in% c("All") &
#     part_age_group %in% c("All-adults") &
#     part_gender == "All" &
#     part_social_group == "All" &
#     part_high_risk == "All" & 
#     part_income == "All" & 
#     part_work_place == "open" & 
#     part_employed != "All" &
#     setting == "Work_genderage"
# ] 
#   
# upper_limit <- 31
# ylabel <- upper_limit - 2
# timeline_size <- 3.5
# emp_p <- ggplot(emp_dt, aes(x = mid_date)) +
#   geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_employed), alpha = 0.3) +
#   geom_line( aes(y = mean, col = part_employed)) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = cols) +
#   scale_x_date(breaks = time_break, date_labels = "%b '%y", name = "") +
#   scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
#   guides(fill=guide_legend(title="Employment"), col = guide_legend(title="Employment")) +
#   labs(title = "", y = "Mean contacts", x = "") +
#   theme(
#     panel.spacing.y =  unit(1, "lines"),
#     legend.position = c(0.35, 0.8),
#     legend.direction = "horizontal",
#     legend.title=element_text(size=12),
#     legend.text=element_text(size=10),
#     panel.grid.major = element_line(colour="grey", size=0.05),
#     axis.text.x = element_text(size = 6.5)
#   ) +
#   annotate("rect",
#            xmin = study_dates[1], xmax = study_dates[2],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[3], xmax = study_dates[4],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   # annotate("rect",
#   #          xmin = study_dates[5], xmax = study_dates[6],
#   #          ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("rect",
#            xmin = study_dates[7], xmax = study_dates[8],
#            ymin = 0, ymax = upper_limit, alpha = .1) +
#   annotate("text", x = as.Date("2020-05-01"), y = ylabel, label = "Lockdown 1 (LD 1)", size = timeline_size) +
#   annotate("text", x = as.Date("2020-11-15"), y = ylabel, label = "LD 2", size = timeline_size) +
#   annotate("text", x = as.Date("2021-01-30"), y = ylabel, label = "LD 3", size = timeline_size) 
#   # annotate("text", x = as.Date("2020-12-22"), y = ylabel, label = "Christmas", size = timeline_size, angle = 0) 
# # ggtitle("A")
# 
# emp_p
# emp_adults_name <- "outputs/emp_adults_work.png"
# ggsave(emp_p, filename = emp_adults_name, width = plot_width, height = 6)

# Income work open ---------------------------------------------------------------

upper_limit <- 55
ylabel <- upper_limit - 2
timeline_size <- 3.5
emp_inc_dt <-  dts_rec[
  part_region %in% c("All") &
    part_age_group %in% c("All-adults") &
    part_gender == "All" &
    part_social_group == "All" &
    part_high_risk == "All" & 
    part_income != "All" & 
    part_work_place == "open" & 
    part_employed != "All" &
    setting == "Work_genderage"
] 
emp_inc_p <-
  ggplot(emp_inc_dt, aes(x = mid_date)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_income), alpha = 0.3) +
  geom_line( aes(y = mean, col = part_income), alpha = 0.8) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  labs(title = "", y = "Mean contacts", x = "") +
  guides(fill=guide_legend(title="Income"), col = guide_legend(title="Income")) +
  facet_wrap(vars(part_employed) , ncol = 1 ) +
  scale_y_continuous(expand = expansion(0)) +
  expand_limits(y = 0) +
  scale_x_date(breaks = time_break, date_labels = date_labs) +
  expand_limits(x = expand_dates) + 
  theme(
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    panel.grid.major = element_line(colour="grey", size=0.05),
    axis.text.x = element_text(size = 6.5),
    #axis.text.x = element_blank(),
    panel.spacing.y =  unit(1, "lines")) +
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

emp_inc_p
emp_inc_adults_name <- "outputs/emp_inc_adults_work.png"
ggsave(emp_inc_p, filename = emp_inc_adults_name, width = plot_width, height = 6)



# Group plots -------------------------------------------------------------
grp_adults_name <-"outputs/se_adults_all.png"
group_plot <- ((emp_p + ggtitle("A")) / (inc_p + ggtitle("B"))) + plot_layout()
group_plot

ggsave(group_plot, filename = grp_adults_name, width = 9, height = 6)


