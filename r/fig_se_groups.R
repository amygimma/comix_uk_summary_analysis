## Create plots of average contacts

library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemr)
ggthemr::ggthemr('dust')
# ggthemr workaround for ggplot error: -----------------------------------
source(file.path("r", "functions", "ggthemr_workaround.R"))


# Load data --------------------------------------------------------------

dts <- qs::qread("data/bs_means_2w.qs")

dts[, table(setting)]
dts[, table(part_age_group, part_gender)]
dts[, table(part_region, part_age_group)]
dts[, table(part_age_group, part_social_group)]
dts[, table(part_income)]
dts[, table(part_high_risk)]
dts[, table(part_work_place)]
dts[, table(part_employed)]


age_levs <- c("All", "All-adults",  "0-4", "5-11", "12-17", "18-29", "30-39", "40-49",  "50-59", 
              "60-69", "70-120")
age_labs <- c("All", "All-adults",  "0-4", "5-11", "12-17", "18-29", "30-39", "40-49",  "50-59", 
              "60-69", "70+")

dts[, part_age_group := factor(part_age_group, levels = age_levs, labels = age_labs)]
dts[, setting := factor(setting, levels = c("All", "Home", "Work/Educ", "Other", "School", "Work"))]


#dts_rec[part_region == "North East and Yorkshire", part_region := "NE & Y"]

# Two plotting functions I think. 
## For a single setting and for all settings.


library(magrittr)



# Set plot parameters -----------------------------------------------------

plotdate <- max(dts$end_date)
expand_dates <- plotdate + 5
plot_width <- 12
plot_height <- 7


# Set the dates for the graphs --------------------------------------------
dts_rec <- dts[mid_date >= as.Date("2020-02-01")]
time_break <- "2 month"

## For 2 weeks
date_labs = "%d-%b-"
plotdate <- plotdate

## For 2 months
plotdate <- paste0(plotdate,"_all")
date_labs = "%d-%b-%y"
dir.create(paste0("outputs/", plotdate), showWarnings = FALSE)

settings_ <- c("All", "Home", "Work/Educ", "Other")



# Income work open ---------------------------------------------------------------

emp_inc_adults_name <- paste0("outputs/", plotdate, "/emp_inc_adults_work", plotdate, ".png")

emp_inc_dt <-  dts_rec[
  part_region %in% c("All") &
  part_age_group %in% c("All-adults") &
  part_gender == "All" &
  part_social_group == "All" &
  part_high_risk == "All" &
  part_income != "All" &
    part_work_place == "open" &
    part_employed != "All" &
    setting == "Work"
]
# 
# emp_inc_dt$part_income <- as.factor(emp_inc_dt$part_income)
upper_limit <- 10
emp_inc_p <- ggplot(emp_inc_dt, aes(x = mid_date)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_income), alpha = 0.1) +
  geom_line( aes(y = mean, col = part_income)) +
  #geom_smooth( aes(y = med), method = "gam") +
  #geom_point( aes(y = avg)) +
  labs(title = "", y = "Mean contacts", x = "") +
  facet_wrap(vars(part_employed) , ncol = 1 ) +
  scale_y_continuous(expand = expansion(0), limits = ) +
  expand_limits(y = 0) +
  theme(text = element_text(size = 16)) + 
  scale_x_date(breaks = time_break, date_labels = date_labs) +
  expand_limits(x = expand_dates) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    #axis.text.x = element_blank(),
    panel.spacing.y =  unit(1, "lines")
  ) +
  guides(fill=guide_legend(title="Income"), col = guide_legend(title="Income")) +
  scale_colour_ggthemr_d() 
  
  
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = factor(part_income)), alpha = 0.1) +
  geom_line( aes(y = mean, color = factor(part_income))) +
  scale_colour_ggthemr_d() +
  # geom_smooth( aes(y = med), method = "gam") +
  # geom_point( aes(y = avg)) +
  labs(title = "", y = "Mean contacts", x = "") +
  facet_wrap(vars(part_employed) , ncol = 1 ) +
  scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
  expand_limits(y = 0) +
  theme(text = element_text(size = 16)) + 
  scale_x_date(breaks = time_break) +
  expand_limits(x = expand_dates) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text.x = element_blank(),
    panel.spacing.y =  unit(1, "lines")
  ) +
  guides(fill=guide_legend(title="Income"), col = guide_legend(title="Income"))

emp_inc_p
ggsave(emp_inc_p, filename = emp_inc_adults_name, width = plot_width, height = 6)


# Plot sc adults ------------------------------------------------------------
sc_adults_name <- paste0("outputs/", plotdate, "/sc_adults_all", plotdate, ".png")

sc_p <-  dts_rec[
  part_region %in% c("All") &
    part_age_group %in% c("All-adults") &
    part_gender == "All" &
    part_social_group != "All" &
    part_high_risk == "All" & 
    part_income == "All" & 
    part_work_place == "All" & 
    part_employed == "All" &
    setting == "All"
] %>% 
  ggplot(aes(x = mid_date)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2) +
  geom_line( aes(y = mean)) +
  #geom_smooth( aes(y = med), method = "gam") +
  #geom_point( aes(y = avg)) +
  labs(title = "", y = "Mean contacts", x = "") +
  facet_wrap(vars(part_social_group) , ncol = 3 ) +
  scale_y_continuous(expand = expansion(0)) +
  expand_limits(y = 0) +
  theme(text = element_text(size = 16)) + 
  scale_x_date(breaks = time_break, date_labels = date_labs) +
  expand_limits(x = expand_dates) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    panel.spacing.y =  unit(1, "lines")
  )

sc_p
ggsave(sc_p, filename = sc_adults_name, width = plot_width, height = 6)



# Income work open ---------------------------------------------------------------

inc_adults_name <- paste0("outputs/", plotdate, "/inc_adults_work", plotdate, ".png")

inc_p <-  dts_rec[
  part_region %in% c("All") &
    part_age_group %in% c("All-adults") &
    part_gender == "All" &
    part_social_group == "All" &
    part_high_risk == "All" & 
    part_income != "All" & 
    part_work_place == "open" & 
    part_employed == "All" &
    setting == "Work"
] %>% 
  ggplot(aes(x = mid_date)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_income), alpha = 0.1) +
  geom_line( aes(y = mean, col = part_income)) +
  #geom_smooth( aes(y = med), method = "gam") +
  #geom_point( aes(y = avg)) +
  labs(title = "", y = "Mean contacts", x = "") +
  scale_y_continuous(expand = expansion(0)) +
  expand_limits(y = 0) +
  theme(text = element_text(size = 16)) + 
  scale_x_date(breaks = time_break, date_labels = date_labs) +
  expand_limits(x = expand_dates) + 
  theme(
    #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text.x = element_blank(),
    panel.spacing.y =  unit(1, "lines")
  ) +
  guides(fill=guide_legend(title="Income"), col = guide_legend(title="Income"))

inc_p
ggsave(inc_p, filename = inc_adults_name, width = plot_width, height = 6)

# Income work open ---------------------------------------------------------------

emp_adults_name <- paste0("outputs/", plotdate, "/emp_adults_work", plotdate, ".png")

emp_p <-  dts_rec[
  part_region %in% c("All") &
    part_age_group %in% c("All-adults") &
    part_gender == "All" &
    part_social_group == "All" &
    part_high_risk == "All" & 
    part_income == "All" & 
    part_work_place == "open" & 
    part_employed != "All" &
    setting == "Work"
] %>% 
  ggplot(aes(x = mid_date)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_employed), alpha = 0.1) +
  geom_line( aes(y = mean, col = part_employed)) +
  #geom_smooth( aes(y = med), method = "gam") +
  #geom_point( aes(y = avg)) +
  labs(title = "", y = "Mean contacts", x = "") +
  scale_y_continuous(expand = expansion(0)) +
  expand_limits(y = 0) +
  theme(text = element_text(size = 16)) + 
  scale_x_date(breaks = time_break, date_labels = date_labs) +
  expand_limits(x = expand_dates) + 
  theme(
    #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text.x = element_blank(),
    panel.spacing.y =  unit(1, "lines")
  ) +
  guides(fill=guide_legend(title="Employment"), col = guide_legend(title="Employment"))

emp_p
ggsave(emp_p, filename = emp_adults_name, width = plot_width, height = 6)

# Income work open ---------------------------------------------------------------

emp_inc_adults_name <- paste0("outputs/", plotdate, "/emp_inc_adults_work", plotdate, ".png")

emp_inc_p <-  dts_rec[
  part_region %in% c("All") &
    part_age_group %in% c("All-adults") &
    part_gender == "All" &
    part_social_group == "All" &
    part_high_risk == "All" & 
    part_income != "All" & 
    part_work_place == "open" & 
    part_employed != "All" &
    setting == "Work"
] %>% 
  ggplot(aes(x = mid_date)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_income), alpha = 0.1) +
  geom_line( aes(y = mean, col = part_income)) +
  #geom_smooth( aes(y = med), method = "gam") +
  #geom_point( aes(y = avg)) +
  labs(title = "", y = "Mean contacts", x = "") +
  facet_wrap(vars(part_employed) , ncol = 1 ) +
  scale_y_continuous(expand = expansion(0)) +
  expand_limits(y = 0) +
  theme(text = element_text(size = 16)) + 
  scale_x_date(breaks = time_break, date_labels = date_labs) +
  expand_limits(x = expand_dates) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    #axis.text.x = element_blank(),
    panel.spacing.y =  unit(1, "lines")
  ) +
  guides(fill=guide_legend(title="Income"), col = guide_legend(title="Income"))

emp_inc_p
ggsave(emp_inc_p, filename = emp_inc_adults_name, width = plot_width, height = 6)


# High risk ---------------------------------------------------------------

hr_adults_name <- paste0("outputs/", plotdate, "/hr_adults_all", plotdate, ".png")

# hr_p <-  dts_rec[
#   part_region %in% c("All") &
#     part_age_group %in% c("All-adults") &
#     part_gender == "All" &
#     part_social_group == "All" &
#     part_high_risk != "All" & 
#     part_income == "All" & 
#     part_work_place == "All" & 
#     part_employed == "All" &
#     setting == "All"
# ] %>% 
#   ggplot(aes(x = mid_date)) +
#   geom_ribbon(aes(ymin = lci, ymax = uci, fill = part_high_risk), alpha = 0.1) +
#   geom_line( aes(y = mean, col = part_high_risk)) +
#   #geom_smooth( aes(y = med), method = "gam") +
#   #geom_point( aes(y = avg)) +
#   labs(title = "", y = "Mean contacts", x = "") +
#   scale_y_continuous(expand = expansion(0)) +
#   expand_limits(y = 0) +
#   theme(text = element_text(size = 16)) + 
#   scale_x_date(breaks = time_break, date_labels = date_labs) +
#   expand_limits(x = expand_dates) + 
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#     panel.spacing.y =  unit(1, "lines")
#   ) + 
#   guides(fill=guide_legend(title="High Risk"), col = guide_legend(title="High Risk"))
# 
# hr_p
# ggsave(hr_p, filename = hr_adults_name, width = plot_width, height = 4)



# Group plots -------------------------------------------------------------
grp_adults_name <- paste0("outputs/", plotdate, "/group_adults_all", plotdate, ".png")
library(patchwork)

group_plot <- (emp_p + ggtitle("A")) / (inc_p + ggtitle("B")) / (emp_inc_p + ggtitle("C")) + plot_layout(heights = c(1,1,2.3))


# ggsave(group_plot, filename = grp_adults_name, width = plot_width, height = 12)


