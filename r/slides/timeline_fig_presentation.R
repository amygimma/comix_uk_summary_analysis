library(data.table)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemr)
library(ggthemes)
library(openxlsx)



cols <- c("#17877b", "#D7402B", "#055a8c", "#daa520", "#20bdcc", "#010f5b", 
  "#d72638")

cols <- c("#0D5257", "#00AEC7")
expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))

dt <- qs::qread('../comix/data/part_min.qs')
adults_id <- dt[#sample_type == "adult" &
  !area_3_name %in% c("Scotland", "Northern Ireland", "Wales") &
    country == "uk" &
    !survey_round %in% c(6,7),]$part_wave_uid
# 
sample_type_count <- dt[part_wave_uid %in% adults_id,
                        .(start_date = min(date), end_date = max(date), .N), by = .(sample_type, survey_round, panel)]
sample_type_count[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round, panel)]

mid_dates <- sample_type_count[, .(sample_type, survey_round, mid_date, panel)]

sample_type_plot <- dcast(sample_type_count, formula = sample_type + panel ~ mid_date, value.var = "N", fill = 0)

sample_type_count[, sample_type := ifelse(sample_type == "child", "Child", "Adult")]

sample_type_count[, sample_type := factor(sample_type)]
sample_type_count[, Recruitment := ifelse(survey_round < 20, "First", "Second")]
# sample_type_count[, Recruitment := NA_character_]
# sample_type_count[, Recruitment := ifelse(panel %in% c("A", "C") & survey_round < 20, "First", Recruitment)]
# sample_type_count[, Recruitment := ifelse(panel %in% c("B", "D") & survey_round < 20, "Second", Recruitment)]
# sample_type_count[, Recruitment := ifelse(panel %in% c("E") & survey_round >= 20, "Third", Recruitment)]
# sample_type_count[, Recruitment := ifelse(panel %in% c("F") & survey_round >= 20, "Fourth", Recruitment)]
# sample_type_count[, Recruitment := factor(Recruitment, levels = c("First", "Second", "Third", "Fourth"))]
# table(sample_type_count$Recruitment)
# ggsave(counts_all_p, filename = "outputs/all_counts.png", width = 9, height  = 4)
counts_all_p_line2 <- ggplot(sample_type_count) +
  geom_point(aes(y = N, x = mid_date,  color = sample_type)) +
  geom_line(aes(y = N, x = mid_date,  color = sample_type, linetype = factor(Recruitment))) +
  # geom_line(aes(y = N, x = mid_date,  color = sample_type)) +
  
  scale_color_manual(values = cols, name = "Sample Type") +
  scale_linetype_manual(values = c(1,2,3,4), name = "Recruitment") +
  # scale_size(range = c(1, 3.5), name = "Number of participants", breaks = c(1000, 1500, 2000)) +
  scale_x_date(breaks = "month", date_labels = "%b '%y", name = "") +
  expand_limits(x = expand_dates, y = c(0,3000))  +
  scale_y_continuous(breaks = seq(0,3000,500)) +
  # expand_limits(x = expand_dates, y = c(0,2800))  +
  annotate("rect",
           xmin = study_dates[1], xmax = study_dates[2],
           ymin = 0, ymax = 3000, alpha = .1) +
  annotate("rect",
           xmin = study_dates[3], xmax = study_dates[4],
           ymin = 0, ymax = 3000, alpha = .1) +
  annotate("rect",
           xmin = study_dates[7], xmax = study_dates[8],
           ymin = 0, ymax = 3000, alpha = .1) +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.05, 0.80),
    panel.grid.major = element_line(colour="grey", size=0.05),
    legend.text =  element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 14)) +
  labs(y = "Number of participants", subtitle = "")
counts_all_p_line2


ggsave(counts_all_p_line2, filename = "outputs/counts_line2.png", height = 3, width = 12)

