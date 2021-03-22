## Name: fig103_precautions
## Description: Plot facemask use and handwashing
## Input file: part.qs
## Functions: 
## Output file: part.qs

# Packages ----------------------------------------------------------------
library(data.table)
library(patchwork)
library(ggplot2)
# Source user written scripts ---------------------------------------------


# Set plot parameters -----------------------------------------------------
expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))
expand_dates_left <- expand_dates
expand_dates_left[2] <- as.Date("2020-08-02")

theme_set(cowplot::theme_cowplot(font_size = 11) + theme(strip.background = element_blank()))

# Load participant data ---------------------------------------------------
part <- qs::qread("../comix/data/part.qs")
contacts <- qs::qread("../comix/data/contacts.qs")


adults_id <- part[sample_type == "adult" &
                  !area_3_name %in% c("Scotland", "Northern Ireland", "Wales") &
                  country == "uk" &
                  !survey_round %in% c(6,7),]$part_wave_uid

part <- part[part_wave_uid %in% adults_id ]
part[ ,  start_date := min(date), by = .(panel, survey_round) ]
part[ ,  end_date := max(date), by = .(panel, survey_round) ]
part[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round)]

dt <- part

# Attitudes ---------------------------------------------------------------
cols_1 <- c('#ccebc5','#a8ddb5','#7bccc4','#43a2ca','#0868ac')

plotter_lk <- function(var, lab, cols_ = cols_1) {
  p1 <- dt[ !is.na(get(var))& get(var) != "no answer"]
  dt[, part_total_responses := .N, by = part_id]
  p2 <- p1[, .N, by = .(get(var), mid_date,survey_round)]
  p2[, perc := N / sum(N), by = .(mid_date, survey_round)]
  p2[, type := "Everyone"]
  
  ggplot(p2, aes(x = mid_date, fill = get)) +
    geom_area(aes(y = perc)) +
    scale_y_continuous(labels = scales::percent, expand = expansion(0)) + 
    scale_x_date(breaks = "month", date_labels = "%b") +
    expand_limits(x = expand_dates) +
    scale_fill_discrete(name = "", type = cols_) +
    theme(legend.position = "top", legend.direction = "horizontal") +
    guides(guide_legend(label.position = "top")) +
    labs(fill = lab) +
    labs(x = "", y = "", subtitle = lab) 
}


lk_spread_p  <- plotter_lk("part_att_spread",  lab = "Likely to give to someone I know") 
lk_catch_p   <- plotter_lk("part_att_likely",  lab = "Likely to get covid")
lk_serious_p <- plotter_lk("part_att_serious",  lab = "Covid would be serious for me")
lk_hr_p      <- plotter_lk("part_high_risk",  lab = "High risk", cols_ = c(cols_1[1], cols_1[5]))




# Avg contacts- no CIs ------------------------------------------------------------

pdt <- qs::qread('data/dt_2w.qs')

map_likert2 <- c("Strongly agree" = "Agree",
                 "Tend to agree" = "Agree",
                 "Neither agree nor disagree" = "Neutral",
                 "Tend to disagree" = "Disagree", 
                 "Strongly disagree" = "Disagree"
)
map_hr <- c(
  "yes" = "Yes",
  "no" = "No"
)

pdt[, part_att_spread_bin := map_likert2[part_att_spread]]
pdt[, part_att_likely_bin := map_likert2[part_att_likely]]
pdt[, part_att_serious_bin := map_likert2[part_att_serious]]
pdt[, part_high_risk_bin := map_hr[part_high_risk]]
  
spread <- pdt[, .(cnt =weighted.mean(n_cnt, w = dayweight)), by = .(part_att_spread_bin, start_date, mid_date, end_date, survey_round)]
likely <- pdt[, .(cnt =weighted.mean(n_cnt, w = dayweight)), by = .(part_att_likely_bin, start_date, mid_date, end_date, survey_round)]
serious <- pdt[, .(cnt =weighted.mean(n_cnt, w = dayweight)), by = .(part_att_serious_bin, start_date, mid_date, end_date, survey_round)]
high_risk <- pdt[, .(cnt =weighted.mean(n_cnt, w = dayweight)), by = .(part_high_risk_bin, start_date, mid_date, end_date, survey_round)]

plot_mean <- function(dt, time_break = "2 month", upper_limit = 6, var){
  ggplot(dt, aes(x = mid_date)) +
    #geom_ribbon(aes(ymin = lci, ymax = uci, group = part_age_group), alpha = 0.2) +
    geom_line( aes(y = cnt, linetype = get(var), col = get(var))) +
    labs(title = "", y = "Mean contacts", x = "") +
    scale_y_continuous(expand = expansion(0), limits = c(0,upper_limit)) +
    expand_limits(y = 0) +
    scale_x_date(breaks = time_break, date_labels = "%b", name = "") +
    expand_limits(x = expand_dates) + 
    theme(
      panel.spacing.y =  unit(1, "lines"),
      legend.position = c(0.1,0.8)
    ) +
    scale_linetype(name = var) +
    scale_color_discrete(name = var) +
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

avg_spread_p  <-  plot_mean(spread, var = "part_att_spread_bin") 
avg_catch_p   <-  plot_mean(likely, var = "part_att_likely_bin")
avg_serious_p <-  plot_mean(serious, var = "part_att_serious_bin")
avg_hr_p      <-  plot_mean(high_risk, var = "part_high_risk_bin")



# Filter data -------------------------------------------------------------
mid_dates <- part[, .(mid_date = first(mid_date)), by = .(survey_round, country)]

contacts <- merge(contacts, mid_dates, by = c("survey_round", "country"))

# Facemask use ------------------------------------------------------------
# fm <-  part[, .(.N, fm = sum(part_face_mask == "yes"), nofm = sum(part_face_mask == "no") ), by = .(survey_round, mid_date, country)] 
# 
# fm[, fm_per := fm/(fm +nofm)]
# 
# 
# facemask_p <- ggplot(fm) +
#   geom_point(aes(x = mid_date, y = fm_per)) +
#   geom_line(aes(x = mid_date, y = fm_per)) +
#   scale_y_continuous(expand = expansion(0), labels = scales::percent, limits = c(0,1)) +
#   expand_limits(x = expand_dates) +
#   labs(y = "", x = "", subtitle = "Use of facemasks") +
#   ggtitle("")
# 
# facemask_p


# # Hand washing and sanitiser ------------------------------------------------------------
# 
# cols_freq <- c('#ccebc5','#a8ddb5','#7bccc4','#43a2ca','#0868ac')
# handwash_p <- ggplot(part[survey_round <20], aes(x = mid_date, fill = part_handwash3h)) + 
#   geom_area(position="fill", stat="count") +
#   labs(fill = "Times in last 3 hours", x = "", y = "", subtitle = "Washed hands in last three hours") +
#   scale_fill_discrete(name = "", type = cols_freq) +
#   scale_y_continuous(expand = expansion(0), labels = scales::percent) +
#   scale_x_date(breaks = "month", date_labels = "%b") +
#   expand_limits(x = expand_dates_left) +
#   theme(legend.position = "top", legend.direction = "horizontal") 
# 
# 
# handsanitise_p <- ggplot(part[survey_round <20], aes(x = mid_date, fill = part_handsanit3h)) + 
#   geom_area(position="fill", stat="count") +
#   labs(fill = "", x = "", y = "", subtitle = "Used hand sanitiser in last three hours") +
#   scale_fill_discrete(name = "", type = cols_freq) +
#   scale_y_continuous(expand = expansion(0), labels = scales::percent) +
#   scale_x_date(breaks = "month", date_labels = "%b") +
#   expand_limits(x = expand_dates_left) +
#   theme(legend.position = "top", legend.direction = "horizontal") 
# 
# 
# layout <- "
# AABB
# AABB
# CCDD
# CCDD
# EEFF
# EEFF
# GGHH
# GGHH
# IIJK
# IIJK
# "
# 
# fig2 <- 
#   lk_catch_p   +   ggtitle("A") +
#   lk_spread_p +    ggtitle("B") +      
#   avg_catch_p +    ggtitle("C") +
#   avg_spread_p +   ggtitle("D") +
#   lk_serious_p +   ggtitle("E") +
#   lk_hr_p +        ggtitle("F") +
#   avg_serious_p +  ggtitle("G") +
#   avg_hr_p +       ggtitle("H") +
#   facemask_p +     ggtitle("I") +
#   handsanitise_p + ggtitle("J") + 
#   handwash_p + plot_layout(design = layout)
# fig2
# 
# ggsave(fig2, filename = "outputs/fig2-prec_behav.png" , height = 16, width = 17, dpi = 300)
# 
# 
#   
# 
# 
# 
# fig2 <- fm_p + pa + pa + pb + pb + plot_layout(design = layout)
# 
# fig2
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Other graphs ------------------------------------------------------------
# 
# 
# part[part_age_group %in% c( "5-11", "12-17"),  part_age_group3 :="5-17"]
# part[part_age_group %in% c("40-49", "50-59", "30-39", "18-29"),  part_age_group3 :="18-59"]
# part[part_age_group %in% c( "70-120", "60-69"),  part_age_group3 := "60+"]
#    
# 
# 
# 
# fm_age <-  part[, .(.N, fm = sum(part_face_mask == "yes"), nofm = sum(part_face_mask == "no") ), by = .(survey_round, mid_date, part_age_group3)] 
# 
# fm_age[, fm_per := fm/(fm +nofm)]
# ggplot(fm_age) +
#   geom_line(aes(x = mid_date, y = fm_per, col= part_age_group3)) +
#   scale_y_continuous(limits = c(0,1)) +
#   ggtitle("A: Facemask use") 
# 
# non_household_cnt <- contacts[cnt_household == 0, .(total_non_hh_contacts = .N), by = c("part_wave_uid")]
# total_cnt <- contacts[, .(total_contacts = .N), by = c("part_wave_uid")]
# 
# part <- merge(part, non_household_cnt, by = "part_wave_uid", all.x = T)
# part[is.na(total_non_hh_contacts), total_non_hh_contacts := 0]
# part <- merge(part, total_cnt, by = "part_wave_uid", all.x = T)
# part[is.na(total_contacts), total_non_hh_contacts := 0]
# 
# 
# fm_age_w_contacts <-  part[total_non_hh_contacts > 0, 
#                            .(.N, fm = sum(part_face_mask == "yes"), nofm = sum(part_face_mask == "no") ), 
#                            by = .(survey_round, mid_date, part_age_group3)] 
# 
# fm_age_w_contacts[, fm_per := fm/(fm +nofm)]
# ggplot(fm_age_w_contacts) +
#   geom_line(aes(x = mid_date, y = fm_per, col= part_age_group3)) +
#   scale_y_continuous(limits = c(0,1)) +
#   ggtitle("A: Facemask use (with non-hh contacts)") 
# 
# fm_region <-  part[country == "uk", .(.N, fm = sum(part_face_mask == "yes"), nofm = sum(part_face_mask == "no") ), by = .(survey_round, mid_date, area_3_name)] 
# fm_region[, fm_per := fm/(fm +nofm)]
# fm_country <-  part[, .(.N, fm = sum(part_face_mask == "yes"), nofm = sum(part_face_mask == "no") ), by = .(survey_round, mid_date, country)] 
# fm_country[, fm_per := fm/(fm +nofm)]
# 
# ggplot(fm_country) +
#   geom_line(aes(x = mid_date, y = fm_per, col= country)) +
#   scale_y_continuous(limits = c(0,1)) +
#   ggtitle("A: Facemask use") 
# 
# ggplot(fm_region) +
#   geom_line(aes(x = mid_date, y = fm_per, col= area_3_name)) +
#   scale_y_continuous(limits = c(0,1)) +
#   ggtitle("A: Facemask use") 





# Likert vars -------------------------------------------------------------



plotter_lk_thresholds <- function(var, lab, threshold = 5) {
  p1 <- dt[ !is.na(get(var))]
  dt[, part_total_responses := .N, by = part_id]
  p2 <- p1[, .N, by = .(get(var), survey_round)]
  p2[, perc := N / sum(N), by = .(survey_round)]
  p2[, type := "Everyone"]
  
  p1_high_response <- dt[ part_total_responses > threshold & !is.na(get(var))]
  p2_high_response <- p1_high_response[, .N, by = .(get(var), survey_round)]
  p2_high_response[, perc := N / sum(N), by = .(survey_round)]
  p2_high_response[, type := paste0("Greater than ", threshold, " responses")]
  
  p_all <- rbind(p2, p2_high_response)
  
  ggplot(p_all, aes(x = survey_round, fill = get)) +
    geom_col(aes(y = perc)) +
    scale_y_continuous(labels = scales::percent) + 
    labs(fill = lab) +
    facet_wrap(.~ type, ncol = 2)
}

part[, table(week, survey_round)]

th <- 5

plotter_lk_thresholds("part_att_spread",  lab = "High risk", threshold = 6)
plotter_lk_thresholds("part_att_spread",  lab = "High risk")
plotter_lk_thresholds("part_att_likely",  lab = "Likely to give to someone I know", threshold = th)
plotter_lk_thresholds("part_att_serious",  lab = "Covid would be serious for me", threshold = th)
plotter_lk_thresholds("part_high_risk",  lab = "High risk", threshold = th)


