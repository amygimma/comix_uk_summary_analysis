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

# Variables ---------------------------------------------------------------
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


# Set plot parameters -----------------------------------------------------
expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))
expand_dates_left <- expand_dates
expand_dates_left[2] <- as.Date("2020-08-02")

theme_set(cowplot::theme_cowplot(font_size = 11) + theme(strip.background = element_blank()))

# Load participant data ---------------------------------------------------
part <- qs::qread("../comix/data/part.qs")
contacts <- qs::qread("../comix/data/contacts.qs")

younger_adults <- c(c("18-29", "30-39", "40-49", "50-59"))
older_adults <- c("60-69", "70-120")
part[part_age_group %in% younger_adults, part_age_group_bin := "18-59"]
part[part_age_group %in% older_adults, part_age_group_bin := "60+"]

adults_id <- part[sample_type == "adult" &
                    !area_3_name %in% c("Scotland", "Northern Ireland", "Wales") &
                    country == "uk" &
                    !survey_round %in% c(6,7),]$part_wave_uid

part <- part[part_wave_uid %in% adults_id ]
part[ ,  start_date := min(date), by = .(panel, survey_round) ]
part[ ,  end_date := max(date), by = .(panel, survey_round) ]
part[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round)]


bin_levs <- c("Agree", "Neutral", "Disagree")
part[, part_att_spread_bin := factor(map_likert2[part_att_spread], levels = bin_levs)]
part[, part_att_likely_bin := factor(map_likert2[part_att_likely], levels = bin_levs)]
part[, part_att_serious_bin := factor(map_likert2[part_att_serious], levels = bin_levs)]
part[, part_high_risk := map_hr[part_high_risk]]


dt <- part

# Attitudes ---------------------------------------------------------------
cols_1 <- c('#ccebc5','#a8ddb5','#7bccc4','#43a2ca','#0868ac')

plotter_lk <- function(var, lab, cols_ = cols_1) {
  # browser()
  p1 <- dt[ !is.na(get(var))& !get(var) %in% c("no answer", "Don’t know", "unknown")]
  dt[, part_total_responses := .N, by = part_id]
  p2 <- p1[, .N, by = .(get(var), mid_date,survey_round, part_age_group)]
  p2[, perc := N / sum(N), by = .(mid_date, survey_round, part_age_group)]
  p2[, type := "Everyone"]
  p2[, part_age_group := paste("Ages", part_age_group)]
  bin_levs <- c("Agree", "Neutral", "Disagree")
  if (var %like% "part_att") p2[, get := factor(get, levels = bin_levs)]
  ggplot(p2, aes(x = mid_date, fill = factor(get))) +
    geom_area(aes(y = perc)) +
    facet_wrap(vars(part_age_group)) +
    scale_y_continuous(labels = scales::percent, expand = expansion(0)) + 
    scale_x_date(breaks = "3 months", date_labels = "%b") +
    expand_limits(x = expand_dates) +
    scale_fill_discrete(name = "", type = cols_) +
    theme(legend.position = "top", legend.direction = "horizontal") +
    guides(guide_legend(label.position = "top")) +
    labs(fill = lab) +
    labs(x = "", y = "", subtitle = lab) 
}


lk_spread_p  <- plotter_lk("part_att_spread_bin", 
                           lab = 'A. "I am worried that I might spread coronavirus to someone who is vulnerable"') 
lk_likely_p  <- plotter_lk("part_att_likely_bin",  
                           lab = 'B. "I am likely to catch coronavirus"')
lk_serious_p <- plotter_lk("part_att_serious_bin",
                           lab = 'C. "Coronavirus would be a serious illness for me"')
lk_hr_p      <- plotter_lk("part_high_risk",  lab = "D. High risk participant", cols_ = c(cols_1[1], cols_1[5]))


perc_proportions <- (lk_spread_p + lk_likely_p + lk_serious_p + lk_hr_p ) +
  plot_layout()

ggsave(plot = perc_proportions, filename = "outputs/perception_proportions.png",
       height = 10, width = 12)



# ggplot(part[!is.na(part_att_likely_bin)], aes(y = part_att_likely_bin, x = part_age_group)) +
#   geom_jitter(alpha = 0.05, color = '#0868ac', fill = '#0868ac') +
#   scale_y_discrete(limits = rev) +
#   facet_wrap(vars(mid_date)) +
#   theme()



