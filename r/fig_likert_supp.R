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
dt <- dt[date >= as.Date("2020-03-23") & date <= as.Date("2021-03-26")]
dt[area_3_name %in% c("Yorkshire and The Humber", "North East"), area := "North East and Yorkshire"]
dt[area_3_name %in% c("East Midlands", "West Midlands"), area := "Midlands"]
area_3_england <- c("South East", "North West", "West Midlands", "East Midlands", 
                    "South West", "Greater London", "North East", "Yorkshire and The Humber", 
                    "East of England")
dt <- dt[country == "uk" & area_3_name %in% area_3_england]



# Attitudes ---------------------------------------------------------------
cols_1 <- c('#ccebc5','#a8ddb5','#7bccc4','#43a2ca','#0868ac')
cols_1 <- c("#055a8c", "#d72638", "#17877b", "#daa520", "#20bdcc", "#010f5b")
plotter_lk <- function(var, lab, cols_ = cols_1, bin_levs = c("Agree", "Neutral", "Disagree")) {
  p1 <- dt[ !is.na(get(var))& !get(var) %in% c("no answer", "Don’t know", "unknown")]
  dt[, part_total_responses := .N, by = part_id]
  p2 <- p1[, .N, by = .(get(var), mid_date,survey_round, part_age_group)]
  p2[, perc := N / sum(N), by = .(mid_date, survey_round, part_age_group)]
  p2[, type := "Everyone"]
  p2[, part_age_group := paste("Ages", part_age_group)]

  if (var %like% "part_att") p2[, get := factor(get, levels = bin_levs)]
  ggplot(p2, aes(x = mid_date, fill = factor(get))) +
    geom_area(aes(y = perc)) +
    facet_wrap(vars(part_age_group)) +
    scale_y_continuous(labels = scales::percent, expand = expansion(0)) + 
    scale_x_date(breaks = "3 months", date_labels = "%b") +
    expand_limits(x = expand_dates) +
    scale_fill_discrete(name = "", type = cols_) +
    theme(legend.position = "top", 
          # legend.direction = "horizontal",
          legend.text = element_text(size = 8.5)) +
    labs(fill = lab) +
    labs(x = "", y = "", subtitle = lab) +
    guides(guide_legend(label.position = "top", ncol = 3, nrow = 2)) 
    
}

cols <- cols_1[c(5,1,6)]
lk_spread_p  <- plotter_lk("part_att_spread_bin", 
                           lab = 'A. I am worried that I might spread coronavirus to someone who is vulnerable',
                           cols_ = cols) 
lk_likely_p  <- plotter_lk("part_att_likely_bin",  
                           lab = 'B. I am likely to catch coronavirus', 
                           cols_ = cols)
lk_serious_p <- plotter_lk("part_att_serious_bin",
                           lab = 'C. Coronavirus would be a serious illness for me',
                           cols_ = cols)
cols <- c(cols_1[c(6,5)])
lk_hr_p      <- plotter_lk("part_high_risk",  lab = "D. High risk participant", 
                           cols_ =  cols)


perc_proportions <- (lk_spread_p + lk_likely_p + lk_serious_p + lk_hr_p ) +
  plot_layout()

ggsave(plot = perc_proportions, filename = "outputs/perception_proportions_c2.png",
height = 10, width = 12)



# ggplot(part[!is.na(part_att_likely_bin)], aes(y = part_att_likely_bin, x = part_age_group)) +
#   geom_jitter(alpha = 0.05, color = '#0868ac', fill = '#0868ac') +
#   scale_y_discrete(limits = rev) +
#   facet_wrap(vars(mid_date)) +
#   theme()

cols <- rev(c( "#D7402B", "#055a8c", "#daa520", "#20bdcc", "#010f5b", 
               "#17877b"))

bin_levs <- c("Strongly agree","Tend to agree", "Neither agree nor disagree","Tend to disagree",  
  "Strongly disagree",  NA)
lk_spread_p  <- plotter_lk("part_att_spread", 
                           lab = 'A. I am worried that I might spread coronavirus to someone who is vulnerable',
                           cols_ = cols, 
                           bin_levs = bin_levs) 
lk_likely_p  <- plotter_lk("part_att_likely",  
                           lab = 'B. I am likely to catch coronavirus', 
                           cols_ = cols, 
                           bin_levs = bin_levs)
lk_serious_p <- plotter_lk("part_att_serious",
                           lab = 'C. Coronavirus would be a serious illness for me',
                           cols_ = cols, 
                           bin_levs = bin_levs)

cols <- c("#17877b", "#D7402B", "#055a8c", "#daa520", "#20bdcc", "#010f5b", 
          "#d72638")
cols <- c(cols_1[c(6,5)])
lk_hr_p      <- plotter_lk("part_high_risk",  lab = "D. High risk participant", 
                           cols_ =  cols, 
                           bin_levs = c("Yes", "No"))


perc_proportions <- (lk_spread_p + lk_likely_p + lk_serious_p + lk_hr_p ) +
  plot_layout()

perc_proportions

ggsave(plot = perc_proportions, filename = "outputs/perception_proportions_c2_all.png",
       height = 9, width = 14)

