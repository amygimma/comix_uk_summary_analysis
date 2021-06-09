study_period_type <- "figure"
source(file.path("r", "table_setup.R"))
source(file.path("r", "functions", "formatting.R"))

source(file.path("r", "functions", "utility_functions.R"))
source(file.path("r", "functions", "new_utility_functions.R"))


part <- part[!is.na(study_period)]
contacts <- merge(contacts, part[, list(part_id, wave, panel, country, study_period)],
                                 by = c("part_id", "wave", "panel", "country"))
contacts <- contacts[!is.na(study_period)]


contacts <- contacts[england == 1 & country == "uk"]
part <- part[england == 1 & country == "uk"]
# part_unique <- part[, list(part_wave_uid)]
mergevars <- c("part_wave_uid", "study_period", "panel", "wave", "nhs_region", "country")
contacts_part <- merge(part[, mergevars, with = F], 
                       contacts, by = mergevars, all.x = T)
contacts_part <- contacts_part[is.na(cnt_mass), cnt_none := 1]
cntp20 <- trim_contacts(contacts_part, 20)
table(cntp20$cnt_none)
cntp50 <- trim_contacts(contacts_part, 50)

byvars <- c("country", "part_wave_uid", "study_period")
partc20 <- cntp20[, .(n_cnt_all = sum(cnt_any, na.rm = T)), by = byvars]
partc50 <- cntp50[, .(n_cnt_all = sum(cnt_any, na.rm = T)), by = byvars]


mean_cnts20 <- partc20[, .(mean_contacts = mean(n_cnt_all)), by = c("study_period")]
mean_cnts50 <- partc50[, .(mean_contacts = mean(n_cnt_all)), by = c("study_period")]
partc20[, study_period := factor(study_period, levels = study_period_levels)]

ggthemr::ggthemr("sky")
ggplot(partc20, aes(x = as.numeric(n_cnt_all))) +
  stat_density(bw = 2,  alpha = 0.7) +
  geom_vline(data = mean_cnts20, aes(xintercept = mean_contacts), linetype = "dashed") +
  scale_x_continuous(limits = c(0,20)) +
  facet_grid(vars(factor(study_period))) +
  theme(strip.text.x = element_text(size = 8)) +
  xlab("Number of Contacts") +
  ylab("Density") +
  theme_bw()



idvars <- c("part_wave_uid", "study_period")
# measvars <- c("n_cnt_all", "n_cnt_home", "n_cnt_work", "n_cnt_school", "n_cnt_other")

# For plotting distribution
partc20a <- cntp20[, .(n_cnt = sum(cnt_any, na.rm = T), setting = "All"), by = c(idvars)]

partc20h <- cntp20[, .(n_cnt = sum(cnt_home, na.rm = T), setting = "Home"), by = c(idvars)]
partc20w <- cntp20[,.(n_cnt = sum(cnt_work, na.rm = T), setting = "Work"), by = c(idvars)]
partc20s <- cntp20[, .(n_cnt = sum(cnt_school, na.rm = T), setting = "School"), by = c(idvars)]
partc20o <- cntp20[, .(n_cnt = sum(cnt_other, na.rm = T), setting = "Other"), by = c(idvars)]

part20settings <- rbindlist(list(partc20a,partc20h, partc20w, partc20s, partc20o))
meanpart20setting <- 
  part20settings[, .(mean_contacts = mean(n_cnt, na.rm = T), linetype = "mean"), 
                 by = c("setting", "study_period")]
meanpart20setting
# For plotting truncated contacts mean
# partl50 <- melt(part50, id.vars = idvars, measure.vars = measvars,
#                 variable.name = "setting", value.name = "n_cnt")
# mean_cnts50_setting <- partl50[, .(mean_contacts = mean(n_cnt)),
#                                by = c("study_period", "setting")]

part20settings[setting == "School", setting := "Education"]
meanpart20setting[setting == "School", setting := "Education"]
setting_levs <- c("All", "Home", "Work", "Education", "Other")
part20settings[, setting := factor(setting, levels = setting_levs)]
# mean_cnts50_setting[, setting := factor(setting, levels = setting_levs)]
part20settings[, study_period := factor(study_period, levels = study_period_levels)]

cd_plot <- ggplot(part20settings, aes(x = as.numeric(n_cnt))) +
  stat_density(bw = 2, alpha = 0.7) +
  geom_vline(data = meanpart20setting, aes(xintercept = mean_contacts, linetype = linetype)) +
  scale_linetype_manual(values = c("mean" = "dashed")) +
  facet_grid(cols = vars(factor(study_period)), rows = vars(setting)) +
  theme(strip.text.x = element_text(size = 8)) +
  labs(fill = "Panel", linetype = "Summary") +
  xlab("Number of Contacts") +
  ylab("Density") +
  theme_bw() +
  theme(strip.text = element_text(size = 7))


cd_plot

ggsave(cd_plot, filename = "outputs/contact_distributions_plotv2.png",
       height = 5.5, width = 8)

