

library(data.table)



# Prep data ---------------------------------------------------------------


part <- qs::qread("../comix/data/part.qs")
contacts <- qs::qread("../comix/data/contacts.qs")


adults_id <- part[
                    !area_3_name %in% c("Scotland", "Northern Ireland", "Wales") &
                    country == "uk" &
                    !survey_round %in% c(6,7),]$part_wave_uid

part <- part[part_wave_uid %in% adults_id ]
part[ ,  start_date := min(date), by = .(panel, survey_round) ]
part[ ,  end_date := max(date), by = .(panel, survey_round) ]
part[, mid_date := start_date + floor((end_date - start_date)/2) , by = .(survey_round)]

pid <- part$part_wave_uid

names(contacts)

contacts <- contacts[part_wave_uid %in% pid]


# Overall stats -----------------------------------------------------------


obs <- nrow(part)
npart <- length(unique(part$part_id))
ncontact <- nrow(contacts)
nweeks <- max(part$survey_round)
mindate <- min(part$date)
maxdate <- max(part$date)
part[, table(sample_type)]
nadult <- length(unique(part[sample_type == "adult"]$part_id))
nchild <- length(unique(part[sample_type == "child"]$part_id))

parttext1 <- "Overall, we recorded %d observations from %d participants who reported %d contacts over a period of %d weeks (%s to %s). %d of the particiapants were children's contacts, and %d for adults."

t1 <- sprintf(parttext1, obs, npart, ncontact , nweeks,  mindate, maxdate, nchild, nadult)


# Number of responses -----------------------------------------------------------
responses <- part[, .N, by = part_id]$N

mean(responses)
med_resp <- median(responses)
max_resp <- max(responses)
per_1_resp <- round(sum(responses==1)/length(responses),3)*100
n_1_resp <- sum(responses==1)

parttext2 <- "The median number of responses was %d (min-max %d-%d) with %1.1f%% (%d) responding only once." 

t2 <- sprintf(parttext2, med_resp, 1, max_resp, per_1_resp, n_1_resp)

cat(paste0(t1, t2))



# Gender ------------------------------------------------------------------

pgen <- part[part_gender %in% c("male", "female"), .(part_gender = first(part_gender)), by = part_uid]
pgen[, table(part_gender)]

male_ <- sum(pgen$part_gender == "male", na.rm = TRUE)
male_per <- male_/nrow(pgen)*100
female_ <- sum(pgen$part_gender == "female", na.rm = TRUE)
female_per <- female_/nrow(pgen)*100

parttext3 <- "The sample consisted of %d (%1.1f%%) females, %d (%1.1f%%) males"

t3 <- sprintf(parttext3, female_, female_per, male_, male_per)

t3

# Region ------------------------------------------------------------------


preg <- part[, .(part_region = first(area_3_name)), by = part_uid]

preg[part_region %in% c("Yorkshire and The Humber", "North East"), part_region := "North East and Yorkshire"]
preg[part_region %in% c("East Midlands", "West Midlands"), part_region := "Midlands"]

preg[, table(part_region)]

reg_obs <- nrow(preg)
preg[, table(part_region)]

mi_ <- sum(preg$part_region == "Midlands", na.rm = TRUE)
mi_per <- mi_/npart*100
nw_ <- sum(preg$part_region == "North West", na.rm = TRUE)
nw_per <- nw_/npart*100

parttext4 <- "Participants were recruited from all over England, the area with the most participants was the Midlands with %d (%1.1f%%) participants and the North West had the least with %d (%1.1f%%)." 

t4 <- sprintf(parttext4, mi_, mi_per, nw_, nw_per)

t4
prop.table(table(preg$part_region))



# Social group ------------------------------------------------------------
psg <- part[, .(part_social_group = first(part_social_group)), by = part_uid]


psg[, table(part_social_group)]

reg_obs <- nrow(preg)
preg[, table(part_region)]

abc1_ <- sum(psg$part_social_group %in% c("A - Upper middle class", "B - Middle class",  "C1 - Lower middle class") , na.rm = TRUE)
abc1_per <- abc1_/npart*100
c2de_ <- sum(psg$part_social_group %in% c("C2 - Skilled working class", "D - Working class",  "E - Lower level of subsistence") , na.rm = TRUE)
c2de_per <- c2de_/npart*100


parttext4 <- "%d (%1.1f%%) of Participants were categorised as middle class (ABC1) and %d (%1.1f%%) was categorised as working class." 

t4 <- sprintf(parttext4, abc1_, abc1_per, c2de_, c2de_per)

t4
prop.table(table(psg$part_social_group))


summary(part[,mean(hh_size), by = survey_round][order(survey_round)])
  