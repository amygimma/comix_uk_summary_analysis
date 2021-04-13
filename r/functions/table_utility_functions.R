replace_na <- function(x, replace) { 
  x[is.na(x)] <- "0 (0.0%)"
  x
}

trim_contacts <- function(cont_dt, n) {
  # order contacts by in order giving individually reported priority, then
  # in order to home, work, and school and assign an index number
  cont_dt[cnt_mass == "individual", individually_reported := 1]
  cont_dt[, part_nth_cont := 0]
  cont_dt[
    order(individually_reported, cnt_home, cnt_work, cnt_school, decreasing  = TRUE), 
    part_nth_cont := seq_along(.I), by = c("country", "panel", "wave", "part_id")]

  cont_dt <- cont_dt[part_nth_cont <= n | cnt_none == 1]
  
  cont_dt[, part_nth_cont := NULL]
  return(cont_dt)
}


match_hh_size <- function(hh_total_dt, pwave_uid) {
  part_hh <- hh_total_dt[part_wave_uid == pwave_uid]
  if (nrow(part_hh) == 0) {
    0
  } else {
    part_hh$hh_size
  }
}

add_england_col <- function(dt) {
  r_england <- c("South East", "North West", "West Midlands", "East Midlands",
                 "South West", "Greater London", "North East", "Yorkshire and The Humber",
                 "East of England", "London", "Midlands", "North East and Yorkshire")
  r_not_england <- c("Scotland", "Northern Ireland", "Wales")
  dt[, england := fcase(
    nhs_region %in% r_england, 1,
    nhs_region %in% r_not_england, 0
  )]
  
  dt
}
