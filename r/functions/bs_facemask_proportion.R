


bs_facemask_proportion <- function(dt, 
                     sims, 
                     prop = 1.0, 
                     area_ = "All", 
                     age_ = "All", 
                     gender_ = "All", 
                     has_non_hh_contacts_ = T
                     

) {
  
  # subset by region --------------------------------------------------------
  regionname <- area_
  if(area_ == "England"){
    area_ <- c("East of England", "Greater London", "Midlands", "North East and Yorkshire", 
               "North West", "South East", "South West")
  } else if(area_ == "All"){
    area_ <- c("East of England", "Greater London", "Midlands", "North East and Yorkshire", 
               "North West", "South East", "South West", "Wales", "Scotland", "Northern Ireland")
  } else if(area_ == "Tier 4"){
    area_ <- c("East of England", "Greater London", "South East")
  } else if(area_ == "Not Tier 4"){
    area_ <- c("Midlands", "North East and Yorkshire", 
               "North West", "South West")
  }
  
  # Subset by age -----------------------------------------------------------
  agename <- age_
  if(age_ == "All"){
    age_ <- c("70-120", "60-69", "40-49", "50-59", "30-39", "18-29", "5-11", 
              "12-17", "0-4", NA)
  } else if(age_ == "All-adults"){
    age_ <- c("70-120", "60-69", "40-49", "50-59", "30-39", "18-29")
  } else if(age_ == "5-17"){
    age_ <- c( "5-11", "12-17")
  } else if(age_ == "18-59"){
    age_ <- c(  "40-49", "50-59", "30-39", "18-29")
  } else if(age_ == "60+"){
    age_ <- c( "70-120", "60-69")
  }
  
  
  # Subset by gender --------------------------------------------------------
  gendername <- gender_
  if(gender_ == "All"){
    gender_ <- c("female", "male", "other", NA)
  } 
  
  dt <- dt[area %in% area_ & part_age_group %in% age_ & part_gender %in% gender_]
  
  
  bs_list <- list()
  for(i in 1:sims){
    pids <- unique(dt$part_id)
    nsamp <- length(pids)*prop
    df_samp <- data.table(part_id = sample(pids, replace = TRUE, size = nsamp))
    
    samp1 <- merge(df_samp, dt, by = "part_id")
    if (has_non_hh_contacts_) {
      # message("Filtering for participants with non-hh contacts")
      samp1 <- samp1[n_cnt_non_household > 0]
    }

    table(samp1$n_cnt_household)
    
    fm <-  samp1[, .(N = .N, 
                    fm = sum(part_face_mask == "yes"),
                    nofm = sum(part_face_mask == "no"),
                    part_age_group = agename,
                    part_gender = gendername,
                    has_non_hh_contacts = has_non_hh_contacts_,
                    iteration = i), 
                by = .(survey_round, start_date, mid_date, end_date, country)]
    fm[, fm_per := fm/(fm +nofm)]
    
    bs_list[[i]] <- fm
  }
  rbindlist(bs_list)
}  



