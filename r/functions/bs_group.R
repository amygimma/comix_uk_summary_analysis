



bs_group <- function(dt, 
                     sims, 
                     prop = 1.0, 
                     area_ = "All", 
                     age_ = "All", 
                     gender_ = "All", 
                     soc_group_ = "All",
                     risk_group_ = "All",
                     att_spread_bin_ = "All",
                     att_likely_bin_ = "All",
                     att_serious_bin_ = "All",
                     employ_ = "All",
                     income_ = "All" ,
                     workplace_ = "All"
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

  
  # Subset by income --------------------------------------------------------
  incomename <- income_
  if(income_ == "All"){
    income_ <- c("<5,000", "5,000-9,999", "10,000-14,999", "15,000-19,999",
                 "20,000-24,999", "25,000-34,999", "35,000-44,999",
                 "45,000-54,999", "55,000-99,999", "100,000+", NA,
                 "prefer not to answer")
  } else if(income_ == "<20k"){
    income_ <- c("<5,000", "5,000-9,999", "10,000-14,999", "15,000-19,999")
  } else if(income_ == "20k-44.9k"){
    income_ <- c("20,000-24,999", "25,000-34,999", "35,000-44,999")
  } else if(income_ == "45k+"){
    income_ <- c("45,000-54,999", "55,000-99,999", "100,000+")
  } 
  
  
  #Subset by employ --------------------------------------------------------
  employname <- employ_
  if(employ_ == "All"){
    employ_ <- c("Full time", "Part time", "Self employed", NA)
  } 
  
  # Subset by workplace --------------------------------------------------------
  workplacename <- workplace_
  if(workplace_ == "All"){
    workplace_ <- c("open", "closed", NA)
  } else if(workplace_ == "open"){
    workplace_ <- "open"
  } else if(workplace_ == "closed"){
    workplace_ <- "closed"
  }
  
  # Subset by social group --------------------------------------------------
  socgroupname <- soc_group_
  if(soc_group_ == "All"){
    soc_group_ <- c(NA, "B - Middle class", "C2 - Skilled working class", 
                    "E - Lower level of subsistence", "C1 - Lower middle class", 
                    "D - Working class", "A - Upper middle class"
    )
  } 

  # Subset by risk group --------------------------------------------------
  
  riskgroupname <- risk_group_
  if(risk_group_ == "All"){
    risk_group_ <- c(NA, "yes", "no", 
                    "no_answer"
    )
  } 
  
  # Subset by att_spread group --------------------------------------------------
  attspreadname <- att_spread_bin_
  if(att_spread_bin_ == "All"){
    att_spread_bin_ <- c(NA, "Agree", "Disagree", "Neutral")
  } 

  # Subset by att_likely group --------------------------------------------------
  attlikelyname <- att_likely_bin_
  if(att_likely_bin_ == "All"){
    att_likely_bin_ <- c(NA, "Agree", "Disagree", "Neutral")
  } 
  
  # Subset by att_serious group --------------------------------------------------
  attseriousname <- att_serious_bin_
  if(att_serious_bin_== "All"){
    att_serious_bin_ <- c(NA, "Agree", "Disagree", "Neutral")
  } 
  
  dt <- dt[area %in% area_ & part_age_group %in% age_ & 
             part_gender %in% gender_ &
             part_social_group %in% soc_group_ & 
             part_high_risk %in% risk_group_ &
             part_att_spread_bin %in% att_spread_bin_ &
             part_att_likely_bin %in% att_likely_bin_ &
             part_att_serious_bin %in% att_serious_bin_ &
             part_income %in% income_ & 
             part_employed %in% employ_ &
             part_work_place %in% workplace_]

  bs_list <- list()
  for(i in 1:sims){
    pids <- unique(dt$part_id)
    nsamp <- length(pids)*prop
    df_samp <- data.table(part_id = sample(pids, replace = TRUE, size = nsamp))
    
    tryCatch({
      samp1 <- merge(df_samp, dt, by = "part_id")
      
    }, error = function(error_condition) {

      message("Resampling for merge error")
      message(paste("Error message:", error_condition, sep = "\n"))
      
      # ns <- c("part_id", "part_social_group")
      # part_id_ <- df_samp$part_id[133]
      df_samp2 <- data.table(part_id = sample(pids, replace = TRUE, size = nsamp))
      samp1 <- merge(df_samp2, dt, by = "part_id")
  
    })
    
    bs_dt <- samp1[, .(
      N = .N,
      part_age_group = agename,
      part_gender = gendername,
      part_social_group = socgroupname,
      part_region = regionname,
      part_high_risk = riskgroupname,
      part_income = incomename,
      part_employed = employname,
      part_work_place = workplacename,
      part_att_spread_bin = attspreadname,
      part_att_likely_bin = attlikelyname,
      part_att_serious_bin = attseriousname,
      iteration = i, 
      All = weighted.mean(n_cnt, w = dayweight),
      All_genderage = weighted.mean(n_cnt, w = dayweight * genderageweight_proportion),
      
      Home = weighted.mean(n_cnt_home,  w = dayweight),
      Home_genderage = weighted.mean(n_cnt_home, w = dayweight * genderageweight_proportion),
      
      Work = weighted.mean(n_cnt_work,  w = dayweight),
      Work_genderage = weighted.mean(n_cnt_work, w = dayweight * genderageweight_proportion),
      
      `Work/Educ` = weighted.mean(n_cnt_workschool,  w = dayweight),
      `Work/Educ_genderage` = weighted.mean(n_cnt_workschool,  w = dayweight * genderageweight_proportion),
      
      Other = weighted.mean(n_cnt_other,  w = dayweight),
      Other_genderage = weighted.mean(n_cnt_other, w = dayweight * genderageweight_proportion)
      
      
      # Physical = weighted.mean(n_cnt_phys,  w = dayweight),
      # Inside = weighted.mean(n_cnt_inside,  w = dayweight),
      # Outside = weighted.mean(n_cnt_outside,  w = dayweight),
      # `Other house` = weighted.mean(n_cnt_other_house,  w = dayweight),
      # `Supermarket` = weighted.mean(n_cnt_supermarket,  w = dayweight),
      # `Bar restaurant` = weighted.mean(n_cnt_bar_rest,  w = dayweight)
      ),
      by = .(start_date, mid_date, end_date, survey_round)
      ]
    bs_list[[i]] <- bs_dt
  }
  rbindlist(bs_list)
}  



