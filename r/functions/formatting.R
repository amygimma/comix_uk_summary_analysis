table_group_levels <- c("Dates", "All",  "All sample type", "Age Group (Children)", 
                        "Age Group (Adults)", "Gender", "Household Size", 
                        "Social Group", "NHS Region")

dates <- c("Start", "End")
age_levels <- c("0-4", "5-11", "12-17", "Unknown age*", "18-29", "30-39", "40-49","50-59",  
                "60-69", "70+")

gender_map <- c(
  "Male" = "male", 
  "Female" = "female"
)

gender_nb_levels <- c("Female", "Male", "Other", "Not reported")

hh_size_levels <- c("1",  "2", "3-5", "6+", "Unknown")
social_group_levels <- c("A - Upper middle class", "B - Middle class", 
                         "C1 - Lower middle class",  "C2 - Skilled working class", 
                         "D - Working class", "E - Lower level of subsistence"
)
nhs_region_levels <- c("East of England", "Greater London", "Midlands",
                       "North East and Yorkshire", "North West", "South East", "South West", 
                       "West Midlands"
)

table_category_levels <- c(dates, "-", age_levels, gender_nb_levels, 
                           hh_size_levels, social_group_levels, nhs_region_levels)

study_period_levels <- c("Lockdown 1", "Reduced restrictions", "Lockdown 2", "Christmas", "Lockdown 3"
)
  
  
  
