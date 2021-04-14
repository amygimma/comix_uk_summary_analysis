# Packages ----------------------------------------------------------
## you'll need the qs package as well

library(data.table)
library(ggplot2)
library(covidregionaldata)
library(patchwork)
library(magrittr)
# Get the data and plot it ------------------------------------------------
# cases <- get_national_data(country = "England")
cases <- get_regional_data(country = "UK")
cases <- as.data.table(cases)
names(cases)
cases <- cases[region == "England"]
class(cases$date)


expand_dates <- c(as.Date("2020-03-15"), as.Date("2021-04-01"))

# case_p <- ggplot(cases) +
#   geom_col(aes(x= date, y = cases_new)) +
#   expand_limits(x = expand_dates) 
# 
# 
# hosp_p <- ggplot(cases) +
#   geom_col(aes(x= date, y = hosp_new)) +
#   expand_limits(x = expand_dates) 
# case_p 
# hosp_p 

fwrite(cases, "data/epiforecasts_cases_hospitilizations.csv", row.names = FALSE)

# Set themes etc

