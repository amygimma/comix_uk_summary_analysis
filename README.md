# CoMix summary analysis for the United Kingdom


 * This repository is unfinished, please return soon for the final version*
 
### Running code

Will fill out this section

#### Installing dependencies

```
install.packages("drat")
drat:::add("epiforecasts")
install.packages("covidregionaldata") 

install.packages("data.table")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("mgcv")
install.packages("patchwork")

# ## If ggthemr is not available for your version of R, install using devtools instead:
# install.packages("ggthemr") 
# # install.packages("devtools")
# # devtools::install_github("Mikata-Project/ggthemr")
```

#### Getting data

Zenodo descpription or summary of other published dataset

#### Running with shell script

For mac or linux: </br>
Run `sh run_analysis_mac.sh` (runs with default of 1000 bootstraps), or to specify number of samples run with an argument, eg `sh run_analysis_mac.sh 200`

For windows: </br>
Run `sh run_analysis_windows.sh` (runs with default of 1000 bootstraps), or to specify number of samples run with an argument, eg `sh run_analysis_windows.sh 200`



#### Data cleaning and analysis

#### Figures

#### Tables

### File structure

#### Data cleaning and analysis

dm100_prep_for_avg_contact_data.R

dm101_calc_avg_contact_data.R      

dm102_calc_avg_contact_data_risk.R

dm103_calc_avg_contact_data_se.R

dm111_calc_avg_proportions.R         


#### Figures

fig1_avg_contacts.R

fig_att_likert_bins.R   - will remove 

fig_att_likert_bins_age.R

fig_facemasks.R

fig_likert_supp.R

fig_se_groups.R



#### Summarize data

results_text.R 

tab_se_vars.R 



#### Functions

bs_group.R 

bs_facemask_proportion.R    

ggthemr_workaround.R # can probably remove

utility_functions.R 


### Outputs

#### Figure 1 - Mean contacts by age

Description to come

#### Figure 2 - Mean contacts by reported risk and risk perception

Description to come

#### Figure 3 - Proportion of participants who wear masks

Description to come

#### Figure 4 - Mean contacts by employment type, income, and social group

Description to come

#### Table 1 - Participant demographics

Description to come