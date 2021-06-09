# Run all code for analysis and figures


if [ -z $1 ]
  then
  boots=1000
  echo "Running 1000 bootstrapped samples, to specify number of samples run with an argument, eg `sh run_analysis_mac.sh 1000`"
else
  boots=$1
fi

# Run analysis

echo "Prepare data for analysis"
Rscript "r/analysis/dm100_prep_for_avg_contact_data.R" $boots

echo "Weigh data for analysis"
Rscript "r/analysis/dm100b_weights_for_avg_contact_data.R" 

echo "Calculate mean contacts by age and setting"
Rscript "r/analysis/dm101_calc_avg_contact_data.R" $boots

echo "Calculate mean contacts by age and risk group"
Rscript "r/analysis/dm102_calc_avg_contact_data_risk.R" $boots

echo "Calculate mean contacts by SE categories"
Rscript "r/analysis/dm102_calc_avg_contact_data_se.R" $boots

echo "Calculate proportion reported"
Rscript "r/analysis/dm111_calc_avg_proportions.R" $boots

echo "Calculate relative difference for study periods with a GAM"
Rscript "r/analysis/gam_study_periods.R" $boots

##

# Create figures
# 
echo "Create figure 1 - mean contacts by age and setting"
Rscript "r/fig1_avg_contacts.R"

echo "Create figure 2 - relative difference study period plot"
Rscript "r/figures/gam_study_period_plot_table.R"

echo "Create figure 3 - contacts by age and risk group"
Rscript "r/fig_att_likert_bins_age.R"


echo "Create figure 4 - proportion reported using face mask"
Rscript "r/figures/fig_facemasks.R"

echo "Create figure 4 - proportion reported using face mask"
Rscript "r/figures/fig_facemasks.R"


# echo "Create figure 5 - contact matrices"
# Rscript "r/-----.R"

echo "Create figure S1 - proportions likert risk group"
Rscript "r/figures/fig_likert_supp.R"

echo "Create figure 5 - contacts by SE categories"
Rscript "r/figures/fig_se_groups.R"

# Create tables

echo "Create table 1"
Rscript "r/tables/table_one_study_periods.R"

