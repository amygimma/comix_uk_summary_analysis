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
Rscript.exe "./r/dm100_prep_for_avg_contact_data.R" $boots

echo "Weigh data for analysis"
Rscript.exe "./r/dm100b_weights_for_avg_contact_data.R" 

echo "Calculate mean contacts by age and risk group"
Rscript.exe "./r/dm102_calc_avg_contact_data_risk.R" $boots

echo "Calculate mean contacts by SE categories"
Rscript.exe "./r/dm102_calc_avg_contact_data_se.R" $boots

echo "Calculate proportion reported"
Rscript.exe "./r/dm111_calc_avg_proportions.R" $boots

# Create figures

echo "Create figure 1 - mean contacts by age and setting"
Rscript.exe "./r/fig1_avg_contacts.R"

echo "Create figure 2 - contacts by age and risk group"
Rscript.exe "./r/fig_att_likert_bins_age.R"

echo "Create figure 3 - contacts by SE categories"
Rscript.exe "./r/fig_se_groups.R"

echo "Create figure 4 - proportion reported using face mask"
Rscript.exe "./r/fig_facemasks.R"

# echo "Create figure 5 - contact matrices"
# Rscript.exe "r/-----.R" 

echo "Create figure S1 - proportions likert risk group"
Rscript.exe "./r/fig_likert_supp.R"

# Create tables

# echo "Create table 1"
Rscript.exe "./r/table_one_study_periods.R"
















