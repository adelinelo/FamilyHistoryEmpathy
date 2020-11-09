# Family History Empathy
Replication files for **"Family Matters: How Immigrant Histories Can Promote Inclusion"**, by Scott Williamson, Claire Adida, Adeline Lo, Melina Platas, Lauren Prather and Seth Werfel.

Public registration details for Study 3 can be found at: https://osf.io/cx2zp.

## Papers
- Manuscript: `Family-Matters_WALPPW.pdf`
- Appendices: `Family-Matters-SI_WALPPW.pdf`

## Code and Data

### Studies 1 and 2
- Main analysis for Studies 1 and 2 is in `family_history_analysis_replication.do` in the folder "Replication Files - Plots - Studies 1 and 2"
- Data used for this analysis comes from `family_history_wave1_data.dta` and `family_history_wave2_data.dta`
- The Do file also includes code to analyze pooled results from Studies 1, 2, and 3. This pooled dataset is `pooled_analysis_data.dta`

### Study 3
- Main analysis and visualizations for Study 3 is in `family_history_experiment-analysis_v2.Rmd`
- Main data for Study 3 is in the `d.rds` object
- Open ended answers to the family history question are further coded in `openendedbytreatment.csv`.

### Plots
- Code for plots is in `family_history_plots_replication.R` in the folder "Replication Files - Plots - Studies 1 and 2"
- This code relies on input from the following files in this folder: `wave1_analysis_data.dta`; `wave2_analysis_data.dta`; `wave3_analysis_data.dta`; `generation_plot_restrict.csv`; `generation_plot_thermometer.csv`; `partisanship_plot_restrict.csv`; `partisan_plot_thermometer.csv`; `trump_plot_restrict.csv`; `trump_plot_thermometer.csv`; `strong_partisanship_plot_restrict.csv`; `strong_partisanship_plot_thermometer.csv`
- The above files were created using the analysis in `family_history_analysis_replication.do`

For questions regarding the replication files in this repository, please email scottrw630@gmail.com.
