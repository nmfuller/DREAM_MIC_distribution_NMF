# DREAM_MIC_distribution_NMF

Paper information:

Title: Country-level Heterogeneity in MDR-TB Drug Susceptibility Supports Country-Specific Policy Development

Authors: N M Fuller, Nicholas G. Davies, Timothy D. McHugh, Gwenan M Knight

Location: TBD 

Code used to analyse MIC distributions from the DREAM database

NOTE: The first R script, "0_clean_prep_data.R", should be run first before running any other script.

"1a_cumlative_data.R" and "1b_cumlative_data_stratified.R" must be run before "2_cumulative_plot_generation.r", "3_histogram_plot_generation.R" and "5_resistance_class_fig.R". 

"4_spearman_correlation_analysis.R" and "6_bdq_use_analysis.R" can be run with only the clean data.

Required packages:

tidyverse / openxlsx / readxl / data.table / stringr / patchwork / ggplot2 / cowplot / ggthemes / ggridges / scales / colorBlindness

Data

This publication is based on research using data from the Johnson & Johnson Family of Companies obtained through https://amr.vivli.org. We cannot provide the data in this repository. 

