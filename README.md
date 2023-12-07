# Mpox-nowcasting-evaluation
## Nowcasting to Monitor Real-Time Mpox Trends During the 2022 Outbreak in New York City: An Evaluation Using Reportable Disease Data Stratified by Race or Ethnicity
[Citation & paper link]

This repository contains the dataset, code, and codebook for the paper, "Nowcasting to Monitor Real-Time Mpox Trends During the 2022 Outbreak in New York City: An Evaluation Using Reportable Disease Data Stratified by Race or Ethnicity." These files to produce unstratified results are provided to support open and reproducible epidemiology and to fulfill journal data availability requirements. Data for stratified results, given small cell sizes, are not provided to protect patient confidentiality.

## Files:
* mpox_nowcasting_evaluation_codebook.csv – Codebook for dataset.
  
* evaluation_df_public.csv - CSV dataset with records and variables used for unstratified analyses shown in paper.
  
* R program files with code used to produce unstratified results included in manuscript:
  * evaluation_packages.R
  * evaluation_helper_functions.R
  * evaluation_code_table_2_3.R (Run first, 1/4)
  * evaluation_code_table_4_S2.R (Run second, 2/4)
  * evalulation_code_table_S1.R (Run third, 3/4)
  * evaluation_code_figure_2_3_S1_S2.R (Run fourth, 4/4)


## Dataset and Code Notes:
The dataset and program files were created in R version 4.2.3 on platform:x86_64-redhat-linux-gnu (64-bit), running under: Red Hat Enterprise Linux 8.7 (Ootpa). 
The dataset file can be imported into statistical software programs other than R (e.g., SAS, Python, SPSS), but the coding language in the program file is specific to R software.

The code requires installing NobBS from CRAN and JAGS outside of CRAN. Download instructions for NobBS are available at https://cran.r-project.org/web/packages/NobBS/index.html and for JAGS are available at https://mcmc-jags.sourceforge.io/.

To run the code, set up two folders: one containing the 6 R program files and one containing the dataset file. Set the "R" and "dataset" paths at the beginning of evaluation_code_table_2_3.R to these filepaths respectively. Then run the code files in this order: evaluation_code_table_2_3.R, evaluation_code_table_4_S2.R, evalulation_code_table_S1.R, evaluation_code_figure_2_3_S1_S2.R. Results will be available in the "dataset" folder.


## Contact:
Rebecca Rohrer, rrohrer@health.nyc.gov

