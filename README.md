# Mpox-nowcasting-evaluation
## Nowcasting to Monitor Real-Time Mpox Trends During the 2022 Outbreak in New York City: An Evaluation Using Reportable Disease Data Stratified by Race or Ethnicity
Rohrer R, Wilson A, Baumgartner J, Burton N, Ortiz RR, Dorsinville A, Jones LE, Greene SK. Nowcasting to Monitor Real-Time Mpox Trends During the 2022 Outbreak in New York City: An Evaluation Using Reportable Disease Data Stratified by Race or Ethnicity. JMIR Preprints. URL: https://preprints.jmir.org/preprint/56495 

This repository contains the dataset, code, and codebook for the above paper. These files to produce key unstratified results are provided to support open and reproducible epidemiology and to fulfill journal data availability requirements. Data for stratified results, given small cell sizes, are not provided to protect patient confidentiality.


## Files:
* mpox_nowcasting_evaluation_codebook.csv – Codebook for dataset.
  
* evaluation_df_public.csv - CSV dataset with records and variables used for unstratified analyses shown in paper.
  
* R program files with code used to produce unstratified results included in manuscript:
  * evaluation_packages.R
  * evaluation_helper_functions.R
  * evaluation_code_1.R (Run first, 1/4)
  * evaluation_code_2.R (Run second, 2/4)
  * evalulation_code_3.R (Run third, 3/4)
  * evaluation_code_4.R (Run fourth, 4/4)
  * code_NobBS_trace_unstrat_daily.R (Use when indicated while running               evaluation_code_2.R) 


## Dataset and Code Notes:
The dataset and program files were created in R version 4.2.3 on platform:x86_64-redhat-linux-gnu (64-bit), running under: Red Hat Enterprise Linux 8.7 (Ootpa). 
The dataset file can be imported into statistical software programs other than R (e.g., SAS, Python, SPSS), but the coding language in the program file is specific to R software.

The code requires installing NobBS from CRAN and JAGS outside of CRAN. Download instructions for NobBS are available at https://CRAN.R-project.org/package=NobBS and for JAGS are available at https://mcmc-jags.sourceforge.io/.

To run the code, set up two folders: one containing the 7 R program files and one containing the dataset file. Set the "R" and "dataset" paths at the beginning of evaluation_code_table_1.R to these filepaths respectively. Then run the code files in this order: evaluation_code 1.R, evaluation_code_2.R, evalulation_code_3.R, evaluation_code_4.R. Results will be available in the "dataset" folder.

Notes on evaluation_code_2.R:
There is a stop in the code asking the analyst to perform a trace() to edit the NobBS function and replace with code in code_NobBS_trace_unstrat_daily.R. The default version of NobBS produces the outputs required to calculate the log score only for the final time unit of the nowcast. This suffices for models at weekly resolution where we evaluated the most recent week. However, for models at daily resolution, in order to calculate the log score for estimates for each of the most recent 7 days, we must edit NobBS to output 6 additional days of nowcast.post.samps.

## Contact:
Rebecca Rohrer, rrohrer@health.nyc.gov


