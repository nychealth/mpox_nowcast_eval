#Conduct mpox nowcasting evaluation

#Analyst: Rebecca Rohrer
#Code review: Allegra Wilson
#Evaluation code structure is based on code originally written by Rebecca Kahn
#Last updated 8/12/2024

## Run this code file first (1/4) ##

# This code loads the analytic dataset and describes reporting lags in mpox case counts among New York City residents diagnosed during 
# July 8–September 30, 2022.

# This code populates:
# Table 2 (descriptive lags from diagnosis to diagnosis report and onset to onset report for the whole cohort of confirmed and probable cases)
# Table 4 (summary of missingness in onset date by month)

#Set paths and source supporting files
R<- "" #update code folder path here, this should be the folder containing all 7 source code files

  
source(paste0(R, "evaluation_packages.R"))
source(paste0(R,"evaluation_helper_functions.R"))

#Read in data that was produced by freeze_mpv_nowcasting_evaluation_dataset.R
dataset_path <- "" #update path for the initial dataset here, and this is also where the code will output results

analytic_dataset <- read.csv(paste0(dataset_path, "evaluation_df_public.csv")) #update dataset name here


#Data management for adf and adf_onset - creating weekly variables for week time unit evaluations
adf <- analytic_dataset %>% 
  mutate(dx_date = as.Date(dx_date, format = "%m/%d/%Y"),
         dx_report_date = as.Date(dx_report_date, format = "%m/%d/%Y"),
         dlag = dx_report_date - dx_date,
         odx_lag = as.Date(dx_date) - as.Date(onset_date, format = "%m/%d/%Y"),
         dx_date_period  = case_when(dx_date >= "2022-07-08" & dx_date <= "2022-7-31" ~ "July 8-31",
                                     dx_date >= "2022-08-01" & dx_date <= "2022-08-31" ~ "August 1-31",
                                     dx_date >= "2022-09-01" & dx_date <= "2022-09-30" ~ "September 1-30"),
         dx_date_period = factor(dx_date_period, levels = c("Full study period", "July 8-31", "August 1-31", "September 1-30")),
         date_period = factor(dx_date_period, levels = c("Full study period", "July 8-31", "August 1-31", "September 1-30")),
         dx_week = as.Date(case_when("2022-07-08" <= dx_date & dx_date <= "2022-07-12" ~ "2022-07-12", #not a full week, week 0
                             "2022-07-13" <= dx_date & dx_date <= "2022-07-19" ~ "2022-07-19",
                             "2022-07-20" <= dx_date & dx_date <= "2022-07-26" ~ "2022-07-26",
                             "2022-07-27" <= dx_date & dx_date <= "2022-08-02" ~ "2022-08-02",
                             "2022-08-03" <= dx_date & dx_date <= "2022-08-09" ~ "2022-08-09",
                             "2022-08-10" <= dx_date & dx_date <= "2022-08-16" ~ "2022-08-16",
                             "2022-08-17" <= dx_date & dx_date <= "2022-08-23" ~ "2022-08-23",
                             "2022-08-24" <= dx_date & dx_date <= "2022-08-30" ~ "2022-08-30",
                             "2022-08-31" <= dx_date & dx_date <= "2022-09-06" ~ "2022-09-06",
                             "2022-09-07" <= dx_date & dx_date <= "2022-09-13" ~ "2022-09-13",
                             "2022-09-14" <= dx_date & dx_date <= "2022-09-20" ~ "2022-09-20",
                             "2022-09-21" <= dx_date & dx_date <= "2022-09-27" ~ "2022-09-27",
                             "2022-09-28" <= dx_date & dx_date <= "2022-09-30" ~ "2022-10-04")),
         dx_rpt_week = as.Date(case_when("2022-07-08" <= dx_report_date & dx_report_date <= "2022-07-12" ~ "2022-07-12", #not a full week, week 0
                                 "2022-07-13" <= dx_report_date & dx_report_date <= "2022-07-19" ~ "2022-07-19",
                                 "2022-07-20" <= dx_report_date & dx_report_date <= "2022-07-26" ~ "2022-07-26",
                                 "2022-07-27" <= dx_report_date & dx_report_date <= "2022-08-02" ~ "2022-08-02",
                                 "2022-08-03" <= dx_report_date & dx_report_date <= "2022-08-09" ~ "2022-08-09",
                                 "2022-08-10" <= dx_report_date & dx_report_date <= "2022-08-16" ~ "2022-08-16",
                                 "2022-08-17" <= dx_report_date & dx_report_date <= "2022-08-23" ~ "2022-08-23",
                                 "2022-08-24" <= dx_report_date & dx_report_date <= "2022-08-30" ~ "2022-08-30",
                                 "2022-08-31" <= dx_report_date & dx_report_date <= "2022-09-06" ~ "2022-09-06",
                                 "2022-09-07" <= dx_report_date & dx_report_date <= "2022-09-13" ~ "2022-09-13",
                                 "2022-09-14" <= dx_report_date & dx_report_date <= "2022-09-20" ~ "2022-09-20",
                                 "2022-09-21" <= dx_report_date & dx_report_date <= "2022-09-27" ~ "2022-09-27",
                                 "2022-09-28" <= dx_report_date & dx_report_date <= "2022-09-30" ~ "2022-10-04"))) %>%
  filter(dx_date >= "2022-07-08" & dx_date <= "2022-09-30") 


adf_onset <- analytic_dataset %>%
  filter(!is.na(onset_date)) %>%
  mutate(onset_date = as.Date(onset_date, format = "%m/%d/%Y"),
         onset_report_date = as.Date(onset_report_date, format = "%m/%d/%Y"),
         olag = onset_report_date - onset_date,
         odx_lag = as.Date(dx_date, format = "%m/%d/%Y") - as.Date(onset_date),
         onset_date_period = case_when(onset_date >= "2022-07-08" & onset_date <= "2022-7-31" ~ "July 8-31",
                                       onset_date >= "2022-08-01" & onset_date <= "2022-08-31" ~ "August 1-31",
                                       onset_date >= "2022-09-01" & onset_date <= "2022-09-30" ~ "September 1-30"),
         onset_date_period = factor(onset_date_period, levels = c("Full study period", "July 8-31", "August 1-31", "September 1-30")),
         date_period = factor(onset_date_period, levels = c("Full study period", "July 8-31", "August 1-31", "September 1-30")),
         onset_week =  as.Date(case_when("2022-07-08" <= onset_date & onset_date <= "2022-07-12" ~ "2022-07-12", #not a full week, week 0
                                 "2022-07-13" <= onset_date & onset_date <= "2022-07-19" ~ "2022-07-19",
                                 "2022-07-20" <= onset_date & onset_date <= "2022-07-26" ~ "2022-07-26",
                                 "2022-07-27" <= onset_date & onset_date <= "2022-08-02" ~ "2022-08-02",
                                 "2022-08-03" <= onset_date & onset_date <= "2022-08-09" ~ "2022-08-09",
                                 "2022-08-10" <= onset_date & onset_date <= "2022-08-16" ~ "2022-08-16",
                                 "2022-08-17" <= onset_date & onset_date <= "2022-08-23" ~ "2022-08-23",
                                 "2022-08-24" <= onset_date & onset_date <= "2022-08-30" ~ "2022-08-30",
                                 "2022-08-31" <= onset_date & onset_date <= "2022-09-06" ~ "2022-09-06",
                                 "2022-09-07" <= onset_date & onset_date <= "2022-09-13" ~ "2022-09-13",
                                 "2022-09-14" <= onset_date & onset_date <= "2022-09-20" ~ "2022-09-20",
                                 "2022-09-21" <= onset_date & onset_date <= "2022-09-27" ~ "2022-09-27",
                                 "2022-09-28" <= onset_date & onset_date <= "2022-09-30" ~ "2022-10-04")),
         onset_rpt_week =  as.Date(case_when("2022-07-08" <= onset_report_date & onset_report_date <= "2022-07-12" ~ "2022-07-12", #not a full week, week 0
                                     "2022-07-13" <= onset_report_date & onset_report_date <= "2022-07-19" ~ "2022-07-19",
                                     "2022-07-20" <= onset_report_date & onset_report_date <= "2022-07-26" ~ "2022-07-26",
                                     "2022-07-27" <= onset_report_date & onset_report_date <= "2022-08-02" ~ "2022-08-02",
                                     "2022-08-03" <= onset_report_date & onset_report_date <= "2022-08-09" ~ "2022-08-09",
                                     "2022-08-10" <= onset_report_date & onset_report_date <= "2022-08-16" ~ "2022-08-16",
                                     "2022-08-17" <= onset_report_date & onset_report_date <= "2022-08-23" ~ "2022-08-23",
                                     "2022-08-24" <= onset_report_date & onset_report_date <= "2022-08-30" ~ "2022-08-30",
                                     "2022-08-31" <= onset_report_date & onset_report_date <= "2022-09-06" ~ "2022-09-06",
                                     "2022-09-07" <= onset_report_date & onset_report_date <= "2022-09-13" ~ "2022-09-13",
                                     "2022-09-14" <= onset_report_date & onset_report_date <= "2022-09-20" ~ "2022-09-20",
                                     "2022-09-21" <= onset_report_date & onset_report_date <= "2022-09-27" ~ "2022-09-27",
                                     "2022-09-28" <= onset_report_date & onset_report_date <= "2022-09-30" ~ "2022-10-04"))) %>%
  filter(onset_date >= "2022-07-08" & onset_date <= "2022-09-30")

#Paper Table 2 and S2 ----
#Diagnosis Date
## Overall period
overall_iqr_dx <- adf %>% 
  dplyr::summarize(iqr = quantile(as.numeric(dlag), probs = c(0.25, 0.50, 0.75, 0.90), type = 2),
                   n= n()) %>%
  mutate(pct = rep(c(0.25, 0.50, 0.75, 0.90), times=1)) %>%
  tidyr::pivot_wider(names_from = pct, values_from = iqr) %>%
  mutate(stratification = "Unstratified",
         dx_date_period = "Full study period")

#Overall, by period
overall_iqr_subp <- adf %>% 
  group_by(dx_date_period) %>%
  dplyr::summarize(iqr = quantile(as.numeric(dlag), probs = c(0.25, 0.50, 0.75, 0.90), type = 2),
                   n = n()) %>%
  mutate(pct = rep(c(0.25, 0.50, 0.75, 0.90), times=1)) %>%
  tidyr::pivot_wider(names_from = pct, values_from = iqr) %>%
  mutate(stratification = "Unstratified")


#Combine
dx_lag_results <- overall_iqr_subp %>%
  full_join(overall_iqr_dx, by = c("stratification", "dx_date_period", "0.25", "0.5", "0.75", "0.9", "n")) %>%
  mutate(variable = "Diagnosis") %>%
  rename(date_period = dx_date_period) %>%
  mutate(date_period = factor(date_period, levels = c("Full study period", "July 8-31", "August 1-31", "September 1-30"))) %>%
  arrange(date_period, stratification) %>%
  select(variable, date_period, stratification, `0.25`, `0.5`, `0.75`, `0.9`, n)

#Onset Date
## Overall period
overall_iqr_onset <- adf_onset %>% 
  dplyr::summarize(iqr = quantile(as.numeric(olag), probs = c(0.25, 0.50, 0.75, 0.90), na.rm = TRUE, type = 2),
                   n = n()) %>%
  mutate(pct = rep(c(0.25, 0.50, 0.75, 0.90), times=1)) %>%
  tidyr::pivot_wider(names_from = pct, values_from = iqr) %>%
  mutate(stratification = "Unstratified",
         onset_date_period = "Full study period")

## Onset to diagnosis lag for descriptive results sentence
odx_lag_results <- adf_onset %>%
  filter(odx_lag >= 0) %>%
  dplyr::summarize(iqr = quantile(as.numeric(odx_lag), probs = c(0.25, 0.50, 0.75, 0.90), na.rm = TRUE, type = 2),
                   n = n()) %>%
  mutate(pct = rep(c(0.25, 0.50, 0.75, 0.90), times=1)) %>%
  tidyr::pivot_wider(names_from = pct, values_from = iqr)

## Overall, by period
overall_iqr_subp_onset <- adf_onset %>% 
  group_by(onset_date_period) %>%
  dplyr::summarize(iqr = quantile(as.numeric(olag), probs = c(0.25, 0.50, 0.75, 0.90), na.rm = TRUE, type = 2),
                   n = n()) %>%
  mutate(pct = rep(c(0.25, 0.50, 0.75, 0.90), times=1)) %>%
  tidyr::pivot_wider(names_from = pct, values_from = iqr) %>%
  mutate(stratification = "Unstratified") 

#Combine
onset_lag_results <- overall_iqr_subp_onset %>%
  full_join(overall_iqr_onset, by = c("stratification", "onset_date_period", "0.25", "0.5", "0.75", "0.9", "n")) %>%
  mutate(variable = "Onset") %>%
  rename(date_period = onset_date_period) %>%
  mutate(date_period = factor(date_period, levels = c("Full study period", "July 8-31", "August 1-31", "September 1-30"))) %>%
  arrange(date_period, stratification) %>%
  select(variable, date_period, stratification, `0.25`, `0.5`, `0.75`, `0.9`, n)


table2_towrite <- rbind(dx_lag_results, onset_lag_results)

WriteCSVToFolder(table2_towrite, "evaluation_table_2_desc_lag_results.csv")


#Paper Table 4 ----
#Summary of missing onset date by month of diagnosis

#Month of diagnosis
missing_onset_total <- adf %>% mutate(missing_onset = is.na(onset_date)) %>%
  group_by(missing_onset) %>%
  summarize(n = n()) 

#Not missing onset total
nmot <- missing_onset_total %>% 
    filter(missing_onset == "FALSE") %>% 
    select(n) %>%
    pull()

#Missing onset total
mot <- missing_onset_total %>% 
      filter(missing_onset == "TRUE") %>% 
      select(n) %>% 
      pull()

#Total
tot <- adf %>% 
  summarize(n = n()) %>%
  pull()


adf_missing_bymonth <- adf %>% mutate(missing_onset = is.na(onset_date)) %>%
  mutate(month = lubridate::month(dx_date)) %>%
  group_by(missing_onset, month) %>%
  summarize(n = n()) %>%
  mutate(pct = case_when(missing_onset == FALSE ~ scales::percent(n/nmot),
                         missing_onset == TRUE ~ scales::percent(n/mot))) %>%
  pivot_wider(names_from = c(missing_onset), values_from = c(n, pct)) %>%
  mutate(rowpct = scales::percent(n_TRUE/(n_TRUE + n_FALSE)),
         colpct = scales::percent(n_TRUE/mot),
         total = n_TRUE + n_FALSE,
         cat = month) %>% 
  select(cat, total, n_TRUE, colpct, rowpct)

last_row <- c("Total", tot, mot, scales::percent(mot/mot), scales::percent(mot/tot))

all_final <- rbind(adf_missing_bymonth, last_row)

write.csv(all_final, file=file.path(dataset_path,"evaluation_table_4_onset_missing_bymonth.csv"))



