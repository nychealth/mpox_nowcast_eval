#Conduct mpox nowcasting evaluation

#Analyst: Rebecca Rohrer
#Code review: Allegra Wilson
#Last updated 8/12/2024

## Run this code file second (2/4) ##

# This code generates performance measures for hindcasting approaches in Nowcasting by Bayesian Smoothing (NobBS), 
# applied to mpox cases diagnosed among New York City residents from July 8 through September 30, 2022.

# The code populates:
# Table 5 (daily time unit performance metrics for diagnosis)
# Table S4 (daily time unit performance metrics for onset)
# Table S5 (weekly time unit performance metrics for diagnosis and onset)

#Note: It takes a long time to run this whole file - better to source files that have 
#already been created starting at line 167 after the first time generating the nowcast evaluation files

#Weekly versions are grouped by Wednesday-Tuesday weeks
#Note that the daily results produced below are later filtered to only those conducted on
#Tuesdays when metrics are generated (dow_conducted == 3)

# Produce results for the scenarios we're testing ----

## Diagnosis -- Week - not stratified ----
nowcasted_master <- NULL
dx2 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-07-26"), restrict_weeks=2, 
                         onset_var="dx_week", rept_var="dx_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(dx2, "comparison_master_all_dx2.csv")

nowcasted_master <- NULL
dx3 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-08-02"), restrict_weeks=3, 
                         onset_var="dx_week", rept_var="dx_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(dx3, "comparison_master_all_dx3.csv")

nowcasted_master <- NULL
dx4 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-08-09"), restrict_weeks=4, 
                         onset_var="dx_week", rept_var="dx_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(dx4, "comparison_master_all_dx4.csv")

nowcasted_master <- NULL
dx5 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-08-16"), restrict_weeks=4, 
                         onset_var="dx_week", rept_var="dx_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(dx5, "comparison_master_all_dx5.csv")

nowcasted_master <- NULL 
dx6 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-08-23"), restrict_weeks=6, 
                         onset_var="dx_week", rept_var="dx_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(dx6, "comparison_master_all_dx6.csv")

nowcasted_master <- NULL 
dx7 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-08-30"), restrict_weeks=7, 
                         onset_var="dx_week", rept_var="dx_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(dx7, "comparison_master_all_dx7.csv")


## Onset -- Week - not stratified ----
nowcasted_master <- NULL
onset2 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-07-26"), restrict_weeks=2, 
                            onset_var="onset_week", rept_var="onset_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(onset2, "comparison_master_all_onset2.csv")

nowcasted_master <- NULL
onset3 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-08-02"), restrict_weeks=3, 
                            onset_var="onset_week", rept_var="onset_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(onset3, "comparison_master_all_onset3.csv")

nowcasted_master <- NULL
onset4 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-08-09"), restrict_weeks=4, 
                            onset_var="onset_week", rept_var="onset_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(onset4, "comparison_master_all_onset4.csv")

nowcasted_master <- NULL
onset5 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-08-16"), restrict_weeks=5, 
                            onset_var="onset_week", rept_var="onset_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(onset5, "comparison_master_all_onset5.csv")

nowcasted_master <- NULL
onset6 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-08-23"), restrict_weeks=6, 
                            onset_var="onset_week", rept_var="onset_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(onset6, "comparison_master_all_onset6.csv")

nowcasted_master <- NULL
onset7 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-08-30"), restrict_weeks=7, 
                            onset_var="onset_week", rept_var="onset_rpt_week", strat=FALSE, timeunit="1 week")
WriteCSVToFolder(onset7, "comparison_master_all_onset7.csv")


stop("STOP: At this point run trace(NobBS, edit=TRUE) and supply the required changes for daily unstratified models per the ReadMe.")
trace(NobBS, edit=TRUE)

## Diagnosis -- Day - not stratified ----
#14 days
nowcasted_master <- NULL
dx14 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-07-21"), restrict_weeks=2, 
                          onset_var="dx_date", rept_var="dx_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(dx14, "comparison_master_all_dx14.csv")

#21 days
nowcasted_master <- NULL
dx21 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-07-28"), restrict_weeks=3, 
                          onset_var="dx_date", rept_var="dx_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(dx21, "comparison_master_all_dx21.csv")

#28 days
nowcasted_master <- NULL
dx28 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-08-04"), restrict_weeks=4, 
                          onset_var="dx_date", rept_var="dx_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(dx28, "comparison_master_all_dx28.csv")

#35 days
nowcasted_master <- NULL
dx35 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-08-11"), restrict_weeks=5, 
                          onset_var="dx_date", rept_var="dx_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(dx35, "comparison_master_all_dx35.csv")

#42 days
nowcasted_master <- NULL
dx42 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-08-18"), restrict_weeks=6, 
                          onset_var="dx_date", rept_var="dx_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(dx42, "comparison_master_all_dx42.csv")

#49 days
nowcasted_master <- NULL
dx49 <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-08-25"), restrict_weeks=7, 
                          onset_var="dx_date", rept_var="dx_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(dx49, "comparison_master_all_dx49.csv")

## Onset -- Day - not stratified ----
#14 days
nowcasted_master <- NULL
onset14 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-07-21"), restrict_weeks=2, 
                             onset_var="onset_date", rept_var="onset_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(onset14, "comparison_master_all_onset14.csv")


#21 days
nowcasted_master <- NULL
onset21 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-07-28"), restrict_weeks=3, 
                             onset_var="onset_date", rept_var="onset_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(onset21, "comparison_master_all_onset21.csv")

#28 days
nowcasted_master <- NULL
onset28 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-08-04"), restrict_weeks=4, 
                             onset_var="onset_date", rept_var="onset_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(onset28, "comparison_master_all_onset28.csv")

#35 days
nowcasted_master <- NULL
onset35 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-08-11"), restrict_weeks=5, 
                             onset_var="onset_date", rept_var="onset_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(onset35, "comparison_master_all_onset35.csv")

#42 days
nowcasted_master <- NULL
onset42 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-08-18"), restrict_weeks=6, 
                             onset_var="onset_date", rept_var="onset_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(onset42, "comparison_master_all_onset42.csv")

#49 days
nowcasted_master <- NULL
onset49 <- DataManageNowcast(data=adf_onset, date_conducted=as.Date("2022-08-25"), restrict_weeks=7, 
                             onset_var="onset_date", rept_var="onset_report_date", strat=FALSE, timeunit="1 day")
WriteCSVToFolder(onset49, "comparison_master_all_onset49.csv")


#Section above is time intensive. Once created, read in source datasets here to avoid rerunning the above
dx14 <- read.csv(paste0(dataset_path,"comparison_master_all_dx14.csv"))
dx21 <- read.csv(paste0(dataset_path,"comparison_master_all_dx21.csv"))
dx28 <- read.csv(paste0(dataset_path,"comparison_master_all_dx28.csv"))
dx35 <- read.csv(paste0(dataset_path,"comparison_master_all_dx35.csv"))
dx42 <- read.csv(paste0(dataset_path,"comparison_master_all_dx42.csv"))
dx49 <- read.csv(paste0(dataset_path,"comparison_master_all_dx49.csv"))

dx2 <- read.csv(paste0(dataset_path,"comparison_master_all_dx2.csv"))
dx3 <- read.csv(paste0(dataset_path,"comparison_master_all_dx3.csv"))
dx5 <- read.csv(paste0(dataset_path,"comparison_master_all_dx5.csv"))
dx4 <- read.csv(paste0(dataset_path,"comparison_master_all_dx4.csv"))
dx6 <- read.csv(paste0(dataset_path,"comparison_master_all_dx6.csv"))
dx7 <- read.csv(paste0(dataset_path,"comparison_master_all_dx7.csv"))

onset14 <- read.csv(paste0(dataset_path,"comparison_master_all_onset14.csv"))
onset21 <- read.csv(paste0(dataset_path,"comparison_master_all_onset21.csv"))
onset28 <- read.csv(paste0(dataset_path,"comparison_master_all_onset28.csv"))
onset35 <- read.csv(paste0(dataset_path,"comparison_master_all_onset35.csv"))
onset42 <- read.csv(paste0(dataset_path,"comparison_master_all_onset42.csv"))
onset49 <- read.csv(paste0(dataset_path,"comparison_master_all_onset49.csv"))

onset2 <- read.csv(paste0(dataset_path,"comparison_master_all_onset2.csv"))
onset3 <- read.csv(paste0(dataset_path,"comparison_master_all_onset3.csv"))
onset4 <- read.csv(paste0(dataset_path,"comparison_master_all_onset4.csv"))
onset5 <- read.csv(paste0(dataset_path,"comparison_master_all_onset5.csv"))
onset6 <- read.csv(paste0(dataset_path,"comparison_master_all_onset6.csv"))
onset7 <- read.csv(paste0(dataset_path,"comparison_master_all_onset7.csv"))


# Metrics ----

#No wkends

dx_dfs <- c("dx14", "dx21", "dx28", "dx35", "dx42", "dx49")

output <- NULL

for(i in 1:length(dx_dfs)){
  output_interim <- Metrics(get(dx_dfs[i]), incl_weekend = F, scenario_name = dx_dfs[i])
  output <- rbind(output, output_interim)
  
}

#Wkends

wkends_dfs <- c("dx2", "dx3", "dx4", "dx5", "dx6", "dx7",
                "onset14", "onset21", "onset28", "onset35", "onset42", "onset49",
                "onset2", "onset3", "onset4", "onset5", "onset6", "onset7")


output_wkends <- NULL

for(i in 1:length(wkends_dfs)){
  output_interim <- Metrics(get(wkends_dfs[i]), incl_weekend = T, scenario_name = wkends_dfs[i])
  output_wkends <- rbind(output_wkends, output_interim)
}

# Paper Table Outputs ----
#bind outputs and save as a csv
all_output <- rbind(output, output_wkends)
write.csv(all_output, paste0(dataset_path,"evaluation_table_5_S4_S5_performance_metrics_",stringr::str_remove_all(today(), "-"),".csv"))

#Example of generating stratified nowcasting results

## Day - stratified by race/ethnicity ----
#14 days
# nowcasted_master <- NULL
# dx14_re <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-07-21"), restrict_weeks=2, 
#                              onset_var="dx_date", rept_var="dx_report_date", strat=TRUE, timeunit="1 day")
# WriteCSVToFolder(dx14_re, "comparison_master_all_dx14_re.csv")

#dx14_re <-  read.csv(paste0(dataset_path,"comparison_master_all_dx14_re.csv"))

#output_row_dx14_re  <- Metrics(dx14_re, incl_weekend = F, scenario_name = "dx14_re") 

