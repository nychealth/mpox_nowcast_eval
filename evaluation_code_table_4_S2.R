#Conduct mpox nowcasting evaluation

#Analyst: Rebecca Rohrer
#Last updated 12/4/2023

## Run this code file second (2/4) ##

# This code generates performance measures for hindcasting approaches in Nowcasting by Bayesian Smoothing (NobBS), 
# applied to mpox cases diagnosed among New York City residents during July 8–October 31, 2022.

# The code populates:
# Table 4 (daily time unit performance metrics)
# Table S2 (weekly time unit performance metrics)

#Note: It takes a long time to run this whole file - better to source files that have 
#already been created starting at line 166 after the first time generating the nowcast evaluation files

#Data management for adf and adf_onset
#Weekly versions need to be grouped by Wednesday-Tuesday weeks
#Tuesday dates to name week numbers "week ending" that Tuesday (ex. week 1 is the week ending 07-19-2022)

# Produce results for the scenarios we're testing ----
#Diagnosis ----
## Day - not stratified ----
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


## Week - not stratified ----
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


#Onset ----

## Day - not stratified ----
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


## Week - not stratified ----
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
#Dx outputs
output_row_dx14 <- Metrics(dx14, incl_weekend = F, scenario_name = "dx14")

output_row_dx21 <- Metrics(dx21, incl_weekend = F, scenario_name = "dx21")

output_row_dx28 <- Metrics(dx28, incl_weekend = F, scenario_name = "dx28")

output_row_dx35 <- Metrics(dx35, incl_weekend = F, scenario_name = "dx35")

output_row_dx42 <- Metrics(dx42, incl_weekend = F, scenario_name = "dx42")

output_row_dx49 <- Metrics(dx49, incl_weekend = F, scenario_name = "dx49")

output_row_dx2 <- Metrics(dx2, incl_weekend = T, scenario_name = "dx2")

output_row_dx3 <- Metrics(dx3, incl_weekend = T, scenario_name = "dx3")

output_row_dx4 <- Metrics(dx4, incl_weekend = T, scenario_name = "dx4")

output_row_dx5 <- Metrics(dx5, incl_weekend = T, scenario_name = "dx5")

output_row_dx6 <- Metrics(dx6, incl_weekend = T, scenario_name = "dx6")

output_row_dx7 <- Metrics(dx7, incl_weekend = T, scenario_name = "dx7")


#Onset outputs
output_row_onset14 <- Metrics(onset14, incl_weekend = T, scenario_name = "onset14")

output_row_onset21 <- Metrics(onset21, incl_weekend = T, scenario_name = "onset21")

output_row_onset28 <- Metrics(onset28, incl_weekend = T, scenario_name = "onset28")

output_row_onset35 <- Metrics(onset35, incl_weekend = T, scenario_name = "onset35")

output_row_onset42 <- Metrics(onset42, incl_weekend = T, scenario_name = "onset42")

output_row_onset49 <- Metrics(onset49, incl_weekend = T, scenario_name = "onset49")


output_row_onset2 <- Metrics(onset2, incl_weekend = T, scenario_name = "onset2")

output_row_onset3 <- Metrics(onset3, incl_weekend = T, scenario_name = "onset3")

output_row_onset4 <- Metrics(onset4, incl_weekend = T, scenario_name = "onset4")

output_row_onset5 <- Metrics(onset5, incl_weekend = T, scenario_name = "onset5")

output_row_onset6 <- Metrics(onset6, incl_weekend = T, scenario_name = "onset6")

output_row_onset7 <- Metrics(onset7, incl_weekend = T, scenario_name = "onset7")



# Paper Table 4 and S2 ----
#bind outputs and save as a csv
output_list <- rbind(output_row_dx14,output_row_dx21,output_row_dx28,output_row_dx35,output_row_dx42,output_row_dx49,
                     output_row_onset14,output_row_onset21,output_row_onset28, output_row_onset35, output_row_onset42,output_row_onset49,
                     output_row_dx2, output_row_dx3, output_row_dx4, output_row_dx5, output_row_dx6,output_row_dx7,
                     output_row_onset2, output_row_onset3, output_row_onset4, output_row_onset5, output_row_onset6,output_row_onset7)

write.csv(output_list, paste0(dataset_path,"evaluation_table_4_and_S2_metrics.csv"))


#Example of generating stratified nowcasting results

## Day - stratified by race/ethnicity ----
#14 days
# nowcasted_master <- NULL
# dx14_re <- DataManageNowcast(data=adf, date_conducted=as.Date("2022-07-21"), restrict_weeks=2, 
#                              onset_var="dx_date", rept_var="dx_report_date", strat=TRUE, timeunit="1 day")
# WriteCSVToFolder(dx14_re, "comparison_master_all_dx14_re.csv")

#dx14_re <-  read.csv(paste0(dataset_path,"comparison_master_all_dx14_re.csv"))

#output_row_dx14_re  <- Metrics(dx14_re, incl_weekend = F, scenario_name = "dx14_re") 

