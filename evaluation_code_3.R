#Conduct mpox nowcasting evaluation

#Analyst: Rebecca Rohrer
#Code review: Allegra Wilson
#Last updated 8/12/2024

## Run this code file third (3/4) ##

# This code calculates the median dispersion ratio from Poisson regression models of mpox cases diagnosed among New York City residents from July 8 through September 30, 2022, 
# by diagnosis date and by onset date, for the nowcasting evaluation scenarios with the temporal units and period lengths used in real time. 
# The evaluation period uses dates of diagnosis and onset from July 8, 2022 
# through effective end date September 27, 2022. We check the dispersion ratio for the full study period and selected 
# windows within these dates. 

# This code populates:
# Table S3 (dispersion ratios)

#Constants
tdy <- str_replace_all(Sys.Date(), "-", "")

# Diagnosis date
## 1. Diagnosis date - Full study period ----
dateseq_daily <- seq(as.Date("2022-07-08"), as.Date("2022-09-27"), by = 1) %>%
  as.data.frame() 

dsd_dx <- dateseq_daily

names(dsd_dx) <- "dx_date"

dx_summary <- adf %>%
  group_by(dx_date) %>%
  summarize(number_dx = n()) %>%
  full_join(dsd_dx, by = "dx_date")

dx_summary_nrow <- dx_summary %>%
  group_by(dx_date) %>%
  summarize(number_dx = n()) %>%
  nrow()

dx_summary <- replace(dx_summary, is.na(dx_summary), 0)

dx_summary <- dx_summary %>%
  mutate(sequence_variable = seq(1:dx_summary_nrow))


dxmodel_test <- glm(data = dx_summary, number_dx ~ dx_date, family = poisson)
check_test <- check_overdispersion(dxmodel_test)

output_fortable_1 <- NULL
output_fortable_1$disp <- check_test$dispersion_ratio
output_fortable_1$pval <- round(check_test$p_value, digits = 10)
output_fortable_1$scenario <- "Diagnosis full period"

## 2. 14 day nowcast dx window  ----

#Sequence of dates to check
end_dates <- seq(as.Date("2022-07-26", origin = "1970-01-01"),as.Date("2022-09-27", origin = "1970-01-01"),"1 week") #setting origins to have an integer from date
dispersion_table <- NULL


#Execute loop to evaluate all overlapping and non overlapping windows - date based
dispersion_table <- NULL
for(day in end_dates){
  dispersion_table <- CheckDispersionStats_date(data=adf, window_end=as.Date(day, origin = "1970-01-01"), restrict_weeks=2, onset_var="dx_date", rept_var="dx_report_date", excl_wkend = TRUE)
}

#Get median dispersion ratio
dispersion_table <- as.data.frame(dispersion_table)
dispersion_table$pval <- as.numeric(dispersion_table$pval)
dispersion_table$disp <- as.numeric(dispersion_table$disp)
med_disp_ratio_dx <- median(dispersion_table$disp)

output_fortable_2 <- NULL
output_fortable_2$disp <- med_disp_ratio_dx

#Next test will tell you if all dispersion ratios are statistically significant at p < .001
output_fortable_2$pval <- all(dispersion_table$pval < .001) #inspect output file below if false
output_fortable_2$scenario <- "Diagnosis 14 day windows"

dispersion_table <- as.matrix(dispersion_table)
WriteCSVToFolder(dispersion_table, "dispersion_data_diagnosis_14_day_windows.csv")


# Onset date
## 3. Onset date - Full study period ----

dsd_onset <- dateseq_daily

names(dsd_onset) <- "onset_date"

dsd_onset$onset_date <- as.Date(dsd_onset$onset_date)

onset_summary <- adf_onset %>%
  group_by(onset_date) %>%
  summarize(number_onset = n()) %>%
  full_join(dsd_onset, by = "onset_date")

onset_summary_nrow <- onset_summary %>%
  group_by(onset_date) %>%
  summarize(number_onset = n()) %>%
  nrow()

onset_summary <- replace(onset_summary, is.na(onset_summary), 0)

onset_summary <- onset_summary %>%
  arrange(onset_date) %>%
  mutate(sequence_variable = seq(1:onset_summary_nrow))

onsetmodel_date <- glm(data = onset_summary, number_onset ~ onset_date, family = poisson)
check_date <- check_overdispersion(onsetmodel_date)

output_fortable_3 <- NULL
output_fortable_3$disp <- check_date$dispersion_ratio 
output_fortable_3$pval <- round(check_date$p_value, digits = 10)
output_fortable_3$scenario <- "Onset full period"


## 4. 21 day onset window ----

#Sequence of dates to check
end_dates <- seq(as.Date("2022-08-02", origin = "1970-01-01"),as.Date("2022-09-27", origin = "1970-01-01"),"1 week")

#Execute loop to evaluate all overlapping and non overlapping windows
dispersion_table <- NULL
for(day in end_dates){
  dispersion_table <- CheckDispersionStats_date(data=adf_onset, window_end=as.Date(day, origin = "1970-01-01"), restrict_weeks=3, onset_var="onset_date", rept_var="onset_report_date", excl_wkend = FALSE)
}

#Get median dispersion ratio
dispersion_table <- as.data.frame(dispersion_table)
dispersion_table$disp <- as.numeric(dispersion_table$disp)
med_disp_ratio_onset <- median(dispersion_table$disp)

output_fortable_4 <- NULL
output_fortable_4$disp <- med_disp_ratio_onset
output_fortable_4$pval <- all(dispersion_table$pval < .001) #confirm in all cases by inspecting dispersion_table above or the output file below
output_fortable_4$scenario <- "Onset 21 day windows"

dispersion_table <- as.matrix(dispersion_table)
WriteCSVToFolder(dispersion_table, "dispersion_data_onset_21_day_windows.csv")

# Paper Table Outputs ----
#Output ready for table shell
table_disp_metrics <- as.data.frame(rbind(output_fortable_1, 
                                output_fortable_2,
                                output_fortable_3, 
                                output_fortable_4)) %>%
  select(scenario, disp, pval) %>%
  rename("Dispersion ratio" = disp,  "pval or bool for all periods' pvals < .001" = pval) %>%
  as.matrix()

#Export as csv
WriteCSVToFolder(table_disp_metrics, "evaluation_table_S3_dispersion_metrics.csv")
