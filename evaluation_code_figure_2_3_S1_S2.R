#Conduct mpox nowcasting evaluation

#Analyst: Rebecca Rohrer
#Last updated 12/10/2023

## Run this code file fourth (4/4) ##

#This code generates figures that visualize the comparison of by-day and by-week hindcasts of reported and estimated 
#but not-yet-reported number of confirmed and probable cases among New York City residents 
#diagnosed with mpox during July 8–September 27, 2022.

# This code populates:
# Figure 2 
# Figure 3
# Figure S1
# Figure S2

#By diagnosis date
## Figure 2 (daily) ----

#For the dx dataset to plot we need:
#1) Daily, the actual case count
dateseq_daily <- seq(as.Date("2022-07-20"), as.Date("2022-09-27"), by = 1) %>% #7/20 is the first date an estimate is produced for 14-day for a nowcast conducted on a Tuesday
  as.data.frame() 

dsd_dx <- dateseq_daily

names(dsd_dx) <- "dx_date"

dsd_dx$dx_date <- as.Date(dsd_dx$dx_date)

plot_totals <- adf %>%
    filter(dx_date <= as.Date("2022-09-27")) %>%
    group_by(dx_date) %>%
    dplyr::summarize(estimate = n()) %>%
  full_join(dsd_dx, by = "dx_date") %>%
  mutate(method= "Actual") 

plot_totals <- replace(plot_totals, is.na(plot_totals), 0)

#2) Daily, the daily case count from each nowcast output (ex. 14-day, dx nowcast)
#just the method, onset_var, and estimate columns

dx14 <- ManageNowcastOutputToGraph("comparison_master_all_dx14.csv", "14 day", include_weekend = TRUE, type="daily")

dx21 <- ManageNowcastOutputToGraph("comparison_master_all_dx21.csv", "21 day", include_weekend = TRUE, type="daily")

dx28 <- ManageNowcastOutputToGraph("comparison_master_all_dx28.csv", "28 day", include_weekend = TRUE, type="daily")

dx35 <- ManageNowcastOutputToGraph("comparison_master_all_dx35.csv", "35 day", include_weekend = TRUE, type="daily")

dx42 <- ManageNowcastOutputToGraph("comparison_master_all_dx42.csv", "42 day", include_weekend = TRUE, type="daily")

dx49 <- ManageNowcastOutputToGraph("comparison_master_all_dx49.csv", "49 day", include_weekend = TRUE, type="daily")

#Combine, not stratified
combined_overall_dx <- plot_totals %>%
  full_join(dx14, by=c("dx_date"="onset_var", "method", "estimate")) %>%
  full_join(dx21, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
  full_join(dx28, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
  full_join(dx35, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
  full_join(dx42, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
  full_join(dx49, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
  mutate(dx_date = as.Date(dx_date)) 


#Combine all data
data_toplot <- plot_totals %>%
  full_join(combined_overall_dx, by=c("dx_date", "method", "estimate")) %>%
  mutate(upper = as.numeric(upper),
         lower = as.numeric(lower),
         upper_edit = case_when((method == "14 day") ~ upper,
                                TRUE ~ as.numeric(NA)),
         lower_edit = case_when((method == "14 day") ~ lower,
                            TRUE ~ as.numeric(NA)),
         upper_edit_crop = case_when(upper_edit >= 120 ~ 120, #cutting off above 120 95% PI for clarity
                                            TRUE ~ upper_edit),
         method = factor(method, levels = c("Actual", "14 day", "21 day", "28 day", "35 day", "42 day", "49 day")),
         dx_date = as.Date(dx_date))
  

WriteCSVToFolder(data_toplot, "dx_daily_data_for_fig2A.csv")

#Make x axis labels at weekly, though there will be tick marks at daily
data_toplot_labels <- data_toplot %>%
  mutate(label = ifelse(dx_date == "2022-07-08" | dx_date == "2022-09-27", format(dx_date, "%d%b"),
                         ifelse( lubridate::wday(dx_date) == 3, 
                          format( dx_date, "%d%b" ), 
                          "" ) ))
y_labels_daily <- c(0,"",20,"",40,"",60,"",80,"",100,"",120)

p_dx <- ggplot(data_toplot) +
  geom_ribbon(data=data_toplot %>% filter(!is.na(lower_edit)), aes(x=dx_date, ymin=lower_edit, ymax=upper_edit_crop,
                                    group=interaction(method),
                                    fill=method), alpha=.15,
              show.legend=FALSE) +
  geom_line(aes(x=dx_date, y=estimate, group=interaction(method), color=method)) +
  geom_point(aes(x=dx_date, y=estimate, group=interaction(method), color=method, shape=method)) +
  scale_shape_manual(name="Nowcasting scenario", values =c("Actual" = 1,"14 day" =2,"21 day"=3,"28 day"=4,"35 day"=5,"42 day"=6,"49 day"=7)) +
  scale_color_manual(name="Nowcasting scenario", values = c("#000000","#5a5a5a","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f")) +
  scale_fill_manual(values = c("#5a5a5a"), name="Nowcasting scenario") +
  scale_x_date(breaks = data_toplot$dx_date, labels = data_toplot_labels$label, expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0,125,10), labels=y_labels_daily)+
  theme_classic() +
  labs(x="Diagnosis date (daily)",
       y = "Confirmed and probable cases",
       color= "Nowcasting scenario") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(face = "bold")) +
  theme(text = element_text(family = "Times New Roman"))

## Figure S1 (weekly) ----
#1) Weekly, not stratified the actual case count Tues-Weds
dx2 <- ManageNowcastOutputToGraph("comparison_master_all_dx2.csv", "2 week", include_weekend = TRUE, type="weekly")

dx3 <- ManageNowcastOutputToGraph("comparison_master_all_dx3.csv", "3 week", include_weekend = TRUE, type="weekly")

dx4 <- ManageNowcastOutputToGraph("comparison_master_all_dx4.csv", "4 week", include_weekend = TRUE, type="weekly")

dx5 <- ManageNowcastOutputToGraph("comparison_master_all_dx5.csv", "5 week", include_weekend = TRUE, type="weekly")

dx6 <- ManageNowcastOutputToGraph("comparison_master_all_dx6.csv", "6 week", include_weekend = TRUE, type="weekly")

dx7 <- ManageNowcastOutputToGraph("comparison_master_all_dx7.csv", "7 week", include_weekend = TRUE, type="weekly")


dateseq_weekly <- seq(as.Date("2022-07-26"), as.Date("2022-09-27"), by = 7) %>% #starting at first date 7/26 when an estimate is produced
  as.data.frame() 

dsw <- dateseq_weekly

names(dsw) <- "onset_var"

dsw$onset_var <- as.Date(dsw$onset_var)

adf_prep <- adf %>% #starting at first date 7/26 when an estimate is produced
  filter(dx_date <= as.Date("2022-09-27")) %>%
  mutate(onset_var = case_when("2022-07-08" <= dx_date & dx_date <= "2022-07-12" ~ "2022-07-12", #not a full week, week 0 - should be none of these
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
                               "2022-09-21" <= dx_date & dx_date <= "2022-09-27" ~ "2022-09-27")
                               ) %>% #should be none of these
  mutate(onset_var = as.Date(onset_var))


plot_totals_weekly <- adf_prep %>%
  filter(onset_var <= as.Date("2022-09-27")) %>% #end date of actual counts should also stop at final point 9/27
  group_by(onset_var) %>%
  summarise(estimate=n()) %>% #calling actual varname estimate so it can merge with nowcast outputs
  full_join(dsw, by = c("onset_var")) %>%
  mutate(method = "Actual")

plot_totals_weekly <- replace(plot_totals_weekly, is.na(plot_totals_weekly), 0)

#Combined, not stratified
combined_overall_dx_weekly <- plot_totals_weekly %>%
  full_join(dx2, by=c("onset_var", "method", "estimate")) %>%
  full_join(dx3, by=c("onset_var", "method", "estimate", "upper", "lower")) %>%
  full_join(dx4, by=c("onset_var", "method", "estimate", "upper", "lower")) %>%
  full_join(dx5, by=c("onset_var", "method", "estimate", "upper", "lower")) %>%
  full_join(dx6, by=c("onset_var", "method", "estimate", "upper", "lower")) %>%
  full_join(dx7, by=c("onset_var", "method", "estimate", "upper", "lower")) %>%
  mutate(dx_date = as.Date(onset_var)) 


#Combine all data
data_toplot_weekly <- plot_totals_weekly %>%
  full_join(combined_overall_dx_weekly, by=c("onset_var", "method", "estimate")) %>%
  mutate(method = factor(method, levels = c("Actual", "2 week", "3 week", "4 week", "5 week", "6 week", "7 week")),
         onset_var = as.Date(onset_var),
         upper = as.numeric(upper),
         lower = as.numeric(lower),
         upper_edit = case_when((method == "2 week")  ~ upper,
                                TRUE ~ as.numeric(NA)),
         lower_edit = case_when((method == "2 week") ~ lower,
                                TRUE ~ as.numeric(NA)),
         upper_edit_crop = case_when(upper_edit >= 600 ~ 600, #crop overall at 600
                                     TRUE ~ upper_edit)) %>%
  filter(onset_var > "2022-07-12") #remove first incomplete week

WriteCSVToFolder(data_toplot_weekly, "dx_weekly_data_for_figS1A.csv")

p_dx_weekly <- ggplot(data_toplot_weekly) +
  geom_ribbon(data=data_toplot_weekly %>% filter(!is.na(lower_edit)), aes(x=onset_var, ymin=lower_edit, ymax=upper_edit_crop,
                                                                   group=interaction(method),
                                                                   fill=method), alpha=.15,
              show.legend=FALSE) +
  geom_line(aes(x=onset_var, y=estimate, group=interaction(method), color=method)) +
  geom_point(aes(x=onset_var, y=estimate, group=interaction(method), color=method, shape=method)) +
  scale_shape_manual(name = "Nowcasting scenario", values =c("Actual"=1,"2 week"=2,"3 week"=3,"4 week"=4,"5 week"=5,"6 week"=6,"7 week"=7)) +
  scale_color_manual(values = c("#000000","#5a5a5a","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f")) +
  scale_fill_manual(values = c("#5a5a5a"), name="Nowcasting scenario") +
  scale_x_date(date_labels = "%b-%d", breaks = data_toplot_weekly$onset_var, expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  theme_classic() +
  labs(x="Diagnosis week ending",
       y = "Confirmed and probable cases",
       color= "Nowcasting scenario") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(face = "bold")) +
  theme(text = element_text(family = "Times New Roman"))


#By illness onset date, non-stratified 

##Figure 3 (daily) ----
#For the onset dataset to plot we need:
#1) the actual case count

dsd_onset <- dateseq_daily

names(dsd_onset) <- "onset_date"

dsd_onset$onset_date <- as.Date(dsd_onset$onset_date)

plot_totals <- adf_onset %>%
 filter(onset_date <= as.Date("2022-09-27")) %>% 
 group_by(onset_date) %>%
  dplyr::summarize(estimate = n()) %>%
  full_join(dsd_onset, by = "onset_date") %>%
  mutate(method= "Actual") 

plot_totals <- replace(plot_totals, is.na(plot_totals), 0)

#2) the daily case count from each nowcast output (ex. 14-day, dx nowcast)
#just the method, onset_var, and n columns

onset14 <- ManageNowcastOutputToGraph("comparison_master_all_onset14.csv", "14 day", include_weekend = TRUE, type="daily")

onset21 <- ManageNowcastOutputToGraph("comparison_master_all_onset21.csv", "21 day", include_weekend = TRUE, type="daily")

onset28 <- ManageNowcastOutputToGraph("comparison_master_all_onset28.csv", "28 day", include_weekend = TRUE, type="daily")

onset35 <- ManageNowcastOutputToGraph("comparison_master_all_onset35.csv", "35 day", include_weekend = TRUE, type="daily")

onset42 <- ManageNowcastOutputToGraph("comparison_master_all_onset42.csv", "42 day", include_weekend = TRUE, type="daily")

onset49 <- ManageNowcastOutputToGraph("comparison_master_all_onset49.csv", "49 day", include_weekend = TRUE, type="daily")

combined_overall_onset <- plot_totals %>%
  full_join(onset14, by=c("onset_date"="onset_var", "method", "estimate")) %>%
  full_join(onset21, by=c("onset_date"="onset_var", "method", "estimate", "upper", "lower")) %>%
  full_join(onset28, by=c("onset_date"="onset_var", "method", "estimate", "upper", "lower")) %>%
  full_join(onset35, by=c("onset_date"="onset_var", "method", "estimate", "upper", "lower")) %>%
  full_join(onset42, by=c("onset_date"="onset_var", "method", "estimate", "upper", "lower")) %>%
  full_join(onset49, by=c("onset_date"="onset_var", "method", "estimate", "upper", "lower")) %>%
  mutate(onset_date = as.Date(onset_date))

data_toplot <- plot_totals %>%
  full_join(combined_overall_onset, by=c("onset_date", "method", "estimate")) %>%
  mutate(upper = as.numeric(upper),
         lower = as.numeric(lower),
         upper_edit = case_when((method == "21 day") ~ upper,
                                TRUE ~ as.numeric(NA)),
         lower_edit = case_when((method == "21 day") ~ lower,
                                TRUE ~ as.numeric(NA)),
         upper_edit_crop = case_when(upper_edit >= 120 ~ 120,
                                     TRUE ~ upper_edit),
         method = factor(method, levels = c("Actual", "14 day", "21 day", "28 day", "35 day", "42 day", "49 day")),
         onset_date = as.Date(onset_date)) 

#Make x axis labels at weekly, though there will be tick marks at daily
data_toplot_labels <- data_toplot %>%
  mutate( label = ifelse(onset_date == "2022-07-08" | onset_date == "2022-09-27", format(onset_date, "%d%b"),
                         ifelse( lubridate::wday(onset_date) == 3, 
                                 format( onset_date, "%d%b" ), 
                                 "" ) ))

WriteCSVToFolder(data_toplot, "onset_daily_data_for_fig2B.csv")

p_onset <- ggplot(data_toplot) +
  geom_ribbon(data=data_toplot %>% filter(!is.na(lower_edit)), aes(x=onset_date, ymin=lower_edit, ymax=upper_edit_crop, fill=method), alpha=.15,
              show.legend=FALSE) +
  geom_line(aes(x=onset_date, y=estimate, color=method, group=interaction(method))) +
  geom_point(aes(x=onset_date, y = estimate, group=interaction(method), color=method, shape=method)) +
  scale_shape_manual(name="Nowcasting scenario", values =c("Actual" = 1,"14 day" =2,"21 day"=3,"28 day"=4,"35 day"=5,"42 day"=6,"49 day"=7)) +
  scale_color_manual(values = c("#000000","#66c2a5","#5a5a5a","#8da0cb","#e78ac3","#a6d854","#ffd92f")) +
  scale_fill_manual(values = c("#5a5a5a"), name="Nowcasting scenario") +
  scale_x_date(breaks = data_toplot$onset_date, labels= data_toplot_labels$label, expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0,125,10), labels=y_labels_daily) +
  theme_classic() +
  labs(x="Onset date (daily)",
       y = "Confirmed and probable cases",
       color= "Nowcasting scenario") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(face = "bold")) +
  theme(text = element_text(family = "Times New Roman"))


##Figure S2 (weekly) ----
#1) Weekly, not stratified the actual case count Weds-Weds
#Wednesday dates to name week numbers "week ending" that Wednesday (ex. week 1 is the week ending 07-20-2022)
onset2 <- ManageNowcastOutputToGraph("comparison_master_all_onset2.csv", "2 week", include_weekend = TRUE, type="weekly")

onset3 <- ManageNowcastOutputToGraph("comparison_master_all_onset3.csv", "3 week", include_weekend = TRUE, type="weekly")

onset4 <- ManageNowcastOutputToGraph("comparison_master_all_onset4.csv", "4 week", include_weekend = TRUE, type="weekly")

onset5 <- ManageNowcastOutputToGraph("comparison_master_all_onset5.csv", "5 week", include_weekend = TRUE, type="weekly")

onset6 <- ManageNowcastOutputToGraph("comparison_master_all_onset6.csv", "6 week", include_weekend = TRUE, type="weekly")

onset7 <- ManageNowcastOutputToGraph("comparison_master_all_onset7.csv", "7 week", include_weekend = TRUE, type="weekly")

adf_onset_prep <- adf_onset %>%
  filter(onset_date <= as.Date("2022-09-27")) %>% 
  mutate(onset_var = case_when("2022-07-08" <= onset_date & onset_date <= "2022-07-12" ~ "2022-07-12", #not a full week, week 0 - should be none of these
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
                               "2022-09-21" <= onset_date & onset_date <= "2022-09-27" ~ "2022-09-27")) %>% 
  mutate(onset_var = as.Date(onset_var))

plot_totals_weekly_onset <- adf_onset_prep %>%
  filter(onset_var <= as.Date("2022-09-27")) %>% 
  group_by(onset_var) %>%
  summarise(estimate=n()) %>% #calling actual varname estimate so it can merge with nowcast outputs
  full_join(dsw, by = c("onset_var")) %>%
  mutate(method = "Actual")

plot_totals_weekly_onset <- replace(plot_totals_weekly_onset, is.na(plot_totals_weekly_onset), 0)

#Combined, not stratified
combined_overall_onset_weekly <- plot_totals_weekly_onset %>%
  full_join(onset2, by=c("onset_var", "method", "estimate")) %>%
  full_join(onset3, by=c("onset_var", "method", "estimate", "lower", "upper")) %>%
  full_join(onset4, by=c("onset_var", "method", "estimate", "lower", "upper")) %>%
  full_join(onset5, by=c("onset_var", "method", "estimate", "lower", "upper")) %>%
  full_join(onset6, by=c("onset_var", "method", "estimate", "lower", "upper")) %>%
  full_join(onset7, by=c("onset_var", "method", "estimate", "lower", "upper")) %>%
  mutate(onset_date = as.Date(onset_var))

#Combine all data
data_toplot_weekly_onset <- plot_totals_weekly_onset %>%
  full_join(combined_overall_onset_weekly, by=c("onset_var", "method", "estimate")) %>%
  mutate(method = factor(method, levels = c("Actual", "2 week", "3 week", "4 week", "5 week", "6 week", "7 week")),
         onset_var = as.Date(onset_var),
         upper = as.numeric(upper),
         lower = as.numeric(lower),
         upper_edit = case_when((method == "3 week")  ~ upper,
                                TRUE ~ as.numeric(NA)),
         lower_edit = case_when((method == "3 week") ~ lower,
                                TRUE ~ as.numeric(NA)),
         upper_edit_crop = case_when(upper_edit >= 1200 ~ 1200, #crop overall at 1200
                                     TRUE ~ upper_edit)) %>%
  filter(onset_var > "2022-07-12") #remove first incomplete week

WriteCSVToFolder(data_toplot_weekly_onset, "onset_weekly_data_for_figS1B.csv")

p_onset_weekly <- ggplot(data_toplot_weekly_onset) +
  geom_ribbon(data=data_toplot_weekly_onset %>% filter(!is.na(lower_edit)), aes(x=onset_var, ymin=lower_edit, ymax=upper_edit_crop,
                                                                          group=method,
                                                                          fill=method), alpha=.15,
              show.legend=FALSE) +
  geom_line(aes(x=onset_var, y=estimate, group=interaction(method), color=method)) +
  geom_point(aes(x=onset_var, y=estimate, group=interaction(method), color=method, shape=method)) +
  scale_shape_manual(name = "Nowcasting scenario", values =c("Actual"=1,"2 week"=2,"3 week"=3,"4 week"=4,"5 week"=5,"6 week"=6,"7 week"=7)) +
  scale_color_manual(values = c("#000000","#66c2a5","#5a5a5a","#8da0cb","#e78ac3","#a6d854","#ffd92f")) +
  scale_fill_manual(values = c("#5a5a5a"), name="Nowcasting scenario") +
  scale_x_date(date_labels = "%b-%d", breaks = data_toplot_weekly_onset$onset_var, expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  theme_classic() +
  labs(x="Onset week ending",
       y = "Confirmed and probable cases",
       color= "Nowcasting scenario") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(face = "bold")) +
  theme(text = element_text(family = "Times New Roman"))


#Look at & save each plot
p_dx
ggsave(filename="evaluation_figure_2.png", path=dataset_path, plot=p_dx, height = 8, width = 10, unit = "in")

p_dx_weekly
ggsave(filename="evaluation_figure_S1.png", path=dataset_path, plot=p_dx_weekly, height = 8, width = 10, unit = "in")

p_onset
ggsave(filename="evaluation_figure_3.png", path=dataset_path, plot=p_onset, height = 8, width = 10, unit = "in")

p_onset_weekly
ggsave(filename="evaluation_figure_S2.png", path=dataset_path, plot=p_onset_weekly, height = 8, width = 10, unit = "in")


#Example of generating a stratified nowcasting evaluation plot
#This code assumes you have generated "comparison_master_all_dx14_re.csv", "comparison_master_all_dx21_re.csv", etc.
# files containing the estimate data for all windows

#Daily totals, not stratified
# combined_overall_dx <- plot_totals %>%
#   full_join(dx14, by=c("dx_date"="onset_var", "method", "estimate")) %>%
#   full_join(dx21, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
#   full_join(dx28, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
#   full_join(dx35, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
#   full_join(dx42, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
#   full_join(dx49, by=c("dx_date"="onset_var", "method", "upper", "lower", "estimate")) %>%
#   mutate(dx_date = as.Date(dx_date)) %>%
#   mutate(race_ethnicity = "Overall") #doing this so the facet plot picks up overall as a 4th facet

# #1) Daily, totals, stratified
# re_date_shell <- as.data.table(dsd_dx) %>%
#   mutate(baa = "Black",
#          hl = "Hispanic/Latino",
#          w = "White") %>%
#   tidyr::pivot_longer(cols=c(baa, hl, w), values_to = "race_ethnicity") %>%
#   select(DIAGNOSIS_DATE, race_ethnicity) 
# 
# 
# plot_totals_re <- adf %>%
#   mutate(race_ethnicity = case_when(race_ethnicity == "Black/African American" ~ "Black",
#                                     TRUE ~ race_ethnicity)) %>%
#   filter(DIAGNOSIS_DATE <= as.Date("2022-09-27")) %>%
#   group_by(DIAGNOSIS_DATE, race_ethnicity) %>%
#   filter(race_ethnicity == "Black" |
#            race_ethnicity == "Hispanic/Latino" |
#            race_ethnicity == "White") %>%
#   summarize(estimate = n()) %>%
#   full_join(re_date_shell, by = c("DIAGNOSIS_DATE", "race_ethnicity")) %>%
#   mutate(method= "Actual")
# 
# plot_totals_re <- replace(plot_totals_re, is.na(plot_totals_re), 0)
# 
# #2) Daily, the daily case count from each nowcast output, stratified (ex. 14-day, dx nowcast)
# dx14_re <- ManageNowcastOutputToGraph("comparison_master_all_dx14_re.csv", "14 day", include_weekend = TRUE, r_e = TRUE, type="daily")
# 
# dx21_re <- ManageNowcastOutputToGraph("comparison_master_all_dx21_re.csv", "21 day", include_weekend = TRUE, r_e = TRUE, type="daily")
# 
# dx28_re <- ManageNowcastOutputToGraph("comparison_master_all_dx28_re.csv", "28 day", include_weekend = TRUE, r_e = TRUE, type="daily")
# 
# dx35_re <- ManageNowcastOutputToGraph("comparison_master_all_dx35_re.csv", "35 day", include_weekend = TRUE, r_e = TRUE, type="daily")
# 
# dx42_re <- ManageNowcastOutputToGraph("comparison_master_all_dx42_re.csv", "42 day", include_weekend = TRUE, r_e = TRUE, type="daily")
# 
# dx49_re <- ManageNowcastOutputToGraph("comparison_master_all_dx49_re.csv", "49 day", include_weekend = TRUE, r_e = TRUE, type="daily")
# 
# 
# #Combine all data
# data_toplot <- plot_totals_re %>%
#   full_join(combined_overall_dx, by=c("DIAGNOSIS_DATE", "method", "race_ethnicity", "estimate")) %>%
#   full_join(dx14_re, by=c("DIAGNOSIS_DATE"="onset_var", "method", "race_ethnicity", "estimate", "upper", "lower")) %>%
#   full_join(dx21_re, by=c("DIAGNOSIS_DATE"="onset_var", "method", "race_ethnicity", "estimate", "upper", "lower")) %>%
#   full_join(dx28_re, by=c("DIAGNOSIS_DATE"="onset_var", "method", "race_ethnicity", "estimate", "upper", "lower")) %>%
#   full_join(dx35_re, by=c("DIAGNOSIS_DATE"="onset_var", "method", "race_ethnicity", "estimate", "upper", "lower")) %>%
#   full_join(dx42_re, by=c("DIAGNOSIS_DATE"="onset_var", "method", "race_ethnicity", "estimate", "upper", "lower")) %>%
#   full_join(dx49_re, by=c("DIAGNOSIS_DATE"="onset_var", "method", "race_ethnicity", "estimate", "upper", "lower")) %>%
#   mutate(upper = as.numeric(upper),
#          lower = as.numeric(lower),
#          upper_edit = case_when((method == "14 day") ~ upper,
#                                 TRUE ~ as.numeric(NA)),
#          lower_edit = case_when((method == "14 day") ~ lower,
#                                 TRUE ~ as.numeric(NA)),
#          upper_edit_crop = case_when(upper_edit >= 120 ~ 120, #cutting off above 120 95% PI for clarity
#                                      TRUE ~ upper_edit),
#          method = factor(method, levels = c("Actual", "14 day", "21 day", "28 day", "35 day", "42 day", "49 day")),
#          DIAGNOSIS_DATE = as.Date(DIAGNOSIS_DATE),
#          race_ethnicity = factor(race_ethnicity, levels=c("Overall", "Black", "Hispanic/Latino", "White"))
#   )
# 
# 
# #Make x axis labels at weekly, though there will be tick marks at daily
# data_toplot_labels <- data_toplot %>%
#   mutate(label = ifelse(DIAGNOSIS_DATE == "2022-07-08" | DIAGNOSIS_DATE == "2022-09-27", format(DIAGNOSIS_DATE, "%d%b"),
#                         ifelse( lubridate::wday(DIAGNOSIS_DATE) == 3, 
#                                 format( DIAGNOSIS_DATE, "%d%b" ), 
#                                 "" ) ))
# 
# p_dx <- ggplot(data_toplot) +
#   geom_ribbon(data=data_toplot %>% filter(!is.na(lower_edit)), aes(x=DIAGNOSIS_DATE, ymin=lower_edit, ymax=upper_edit_crop,
#                                                                    group=interaction(method, race_ethnicity),
#                                                                    fill=method), alpha=.15,
#               show.legend=FALSE) +
#   geom_line(aes(x=DIAGNOSIS_DATE, y=estimate, group=interaction(method, race_ethnicity), color=method)) +
#   geom_point(aes(x=DIAGNOSIS_DATE, y=estimate, group=interaction(method, race_ethnicity), color=method, shape=method)) +
#   scale_shape_manual(name="Nowcasting scenario", values =c("Actual" = 1,"14 day" =2,"21 day"=3,"28 day"=4,"35 day"=5,"42 day"=6,"49 day"=7)) +
#   scale_color_manual(name="Nowcasting scenario", values = c("#000000","#5a5a5a","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f")) +
#   scale_fill_manual(values = c("#5a5a5a"), name="Nowcasting scenario") +
#   scale_x_date(breaks = data_toplot$DIAGNOSIS_DATE, labels = data_toplot_labels$label, expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0))+
#   theme_classic() +
#   labs(x="Diagnosis date (daily)",
#        y = "Confirmed and probable cases",
#        color= "Nowcasting scenario") +
#   theme(axis.text.x = element_text(angle = 90, vjust = .5),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         strip.text = element_text(face = "bold")) +
#   facet_rep_grid(race_ethnicity ~ ., scales="free_y", repeat.tick.labels="all") +
#   theme(text = element_text(family = "Times New Roman"))


