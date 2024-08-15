#Conduct mpox nowcasting evaluation

#Analyst: Rebecca Rohrer
#Code review: Allegra Wilson
#Helper functions are based on code originally written by Rebecca Kahn
#Last updated 12/7/2023

# This code contains helper functions for the evaluation

# The race_ethnicity variable is not provided in the limited dataset evaluation_df_public.csv. 
# However, the functions below have options for stratified analysis if race_ethnicity were provided as a column. 
# See the bottom of each code file for examples of how stratified nowcasting results could be generated.

# rRMSE
rRMSE <- function(predicted,true){
  #This function returns the relative root mean square error of two columns, predicted and true
  #Note this is infinite if the true number of cases is ever zero.
  round(sqrt(mean(((predicted-true)/true)^2)), digits = 2)
}

# MAE
MAE <- function(predicted,true){
  #This function generates the absolute error between two columns
  round(mean(abs(predicted-true)), digits = 5)
}


Metrics <- function(data, incl_weekend = T, scenario_name){
  # This function rolls up metrics for each dataframe produced by DataManageNowcast
  # data: a data frame created by nowcasting function
  ## Input data should have columns: estimate, n, in_CI
  # incl_weekend: indicates whether estimates on weekends should be included in the calculation of metrics
  # Defaults to including weekends with incl_weekend = T if no value provided
  # scenario_name: a string to populate a column called scenario, to be used if multiple scenarios are being compared
  #Filters to nowcasts for periods ending on a Tuesday
  
  if(incl_weekend == F){ 
    data <- data %>%
      dplyr::filter(dow!=7 & dow!=1) 
  }
  
  data %>%
    filter(date_conducted <= as.Date("2022-09-27")) %>%
    mutate(dow_conducted = lubridate::wday(date_conducted)) %>%
    filter(dow_conducted == 3) %>% #To mimic nowcasting on Wednesdays with data through Tuesdays, we use "conducted on" Tuesdays
    mutate(n = as.numeric(n),
           estimate = as.numeric(estimate),) %>%
    summarise(scenario = scenario_name,
              MAE = round(MAE(estimate, n), digits=2),
              PI_95 = round((sum(in_CI)/n())*100, digits=2),
              avg_score = round(exp(mean(log_bin_prob_clean)), digits=2),
              n_estimates = n(),
              n_models = n_distinct(date_conducted),
              min_date_conducted = min(date_conducted),
              max_date_conducted = max(date_conducted),
              min_data_var_date = min(onset_var),
              max_data_var_date = max(onset_var)) -> output
  
  data %>%
    filter(date_conducted <= as.Date("2022-09-27")) %>%
    mutate(dow_conducted = lubridate::wday(date_conducted)) %>%
    filter(dow_conducted == 3) %>% #To mimic nowcasting on Wednesdays with data through Tuesdays, we use "conducted on" Tuesdays
    mutate(n = as.numeric(n)) %>%
    filter(n != 0) %>% #exclude values where n = 0 ONLY in rRMSE
    summarise(scenario = scenario_name,
              rRMSE = rRMSE(estimate, n)) -> output_2
  
  output <- output %>%
    left_join(output_2, by = "scenario") %>%
    select(scenario, MAE, rRMSE, PI_95, avg_score, n_estimates, n_models,
           min_date_conducted, max_date_conducted, min_data_var_date, max_data_var_date)
  
  
  return(output)
}



WriteCSVToFolder <- function(data, filename){
  #This function takes a data output produced by DataManageNowcast and writes it to the output folder set universally as dataset_path
  write.csv(data,paste0(dataset_path,filename), row.names = FALSE)
}




nowcast_compare <- function(date_conducted,restrict_weeks,data,strat,timeunit,onset_var,rept_var,diag_day){
  # This function takes parameters and returns a series of nowcasts. Should be called from within DataManageNowcast()
  # "date_conducted" is the day the first nowcast is done, using a time period of "restrict_weeks" 
  # if date_conducted is "2020-03-16" and restrict_weeks is 3, nowcasts will be done using data from 2/24-3/15
  # the function outputs the last week of data so in this example, nowcasted estimates for 3/9-3/15 will be reported
  # "data" is the input data
  # if "strat" = TRUE, a stratified nowcast by race/ethnicity is done
  # onset_var is the name as a string of the column of interest you're using as the onset date within the analysis data frame
  # rept_var is the name as a string of the column of interest you're using as the onset report date within the analysis data frame

  date_conducted <- as.Date(date_conducted) # day doing the nowcasting
  
  ## stratified analysis ----
  if (strat==TRUE){ 
    max <- date_conducted 
    min <- as.Date((max - restrict_weeks*7) + 1) # restrict nowcast to [min,max]
    
    ## by-day ----    
    if(timeunit == "1 day"){   
      # Restrict to X weeks - rept var needs to be in timeframe
      data %>%
        subset(as.Date(onset_var, format = "%Y-%m-%d") >= min & as.Date(onset_var, format = "%Y-%m-%d") <= max) %>%
        subset(as.Date(rept_var) <= max) %>%
        mutate(race_ethnicity=as.character(race_ethnicity)) %>%
        dplyr::filter(race_ethnicity == "Hispanic/Latino" |
                 race_ethnicity == "Black/African American" | 
                 race_ethnicity == "White") %>%
        arrange(onset_var) -> data_nc
      
      
      #Execute nowcast 
      nowcastD <- NobBS.strat(data=data_nc, now=date_conducted, specs=list(nAdapt=10000,dist="NB"),
                                   units=timeunit,onset_date="onset_var",report_date="rept_var",
                                   strata="race_ethnicity",
                                   quiet=FALSE)
      
      nowcasted_master <- rbind(nowcasted_master,
                                nowcastD$estimates %>% 
                                  subset(onset_date <= max & onset_date >=max-6)) # save just the last week
      
      true_ns <- diag_day %>%
        subset(onset_var <= max & onset_var >=max-6) %>%
        rename(true_n = n)
      
      bins_5 <- seq(0,39,by=5) #set after reviewing range of counts
      
      true_ns$bin_of_truecases <- bins_5[findInterval(true_ns$true_n,bins_5)]
      
      true_ns$top_of_bin_truecases <- true_ns$bin_of_truecases +4
      
      #Initialize bin_prob column to fill in
      true_ns$bin_prob <- NA
      
      #Use strata options in correct order to match post samps
      r_e_options <- unique(data_nc[,"race_ethnicity"]) #Per correspondence with Sarah McGough, this is the order of the cols for stratified post samps
      
      #Loop through date(s) and the strata options for each date to calculate the log score
      for(date in unique(true_ns$onset_var)){

      for(i in 1:length(r_e_options)){
        print(as.Date(date))
        print(r_e_options[i])
      #All possible bin values for the bin where the true value fell
      bottom_top_range <- c(true_ns[true_ns$race_ethnicity == r_e_options[i] & true_ns$onset_var == date, ]$bin_of_truecases:true_ns[true_ns$race_ethnicity == r_e_options[i] & true_ns$onset_var == date, ]$top_of_bin_truecases)

      #Pull out the nowcast.post.samps for the relevant day (starting from t-0 where it is post.samps, then tminus1, tminus2, etc.)
      ##on the latest day, set it to nowcast.post.samps
      ##on all other days, calculate which tminus we're on and pull that set of post.samps
      if(date == max){
        post.samps <- nowcastD$nowcast.post.samps[[i]] #get vector corresponding to correct r/e
      }  else {
        tminus <- as.numeric(max - date) 
        
        tminus_samps <- paste0("nowcast.post.samps.tminus", tminus)
        
        post.samps <- nowcastD[names(nowcastD) == tminus_samps][[1]][[i]] #unlist and then get vector corresponding to correct r/e
      }
      
      #Set proportion of nowcast.post.samps falling into the bin to the bin prob column
      true_ns[true_ns$race_ethnicity  == r_e_options[i] & true_ns$onset_var == date, ]$bin_prob[] <- mean(post.samps%in%bottom_top_range)
      print(true_ns)
      }
      }
  
      bins_w_prob <- true_ns %>%
        mutate(log_bin_prob_clean = case_when(bin_prob <= 1e-10 ~ -10,
                                              TRUE ~ log(bin_prob)),
               log_bin_prob = log(bin_prob)) 
      
      nowcasted_master <- nowcasted_master %>%
        left_join(bins_w_prob, by = c("onset_date" = "onset_var", "stratum" = "race_ethnicity"))
      
      print(nowcasted_master)
    }
    ## by-week ----   
    if(timeunit == "1 week"){
      data %>%     # Restrict to X weeks - rept var needs to be in timeframe
        subset(as.Date(onset_var, format = "%Y-%m-%d") >= min & as.Date(onset_var, format = "%Y-%m-%d") <= max) %>% 
        subset(as.Date(rept_var) <= max) %>%
        mutate(race_ethnicity=as.character(race_ethnicity)) %>%
        dplyr::filter(race_ethnicity == "Hispanic/Latino" |
                 race_ethnicity == "Black/African American" | 
                 race_ethnicity == "White") %>%
        arrange(onset_var) -> data_nc
      
      
      #Execute nowcast
      nowcastD <- NobBS.strat(data=data_nc, now=date_conducted, specs=list(nAdapt=10000,dist="NB"),
                             units=timeunit,onset_date="onset_var",report_date="rept_var",
                             strata="race_ethnicity",
                             quiet=FALSE)
      print(nowcastD$estimates)
      
      
      true_ns <- diag_day %>%
        subset(onset_var == max) %>%
        rename(true_n = n)
      
      
      bins_20 <- seq(0,179,by=20) #set after reviewing range of counts
      
      true_ns$bin_of_truecases <- bins_20[findInterval(true_ns$true_n,bins_20)]
      
      true_ns$top_of_bin_truecases <- true_ns$bin_of_truecases +19
      
      #Initialize bin_prob column to fill in
      true_ns$bin_prob <- NA
      
      #Use strata options in correct order to match post samps
      r_e_options <- unique(data_nc[,"race_ethnicity"]) #Per correspondence with Sarah McGough, this is the order of the cols for stratified post samps
      
      #Loop through date(s) and the strata options for each date to calculate the log score
      for(date in unique(true_ns$onset_var)){
        print(date)
      for(i in 1:length(r_e_options)){
        print(i)
        
      #All possible bin values for the bin where the true value fell
      bottom_top_range <- c(true_ns[true_ns$race_ethnicity == r_e_options[i] & true_ns$onset_var == date, ]$bin_of_truecases:true_ns[true_ns$race_ethnicity == r_e_options[i] & true_ns$onset_var == date, ]$top_of_bin_truecases)

      
      #Set proportion of nowcast.post.samps falling into the bin to the bin prob column
      true_ns[true_ns$race_ethnicity  == r_e_options[i] & true_ns$onset_var == date, ]$bin_prob[] <- 
        mean(nowcastD$nowcast.post.samps[[i]]%in%bottom_top_range)
      }
      }
      print(true_ns)
      
      bins_w_prob <- true_ns %>%
        mutate(log_bin_prob_clean = case_when(bin_prob <= 1e-10 ~ -10,
                                              TRUE ~ log(bin_prob)),
               log_bin_prob = log(bin_prob)) 
      
      nowcasted_master <- rbind(nowcasted_master,
                                nowcastD$estimates %>% 
                                  subset(onset_date == max)) # save just the last week for by-week (max)
      
      nowcasted_master <- nowcasted_master %>%
        left_join(bins_w_prob, by = c("onset_date" = "onset_var", "stratum" = "race_ethnicity"))
      

      
    }
    
    ## non-stratified analysis ----  
  } else if(strat==FALSE){
    
    max <- as.Date(date_conducted) 
    min <- as.Date((max - restrict_weeks*7) + 1) # restrict nowcast to [min,max)
    
    ## by-day ----    
    if(timeunit == "1 day"){    
      # Restrict to X weeks - rept var needs to be in timeframe
      data %>%
        subset(as.Date(onset_var, format = "%Y-%m-%d") >= min & as.Date(onset_var, format = "%Y-%m-%d") <= max) %>%
        subset(as.Date(rept_var, format = "%Y-%m-%d") <= max) %>%
        arrange(onset_var) -> data_nc
      

      #Execute nowcast 
      nowcastD <- NobBS(data=data_nc, now=date_conducted, specs=list(nAdapt=10000,dist="NB"),
                             units=timeunit,onset_date="onset_var",report_date="rept_var",
                             quiet=FALSE)
      
      nowcasted_master <- rbind(nowcasted_master,
                                nowcastD$estimates %>% 
                                  subset(onset_date <= max & onset_date >=max-6)) # save just the last week
     
      
      true_ns <- diag_day %>%
        subset(onset_var <= max & onset_var >=max-6) %>%
        rename(true_n = n)
      
      bins_10 <- seq(0,99,by=10) #set after reviewing range of counts
      
      true_ns$bin_of_truecases <- bins_10[findInterval(true_ns$true_n,bins_10)]
      
      true_ns$top_of_bin_truecases <- true_ns$bin_of_truecases +9
      
      #Initialize bin_prob column to fill in
      true_ns$bin_prob <- NA
      
      #Loop through date(s) and the strata options for each date to calculate the log score
      for(date in unique(true_ns$onset_var)){
          print(as.Date(date))

          #All possible bin values for the bin where the true value fell
          bottom_top_range <- c(true_ns[true_ns$onset_var == date, ]$bin_of_truecases:true_ns[true_ns$onset_var == date, ]$top_of_bin_truecases)
          
          #Pull out the nowcast.post.samps for the relevant day (starting from t-0 where it is post.samps, then tminus1, tminus2, etc.)
          ##on the latest day, set it to nowcast.post.samps
          ##on all other days, calculate which tminus we're on and pull that set of post.samps
          if(date == max){
            post.samps <- nowcastD$nowcast.post.samps #get vector of post.samps
          }  else {
            tminus <- as.numeric(max - date) 
            
            tminus_samps <- paste0("nowcast.post.samps.tminus", tminus)
            
            post.samps <- nowcastD[names(nowcastD) == tminus_samps][[1]] #unlist vector of post.samps for the correct day
          }
          
          #Set proportion of nowcast.post.samps falling into the bin to the bin prob column
          true_ns[true_ns$onset_var == date, ]$bin_prob[] <- 
            mean(post.samps%in%bottom_top_range)
          print(true_ns)
        }
      
      
      bins_w_prob <- true_ns %>%
        mutate(log_bin_prob_clean = case_when(bin_prob <= 1e-10 ~ -10,
                                              TRUE ~ log(bin_prob)),
               log_bin_prob = log(bin_prob)) 
      
      nowcasted_master <- nowcasted_master %>%
        left_join(bins_w_prob, by = c("onset_date" = "onset_var"))
      
      print(nowcasted_master)
      
    } 
    
# by-week ----   
    if(timeunit == "1 week"){
      data %>%     # Restrict to X weeks - rept var needs to be in timeframe
        subset(as.Date(onset_var) >= min & as.Date(onset_var) <= max) %>% 
        subset(as.Date(rept_var) <= max) %>%
        arrange(onset_var) -> data_nc
      
      
      #Execute nowcast
      nowcastD <- NobBS(data=data_nc, now=date_conducted, specs=list(nAdapt=10000,dist="NB"),
                             units=timeunit,onset_date="onset_var",report_date="rept_var",
                             quiet=FALSE)
      print(nowcastD$estimates)
      
      nowcasted_master <- rbind(nowcasted_master,
                                nowcastD$estimates %>% 
                                  subset(onset_date == max)) # save just the last week for weekly (max)
  
      
      true_ns <- diag_day %>%
        subset(onset_var == max) %>%
        rename(true_n = n)
      
      bins_50 <- seq(0,549,by=50)
      
      true_ns$bin_of_truecases <- bins_50[findInterval(true_ns$true_n,bins_50)]
      true_ns$top_of_bin_truecases <- true_ns$bin_of_truecases +49
      
      true_ns$bin_prob <- mean(nowcastD$nowcast.post.samps%in%c(true_ns$bin_of_truecases:(true_ns$top_of_bin_truecases))) 
      
      bins_w_prob <- true_ns %>%
        mutate(log_bin_prob_clean = case_when(bin_prob <= 1e-10 ~ -10,
                                              TRUE ~ log(bin_prob)),
               log_bin_prob = log(bin_prob)) 
      
      nowcasted_master <- nowcasted_master %>%
        left_join(bins_w_prob, by = c("onset_date" = "onset_var"))
      
      print(nowcasted_master)
     
    }
    print(nrow(nowcasted_master))
    
    
  }
  return(nowcasted_master)
}

DataManageNowcast <- function(date_conducted,restrict_weeks,data,strat,onset_var,rept_var,timeunit){
  
  comparison_master <- NULL
  
  summary_master <- NULL
  RT_master <- NULL
  nowcasted_master <- NULL
  
  names(data)[names(data) == onset_var] <- "onset_var"
  names(data)[names(data) == rept_var] <- "rept_var"
  if("onset_date" %in% names(data)){
    names(data)[names(data) == "onset_date"] <- "onset_date_original" #this is needed because NobBS uses "onset_date" as a variable name
  }
  
  #create true counts # summarize number infected by day ----
  #non-stratified ----
  if(strat == FALSE){
  
    # by-day ----
    if(timeunit == "1 day"){
      seq <- seq.Date(as.Date(date_conducted),as.Date("2022-09-27"), by = "day") %>%
        as.data.frame()
      
      seq <- setnames(seq, '.', 'onset_var')
      
    data %>%
      group_by(onset_var) %>%
      summarise(n=n()) -> diag_day
    
    diag_day <- diag_day %>%
      full_join(seq, by = "onset_var") %>%
      mutate(n = replace_na(n, 0))
    
    }
    # by-week ----
    if(timeunit == "1 week"){
      
      seq <- seq.Date(as.Date(date_conducted),as.Date("2022-09-27"), by = "week") %>%
        as.data.frame()
      
      seq <- setnames(seq, '.', 'onset_var')

        data %>%
        mutate(onset_var = as.Date(case_when("2022-07-08" <= onset_var & onset_var <= "2022-07-12" ~ "2022-07-12", #not a full week, week 0
                                     "2022-07-13" <= onset_var & onset_var <= "2022-07-19" ~ "2022-07-19",
                                     "2022-07-20" <= onset_var & onset_var <= "2022-07-26" ~ "2022-07-26",
                                     "2022-07-27" <= onset_var & onset_var <= "2022-08-02" ~ "2022-08-02",
                                     "2022-08-03" <= onset_var & onset_var <= "2022-08-09" ~ "2022-08-09",
                                     "2022-08-10" <= onset_var & onset_var <= "2022-08-16" ~ "2022-08-16",
                                     "2022-08-17" <= onset_var & onset_var <= "2022-08-23" ~ "2022-08-23",
                                     "2022-08-24" <= onset_var & onset_var <= "2022-08-30" ~ "2022-08-30",
                                     "2022-08-31" <= onset_var & onset_var <= "2022-09-06" ~ "2022-09-06",
                                     "2022-09-07" <= onset_var & onset_var <= "2022-09-13" ~ "2022-09-13",
                                     "2022-09-14" <= onset_var & onset_var <= "2022-09-20" ~ "2022-09-20",
                                     "2022-09-21" <= onset_var & onset_var <= "2022-09-27" ~ "2022-09-27",
                                     "2022-09-28" <= onset_var & onset_var <= "2022-10-04" ~ "2022-10-04"))) %>%  #not a full week, end the evaluation periods 9/27, will be filtered out
        group_by(onset_var) %>%
        summarise(n=n()) -> diag_day
        
        diag_day <- diag_day %>%
          full_join(seq, by = "onset_var") %>%
          mutate(n = replace_na(n, 0))
      
    }
    
  }
  
  ## stratified ----
  if(strat == TRUE){
    ## by-day ----
    if(timeunit == "1 day"){
      
        seq <- seq.Date(as.Date(date_conducted),as.Date("2022-09-27"), by = "day") %>%
          as.data.frame()
        
        seq <- setnames(seq, '.', 'onset_var')
        
        seq <- as.data.table(seq) %>%
          mutate(baa = "Black/African American",
                 hl = "Hispanic/Latino",
                 w = "White") %>%
          tidyr::pivot_longer(cols=c(baa, hl, w), values_to = "race_ethnicity") %>%
          mutate(onset_var = as.Date(onset_var)) %>%
          select(onset_var, race_ethnicity)

    
    data %>%
      subset(race_ethnicity == "Hispanic/Latino" |
               race_ethnicity == "Black/African American" | 
               race_ethnicity == "White") %>%
      group_by(onset_var,race_ethnicity) %>%
      summarise(n=n()) -> diag_day
    
    diag_day <- diag_day %>%
      full_join(seq, by = c("onset_var","race_ethnicity")) %>%
      mutate(n = replace_na(n, 0))
    }
    ## by-week ----
    if(timeunit == "1 week"){
      
      seq <- seq.Date(as.Date(date_conducted),as.Date("2022-09-27"), by = "week") %>%
        as.data.frame()
      
      seq <- setnames(seq, '.', 'onset_var')
      
      seq <- as.data.table(seq) %>%
        mutate(baa = "Black/African American",
               hl = "Hispanic/Latino",
               w = "White") %>%
        tidyr::pivot_longer(cols=c(baa, hl, w), values_to = "race_ethnicity") %>%
        mutate(onset_var = as.Date(onset_var)) %>%
        select(onset_var, race_ethnicity)
      
      data %>%
        subset(race_ethnicity == "Hispanic/Latino" |
                 race_ethnicity == "Black/African American" | 
                 race_ethnicity == "White") %>%
        mutate(onset_var = as.Date(case_when("2022-07-08" <= onset_var & onset_var <= "2022-07-12" ~ "2022-07-12", #not a full week, week 0
                              "2022-07-13" <= onset_var & onset_var <= "2022-07-19" ~ "2022-07-19",
                              "2022-07-20" <= onset_var & onset_var <= "2022-07-26" ~ "2022-07-26",
                              "2022-07-27" <= onset_var & onset_var <= "2022-08-02" ~ "2022-08-02",
                              "2022-08-03" <= onset_var & onset_var <= "2022-08-09" ~ "2022-08-09",
                              "2022-08-10" <= onset_var & onset_var <= "2022-08-16" ~ "2022-08-16",
                              "2022-08-17" <= onset_var & onset_var <= "2022-08-23" ~ "2022-08-23",
                              "2022-08-24" <= onset_var & onset_var <= "2022-08-30" ~ "2022-08-30",
                              "2022-08-31" <= onset_var & onset_var <= "2022-09-06" ~ "2022-09-06",
                              "2022-09-07" <= onset_var & onset_var <= "2022-09-13" ~ "2022-09-13",
                              "2022-09-14" <= onset_var & onset_var <= "2022-09-20" ~ "2022-09-20",
                              "2022-09-21" <= onset_var & onset_var <= "2022-09-27" ~ "2022-09-27",
                              "2022-09-28" <= onset_var & onset_var <= "2022-10-04" ~ "2022-10-04"))) %>%  #not a full week, data ends 9/30, evaluation period ends 9/27
        group_by(onset_var, race_ethnicity) %>%
        summarise(n=n()) -> diag_day
      
      diag_day <- diag_day %>%
        full_join(seq, by = c("onset_var","race_ethnicity")) %>%
        mutate(n = replace_na(n, 0))
      
    }
    
  }
  
  # Conduct evaluation using nowcasted function and diagnosis date code
  if(timeunit == "1 day"){
    dates_conducted <- seq(date_conducted,as.Date("2022-09-27"),"1 day")
  }
  if(timeunit == "1 week"){
    #These are the Wednesdays in the time period
    dates_conducted <- seq(date_conducted,as.Date("2022-09-27"),"1 week")
    
  }
  message("Starting nowcasts")
  for (day in dates_conducted){
    day <- as.Date(day,origin="1970-01-01")
    cat(lubridate::wday(as.Date(day,origin="1970-01-01")))
    
    ### conduct nowcasting
    date_conducted <- day # day first nowcast is done (excluding this day)
    
    # do for all
	nowcasted_test <- nowcast_compare(date_conducted=date_conducted,restrict_weeks=restrict_weeks,data=data,onset_var=onset_var,rept_var=rept_var,timeunit=timeunit,strat=strat,diag_day=diag_day) 
	print(nowcasted_test)
    
    # compare nowcasted data to actual
    if(strat==FALSE){
      comparison <- merge(diag_day,nowcasted_test,by.x=c("onset_var"),by.y=c("onset_date"))
    }
    if(strat==TRUE){
      comparison <- merge(diag_day,nowcasted_test,by.x=c("onset_var","race_ethnicity"),by.y=c("onset_date","stratum"))
    print(comparison)
      }
    
    # # # # # # 95% percentile interval coverage # # # # # # # #
    comparison$in_CI <- ifelse((comparison$n<=comparison$upper)&(comparison$n>=comparison$lower),1,0)
    
    comparison %>%
      add_column(date_conducted = day) %>%
      bind_rows(comparison_master) %>%
      data.frame() -> comparison_master
    
  }
  
  message("Finished nowcasts")
  
  comparison_master$dow <- wday(as.Date(comparison_master$onset_var,origin="1970-01-01")) #day of week of onset var
  
  comparison_master %>%
    add_column(method = "test") %>%
    mutate(lower = as.integer(lower),
           upper = as.integer(upper)) -> comparison_master
  
 
}


ManageNowcastOutputToGraph <- function(file_name, method_name, include_weekend, r_e = FALSE, type){
  #Function to read csv, filter the dataset and assign method in preparation for graphing
  #Filters to nowcasts conducted on Tuesday 
  
  dataset <- read.csv(paste0(dataset_path,file_name))
  
#Start lines on graph on the first date conducted on a Tuesday
  linestartdate <- dataset %>% 
    dplyr::filter(lubridate::wday(date_conducted) == 3) %>%
    summarize(min(onset_var)) %>%
    pull()
  
  if(type == "daily"){
    dateseq <- seq(as.Date("2022-07-08"), as.Date("2022-9-27"), by = 1) %>%
      as.data.frame()  
  }
  if(type == "weekly"){
    dateseq <- seq(as.Date("2022-07-19"), as.Date("2022-09-27"), by = 7) %>%
      as.data.frame() 
    }
    
    names(dateseq) <- "onset_var"
    
    dateseq$onset_var <- as.Date(dateseq$onset_var)
    
    dateseq <- dateseq %>% 
      mutate(method = method_name) %>%
      dplyr::filter(onset_var >= as.Date(linestartdate))
  

  if(r_e == FALSE){
    test <- dataset %>%
      filter(onset_var <= as.Date("2022-09-27")) %>%
      mutate(method = method_name,
             dow_conducted = lubridate::wday(date_conducted),
             onset_var = as.Date(onset_var)) %>%
      dplyr::filter(dow_conducted == 3) %>%
      full_join(dateseq, by = c("onset_var", "method")) %>%
      arrange(onset_var) %>%
      select(onset_var, estimate, upper, lower, method)
    
    test <- replace(test, is.na(test), 0)
  }
  if(r_e == TRUE){
    
    re_date_shell <- as.data.table(dateseq) %>%
     mutate(baa = "Black",
             hl = "Hispanic/Latino",
             w = "White") %>%
      tidyr::pivot_longer(cols=c(baa, hl, w), values_to = "race_ethnicity") %>%
      mutate(onset_var = as.Date(onset_var)) %>%
      select(onset_var, race_ethnicity) %>%
      mutate(method = method_name)
    
    test <- dataset %>%
      mutate(race_ethnicity = case_when(race_ethnicity == "Black/African American" ~ "Black",
                       TRUE ~ race_ethnicity)) %>%
      filter(onset_var <= as.Date("2022-09-27")) %>%
      mutate(method = method_name,
             dow_conducted = lubridate::wday(date_conducted)) %>%
      dplyr::filter(dow_conducted == 3) %>%
      mutate(onset_var = as.Date(onset_var)) %>%
      full_join(re_date_shell, by = c("onset_var", "race_ethnicity", "method")) %>%
      arrange(onset_var) %>%
      select(onset_var, estimate, upper, lower, method, race_ethnicity)
    
    test <- replace(test, is.na(test), 0)
  
  }
    if(include_weekend == FALSE){
      test <- test %>%
        dplyr::filter(lubridate::wday(onset_var) != 7 & lubridate::wday(onset_var) != 1)
  }
  
  return(test)
}


CheckDispersionStats_date <- function(data, window_end, restrict_weeks, onset_var, rept_var, excl_wkend){
  #Function to check the dispersion ratio for subsections of a time period
  window_end <- as.Date(window_end)
  names(data)[names(data) == onset_var] <- "onset_var"
  names(data)[names(data) == rept_var] <- "rept_var"
  
  max <- as.Date(window_end) 
  min <- as.Date((max - restrict_weeks*7) + 1) # restrict nowcast to [min,max)
  
  dateseq <- seq(min, max, by = 1) %>%
    as.data.frame() 
  
  names(dateseq) <- "onset_var"
  
  # Restrict to X weeks - rept var needs to be in timeframe for retrospective evaluation
  data %>%
    subset(as.Date(onset_var, format = "%Y-%m-%d") >= min & as.Date(onset_var, format = "%Y-%m-%d") <= max) %>%
    subset(as.Date(rept_var) <= max) -> data_window
  
  if(excl_wkend == TRUE){
    data_window <- data_window %>%
      dplyr::filter(lubridate::wday(onset_var) != 7 & lubridate::wday(onset_var) != 1)
  }
  
  window_summary <- data_window %>%
    group_by(onset_var) %>%
    summarize(number_onset = n()) %>%
    full_join(dateseq, by = "onset_var") 
  
  window_summary <- replace(window_summary, is.na(window_summary), 0)
  
  window_summary_nrow <- window_summary %>%
    group_by(onset_var) %>%
    summarize(number = n()) %>%
    nrow()
  
  window_summary <- window_summary %>%
    arrange(onset_var) %>%
    mutate(sequence_variable = seq(1:window_summary_nrow))
  
  model <- glm(data = window_summary, number_onset ~ onset_var, family = poisson)
  check <- check_overdispersion(model)
  
  row <- NULL
  row$timeperiod <- paste0(min, " - ", max)
  row$disp <- check$dispersion_ratio 
  row$pval <- as.numeric(round(check$p_value, digits = 10))
  
  dispersion_table <- rbind(dispersion_table, row)
}







