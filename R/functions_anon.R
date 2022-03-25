##### READ IN DATA #####

merge_files <- function(fns){
  
  dfs <- list()
  
  for (i in 1:length(fns)){
    df <- read.table(fns[i], header=T,  sep="\t", na.strings="-")
    df$Filename <- fns[i]
    
    if (fns[i] %like% "6_SEM")  # missing explicit semester variable for sixth semesters
      df$Semester <- 6
    
    dfs[[i]] <- df
    cat("\014", round(which(fns[i] == fns)/length(fns)*100, 2), "% files read in\n")
  }
  
  return(
    plyr::rbind.fill(dfs)
  )
}

merge_files_validation <- function(fns){
  
  dfs <- list()
  
  for (i in 1:length(fns)){
    df <- read.table(fns[i], header=F, sep="\t", na.strings="-", fill=T)
    df$Filename <- fns[i]
    dfs[[i]] <- df
    cat("\014", round(which(fns[i] == fns)/length(fns)*100, 2), "% files read in\n")
  }
  
  return(
    plyr::rbind.fill(dfs)
  )
}

merge_files_befundungsdata <- function(fns){
  
  dfs <- list()
  
  for (i in 1:length(fns)){
    df <- readxl::read_excel(fns[1])
    df$Filename <- fns[i]
    dfs[[i]] <- df
    cat("\014", round(which(fns[i] == fns)/length(fns)*100, 2), "% files read in\n")
  }
  
  return(
    plyr::rbind.fill(dfs)
  )
}


##### CLEAN DATA #####

clean_master <- function(d){
  return(
    d %>%
      clean_variable_names_and_select_variables %>%
      clean_select_event_categories %>%
      clean_variable_values
  )
}

clean_variable_names_and_select_variables <- function(d){
  return(
    d %>%
      janitor::clean_names() %>%
      rename(event_start = event_start_trial_time_ms, event_end = event_end_trial_time_ms,
             event_duration = event_duration_ms,
             fix_x = fixation_position_x_px, fix_y = fixation_position_y_px,
             fix_apd = fixation_average_pupil_diameter_mm) %>%
      select(filename, participant, semester, trial, stimulus, aoi_name,
             event_start, event_end, event_duration, fix_x, fix_y, fix_apd, category)
  )
}

clean_select_event_categories <- function(d){
  return(
    d %>% filter(category == "Fixation") %>% select(-category)
  )
}  

clean_variable_values <- function(d){
  d <- d %>% mutate(stimulus = str_sub(stimulus, -13, -6))
  d$stimulus <- d$stimulus %>%
    as.factor() %>%
    fct_recode(
                  S1_B01 = "wct_001b",
                  S1_B02 = "wct_004b",
                  S1_B03 = "wct_005b",
                  S1_B04 = "wct_006b",
                  S1_B05 = "wct_007b",
                  S1_B06 = "wct_013b",
                  S1_B07 = "wct_014b",
                  S1_B08 = "wct_027b",
                  S1_B09 = "wct_029b",
                  S1_B10 = "wct_030b",
                  FixKreuz = "fixation"
              )
  d$semester[d$semester == "7.Fachsemester"] <- "7"
  return(d)
}

##### CREATE VARIABLES #####

create_vars_master <- function(d){
  return(
    d %>%
      create_row_index %>%
      create_aoi_stim_combination %>%
      create_session_id %>%
      create_trial_id %>%
      create_signatur_ess %>%
      calculate_pupil_baseline_and_omit_fixation_crosses %>%
      create_trial_time_cumsum
  )
}

create_row_index <- function(d){
  d$row <- 1:nrow(d)
  return(d)
}

create_aoi_stim_combination <- function(d){
  return(
    d %>%
      mutate(aoi_stim = mapply(paste, stimulus, aoi_name, sep="_"))
  )
}

create_session_id <- function(d){
  return(
    d %>% 
      group_by(filename, participant) %>%
      mutate(session_id = cur_group_id()) %>%
      ungroup()
  )
}

create_trial_id <- function(d){
  return(
    d %>% 
      group_by(filename, participant, trial) %>%
      mutate(trial_id = cur_group_id()) %>%
      ungroup()
  )
}

create_signatur_ess <- function(d){
  return(
    d %>% 
      mutate(signatur = filename %>% 
               sub(pattern = ".*Single_", replacement = "") %>%
               gsub(pattern = ".txt", replacement = ""))
  )
}

calculate_pupil_baseline_and_omit_fixation_crosses <- function(d){
  baselines <- d %>% 
    select(trial_id, stimulus, fix_apd) %>%
    filter(stimulus == "FixKreuz") %>%
    group_by(trial_id, stimulus) %>%
    summarise(apd_baseline = mean(fix_apd)) %>%
    select(-stimulus)
  d <- d %>% left_join(baselines, by="trial_id")
  d <- d %>% fill(apd_baseline, .direction = "up")
  d$apd_baselined <- d$fix_apd - d$apd_baseline 
  d <- d %>% filter(stimulus != "FixKreuz")
  return(d)
}

create_trial_time_cumsum <- function(d){
  return(
    d %>% group_by(trial_id) %>% mutate(trial_time_cumsum = cumsum(event_duration)) %>% ungroup()
  )
}

##### SELECT AOIs #####

select_aois <- function(d){
  d <- d[!(d$aoi_stim %in%   # omit contentious anomalies
             c(
               "S1_B03_A06",
               "S1_B03_A08",
               "S1_B03_A07",
               "S1_B05_A10",
               "S1_B07_A01",
               "S1_B07_A08",
               "S1_B07_A09",
               "S1_B08_A03",
               "S1_B08_A01f",
               "S1_B09_A03",
               "S1_B09_A04",
               "S1_B10_A06",
               "S1_B10_A05",
               "S1_B04_A12",
               "S1_B08_A01d",
               "S1_B02_A08",
               "S1_B02_A07",
               "S1_B06_A07",
               "S1_B04_A08"
             )
  ),]
  return(d)
}

##### CREATE TIMESLICES #####

create_timeslices_master_9 <- function(d){
  return(
    d %>%
      create_timeslices_top_down_9
  )
}

create_timeslices_master_3 <- function(d){
  return(
    d %>%
      create_timeslices_top_down_3
  )
}

create_timeslices_top_down_3 <- function(d){
  
  reference <- d %>% group_by(session_id, trial_id) %>% count %>% ungroup()
  reference$session_split_td <- rep(1:10, times=nrow(reference)/10)
  
  d <- d %>% left_join(reference %>% select(-n, -session_id), by="trial_id")
  
  d$session_split_td <- d$session_split_td %>% as.factor()
  
  d$trial_split_td <- NA   # alle 30 Sekunden
  
  d$trial_split_td[d$trial_time_cumsum <= 30000] <- 1
  d$trial_split_td[d$trial_time_cumsum > 30000 & d$trial_time_cumsum <= 60000] <- 2
  d$trial_split_td[d$trial_time_cumsum > 60000] <- 3
  
  d$trial_split_td <- d$trial_split_td %>% as.factor()
  
  return(d)
}

create_timeslices_top_down_9 <- function(d){
  
  reference <- d %>% group_by(session_id, trial_id) %>% count %>% ungroup()
  reference$session_split_td <- rep(1:10, times=nrow(reference)/10)
  
  d <- d %>% left_join(reference %>% select(-n, -session_id), by="trial_id")
  
  d$session_split_td <- d$session_split_td %>% as.factor()
  
  d$trial_split_td <- NA   # alle 10 Sekunden
  
  d$trial_split_td[d$trial_time_cumsum <= 10000] <- 1
  d$trial_split_td[d$trial_time_cumsum > 10000 & d$trial_time_cumsum <= 20000] <- 2
  d$trial_split_td[d$trial_time_cumsum > 20000 & d$trial_time_cumsum <= 30000] <- 3
  d$trial_split_td[d$trial_time_cumsum > 30000 & d$trial_time_cumsum <= 40000] <- 4
  d$trial_split_td[d$trial_time_cumsum > 40000 & d$trial_time_cumsum <= 50000] <- 5
  d$trial_split_td[d$trial_time_cumsum > 50000 & d$trial_time_cumsum <= 60000] <- 6
  d$trial_split_td[d$trial_time_cumsum > 60000 & d$trial_time_cumsum <= 70000] <- 7
  d$trial_split_td[d$trial_time_cumsum > 70000 & d$trial_time_cumsum <= 80000] <- 8
  d$trial_split_td[d$trial_time_cumsum > 80000] <- 9
  
  d$trial_split_td <- d$trial_split_td %>% as.factor()
  
  return(d)
}


##### AGGREGATE ESS DATA #####

aggregate_AOIs_trial_master <- function(d){
  d <- d %>% filter(aoi_name != "White Space")
  return(
    d %>%
      mutate(aoi_stim = mapply(paste, stimulus, aoi_name, sep="_")) %>%
      group_by(participant, signatur, session_split_td, trial_split_td, aoi_stim) %>%
      summarise(
        mean_apd_baselined = mean(apd_baselined),
        mean_fixation_duration = mean(event_duration),
        n_fixations = n(),
        signatur = unique(signatur)
      ) %>% 
      ungroup() %>%
      rename(id = participant) %>%
      select(id, signatur, aoi_stim, session_split_td, trial_split_td, 
             mean_apd_baselined, mean_fixation_duration, n_fixations, signatur)
      
  )
}

aggregate_all_trial_master <- function(d) {
  return(
    d %>%
      group_by(participant, signatur, session_split_td, trial_split_td) %>%
      summarise(
        mean_apd_baselined = mean(apd_baselined),
        mean_fixation_duration = mean(event_duration),
        n_fixations = n(),
        signatur = unique(signatur)
      ) %>% 
      ungroup() %>%
      rename(id = participant) %>%
      select(id, signatur, session_split_td, trial_split_td, 
             mean_apd_baselined, mean_fixation_duration, n_fixations, signatur)
    
  )
}

##### CLEAN BEFUNDUNGSDATEN #####

clean_befundungsdaten_master <- function(d) {
  return(
    d %>%
      bfd_clean_sem %>%
      bfd_clean_id_and_signatur %>%
      bfd_select_variables_and_aois
  )
}

bfd_clean_sem <- function(d) {
  d$sem <- NA
  d$sem[str_detect(d$Filename, "6.Semester")] <- "6"
  d$sem[str_detect(d$Filename, "7.Semester")] <- "7"
  d$sem[str_detect(d$Filename, "8.Semester")] <- "8"
  d$sem[str_detect(d$Filename, "9.Semester")] <- "9"
  d$sem[str_detect(d$Filename, "10.Semester")] <- "10"
  return(d)
}

bfd_clean_id_and_signatur <- function(d) {
  d <- d %>% 
    mutate(signatur = Filename %>% 
             sub(pattern = ".*BFD_", replacement = "") %>%
             gsub(pattern = ".xlsx", replacement = ""))
  
  d <- d %>% 
    filter(!is.na(id)) %>%
    select(-Filename)
  
  return(d)
}

bfd_select_variables_and_aois <- function(d) {
  
  d <- d %>% select(-sem) 
  
  d <- d %>% select(
    -ends_with("_zu_cen"),
    -ends_with("_zu_per"),
    -ends_with("Sum"),
    -ends_with("_f")   # omit contentious diagnoses 
  ) 
  
  return(d)
}

##### SELECT FALSE POSITIVE DATA, DROP FALSE POSITIVES DATA #####

bfd_select_fps <- function(d){
  
  return(
    d %>% select(id, signatur, ends_with("_zu"))
  )
  
}

bfd_drop_fps <- function(d){
  
  return(
    d %>% select(-ends_with("_zu"))
  )
  
}

##### AGGREGATE BEFUNDUNGSDATEN #####

aggregate_befundungsdaten_master <- function(d) {
  return(
    d %>%
      summarise_close_aois %>%
      bfd_wide_to_long %>%
      bfd_clean_variables
  )
}

summarise_close_aois <- function(d) {
  
  names(d) <- names(d) %>%               
    str_replace_all("_[:alpha:]$", "")
  
  d <- d %>% 
    mutate(
      
      S1_B01_A04 = sum(S1_B01_A04a, S1_B01_A04b),
      S1_B01_A05 = sum(S1_B01_A05a, S1_B01_A05b),
      S1_B01_A06 = sum(S1_B01_A06a, S1_B01_A06b, S1_B01_A06c, S1_B01_A06d, S1_B01_A06e),
      S1_B01_A07 = sum(S1_B01_A07a, S1_B01_A07b),
      S1_B01_A08 = sum(S1_B01_A08a, S1_B01_A08b),
      S1_B01_A09 = sum(S1_B01_A09a, S1_B01_A09b),
      S1_B01_A12 = sum(S1_B01_A12a, S1_B01_A12b, S1_B01_A12c, S1_B01_A12d),
      
      S1_B02_A01 = sum(S1_B02_A01a, S1_B02_A01b, S1_B02_A01c),
      S1_B02_A02 = sum(S1_B02_A02a, S1_B02_A02b),
      
      S1_B04_A01 = sum(S1_B04_A01a, S1_B04_A01b),
      S1_B04_A04 = sum(S1_B04_A04a, S1_B04_A04b),
      
      S1_B05_A01 = sum(S1_B05_A01a, S1_B05_A01b),
      S1_B05_A02 = sum(S1_B05_A02a, S1_B05_A02b),
      S1_B05_A03 = sum(S1_B05_A03a, S1_B05_A03b),
      S1_B05_A05 = sum(S1_B05_A05a, S1_B05_A05b),
      S1_B05_A08 = sum(S1_B05_A08a, S1_B05_A08b),
      S1_B05_A09 = sum(S1_B05_A09a, S1_B05_A09b, S1_B05_A09c),
      
      S1_B08_A01ae = sum(S1_B08_A01a, S1_B08_A01e),
      S1_B08_A01bc = sum(S1_B08_A01b, S1_B08_A01c),
      S1_B08_A01d = sum(S1_B08_A01d), 
      # S1_B08_A01f = sum(S1_B08_A01f), # omit contentious diagnosis
      
      S1_B09_A02 = sum(S1_B09_A02a, S1_B09_A02b)
      
      ) %>%
    mutate(
      
      S1_B01_A04 = ifelse(S1_B01_A04 >= 1, 1, 0), 
      S1_B01_A05 = ifelse(S1_B01_A05 >= 1, 1, 0), 
      S1_B01_A06 = ifelse(S1_B01_A06 >= 1, 1, 0), 
      S1_B01_A07 = ifelse(S1_B01_A07 >= 1, 1, 0), 
      S1_B01_A08 = ifelse(S1_B01_A08 >= 1, 1, 0), 
      S1_B01_A09 = ifelse(S1_B01_A09 >= 1, 1, 0), 
      S1_B01_A12 = ifelse(S1_B01_A12 >= 1, 1, 0), 
      
      S1_B02_A01 = ifelse(S1_B02_A01 >= 1, 1, 0), 
      S1_B02_A02 = ifelse(S1_B02_A02 >= 1, 1, 0), 
      
      S1_B04_A01 = ifelse(S1_B04_A01 >= 1, 1, 0), 
      S1_B04_A04 = ifelse(S1_B04_A04 >= 1, 1, 0), 
      
      S1_B05_A01 = ifelse(S1_B05_A01 >= 1, 1, 0), 
      S1_B05_A02 = ifelse(S1_B05_A02 >= 1, 1, 0), 
      S1_B05_A03 = ifelse(S1_B05_A03 >= 1, 1, 0), 
      S1_B05_A05 = ifelse(S1_B05_A05 >= 1, 1, 0), 
      S1_B05_A08 = ifelse(S1_B05_A08 >= 1, 1, 0), 
      S1_B05_A09 = ifelse(S1_B05_A09 >= 1, 1, 0), 
      
      S1_B08_A01ae = ifelse(S1_B08_A01ae >= 1, 1, 0), 
      S1_B08_A01bc = ifelse(S1_B08_A01bc >= 1, 1, 0), 
      S1_B08_A01d = ifelse(S1_B08_A01d >= 1, 1, 0), 
      
      S1_B09_A02 = ifelse(S1_B09_A02 >= 1, 1, 0), 
      
    ) %>%
    select(
      -S1_B01_A04a, 
      -S1_B01_A04b,
      -S1_B01_A05a, 
      -S1_B01_A05b,
      -S1_B01_A06a, 
      -S1_B01_A06b, 
      -S1_B01_A06c, 
      -S1_B01_A06d, 
      -S1_B01_A06e,
      -S1_B01_A07a, 
      -S1_B01_A07b,
      -S1_B01_A08a, 
      -S1_B01_A08b,
      -S1_B01_A09a, 
      -S1_B01_A09b,
      -S1_B01_A12a, 
      -S1_B01_A12b, 
      -S1_B01_A12c, 
      -S1_B01_A12d,
      -S1_B02_A01a, 
      -S1_B02_A01b, 
      -S1_B02_A01c,
      -S1_B02_A02a, 
      -S1_B02_A02b,
      -S1_B04_A01a, 
      -S1_B04_A01b,
      -S1_B04_A04a, 
      -S1_B04_A04b,
      -S1_B05_A01a, 
      -S1_B05_A01b,
      -S1_B05_A02a, 
      -S1_B05_A02b,
      -S1_B05_A03a, 
      -S1_B05_A03b,
      -S1_B05_A05a, 
      -S1_B05_A05b,
      -S1_B05_A08a, 
      -S1_B05_A08b,
      -S1_B05_A09a, 
      -S1_B05_A09b, 
      -S1_B05_A09c,
      -S1_B08_A01a, 
      -S1_B08_A01e,
      -S1_B08_A01b, 
      -S1_B08_A01c,
      -S1_B08_A01d,
      -S1_B09_A02a, 
      -S1_B09_A02b,
      -S1_B05_zu_per...96,
      -S1_B05_zu_per...97
    )
  
  return(d)
}


bfd_wide_to_long <- function(d) {
  return(
    d %>%
      reshape2::melt() %>%
      rename(aoi_stim = "variable", befundung = "value") %>%
      arrange(id, signatur)
  )
}

bfd_clean_variables <- function(d) {
  
  return(
    d %>% 
      rename(bef = befundung) %>%
      mutate(session = paste(id, signatur, sep = "_")) %>%
      mutate(id = session %>% str_extract("[:alnum:]+")) %>%
      mutate(sem = session %>% str_extract("[6789]_SEM|10_SEM|10_Semester") %>% str_replace_all("_SEM|_Semester", "")) %>%
      mutate(mzp = session %>% str_extract("MZP_[123]|MZP[123]") %>% str_replace_all("MZP_|MZP", "")) %>%
      mutate(mzp = ifelse(is.na(mzp), 1, mzp)) %>%                                       # treat single mzp as 1 
      mutate(bild = aoi_stim %>% str_extract_all("B[:alnum:]+") %>% unlist) %>%  # image becomes "session split"
      mutate(aoi = aoi_stim %>% str_extract_all("A.*") %>% unlist) %>%           # extract aoi from stimulus variable
      select(id, sem, mzp, bild, aoi, bef) %>%
      arrange(id, sem, mzp, bild, aoi)
  )
  
}

aggregate_false_positives <- function(d) {
  
  d <- d %>% 
    reshape2::melt() %>%
    rename(bild = variable, fp = value)
  
  d <- d %>% 
    mutate(session = paste(id, signatur, sep = "_")) %>%
    mutate(id = session %>% str_extract("[:alnum:]+")) %>%
    mutate(sem = session %>% str_extract("[6789]_SEM|10_SEM|10_Semester") %>% str_replace_all("_SEM|_Semester", "")) %>%
    mutate(mzp = session %>% str_extract("MZP_[123]|MZP[123]") %>% str_replace_all("MZP_|MZP", "")) %>%
    mutate(mzp = ifelse(is.na(mzp), 1, mzp)) %>%                                       
    mutate(bild = bild %>% str_replace_all("S1_|_zu", "")) %>% 
    select(id, sem, mzp, bild, fp) %>%
    arrange(id, sem, mzp, bild)
  
  return(d)
}

combine_ess_bef_fp <- function(d_ess, d_bfd, d_fp) {
  
  d_ess <- d_ess %>% left_join(d_bfd, by=c("id", "sem", "mzp", "bild", "aoi")) %>% select(id, sem, ck, mzp, bild, aoi, bef, everything())
  
  d_ess <- d_ess %>% left_join(d_fp, by=c("id", "sem", "mzp", "bild")) %>% select(id, sem, ck, mzp, bild, fp, everything())
 
  return(d_ess)
   
}

##### write_participant_calibration_txt #####

calibration_master <- function(d) {
  
  d <- d %>% janitor::clean_names()
  
  d <- d %>% 
    filter(right_eye_deviation_x > .6 | right_eye_deviation_y > .6) %>%
    select(participant, semester, filename, 
           right_eye_deviation_x, right_eye_deviation_y)
  
  d <- d %>% 
    mutate(signatur = filename %>% 
             sub(pattern = ".*Overview_", replacement = "") %>%
             gsub(pattern = ".txt", replacement = "") %>%
             sub(pattern = ".*OverviewSingle_", replacement = "") %>%
             sub(pattern = ".*Single_", replacement = "")) %>%
    select(-filename)
  
  sink("documentation/calibration_exclusion_results.txt")
  print(d)
  sink()
  
  return(d %>% select(participant, signatur))
}

##### write_trial_tracking_ratio_txt #####

tracking_ratio_master <- function(d) {
  
  d <- d %>% janitor::clean_names()
  
  d <- d %>% 
    filter(tracking_ratio < 80.00) %>%
    select(participant, semester, filename, tracking_ratio)
  
  d <- d %>% 
    mutate(signatur = filename %>% 
             sub(pattern = ".*Overview_", replacement = "") %>%
             gsub(pattern = ".txt", replacement = "") %>%
             sub(pattern = ".*OverviewSingle_", replacement = "") %>%
             sub(pattern = ".*Single_", replacement = "")) %>%
    select(-filename)
  
  sink("documentation/tracking_ratio_exclusion_results.txt")
  print(d)
  sink()
  
  return(d %>% select(participant, signatur))
}

##### APPLY EXCLUSION #####

apply_exclusion <- function(d, cal, tr) {
  
  exclude <- rbind(cal, tr) %>% unique()
  
  n_all <- d %>% group_by(participant, signatur) %>% count %>% nrow
  n_excluded <- exclude %>% group_by(participant, signatur) %>% count %>% nrow
  
  sink("documentation/exclusion_ratio_statistic.txt")
  cat("This resulted in an exclusion of:")
  cat((n_excluded*100/n_all) %>% round(2))
  cat("% of experimental sessions\n")
  sink()
  
  d <- d[!(d$participant %in% exclude$participant & d$signatur %in% exclude$signatur),]
  
  return(d)
}

##### JOIN FALSE POSITIVE DATA #####

add_fp <- function(d, dfp) {
  
  d$bild <- d$aoi_stim %>% 
    gsub(pattern = "_A.*", replacement = "")
  
  dfp <- dfp %>% 
    reshape2::melt() %>%
    rename(bild = variable, n_fp = value)
  
  dfp$bild <- dfp$bild %>%
    gsub(pattern = "_zu.*", replacement = "")
  
  return(
    d %>% left_join(dfp, by = c("id", "signatur", "bild"))
  )
  
}

##### CK PROCESSING #####

read.qualtrics.csv <- function(filename, stringsAsFactors = FALSE, ...) {
  n <- read.csv(filename, nrows = 1, stringsAsFactors = FALSE)
  dat <- read.csv(filename, header = FALSE, skip = 2, stringsAsFactors = stringsAsFactors, ...)
  names(dat) <- names(n)
  names(dat)[1:10] <- n[1,1:10]
  for(i in seq_along(dat)) {
    attr(dat[,i], "question") <- n[1,i]
  }
  dat
}

ck_pre_processing <- function(path) {
  
  data <- read.qualtrics.csv(path) # vorher 2
  write.table(data, "ck.txt", sep="\t")
  dat.ck.raw <- read.csv("ck.txt", header=TRUE, sep="\t")
  system("rm ck.txt")
  
  dat.ck.raw <- dat.ck.raw%>%
    mutate(Date = str_sub(NA., 1, 10))%>%
    select(ID, Age, Sex, Subject, Semester, Date, Brille.Kontaktlinsen, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q18, Q19, Q20)
  
  # einheitliche Bezeichnung Sex: w, m
  dat.ck.raw$Sex[dat.ck.raw$Sex == "Weiblich"] <- "w"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "weiblich"] <- "w"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "mÃ¤nnlich"] <- "m"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "MÃ¤nnlich"] <- "m"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "mÃ¤nnlivh"] <- "m"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "mÃ¤nnlih"] <- "m"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "m?nnlich"] <- "m"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "M?nnlich"] <- "m"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "M"] <- "m"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "W"] <- "w"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "f"] <- "w"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "na"] <- NA
  dat.ck.raw$Sex[dat.ck.raw$Sex == "x"] <- NA
  dat.ck.raw$Sex[dat.ck.raw$Sex == "m?nnlivh"] <- "m"
  dat.ck.raw$Sex[dat.ck.raw$Sex == "m?m"] <- "m"
  #dat.ck.raw$Sex[dat.ck.raw$Sex == "weiblich "] <- "w"
  
  #### Data filtered by semester ###
  
  dat.ck.SS17.6Sem <- dat.ck.raw%>%
    filter(Date == "2017-07-21")%>%  # Filter
    mutate(Semester = 6, Time = "SS17", Cohort = "A") 
  
  dat.ck.WS1718.6Sem <- dat.ck.raw%>%
    filter(Date == "2018-02-23")%>%
    mutate(Semester = 6, Time = "WS1718", Cohort = "F")
  
  dat.ck.SS18.6Sem <- dat.ck.raw%>%
    filter(Date == "2018-08-03" | Date == "2018-08-17")%>%
    mutate(Semester = 6, Time = "SS18", Cohort = "G")
  
  dat.ck.WS1819.6Sem <- dat.ck.raw%>%
    filter(Date == "2019-01-14")%>% 
    mutate(Semester = 6, Time = "WS1819", Cohort = "H")
  
  dat.ck.SS17.7Sem <- dat.ck.raw%>%
    filter(Date == "2017-07-24")%>%
    mutate(Semester = 7, Time = "SS17", Cohort = "B")
  
  dat.ck.WS1718.7Sem <- dat.ck.raw%>%
    filter(Date == "2017-11-03" | Date == "2017-11-13")%>%
    mutate(Semester = 7, Time = "WS1718", Cohort = "A")
  
  dat.ck.SS18.7Sem <- dat.ck.raw%>%
    filter(Date == "2018-04-23", Semester %in% c("7", "07", "7."))%>%
    mutate(Semester = 7, Time = "SS18", Cohort = "F")
  
  dat.ck.WS1819.7Sem <- dat.ck.raw%>%
    filter(Date == "2018-12-10" | Date == "2018-12-13", Semester %in% c("7", "07", "-"))%>%
    mutate(Semester = 7, Time = "WS1819", Cohort = "G")
  
  dat.ck.SS17.8Sem <- dat.ck.raw%>%
    filter(Date == "2017-06-21" | Date == "2017-06-27", Semester %in% c("8", "8."))%>%
    mutate(Semester = 8, Time = "SS17", Cohort = "C")
  
  dat.ck.WS1718.8Sem <- dat.ck.raw%>%
    filter(Date == "2018-01-24" | Date == "2018-01-16", Semester %in% c("8"))%>%
    mutate(Semester = 8, Time = "WS1718", Cohort = "B")
  
  dat.ck.SS18.8Sem <- dat.ck.raw%>%
    filter(Date == "2018-05-02" | Date == "2018-05-09" | Date == "2018-05-17", Semester %in% c("8", "8. Fachsemester"))%>%
    mutate(Semester = 8, Time = "SS18", Cohort = "A")
  
  dat.ck.WS1819.8Sem <- dat.ck.raw%>%
    filter(Date == "2018-12-10" | Date == "2018-11-14", Semester %in% c("8"))%>%
    mutate(Semester = 8, Time = "WS1819", Cohort = "F")
  
  dat.ck.SS17.9Sem <- dat.ck.raw%>%
    filter(Date == "2017-06-21" | Date == "2017-06-27", Semester %in% c("9", "09"))%>%
    mutate(Semester = 9, Time = "SS17", Cohort = "D")
  
  dat.ck.WS1718.9Sem <- dat.ck.raw%>%
    filter (Date == "2018-01-16" | Date == "2018-01-24", Semester %in% c("9", "9.", "09", ""))%>%
    mutate(Semester = 9, Time = "WS1718", Cohort = "C")
  
  dat.ck.SS18.9Sem <- dat.ck.raw%>%
    filter(Date == "2018-05-08" | Date == "2018-05-17")%>%
    mutate(Semester = 9, Time = "SS18", Cohort = "B")
  
  dat.ck.WS1819.9Sem <- dat.ck.raw%>%
    filter(Date == "2018-12-13" | Date == "2018-11-20", Semester %in% c("9"))%>%
    mutate(Semester = 9, Time = "WS1819", Cohort = "A")
  
  dat.ck.SS17.10Sem <- dat.ck.raw%>%
    filter(Date == "2017-05-23")%>%
    mutate(Semester = 10, Time = "SS17", Cohort = "E")
  
  dat.ck.WS1718.10Sem <- dat.ck.raw%>%
    filter (Date == "2017-12-15")%>%
    mutate(Semester = 10, Time = "WS1718", Cohort = "D")
  
  dat.ck.SS18.10Sem <- dat.ck.raw%>%
    filter(Date == "2018-07-11")%>%
    mutate(Semester = 10, Time = "SS18", Cohort = "C")
  
  dat.ck.WS1819.10Sem <- dat.ck.raw%>%
    filter(Date == "2019-01-15")%>%
    mutate(Semester = 10, Time = "WS1819", Cohort = "B")
  
  dat.ck <- dat.ck.SS17.6Sem%>%
    bind_rows(dat.ck.WS1718.6Sem,dat.ck.SS18.6Sem, dat.ck.WS1819.6Sem,
              dat.ck.SS17.7Sem, dat.ck.WS1718.7Sem, dat.ck.SS18.7Sem, dat.ck.WS1819.7Sem,
              dat.ck.SS17.8Sem, dat.ck.WS1718.8Sem, dat.ck.SS18.8Sem, dat.ck.WS1819.8Sem,
              dat.ck.SS17.9Sem, dat.ck.WS1718.9Sem, dat.ck.SS18.9Sem, dat.ck.WS1819.9Sem,
              dat.ck.SS17.10Sem, dat.ck.WS1718.10Sem, dat.ck.SS18.10Sem, dat.ck.WS1819.10Sem)
  
  dat.ck <- dat.ck[dat.ck$ID != "a",]
  dat.ck <- dat.ck[dat.ck$ID != "w",]
  
  ### Clean ID variables ##
  
  dat.ck$ID <- as.character(dat.ck$ID)
  
  dat.ck$ID[dat.ck$ID == "P01" & dat.ck$Time == "WS1819"] <- "anon181"
  
  dat.ck$ID[dat.ck$ID == "anon3" & dat.ck$Semester == "7" & dat.ck$Time == "SS17"] <- "anon3.1"
  dat.ck$ID[dat.ck$ID == "anon3" & dat.ck$Semester == "8" & dat.ck$Time == "WS1718"] <- "anon3.1"
  dat.ck$ID[dat.ck$ID == "anon3" & dat.ck$Semester == "9" & dat.ck$Time == "SS18"] <- "anon3.1"
  dat.ck$ID[dat.ck$ID == "anon3" & dat.ck$Semester == "10" & dat.ck$Time == "WS1819"] <- "anon3.1"
  dat.ck$ID[dat.ck$ID == "anon3" & dat.ck$Semester == "6" & dat.ck$Time == "SS18"] <- "anon3.2"
  dat.ck$ID[dat.ck$ID == "anon3" & dat.ck$Semester == "7" & dat.ck$Time == "WS1819"] <- "anon3.2"
  
  
  dat.ck$ID[dat.ck$ID == "anon25" & dat.ck$Semester == "6" & dat.ck$Time == "WS1718"] <- "anon25.1"
  dat.ck$ID[dat.ck$ID == "anon25" & dat.ck$Semester == "7" & dat.ck$Time == "SS18"] <- "anon25.1"
  dat.ck$ID[dat.ck$ID == "anon25" & dat.ck$Semester == "8" & dat.ck$Time == "WS1819"] <- "anon25.1"
  dat.ck$ID[dat.ck$ID == "anon25" & dat.ck$Semester == "6" & dat.ck$Time == "WS1819"] <- "anon25.2"
  
  
  dat.ck$ID <- factor(dat.ck$ID)
  
  ##### Code Responses  ####
  
  fragen <- dat.ck%>%
    select(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q18, Q19, Q20)
  
  # Correct responses to questions
  n <- c(1, 1, 3, 2, 2, 3, 3, 2, 3, 2, 1, 2, 3, 3, 2, 2, 1, 3, 3, 3) 
  
  dat2 <- data.frame()
  
  for(j in 1:dim(fragen)[1]){
    for(i in 1:20){   
      if(is.na(fragen[j,i])){
        dat2[j,i] <- NA
      }
      else if(fragen[j,i] == n[i]){
        dat2[j,i] <- 1 
      }
      
      else { 
        dat2[j,i] <- 0
      }
    }
  }
  
  dat.ck$CK.Score <- dat2$V1 + dat2$V2 + dat2$V3 + dat2$V4 + dat2$V5 + dat2$V6 + dat2$V7 + dat2$V8 + dat2$V9 + dat2$V10 + dat2$V11 +dat2$V12 +dat2$V13 + dat2$V14 + dat2$V15 + dat2$V16 + dat2$V17 + dat2$V18 + dat2$V19 + dat2$V20
  
  dat.ck <- dat.ck%>%
    select(ID, Time, Semester, Cohort, Age, Sex, CK.Score, Brille.Kontaktlinsen)
  
  dat.ck$Semester <- factor(dat.ck$Semester)
  dat.ck$Time <- factor(dat.ck$Time)
  dat.ck$Cohort <- factor(dat.ck$Cohort)
  
  return(dat.ck)
}

combine_ess_ck <- function(d_ess, d_ck) {
  
  d_ck <- d_ck %>%
    select(ID, Semester, CK.Score) %>%
    rename(id = ID, sem = Semester, ck = CK.Score)
  
  d_ck$id <- d_ck$id %>% 
    str_replace_all("[:punct:]1|[:punct:]2", "") 
  
  # Handling duplicates, imputing case mean
  
  d_ck$ck <- d_ck$ck %>% as.numeric()
  
  d_ck <- d_ck[!is.na(d_ck$ck),]
  
  d_ck <- d_ck %>% 
    group_by(id, sem) %>%
    summarise(ck = mean(ck)) %>%
    ungroup()
  
  # Imputations by mean of semester
  
  #  1 anon125      MZP_2_SS_18_6_SEM_SET_1_2
  #2 anon91      MZP_2_SS_18_6_SEM_SET_1_2
  #3 anon169      SS17_10_Semester_SET_1   
  #4 anon21      SS17_MZP_2_6_SEM_SET_1   
  #5 anon125      SS18_MZP_1_6_SEM_SET_1   
  #6 anon130      WS1718_8_SEM_SET_1_3     
  #7 anon105      WS1718_9_SEM_SET_1_3     
  #8 anon147      WS1718_MZP2_6_SEM_SET_1_2
  #9 anon12      WS1718_MZP2_6_SEM_SET_1_2 
  #10 anon131      WS1718_MZP2_6_SEM_SET_1_2
  #11 anon68      WS1718_MZP2_6_SEM_SET_1_2
  
  d_ck$id <- d_ck$id %>% as.character()
  
  d_ck <- rbind(d_ck, c("anon125", 6, mean(d_ck$ck[d_ck$sem==6], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  d_ck <- rbind(d_ck, c("anon91", 6, mean(d_ck$ck[d_ck$sem==6], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  d_ck <- rbind(d_ck, c("anon169", 10, mean(d_ck$ck[d_ck$sem==10], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  d_ck <- rbind(d_ck, c("anon21", 6, mean(d_ck$ck[d_ck$sem==6], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  d_ck <- rbind(d_ck, c("anon130", 8, mean(d_ck$ck[d_ck$sem==8], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  d_ck <- rbind(d_ck, c("anon105", 9, mean(d_ck$ck[d_ck$sem==9], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  d_ck <- rbind(d_ck, c("anon147", 6, mean(d_ck$ck[d_ck$sem==6], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  d_ck <- rbind(d_ck, c("anon12", 6, mean(d_ck$ck[d_ck$sem==6], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  d_ck <- rbind(d_ck, c("anon131", 6, mean(d_ck$ck[d_ck$sem==6], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  d_ck <- rbind(d_ck, c("anon68", 6, mean(d_ck$ck[d_ck$sem==6], na.rm=T))); d_ck$ck <- d_ck$ck %>% as.numeric()
  
  return(d_ess %>% left_join(d_ck, by=c("id", "sem")) %>% select(id, sem, ck, mzp, everything())) # reorder to participant variables
  
}

##### FILL IN AOIS NOT LOOKED AT IN SESSION ####

clean_and_fill_trial_td_df <- function(d) {
  
  d <- d %>% 
    mutate(session = paste(id, signatur, sep = "_")) %>%
    complete(session, aoi_stim, trial_split_td)
  
  d <- d %>% 
    mutate(id = session %>% str_extract("[:alnum:]+")) %>%
    mutate(sem = session %>% str_extract("[6789]_SEM|10_SEM|10_Semester") %>% str_replace_all("_SEM|_Semester", "")) %>%
    mutate(mzp = session %>% str_extract("MZP_[123]|MZP[123]") %>% str_replace_all("MZP_|MZP", "")) %>%
    mutate(mzp = ifelse(is.na(mzp), 1, mzp)) %>%                                       
    mutate(bild = aoi_stim %>% str_extract_all("B[:alnum:]+") %>% unlist) %>% 
    mutate(aoi = aoi_stim %>% str_extract_all("A.*") %>% unlist) %>%           
    rename(tsplit = trial_split_td) %>%                                        
    rename(apd = mean_apd_baselined, mfd = mean_fixation_duration) %>%
    rename(nfix = n_fixations) %>%
    arrange(id, sem, mzp, bild, tsplit, aoi) %>%
    select(id, sem, mzp, bild, aoi, tsplit, apd, mfd, nfix, session)
  
  d <- d %>%
    mutate(signatur = session %>% str_remove("[:alnum:]+")) %>% 
    mutate(time = signatur %>% str_extract("[WS]S[_]?[[:digit:]]{2,}") %>% str_remove_all("_")) %>%
    mutate(sem = signatur %>% str_extract("[6789]_SEM|10_SEM|10_Semester") %>% str_replace_all("_SEM|_Semester", ""))
  
  d$cohort <- NA
  d$cohort[d$sem==6 & d$time=="SS17"] <- "A" 
  d$cohort[d$sem==6 & d$time=="WS1718"] <- "F" 
  d$cohort[d$sem==6 & d$time=="SS18"] <- "G" 
  d$cohort[d$sem==7 & d$time=="SS17"] <- "B" 
  d$cohort[d$sem==7 & d$time=="WS1718"] <- "A" 
  d$cohort[d$sem==8 & d$time=="SS17"] <- "C" 
  d$cohort[d$sem==8 & d$time=="WS1718"] <- "B" 
  d$cohort[d$sem==8 & d$time=="SS18"] <- "A" 
  d$cohort[d$sem==9 & d$time=="SS17"] <- "D" 
  d$cohort[d$sem==9 & d$time=="WS1718"] <- "C" 
  d$cohort[d$sem==9 & d$time=="SS18"] <- "B" 
  d$cohort[d$sem==10 & d$time=="SS17"] <- "E" 
  d$cohort[d$sem==10 & d$time=="WS1718"] <- "D" 
  d$cohort[d$sem==10 & d$time=="SS18"] <- "C" 
  
  d <- d %>%
    mutate(mzp = signatur %>% str_extract("MZP_[123]|MZP[123]") %>% str_replace_all("MZP_|MZP", "")) %>%
    mutate(mzp = ifelse(is.na(mzp), 1, mzp))
  
  d <- d %>%
    select(id, time, cohort, sem, mzp, bild, aoi, tsplit, apd, mfd, nfix)
  
  return(d)
}

clean_and_fill_B <- function(d) {
  
  d <- d %>% 
    mutate(session = paste(id, signatur, sep = "_")) %>%
    complete(session, session_split_td, trial_split_td)
  
  d <- d %>% 
    mutate(id = session %>% str_extract("[:alnum:]+")) %>%
    mutate(sem = session %>% str_extract("[6789]_SEM|10_SEM|10_Semester") %>% str_replace_all("_SEM|_Semester", "")) %>%
    mutate(mzp = session %>% str_extract("MZP_[123]|MZP[123]") %>% str_replace_all("MZP_|MZP", "")) %>%
    mutate(mzp = ifelse(is.na(mzp), 1, mzp)) %>%                                       
    mutate(bild = session_split_td) %>%                                        
    rename(tsplit = trial_split_td) %>%                                        
    rename(apd = mean_apd_baselined, mfd = mean_fixation_duration) %>%
    rename(nfix = n_fixations) %>%
    arrange(id, sem, mzp, bild, tsplit) %>%
    select(id, sem, mzp, bild, tsplit, apd, mfd, nfix, session)
  
  d <- d %>%
    mutate(signatur = session %>% str_remove("[:alnum:]+")) %>% 
    mutate(time = signatur %>% str_extract("[WS]S[_]?[[:digit:]]{2,}") %>% str_remove_all("_")) %>%
    mutate(sem = signatur %>% str_extract("[6789]_SEM|10_SEM|10_Semester") %>% str_replace_all("_SEM|_Semester", ""))
  
  d$cohort <- NA
  d$cohort[d$sem==6 & d$time=="SS17"] <- "A" 
  d$cohort[d$sem==6 & d$time=="WS1718"] <- "F" 
  d$cohort[d$sem==6 & d$time=="SS18"] <- "G" 
  d$cohort[d$sem==7 & d$time=="SS17"] <- "B" 
  d$cohort[d$sem==7 & d$time=="WS1718"] <- "A" 
  d$cohort[d$sem==8 & d$time=="SS17"] <- "C" 
  d$cohort[d$sem==8 & d$time=="WS1718"] <- "B" 
  d$cohort[d$sem==8 & d$time=="SS18"] <- "A" 
  d$cohort[d$sem==9 & d$time=="SS17"] <- "D" 
  d$cohort[d$sem==9 & d$time=="WS1718"] <- "C" 
  d$cohort[d$sem==9 & d$time=="SS18"] <- "B" 
  d$cohort[d$sem==10 & d$time=="SS17"] <- "E" 
  d$cohort[d$sem==10 & d$time=="WS1718"] <- "D" 
  d$cohort[d$sem==10 & d$time=="SS18"] <- "C" 
  
  d <- d %>%
    mutate(mzp = signatur %>% str_extract("MZP_[123]|MZP[123]") %>% str_replace_all("MZP_|MZP", "")) %>%
    mutate(mzp = ifelse(is.na(mzp), 1, mzp))
  
  d <- d %>%
    select(id, time, cohort, sem, mzp, bild, tsplit, apd, mfd, nfix)
  
  return(d)
}


##### Long to Wide #####

long_2_wide_trial <- function(d) {
  
  d <- d %>% 
    pivot_wider(
      id_cols = c("id", "sem", "mzp", "bild", "aoi"), 
      names_from = tsplit, 
      values_from = all_of(c("apd", "mfd", "nfix", "lastfix")),
    )
  
  return(d)
}

##### Master Data Sets #####

join_d_bef <- function(d_trial, d_session, d_trial_bu) {
  
  d <- d_session %>% left_join(d_trial, by = c("id", "sem", "ck", "mzp", "bild", "fp", "aoi", "bef", "etg"))
  
  d <- d %>% left_join(d_trial_bu, by = c("id", "sem", "ck", "mzp", "bild", "fp", "aoi", "bef", "etg"), suffix = c("_td", "_bu"))
  
  return(d)
  
}

##### Fill NA with 0 (but not for Entry time) #####

fill_na_d <- function(d) {
  
  nofill <- names(d) %>% grep(pattern="et[sg]")
  all <- 1:length(names(d))
  
  d[, setdiff(all, nofill)][is.na(d[, setdiff(all, nofill)])] <- 0
  
  return(d)
}

##### Restructure variables for model building #####

clean_final_data <- function(d) {
  
  d <- d %>% 
    mutate(
      bildaoi = paste(bild, aoi, sep=""),
      trial = bild %>% str_remove_all("B") %>% str_remove_all("^0") %>% as.numeric()
      )
  
  d$sem <- d$sem %>% as.numeric
  d$sem[d$mzp==2] <- d$sem[d$mzp==2] + 1/3
  d$sem[d$mzp==3] <- d$sem[d$mzp==3] + 2/3
  
  d <- d %>% select(id, time, cohort, sem, ck, trial, bildaoi, tsplit, apd, mfd, nfix, bef)
  
  # Must have looked at AOI at least once
  
  d <- d %>% filter(!is.na(nfix))
  
  d$apd <- (d$apd - mean(d$apd)) / sd(d$apd) # scale pupil diameter
  d$mfd = d$mfd/1000                         # fixation duration in seconds
  
  # Technical error: This session contained trials of over 90 s and will be omitted
  
  d <- d[!(d$id=="anon35" & d$time=="SS18"),]
  
  return(d)
  
}

clean_final_B <- function(d) {
  
  d <- d %>% 
    mutate(
      trial = bild
    )
  
  d$sem <- d$sem %>% as.numeric
  d$sem[d$mzp==2] <- d$sem[d$mzp==2] + 1/3
  d$sem[d$mzp==3] <- d$sem[d$mzp==3] + 2/3
  
  d <- d %>% select(id, time, cohort, sem, trial, tsplit, apd, mfd, nfix)
  
  d$mfd = d$mfd/1000                         # fixation duration in seconds
  
  d$id <- factor(d$id)
  d$time <- factor(d$time, levels=c("SS17", "WS1718", "SS18"))
  d$cohort <- factor(d$cohort, levels=c("A", "B", "C", "D", "E", "F", "G"))
  d$trial <- factor(d$trial)
  d$tsplit <- factor(d$tsplit)
  
  # Target variables approximately normally distributed at log()
  
  d$mfd.log <- d$mfd %>% log()
  d$nfix.log <- d$nfix %>% log()
  
  d$apd <- (d$apd - mean(d$apd, na.rm = T)) / sd(d$apd, na.rm = T) # scale pupil diameter
  
  # Technical error: This session contained trials of over 90 s and will be omitted
  
  d <- d[!(d$id=="anon35" & d$time=="SS18"),]
  
  return(d)
  
}

##### Confirmatory set #####

clean_to_analysis <- function(d) {
  
  d$id <- factor(d$id)
  d$time <- factor(d$time, levels=c("SS17", "WS1718", "SS18"))
  d$cohort <- factor(d$cohort, levels=c("A", "B", "C", "D", "E", "F", "G"))
  d$id <- factor(d$id)
  d$trial <- factor(d$trial)
  d$bildaoi <- factor(d$bildaoi)
  
  d$cksplit <- ifelse(d$ck >= median(d$ck), 1, 0)
  d$cksplit <- factor(d$cksplit, levels=0:1, labels=c("low", "high"))
  
  d <- d %>% select(id, time, cohort, sem, ck, trial, bildaoi, tsplit, cksplit, apd, mfd, nfix, bef)
  
  return(d)
}

confirmatory_subsetting <- function(d) {
  
  d <- d[d$tsplit %in% c(1, 9),]
  
  d$tsplit <- d$tsplit %>% as.character() %>%
    fct_recode(
      first = "1",
      last = "9"
    )
  
  return(d)
  
}
