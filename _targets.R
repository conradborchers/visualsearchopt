library(targets)

targets::tar_option_set(packages = c("here", "tidyverse", "data.table"))

source(here::here("R", "functions_anon.R"))

list(
  
  # Data reading and cleaning
  
  tar_target(raw_ess, here::here("data_anon", dir(here::here("data_anon"), pattern="*Event_Statistics_Single*")) %>% merge_files),
  tar_target(raw_po, here::here("data_anon", dir(here::here("data_anon"), pattern="*Participant_Overview*")) %>% merge_files),
  tar_target(raw_to, here::here("data_anon", dir(here::here("data_anon"), pattern="*Trial_Overview*")) %>% merge_files),
  tar_target(raw_vro, here::here("data_anon", dir(here::here("data_anon"), pattern="*Validation_Results_Overview*")) %>% merge_files_validation),
  
  tar_target(cal, raw_po %>% calibration_master),
  tar_target(tr, raw_po %>% tracking_ratio_master),
  
  tar_target(d_clean, raw_ess %>% clean_master),
  
  tar_target(d_create_vars, d_clean %>% create_vars_master),
  
  tar_target(d_exclusion_applied, apply_exclusion(d_create_vars, cal, tr)),
  
  tar_target(d_aoi_selection, d_exclusion_applied %>% select_aois),
  
  # Data aggregating, timeslices with white space for fixation measures, AOI only for pupil dilation
  
  tar_target(d_timeslice_3, d_aoi_selection %>% create_timeslices_master_3),
  tar_target(d_timeslice_9, d_aoi_selection %>% create_timeslices_master_9),
  
  # A: Aggregate mean fix and n fix over 3 and 9 slices
  
  tar_target(d_aggregated_trial_3, d_timeslice_3 %>% aggregate_all_trial_master),
  tar_target(d_trial_clean_3, d_aggregated_trial_3 %>% clean_and_fill_B),
  
  tar_target(d_aggregated_trial_9, d_timeslice_9 %>% aggregate_all_trial_master),
  tar_target(d_trial_clean_9, d_aggregated_trial_9 %>% clean_and_fill_B),
  
  # B: Predict detection with pupil dilation at each AOIs excluding white space
  
  tar_target(d_aggregated_aoi_3, d_timeslice_3 %>% aggregate_AOIs_trial_master),
  tar_target(d_aoi_clean_filled_3, d_aggregated_aoi_3 %>% clean_and_fill_trial_td_df),
  
  tar_target(d_aggregated_aoi_9, d_timeslice_9 %>% aggregate_AOIs_trial_master),
  tar_target(d_aoi_clean_filled_9, d_aggregated_aoi_9 %>% clean_and_fill_trial_td_df),
  

  # Final Cleaning
  tar_target(clean_fix_3, d_trial_clean_3 %>% clean_final_B),
  tar_target(clean_fix_9, d_trial_clean_9 %>% clean_final_B),
  
  # Export data sets by analysis section
  
  tar_target(H1, clean_fix_3 %>% filter(tsplit %in% c(1, 3))),
  tar_target(H2, read_csv(here::here('data_anon', 'H2_anon.csv'))),
  
  tar_target(plot_gla, clean_fix_9 %>% mutate(fps=nfix/10)), # ts plot and gaze-likelihood analysis, fixations per second
  tar_target(plot_decay, read_csv(here::here('data_anon', 'plot_decay_anon.csv')))
  
)
