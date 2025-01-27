library(targets)
library(tarchetypes)
library(tidyverse)
library(future)

source("prepare data.R")
source("models.R")
source("figures.R")

tar_option_set(packages = c("tidyverse", "glmmTMB", "ggplot2", "DHARMa", "sjPlot", "emmeans", "rstatix","ggpubr",
                            "gridExtra", "car", "lme4", "lmerTest", "splines", "multcomp","stringi"))
options(tidyverse.quiet = TRUE)




# workflow
list(

#0) data preparation####
##load data####
tar_target(visits,read.csv("Data/visits_final.csv")[,-1]),
tar_target(duration1,read.csv("Data/duration.txt")), #data collection time at each nestbox
tar_target(important_dates1,read.csv("Data/important_dates.csv")[,-1]),#laying, hatching and fledlging date of each breeding attempt
tar_target(median_hatching,read.csv("Data/median_hatching.csv")[,-1]), #median hatching date at each site
tar_target(all_dates,read.csv("Data/all_dates.csv")[,-1]), #number of chicks and eggs at each nestbox for each day
tar_target(fledglings,read.csv("Data/fledglings.csv")[,-1]),#number and mass of fledglings afor each breeding attempt
tar_target(site_periods,read.csv("Data/site_periods.csv")[,-1]),#average stage (incubation/provisiong/post-fledge) of breeding attempt at each site for each day
tar_target(owners,read.csv("Data/owners_combined.csv")[,-1]),#which bird own which nestbox
tar_target(broken_tag_2023,read.csv("Data/broken_tags_2023.csv", sep = ";")[,-1]), #list of disfunctioning RFID tags and the disfunctioning time window
tar_target(broken_tag_2022,read.csv("Data/broken_tags_2022.csv")[,-1]),
tar_target(age,read.csv("Data/age.csv")), #minimum age of each bird
tar_target(LH,import_LH("Data/Life history.txt")), #used to get the sex of each bird
##formating####
tar_target(duration,duration_format(duration1)),
tar_target(duration_per_day,duration_day(duration)),
tar_target(broken_tags,broken_tags_format(broken_tag_2023,broken_tag_2022)),
tar_target(broken_tags_day,broken_tags_by_day(broken_tags)),
##define parameters####
#data inclusion thresholds
tar_target(period_threshold,0.5),
tar_target(day_threshold,0.95),
##nestbox level dataframes####
tar_target(visits_with_dummies,dummy_visits_box(visits,all_dates,site_periods)),
tar_target(per_box_day1,box_by_day(visits_with_dummies)), 
tar_target(per_box_day2,add_info_box_day(per_box_day1,broken_tags_day,median_hatching,duration_per_day,fledglings,important_dates1)), #group 
tar_target(per_box_day3,box_day_cleanup(per_box_day2,day_threshold)),
#calculate rate (number of visits/hour)
tar_target(per_box_day,box_day_rates(per_box_day3)),
#without threshold
tar_target(per_box_day3_tr0,box_day_cleanup(per_box_day2,0)),
tar_target(per_box_day_tr0,box_day_rates(per_box_day3_tr0)),
###per phase
tar_target(per_box_phase1,box_phase(per_box_day)),
tar_target(per_box_phase1_tr0,box_phase(per_box_day_tr0)),

tar_target(per_box_phase2,box_phase_add_info(per_box_phase1,fledglings)),
tar_target(per_box_phase2_tr0,box_phase_add_info(per_box_phase1_tr0,fledglings)),

tar_target(important_dates,phase_duration(important_dates1,per_box_phase2)),

tar_target(per_box_phase,box_phase_cleanup(per_box_phase2,important_dates,period_threshold)),
tar_target(per_box_phase_tr0,box_phase_cleanup(per_box_phase2_tr0,important_dates,0)),

##nestbox owners dataframes####
tar_target(birds_with_dummies,bird_day_dummies(visits,owners,LH,all_dates,age)),
tar_target(per_bird_day1,bird_day(birds_with_dummies)),
tar_target(per_bird_day2,bird_broken_tags(per_bird_day1,broken_tags_day)),
tar_target(per_bird_day3,bird_day_add_info(per_bird_day2,duration_per_day,median_hatching,fledglings)),

tar_target(per_bird_day4,bird_day_cleanup(per_bird_day3,day_threshold)),
tar_target(per_bird_day4_tr0,bird_day_cleanup(per_bird_day3,0)),

tar_target(per_bird_day,bird_rate(per_bird_day4)),
tar_target(per_bird_day_tr0,bird_rate(per_bird_day4_tr0)),

tar_target(per_bird_phase1,bird_phase(per_bird_day)),
tar_target(per_bird_phase1_tr0,bird_phase(per_bird_day_tr0)),

tar_target(per_bird_phase2,bird_phase_add_info(per_bird_phase1,fledglings)),
tar_target(per_bird_phase2_tr0,bird_phase_add_info(per_bird_phase1_tr0,fledglings)),

tar_target(per_bird_phase,bird_phase_cleanup(per_bird_phase2,important_dates,period_threshold)),
tar_target(per_bird_phase_tr0,bird_phase_cleanup(per_bird_phase2_tr0,important_dates,0)),

##all birds dataframe####
tar_target(all_birds_with_dummies,all_birds_dummies(visits,age,LH)),
tar_target(all_birds_phase1,all_bird_phase(all_birds_with_dummies)),
tar_target(all_birds_phase2,site_assignation(all_birds_phase1,visits)),
tar_target(all_birds_phase3,all_birds_add_info(all_birds_phase2,site_periods,owners)),
tar_target(all_birds_phase4,all_birds_broken_tags(all_birds_phase3,broken_tags_day,site_periods)),

tar_target(all_birds_phase,all_birds_cleanup(all_birds_phase4,period_threshold)),
tar_target(all_birds_phase_tr0,all_birds_cleanup(all_birds_phase4,0)),

  #1)bird-level analysis####
  ##load data####
 # ##with threshold
 #  tar_target(per_bird_phase, read.csv("Data/per_bird_phase.csv")[,-1]),
 #  tar_target(per_bird_day, read.csv("Data/per_bird_day.csv")[,-1]),
 #  tar_target(all_birds_phase, read.csv("Data/all_bird_phase.csv")[,-1]),
 #  ###without threshold
 #  tar_target(per_bird_phase_tr0, read.csv("Data/per_bird_phase_tr0.csv")[,-1]),
 # tar_target(per_bird_day_tr0, read.csv("Data/per_bird_day_tr0.csv")[,-1]),
 #  tar_target(all_birds_phase_tr0, read.csv("Data/all_bird_phase_tr0.csv")[,-1]),


  ##1.1) individual characteristics####

  tar_target(indiv_data,indiv_charact_data(all_birds_phase)),
  tar_target(indiv_data_tr0,indiv_charact_data(all_birds_phase_tr0)),

  tar_target(indiv_model,indiv_model(indiv_data)),
 tar_target(age_general,age_plot(indiv_data,indiv_model,"general")),
 
 
  tar_target(indiv_model_tr0,indiv_model(indiv_data_tr0)),
  #per phase
  ### incubation####
  tar_target(indiv_incub_data, filter(indiv_data,site_phase == "Incubation")),
  tar_target(indiv_model_incub, indiv_model_phase(indiv_incub_data)),
 tar_target(age_incub,age_plot(indiv_incub_data,indiv_model_incub,"incubation")),

  tar_target(indiv_incub_data_tr0, filter(indiv_data_tr0,site_phase == "Incubation")),
  tar_target(indiv_model_incub_tr0, indiv_model_phase(indiv_incub_data_tr0)),

  ### provisioning####

  tar_target(indiv_prov_data, filter(indiv_data,site_phase == "Provisioning")),
  tar_target(indiv_model_prov, indiv_model_phase(indiv_prov_data)),
 tar_target(age_prov,age_plot(indiv_prov_data,indiv_model_prov,"provisioning")),

  tar_target(indiv_prov_data_tr0, filter(indiv_data_tr0,site_phase == "Provisioning")),
  tar_target(indiv_model_prov_tr0, indiv_model_phase(indiv_prov_data_tr0)),

  ###post-fledge####

  tar_target(indiv_pf_data, filter(indiv_data,site_phase == "Fledge")),
  tar_target(indiv_model_pf, indiv_model_phase(indiv_pf_data)),
 tar_target(age_pf,age_plot(indiv_pf_data,indiv_model_pf,"post-fledging")),

  tar_target(indiv_pf_data_tr0, filter(indiv_data_tr0,site_phase == "Fledge")),
  tar_target(indiv_model_pf_tr0, indiv_model_phase(indiv_pf_data_tr0)),

  ##1.2) effect on parental activity####

  tar_target(parental_data, parental_data(per_bird_phase)),
  tar_target(parental_model,parental_model(parental_data)),
  tar_target(trade_off_plot,trade_off_plot(parental_data,parental_model)),
 
  tar_target(parental_data_tr0, parental_data(per_bird_phase_tr0)),
  tar_target(parental_model_tr0,parental_model(parental_data_tr0)),
 


  ##1.3) effect on breeding success####

  tar_target(fitness_data, fitness_data(per_bird_phase)),
  tar_target(fitness_model,fitness_model(fitness_data)),
  tar_target(fitness_plot,fitness_plot(fitness_model)),

  tar_target(fitness_data_tr0, fitness_data(per_bird_phase_tr0)),
  tar_target(fitness_model_tr0,fitness_model(fitness_data_tr0)),
  ##1.4) effect of breeding failure####
  tar_target(bird_failure_data,bird_failure_data(per_bird_day)),
  tar_target(bird_failure_data_tr0,bird_failure_data(per_bird_day_tr0)),


  tar_target(bird_failure_model, bird_failure_model(bird_failure_data)),
  tar_target(bird_failure_model_tr0, bird_failure_model(bird_failure_data_tr0)),
 
  tar_target(bird_failure_sim,simulation_n(bird_failure_model,2000)),
  tar_target(bird_failure_sim_tr0,simulation_n(bird_failure_model_tr0,2000)),

  tar_target(bird_failure_tukey,summary(glht(bird_failure_model, linfct = mcp(FA = "Tukey")))),
  tar_target(bird_failure_tukey_tr0,summary(glht(bird_failure_model_tr0, linfct = mcp(FA = "Tukey")))),

  tar_target(failure_outlier_test,testOutliers(bird_failure_sim, type = "bootstrap")),

  tar_target(fig_failure_bird,failure_pred_full_plot(bird_failure_data,bird_failure_model,important_dates)),

  #2) box level####
  ##load data####
 #  ###with threshold
 # tar_target(per_box_phase, read.csv("Data/per_box_phase.csv")[,-1]),
 # tar_target(per_box_day, read.csv("Data/per_box_day.csv")[,-1]),
 # ###without threshold
 #  tar_target(per_box_phase_tr0, read.csv("Data/per_box_phase_tr0.csv")[,-1]),
 # tar_target(per_box_day_tr0, read.csv("Data/per_box_day_tr0.csv")[,-1]),

  ##2.1) phase comparison####

  tar_target(phase_comp_data,box_phase_comparison(per_box_phase)),
  tar_target(phase_comp_data_tr0,box_phase_comparison(per_box_phase_tr0)),

  tar_target(phase_comparison_model,phase_comp_model(phase_comp_data)),
  tar_target(phase_comparison_model_tr0,phase_comp_model(phase_comp_data_tr0)),

  tar_target(phase_comp_sim,simulation(phase_comparison_model)),
  tar_target(phase_comp_sim_tr0,simulation(phase_comparison_model_tr0)),

  tar_target(phase_comp_tukey,pairs(emmeans(phase_comparison_model,~phase),adjust = "Tukey")),
  tar_target(phase_comp_fig, phase_comp_plot(phase_comp_data,phase_comp_tukey)),

  ##2.2) day by day phenology####

  tar_target(day_day_data, day_by_day_data(per_box_day)),
  tar_target(day_day_data_tr0, day_by_day_data(per_box_day_tr0)),

  tar_target(day_day_model,day_day_model(day_day_data)),
  tar_target(day_day_model_tr0,day_day_model(day_day_data_tr0)),

  tar_target(day_day_sim,simulation(day_day_model)),
  tar_target(day_day_sim_tr0,simulation(day_day_model_tr0)),

  tar_target(day_day_plot,phase_aggregated_plot(day_day_data,day_day_model,phase_comp_fig)),

  ##2.3) effect of parental activity####

  tar_target(parental_box_data,parental_box_data(per_box_phase)),
  tar_target(parental_box_data_tr0,parental_box_data(per_box_phase_tr0)),

  tar_target(colin,cor.test(parental_box_data$median_chick, parental_box_data$parental_rate)),
  ###parental activity only
  tar_target(parental_only_model,parental_only_model(parental_box_data)),
  tar_target(parental_only_model_tr0,parental_only_model(parental_box_data_tr0)),

  tar_target(parental_only_sim,simulation(parental_only_model)),
  tar_target(parental_only_sim_tr0,simulation(parental_only_model_tr0)),

  tar_target(parental_plot,parental_plot(parental_only_model)),

  ###chicks only
  tar_target(chick_only_model,chick_only_model(parental_box_data)),
  tar_target(chick_only_model_tr0,chick_only_model(parental_box_data_tr0)),

  tar_target(chick_only_sim,simulation(chick_only_model)),
  tar_target(chick_only_sim_tr0,simulation(chick_only_model_tr0)),

  tar_target(chick_plot,chick_plot(chick_only_model)),

  ##both
  tar_target(chick_parental_model,chick_parental_model(parental_box_data)),
  tar_target(chick_parental_model_tr0,chick_parental_model(parental_box_data_tr0)),

  tar_target(chick_parental_sim,simulation(chick_parental_model)),
  tar_target(chick_parental_sim_tr0,simulation(chick_parental_model_tr0)),


  ##2.4)effect of failure on nestbox attractivness####
  ###all periods
  tar_target(box_failure_data,box_failure_data(per_box_day)),
  tar_target(box_failure_data_tr0,box_failure_data(per_box_day_tr0)),

  tar_target(box_failure_model,box_failure_model(box_failure_data)),
  tar_target(box_failure_model_tr0,box_failure_model(box_failure_data_tr0)),

  tar_target(box_failure_sim,simulation(box_failure_model)),
  tar_target(box_failure_sim_tr0,simulation(box_failure_model_tr0)),

  ###pre-fledge only
  tar_target(failure_prefledge_data,filter(box_failure_data, site_phase != "Fledge")),
  tar_target(failure_prefledge_data_tr0,filter(box_failure_data_tr0, site_phase != "Fledge")),

  tar_target(box_failure_preFledge_model,box_failure_phase_model(failure_prefledge_data)),
  tar_target(box_failure_preFledge_model_tr0,box_failure_phase_model(failure_prefledge_data_tr0)),

  tar_target(failure_preFledge_sim, simulation(box_failure_preFledge_model)),
  tar_target(failure_preFledge_sim_tr0, simulation(box_failure_preFledge_model_tr0)),

  tar_target(failure_preFledge_tukey,summary(glht(box_failure_preFledge_model, linfct = mcp(FA = "Tukey")))),
  tar_target(failure_preFledge_tukey_tr0,summary(glht(box_failure_preFledge_model_tr0, linfct = mcp(FA = "Tukey")))),


  ###post-fledge only
  tar_target(failure_fledge_data,filter(box_failure_data, site_phase == "Fledge")),
  tar_target(failure_fledge_data_tr0,filter(box_failure_data_tr0, site_phase == "Fledge")),

  tar_target(box_failure_fledge_model,box_failure_phase_model(failure_fledge_data)),
  tar_target(box_failure_fledge_model_tr0,box_failure_phase_model(failure_fledge_data_tr0)),

  tar_target(failure_fledge_sim, simulation(box_failure_fledge_model)),
  tar_target(failure_fledge_sim_tr0, simulation(box_failure_fledge_model_tr0)),

  tar_target(failure_fledge_tukey,summary(glht(box_failure_fledge_model, linfct = mcp(FA = "Tukey")))),
  tar_target(failure_fledge_tukey_tr0,summary(glht(box_failure_fledge_model_tr0, linfct = mcp(FA = "Tukey")))),

   tar_target(failure_box_fig,box_failure_pred_full(failure_prefledge_data,box_failure_preFledge_model,failure_preFledge_tukey,
                                                  failure_fledge_data,box_failure_fledge_model,failure_fledge_tukey)),
 
  #print results pdf####
  tar_render(results, "results.Rmd")

)


















