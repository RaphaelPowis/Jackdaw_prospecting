

simulation <- function(model){
  return(simulateResiduals(fittedModel = model, plot = T, seed = 1))
}

simulation_n <- function(model,n){
  return(simulateResiduals(fittedModel = model, plot = T, seed = 1,n = n))
}


indiv_model <- function(d){
  
  individual_model = glmmTMB(data = d,prospect_visits~ sex+ age + site_phase
                                         +year + site +site_phase*sex+
                                           site_phase*age
                                         +(1|JID)+(1|breeding_attempt)
                                         +offset(log(functional_days))
                                         ,family = genpois)
  return(individual_model)
  
}


indiv_model_phase <- function(d){
  out = glmmTMB(data = d,prospect_visits~
                          sex+age+(1|JID)
                        +(year)+site+offset(log(functional_days))
                        ,family =  genpois)
  
  return(out)
}


parental_model <- function(d){
  parental_model = glmmTMB(data = d, parental_visits~prospect_rate_loggerOK+
                             mean_chick+
                             (1|JID)+(1|breeding_attempt)+site+year+sex+age+ 
                             offset(log(total_logger_duration)),
                           family = nbinom2)
  return(parental_model)
}

fitness_model <- function(d){
  fitness_model <- lmer(data = d, mass_dev~total_prospect_rate+
                         (1|owned_box)+year+(1|site)+mean_age)
  return(fitness_model)
}


bird_failure_model <- function(d){
  knots = quantile(d$relative_day,seq(0.1,0.9,by= 0.1))
  FA_model <- glmmTMB(data = d,prospect_visits ~ ns(relative_day, knots = knots)*sex+
                        FA+(1|JID)+(1|breeding_attempt)+year+(1|site)
                      ,ziformula = ~1
                      ,family = genpois)
  return(FA_model)
}

failure_model_sex <- function(d){
  
  knots = quantile(d$relative_day,seq(0.1,0.9,by= 0.1))
  FA_model <- glmmTMB(data = d,prospect_visits ~ FA+ns(relative_day, knots = knots)+
                           (1|JID)+(1|breeding_attempt)+(1|site)+year+age,
                         family = genpois)
  return(FA_model)
  
}

phase_comp_model <- function(d){
  phase_comparison = glmmTMB(data = d, prospect_visits~phase+(1|box)+
                               (1|breeding_attempt)+(site)+year,
                             offset = log(total_duration),
                             family = nbinom2)
  return(phase_comparison)
}

day_day_model <- function(d){
  
  knots = quantile(d$relative_day,seq(0.1,0.9,by= 0.1))
  day_day_trend <- glmmTMB(data = d,prospect_visits ~ ns(relative_day, knots=knots)+
                             (1|box)+(1|breeding_attempt)+(site)+year,family = nbinom2)
  return(day_day_trend)
}

parental_only_model <- function(d){
  
  parental_model = glmmTMB(data = d, prospect_visits~
                             parental_rate+
                            # median_chick+
                             (1|box)+(1|site)+(year)+offset(log(total_duration)),
                           family = nbinom2)
  return(parental_model)
  
}

chick_only_model <- function(d){
  
  chick_model = glmmTMB(data = d, prospect_visits~
                             #parental_rate+
                             median_chick+
                             (1|box)+(1|site)+(year)+offset(log(total_duration)),
                           family = nbinom2)
  return(chick_model)
  
}

chick_parental_model <- function(d){
  parental_model = glmmTMB(data = d, prospect_visits~
                             parental_rate+
                              median_chick+
                             (1|box)+(1|site)+(year)+offset(log(total_duration)),
                           family = nbinom2)
  return(parental_model)
}

box_failure_model <- function(d){
  knots = quantile(d$relative_day,seq(0.1,0.9,by= 0.1))
  FA_model <- glmmTMB(data = d,prospect_visits ~ FA+ ns(relative_day, knots=knots)+
                        (1|box)+(1|breeding_attempt)+(1|site)+year+FA*obs,family = nbinom2)
  return(FA_model)
}

box_failure_phase_model <- function(d){
  knots = quantile(d$relative_day,seq(0.1,0.9,by= 0.1))
  FA_model <- glmmTMB(
    data = d,
    prospect_visits ~ FA + ns(relative_day, knots = knots)+
      (1|box) + (1|breeding_attempt) + (1|site) + year,
    family = nbinom2
  )
  
  return(FA_model)
}


