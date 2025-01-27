
#general data####

#import life history of the birds
import_LH <- function(file){
  LH = read.csv(file, sep = ";", dec = ",")
  LH$DATE = as.Date(LH$DATE, format="%d-%m-%Y")
  LH$year = as.numeric(year(LH$DATE))
  return(LH)
}

duration_format <- function(duration){
  duration$first_read <-as.POSIXct(duration$first_read, format = "%Y-%m-%dT%H:%M:%SZ")
  duration$last_read <-as.POSIXct(duration$last_read, format = "%Y-%m-%dT%H:%M:%SZ")
  duration$last_good_read<-  as.POSIXct(duration$last_good_read, format = "%Y-%m-%dT%H:%M:%SZ")
  return(duration)
  
}

broken_tags_format <- function(broken_tag_2023,broken_tag_2022){
  broken_tag_2023$First_broken<- as.Date(broken_tag_2023$First_broken, format = "%d-%m-%y")
  broken_tag_2023$Start_day <- as.Date(broken_tag_2023$Start_day, format = "%d-%m-%y")
  broken_tag_2023$Repaired <- as.Date(broken_tag_2023$Repaired, format = "%d-%m-%y")
  broken_tag_2022$First_broken <- as.Date(broken_tag_2022$First_broken, format = "%Y-%m-%d")
  broken_tag_2022$Start_day<- as.Date(broken_tag_2022$Start_day, format = "%Y-%m-%d")
  broken_tag_2022$Repaired <- as.Date(broken_tag_2022$Repaired, format = "%Y-%m-%d")
  broken_tags <- rbind(broken_tag_2022,broken_tag_2023)
  return(broken_tags)
}

#reformat the duration dataframe in a dataframe with one line per day
duration_day <- function(duration){
  duration <- filter(duration, !is.na(first_read) & !is.na(last_read))
  #create empty dataframe
  duration_per_day = data.frame(box = character(),
                                day = as.Date(character()),
                                first_read = as.POSIXct(character(), tz = "UTC"),
                                last_good_read = as.POSIXct(character(), tz = "UTC"),
                                reliable_duration = numeric())

  #fill the dataframe
  for(i in 1:nrow(duration)){
    start <- duration$first_read[i]
    end_reliable <- duration$last_good_read[i]
    timezone_start <- attr(start, "tzone")
    timezone_end <- attr(end_reliable, "tzone")
    days<- seq(as.Date(start, tz = timezone_start), as.Date(end_reliable, tz = timezone_end), by = "day") #vector of all the days covered by the current line of data
    if(length(days)>1){
      for (a in 1:length(days)){ #for every day covered
        new_reliable_end <- as.POSIXct(paste0(as.Date(days[a], format = "%y-%m-%d") + 1, " 00:00:01"),tz=timezone_end) #end of the current day
        new_start <- as.POSIXct(paste0(as.Date(days[a], format = "%y-%m-%d"), " 00:00:01"),tz=timezone_start) #start of the current day
        if(new_start< start){new_start = start} #start is the start of the duration line (for the whole file). So, if new start is earlier than start, that means that this is the first day of the file and the start of the data collection is "start". If not, then we are at least a day after the start of the file, and the start of the data collection for this day is the start of the day (new_start stays new_start) 
        if(new_reliable_end>end_reliable){new_reliable_end = end_reliable}
        new_reliable_duration = difftime(new_reliable_end,new_start, unit = "hour")
        
        toAdd = data.frame(box = duration$box[i],
                           day = days[a],
                           first_read = new_start,
                           last_good_read = new_reliable_end,
                           reliable_duration = new_reliable_duration)
        duration_per_day = rbind(duration_per_day,toAdd)
      }
    }
    else{
      toAdd = data.frame(box = duration$box[i],
                         day = days[1],
                         first_read = duration$first_read[i],
                         last_good_read = duration$last_good_read[i],
                         reliable_duration = duration$reliable_duration[i])
      duration_per_day = rbind(duration_per_day,toAdd)
    }
    
  }
  
  
  #merge the lines that cover the same day for each nestbox
  duration_per_day <- duration_per_day %>%
    group_by(day, box) %>%
    summarise(reliable_duration = sum(reliable_duration)) %>%
    ungroup()
  
  return(duration_per_day)
  
}



#add dummy visits to ensure that nestboxes that received no visits for a day
#are added as "0 prospecting visits" for that day instead of omitted when grouping the data.
#Those visits do not get counted when creating the data frames used in the models
dummy_visits_box <- function(visits,all_dates,site_periods){
  
  dummies_box = all_dates[,c("box","date","phase","site")]
  dummies_box = filter(dummies_box, !is.na(phase) & (site == "X" | site == "Z" | site == "Y"))
  dummies_box$visitor_type = "D" #D for dummy
  dummies_box$site_phase <- site_periods$phase[match(paste(dummies_box$date,dummies_box$site),
                                                     paste(site_periods$date,site_periods$site))]
  names(dummies_box) <- c("box","date","box_phase","site","visitor_type","site_phase")
  visits_with_dummies = merge(visits, dummies_box , by = c("box", "date","visitor_type","box_phase",
                                                           "site","site_phase"), all = TRUE)
  return(visits_with_dummies)
}

#reformat the broken_tags dataframe in a dataframe with one line per day
broken_tags_by_day <- function(broken_tags){
  broken_tags_day <- data.frame(
    date = as.Date(character()),
    box = character(),
    JID = character()
  )
  
  for(i in 1:nrow(broken_tags)){
    seq <- seq(broken_tags$First_broken[i],broken_tags$Repaired[i], by = 'day')
    for(j in 1:length(seq)){
      to_add <- data.frame(
        date = seq[j],
        box = broken_tags$box[i],
        JID = broken_tags$JID[i]
      )
      broken_tags_day <- bind_rows(broken_tags_day,to_add)
    }
    
  }
  broken_tags_day$broken_tag = T

  
  return(broken_tags_day)
  
}

#group data per day
box_by_day <- function(visits_with_dummies){
  per_box_day <- visits_with_dummies %>% 
    group_by(date,box) %>% 
    summarise(
      prospect_visits = sum(visitor_type == 'P' & reliable_rate),
      parental_visits = sum(visitor_type == 'O' & reliable_rate),
      male_visits = sum(visitor_type == "O" & sex == "M" & reliable_rate),
      female_visits = sum(visitor_type == "O"& sex == "F" & reliable_rate),
      phase = first(box_phase),
      site_phase = first(site_phase),
      n_visitors = sum(n_distinct(JID[visitor_type == "P"])),
      site = first(site)
    ) %>%
    ungroup()
  return(per_box_day)
}


add_info_box_day <- function(d,broken_tags_day,median_hatching,duration_per_day,fledglings,important_dates){
  per_box_day <- d
  #was the bird tag functional?
  per_box_day$broken_tag = broken_tags_day$broken_tag[match(paste(per_box_day$date,per_box_day$box),
                                                            paste(broken_tags_day$date,broken_tags_day$box))]
  per_box_day$broken_tag[is.na(per_box_day$broken_tag)] = F
  #year
  per_box_day$year <- year(per_box_day$date)
  #median hatching date of the site
  per_box_day$site_median_hatch <- median_hatching$date[match(paste(per_box_day$site,per_box_day$year),
                                                              paste(median_hatching$site,median_hatching$year))]
  #hour of data collection of the logger for that day
  per_box_day$duration <- duration_per_day$reliable_duration[match(paste(per_box_day$date,per_box_day$box),
                                                                   paste(duration_per_day$day,duration_per_day$box))]
  #number and mass of fledglings at the nestbox
  fledglings_sub <- fledglings[,-5]
  per_box_day <- per_box_day %>%
    left_join(fledglings_sub, by = c("box" = "BOX","year" = "year"))
  per_box_day[which(is.na(per_box_day$fledgling_mass)),]$fledgling_mass = 0
  #laying, hatching and fledgling date of the nestbox
  per_box_day <- per_box_day %>%
    left_join(important_dates, by = c("box" = "box","year" = "year", "site" = "site"))
  return(per_box_day)
}

#apply data inclusion thresholds
box_day_cleanup <- function(d,day_threshold){
  per_box_day <- d
  per_box_day = filter(per_box_day,!is.na(duration)) #remove the nestboxes with no logger attached
  per_box_day = filter(per_box_day, duration > 24*day_threshold)
  #remove X18, had the lid open since late provisioning
  per_box_day =filter(per_box_day,!(box == "X18" & year == 2023& (phase != "Incubation")))
  return(per_box_day)
}

#divide the number of visits per number of hours of data collection
box_day_rates <- function(d){
  per_box_day <- d
  per_box_day$duration <- as.numeric(per_box_day$duration)
  
  per_box_day$prospect_rate <- per_box_day$prospect_visits/per_box_day$duration
  per_box_day$parental_rate <- per_box_day$parental_visits/per_box_day$duration
  per_box_day$male_rate <- per_box_day$male_visits/per_box_day$duration
  per_box_day$female_rate <- per_box_day$female_visits/per_box_day$duration
  return(per_box_day)
}

#group the data by breeding stage
box_phase <- function(d){
  per_box_day <- d
  per_box_phase <- per_box_day %>% 
    group_by(box,year,phase) %>% 
    summarise(
      prospect_visits = sum(prospect_visits),
      prospect_rate_total = weighted.mean(prospect_rate, duration),
      prospect_rate_tagsOK = weighted.mean(prospect_rate[!broken_tag], duration[!broken_tag], na.rm = TRUE),
      #prospecting rates for the days when the owner tags were working : when comparing the effect of parental activity on prospecting, it is better to use the same days
      parental_visits = sum(parental_visits),
      male_rate = weighted.mean(male_rate[!broken_tag],duration[!broken_tag]),
      female_rate = weighted.mean(female_rate[!broken_tag],duration[!broken_tag]),
      parental_rate = weighted.mean(parental_rate[!broken_tag],duration[!broken_tag]),
      nlines = n(),
      nlines_parental = sum(!broken_tag),
      first_day = first(date),
      last_day = last(date),
      total_duration = sum(duration)

    )
  return(per_box_phase)
}

box_phase_add_info <- function(d,fledglings){
  per_box_phase <- d
  fledglings_sub <- fledglings[,-5]
  per_box_phase <- per_box_phase %>%
    left_join(fledglings_sub, by = c("box" = "BOX","year" = "year"))
  return(per_box_phase)
}

#create a dataframe with the duration of each breeding stage for each nestbox.
#will be used when applaying the data inclusion threshold
phase_duration <- function(d,per_box_phase){
  important_dates <- d
  
  important_dates$prov_dur = NA #provisioning phase duration
  important_dates$PF_dur = NA #post-fledgling
  important_dates$inc_dur = NA #incubation
  for(i in 1:nrow(important_dates)){
    if(!is.na(important_dates$Fledge[i])){
      important_dates$prov_dur[i] = as.Date(important_dates$Fledge[i]) - as.Date(important_dates$Hatch[i])
      #end of the post-fledgling phase 
      last_data_day = filter(per_box_phase,box == important_dates$box[i] & year == year(important_dates$Laid[i]) & phase == "Fledge")$last_day
      last_day = max(as.Date(paste(important_dates$year[i],"-06-30", sep = "")),as.Date(last_data_day))
      important_dates$PF_dur[i] = as.Date(last_day)-as.Date(important_dates$Fledge[i])
      
    }
    else if(!is.na(important_dates$Fail[i]) && !is.na(important_dates$Hatch[i])){
      important_dates$prov_dur[i] = as.Date(important_dates$Fail[i]) - as.Date(important_dates$Hatch[i])
    }
    
    if(!is.na(important_dates$Hatch[i])){
      important_dates$inc_dur[i] = as.Date(important_dates$Hatch[i]) - as.Date(important_dates$Laid[i]) 
    }
    else if (!is.na(important_dates$Laid[i])){
      important_dates$inc_dur[i] = as.Date(important_dates$Fail[i]) - as.Date(important_dates$Laid[i]) 
      
    }
    
  }
  return(important_dates)
  
}

#apply data inclusion thresholds
box_phase_cleanup <- function(d,important_dates,period_threshold){
  per_box_phase <- d
  
  #add durations to dataframe
  per_box_phase <- per_box_phase %>%
    left_join(important_dates, by = c( "box" = "box", "year"))
  per_box_phase$phase_duration <-  NA
  
  #assign the total number of days of the relevant period
  per_box_phase$phase_duration <- ifelse(per_box_phase$phase == "Incubation", per_box_phase$inc_dur, per_box_phase$phase_duration)
  per_box_phase$phase_duration <- ifelse(per_box_phase$phase == "Provisioning", per_box_phase$prov_dur, per_box_phase$phase_duration)
  per_box_phase$phase_duration <- ifelse(per_box_phase$phase == "Fledge", per_box_phase$PF_dur, per_box_phase$phase_duration)
  
  #apply threshold
  per_box_phase$min_duration_required <- per_box_phase$phase_duration*period_threshold
  
  per_box_phase$enough_prospect_data = TRUE
  per_box_phase$enough_parental_data = TRUE
  per_box_phase[which(per_box_phase$nlines < per_box_phase$min_duration_required),]$enough_prospect_data = FALSE
  per_box_phase[which(per_box_phase$nlines_parental < per_box_phase$min_duration_required),]$enough_parental_data = FALSE
  
  return(per_box_phase)
}

#add dummy visits to ensure that birds that did not visit during a day
#are added as "0 prospecting visits" for that day instead of omitted when grouping the data.
#Those visits do not get counted when creating the data frames used in the models
bird_day_dummies <- function(d,owners,LH,all_dates,age){
  visits <- d
  
  visits_with_dummies = visits
  for(current_year in unique(visits$year)){ #by year because ownership of the nestboxes change each year
    for(current_sex in c("F","M")){
      
      dummy = all_dates[,c("box","date","phase","site")]
      dummy = filter(dummy, !is.na(phase) & (site == "X" | site == "Z" | site == "Y"))
      dummy$year = year(as.Date(dummy$date))
      dummy = filter(dummy, year == current_year)
      unisex_owners = filter(owners, sex == current_sex & year == current_year)
      dummy$JID = unisex_owners$JID[match(dummy$box,unisex_owners$box)]
      dummy = filter(dummy, !is.na(phase) & (site == "X" | site == "Z" | site == "Y"))
      dummy$visitor_type = "D" #D for dummy
      names(dummy) = c("visitor_owned_box","date","visitor_phase","site","year","JID","visitor_type")
      dummy$date = as.Date(dummy$date)
      visits_with_dummies$date = as.Date(visits_with_dummies$date)
      visits_with_dummies = merge(visits_with_dummies, dummy , by = c("JID","visitor_owned_box", "date","visitor_type","visitor_phase","site","year"), all = TRUE)
      
      
    }
    
  }
  visits_with_dummies$date = as.Date(visits_with_dummies$date)
  
  Current_year = 2023
  visits_with_dummies$age <- age$min_age[match(visits_with_dummies$JID,age$ID)]
  #The previous line added the 2023 age. Let's adjust the age for the data from 2022.
  visits_with_dummies$age[visits_with_dummies$year != Current_year] <- visits_with_dummies$age[visits_with_dummies$year != Current_year] - 1
  
  LH_noNa <- LH[!is.na(LH$SEX),] # the sex is not specified for every line for every individual. If we don't remove the NAs, the matching below will match NAs 
  LH_noNa <- filter(LH_noNa, SEX != "")
  
  visits_with_dummies$sex <- LH_noNa$SEX[match(visits_with_dummies$JID,LH_noNa$ID)]
  
  visits_with_dummies <- filter(visits_with_dummies, !is.na(JID))#site X 2022
  
  return(visits_with_dummies)
}

#group data by day
bird_day <- function(visits_with_dummies){
  per_bird_day <- visits_with_dummies %>% 
    group_by(JID,date) %>% 
    summarise(
      prospect_visits = sum(visitor_type == "P"), #prospecting does not depend on a specific nestbox and can include all data
      prospect_visits_loggerOK = sum(visitor_type == "P"& reliable_rate),
      parental_visits = sum(visitor_type == "O" & reliable_rate), #parental activity depends on the nestbox, to allow for good comparison, we included on the nestboxes that were perfectly functional
      sex = first(sex,na_rm = T),
      age = first(age,na_rm = T),
      phase = first(visitor_phase),
      year = first(year),
      owned_box = first(visitor_owned_box)
    )%>%
    ungroup()
  return(per_bird_day)
}

#remove the lines for the birds that had a disfunctional tag
#they did not made 0 visits, we don't know how many visits they made
bird_broken_tags <- function(d,broken_tags_day){
  per_bird_day <- d
  per_bird_day$broken_tag <- broken_tags_day$broken_tag[match(paste(per_bird_day$date,per_bird_day$JID),
                                                              paste(broken_tags_day$date,broken_tags_day$JID))]
  per_bird_day <- filter(per_bird_day, is.na(broken_tag))
  per_bird_day <- per_bird_day[,-length(per_bird_day)]
  return(per_bird_day)
}

bird_day_add_info <- function(d,duration_per_day,median_hatching,fledglings){
  per_bird_day <- d
  per_bird_day$site <- stri_sub(per_bird_day$owned_box,1,1) 
  per_bird_day$duration <- duration_per_day$reliable_duration[match(paste(per_bird_day$date,per_bird_day$owned_box),
                                                                    paste(duration_per_day$day,duration_per_day$box))]
  per_bird_day$year <- year(per_bird_day$date)
  per_bird_day$site_median_hatch <- median_hatching$date[match(paste(per_bird_day$site,per_bird_day$year),
                                                               paste(median_hatching$site,median_hatching$year))]
  fledglings_sub <- fledglings[,-5]
  per_bird_day <- per_bird_day %>%
    left_join(fledglings_sub, by = c("owned_box" = "BOX","year" = "year"))
  per_bird_day[which(is.na(per_bird_day$fledgling_mass)),]$fledgling_mass = 0
  
  per_bird_day$breeding_attempt <- paste(per_bird_day$owned_box,per_bird_day$year)
  return(per_bird_day)
}

#apply data inclusion thresholds
bird_day_cleanup <- function(d,day_threshold){
  per_bird_day <- d
  per_bird_day$duration[is.na(per_bird_day$duration)] = 0
  per_bird_day$full_day = T #full day of data collection
  per_bird_day[per_bird_day$duration < 24*day_threshold, ]$full_day = F
  return(per_bird_day)
}

#divide number of visits per hour of data collection
bird_rate <- function(d){
  per_bird_day <- d
  
  per_bird_day$duration = as.numeric(per_bird_day$duration)
  per_bird_day$prospect_rate = per_bird_day$prospect_visits/24 #prospective rate doesn't depend on the duration of the home logger
  per_bird_day$parental_rate = per_bird_day$parental_visits/per_bird_day$duration
  per_bird_day$prospect_rate_loggerOK = per_bird_day$prospect_visits_loggerOK/per_bird_day$duration
  return(per_bird_day)
}

#group by breeding phase
bird_phase <- function(per_bird_day){
  
  per_bird_phase = per_bird_day %>%
    filter(!is.na(owned_box)) %>% 
    group_by(JID, year, phase) %>%
    summarise(prospect_visits = sum(prospect_visits),
              prospect_rate = mean(prospect_rate),
              parental_visits = sum(parental_visits[full_day]),
              parental_rate = weighted.mean(parental_rate[full_day], duration[full_day], na.rm = T), #na.rm = true because there can be NAs if the logger at the nest is off but the bird prospected
              prospect_rate_loggerOK = weighted.mean(prospect_rate_loggerOK[full_day], duration[full_day], na.rm = T),
              prospect_visits_loggerOK = sum(prospect_visits_loggerOK),
              owned_box = first(owned_box),
              sex = first(sex),
              age = first(age),
              nlines = n(),
              nlines_parental = sum(full_day),
              first_day = min(as.Date(date)),
              last_day = max(as.Date(date)),
              breeding_attempt = first(breeding_attempt),
              total_logger_duration = sum(duration[full_day])
              
    ) %>%
    ungroup()
  
  per_bird_phase <- filter(per_bird_phase,!is.na(owned_box))
  return(per_bird_phase)
}


bird_phase_add_info <- function(d,fledglings){
  per_bird_phase <- d
  fledglings_sub <- fledglings[,-5]
  per_bird_phase <- per_bird_phase %>%
    left_join(fledglings_sub, by = c("owned_box" = "BOX","year" = "year"))
  return(per_bird_phase)
}

#apply data inclusion threshold
bird_phase_cleanup <- function(d,important_dates,period_threshold){
  per_bird_phase <- d
  per_bird_phase <- per_bird_phase %>%
    left_join(important_dates, by = c( "owned_box" = "box", "year"))
  per_bird_phase$phase_duration <- NA
  
  per_bird_phase$phase_duration[per_bird_phase$phase == "Incubation"] = per_bird_phase$inc_dur[per_bird_phase$phase == "Incubation"]
  per_bird_phase$phase_duration[per_bird_phase$phase == "Provisioning"] <- per_bird_phase$prov_dur[per_bird_phase$phase == "Provisioning"]
  per_bird_phase$phase_duration[per_bird_phase$phase == "Fledge"] =  per_bird_phase$PF_dur[per_bird_phase$phase == "Fledge"]
  
  #remove the days during which the tag of the bird was broken
  per_bird_phase$min_duration_required = per_bird_phase$phase_duration*period_threshold
  
  per_bird_phase$enough_prospect_data = T
  per_bird_phase$enough_parental_data = T
  per_bird_phase$nlines_parental[is.na(per_bird_phase$nlines_parental)] = 0
  per_bird_phase[which(per_bird_phase$nlines < per_bird_phase$min_duration_required),]$enough_prospect_data = F
  per_bird_phase[which(per_bird_phase$nlines_parental < per_bird_phase$min_duration_required),]$enough_parental_data = F

  return(per_bird_phase)
  
  }

#add dummy visits to ensure that birds that did not visit during breeding phase
#are added as "0 prospecting visits" instead of omitted when grouping the data.
#Those visits do not get counted when creating the data frames used in the models
all_birds_dummies <- function(visits,age,LH){
  years <- unique(visits$year)
  dummy_all <- data.frame(
    JID = character(),
    site_phase = character(),
    year = numeric(),
    visitor_type = character(),
    site = character()
  )
  for(y in years){
    visity <- filter(visits, year == y)
    JIDs <- unique(visity$JID)
    phases <-c("Pre-Lay","Incubation","Provisioning","Fledge")
    to_add <- expand.grid(JID = JIDs,site_phase = phases)
    to_add$year <- y
    to_add$visitor_type <- 'D'
    to_add$site <- "NA"
    dummy_all <- bind_rows(dummy_all, to_add)
    
  }
  visits_with_dummies = merge(visits, dummy_all , by = c("JID","visitor_type","year","site_phase","site"), all = TRUE)
  
  Current_year = 2023
  visits_with_dummies$age <- age$min_age[match(visits_with_dummies$JID,age$ID)]
  #The previous line added the 2023 age. Let's adjust the age for the data from 2022.
  visits_with_dummies$age[visits_with_dummies$year != Current_year] <- visits_with_dummies$age[visits_with_dummies$year != Current_year] - 1
  
  LH_noNa <- LH[!is.na(LH$SEX),] # the sex is not specified for every line for every individual. If we don't remove the NAs, the matching below will match NAs 
  LH_noNa <- filter(LH_noNa, SEX != "")
  visits_with_dummies$sex <- LH_noNa$SEX[match(visits_with_dummies$JID,LH_noNa$ID)]
  visits_with_dummies$date <- as.Date(visits_with_dummies$date)
  return(visits_with_dummies)
}

#group by breeding stage
all_bird_phase <- function(visits_with_dummies){
  
  all_bird_phase <- visits_with_dummies %>%
    group_by(JID, year, site_phase) %>%
    summarise(prospect_visits = sum(visitor_type == "P"),
              owned_box = first(visitor_owned_box, na_rm = T),
              sex = first(sex, na_rm = T),
              age = first(age, na_rm = T),
              ndays_prospect =  sum(n_distinct(date[visitor_type == "P"])),
              most_visited_site = names(sort(table(site), decreasing = TRUE))[1],
              prop_Z_visits = sum(site == "Z" & visitor_type == "P") / sum(visitor_type == "P"),
              prop_Y_visits = sum(site == "Y" & visitor_type == "P") / sum(visitor_type == "P")
              
    ) %>%
    ungroup()
  return(all_bird_phase)
  
}

#birds that visited two sites needs to be assigned to the site that they visited the most
site_assignation <- function(d,visits){
  all_bird_phase <- d
  all_bird_phase$most_visited_site[all_bird_phase$JID == "J3508"] = "Z" #nestbox owner that visited two sites, assigned to its nesting site
  all_bird_phase$most_visited_site[all_bird_phase$JID == "J1392"] = "Z" #nest location known, assigned to its nesting site
  all_bird_phase$most_visited_site[all_bird_phase$JID == "J4552"] = "Y" #low confidence, but seems to be in Y

  bird_site <- filter(visits,site != "NA") %>%
    group_by(JID, year) %>%
    summarise(most_visited_site = names(sort(table(site), decreasing = TRUE))[1],
              prop_Z_visits = sum(site == "Z" & visitor_type == "P") / sum(visitor_type == "P"),
              prop_Y_visits = sum(site == "Y" & visitor_type == "P") / sum(visitor_type == "P")
              
    ) %>%
    ungroup()
  
  all_bird_phase[all_bird_phase$most_visited_site == "NA",]$most_visited_site = bird_site$most_visited_site[match(paste(all_bird_phase[all_bird_phase$most_visited_site == "NA",]$JID,                                           all_bird_phase[all_bird_phase$most_visited_site == "NA",]$year),
                                                                                                                  paste(bird_site$JID,bird_site$year))]
  return(all_bird_phase)
}

all_birds_add_info <- function(d,site_periods,owners){
  all_bird_phase<-d
  
  all_bird_phase$owned_box <- owners$box[match(paste(all_bird_phase$JID,all_bird_phase$year),
                                               paste(owners$JID,owners$year))]
  
  site_periods$year <- year(site_periods$date)
  site_periods$date <- as.Date(site_periods$date)
  site_duration <- site_periods %>%
    group_by(site,year,phase) %>%
    summarise(start_phase = min(date),
              end_phase = max(date),
              n = n()
              
    ) %>%
    ungroup()
  site_duration$duration = as.Date(site_duration$end_phase)-as.Date(site_duration$start_phase)+1
  
  all_bird_phase <- all_bird_phase %>%
    left_join(site_duration, by = c( "most_visited_site" = "site", "year","site_phase" = "phase"))
  return(all_bird_phase)
}

#add a column with the number of days for which the bird's tag was functional
#will be used as offset in the model
all_birds_broken_tags <- function(d,broken_tags_day,site_periods){
  all_bird_phase <- d
  
  broken_tags_day$year <- year(broken_tags_day$date)
  broken_tags_day$site <- stri_sub(broken_tags_day$box,1,1) 
  broken_tags_day$site_phase <- site_periods$phase[match(paste(broken_tags_day$site,broken_tags_day$date),
                                                         paste(site_periods$site,site_periods$date))]
  broken_tags_phase <- broken_tags_day %>% 
    group_by(JID,site_phase,year) %>% 
    summarise(
      n_broken_days = n()
    ) %>% 
    ungroup()
  
  all_bird_phase$broken_tag <- broken_tags_phase$n_broken_days[match(paste(all_bird_phase$JID,
                                                                           all_bird_phase$site_phase,
                                                                           all_bird_phase$year),
                                                                     paste(broken_tags_phase$JID,
                                                                           broken_tags_phase$site_phase,
                                                                           broken_tags_phase$year))]
  
  all_bird_phase$broken_tag[is.na(all_bird_phase$broken_tag)] = 0
  all_bird_phase$functional_days <- as.numeric(all_bird_phase$duration) -all_bird_phase$broken_tag
  
  return(all_bird_phase)
}

#apply data inclusion thresholds
all_birds_cleanup <- function(d,period_threshold){
  all_bird_phase <- d
  all_bird_phase$enough_data = T
  all_bird_phase$enough_data[all_bird_phase$functional_days < all_bird_phase$duration*period_threshold] = F
  return(all_bird_phase)
}







#specific model data####

#prepararing the data for the model testing indivudual characteristics of prospectors
indiv_charact_data <- function(d){
  individual_data <- d
  ## filters#####
  #keep only the relevant periods
  individual_data <- filter(individual_data,site_phase == "Incubation" | site_phase == "Provisioning" | site_phase == "Fledge")
  #remove fledglings
  individual_data <- filter(individual_data, age>1)
  #apply phase-level threshold
  individual_data <- filter(individual_data, enough_data)
  #for tr0, remove individuals with no data
  individual_data <- filter(individual_data, functional_days > 0)
  
  ##add info####
  individual_data$sex[is.na(individual_data$sex)] <- "NA"
  #breeding attempt
  individual_data$breeding_attempt <- paste(individual_data$owned_box,individual_data$year)
  #add a unique breeding attempt for non-box owners
  individual_data$breeding_attempt[is.na(individual_data$owned_box)] <-
    paste(individual_data$JID[is.na(individual_data$owned_box)],individual_data$year[is.na(individual_data$owned_box)])
  
  ##variable type####
  individual_data$year <- as.factor(individual_data$year)
  individual_data$site <- individual_data$most_visited_site
  individual_data$functional_days <- as.numeric(individual_data$functional_days)
  individual_data$sex <- as.factor(individual_data$sex)
  individual_data$breeding_attempt <- as.factor(individual_data$breeding_attempt)
  
  ##re-level
  individual_data$site <- relevel(as.factor(individual_data$site),"Y")
  individual_data$site_phase <- relevel(as.factor(individual_data$site_phase), "Provisioning")
  individual_data$sex <- factor(individual_data$sex, levels = c("F","NA","M"))
  
  return(individual_data)
}

parental_data <- function(d){
  
  parental_data = d
  
  ##filters#####
  #keep only the relevant periods
  parental_data <- filter(parental_data,phase == "Provisioning")
  #keep only the successful nests
  parental_data <- filter(parental_data,fledglings>0)
  #keep only the breeding attempt with enough data
  parental_data <- filter(parental_data,enough_parental_data)
  #remove X18, which had the lid open since late provisioning
  parental_data <- filter(parental_data,!(owned_box == "X18" & year == 2023))
  
  ##add info####
  parental_data$breeding_attempt <- paste(parental_data$year,parental_data$owned_box)
  
  all_dates<- read.csv("Data/all_dates.csv")[,-1]
  dates_prov <- filter(all_dates, phase == "Provisioning")
  dates_prov$year <- year(dates_prov$date)
  
  mean_chicks <-  dates_prov %>% 
    group_by(year,box) %>% 
    summarise(
      mean_chicks = mean(chicks),
      meadian_chicks = median(chicks)
    )
  parental_data$mean_chick <- mean_chicks$mean_chicks[match(paste(parental_data$owned_box,parental_data$year),
                                                            paste(mean_chicks$box,mean_chicks$year))]
  
##variable type####
  parental_data$site <- as.factor(parental_data$site)
  parental_data$year <- as.factor(parental_data$year)
  parental_data$phase <- as.factor(parental_data$phase)
  parental_data$parental_rate <- as.numeric(parental_data$parental_rate)
  parental_data$fledglings <- as.numeric(parental_data$fledglings)
  parental_data$breeding_attempt <- as.factor (parental_data$breeding_attempt)
  parental_data$site <- relevel(as.factor(parental_data$site),"Y")
  return(parental_data)
}

fitness_data <- function(d){
  
  fitness_pre_transform = filter(d, phase == "Provisioning")
  
  #group by breeding attempt
  
  fitness_per_box <- fitness_pre_transform %>%
    group_by(year, owned_box) %>%
    summarise(fledglings = first(fledglings),
              mass_dev = first(fledgling_massDev),
              fledgling_mass = first(fledgling_mass),
              total_prospect_rate = sum(prospect_rate),
              male_prospect_rate = sum(prospect_rate[sex == "M"]),
              female_prospect_rate =  sum(prospect_rate[sex == "F"]),
              total_prospect_visits = sum(prospect_visits),
              male_prospect_visits = sum(prospect_visits[sex == "M"]),
              female_prospect_visits =  sum(prospect_visits[sex == "F"]),
              site = first(site),
              mean_age = mean(age),
              enough_data = sum(enough_prospect_data),
              lines = sum(nlines),
              min = sum(min_duration_required)) %>%
    ungroup()
  
  #filters####
  #keep only the boxes that had both parents with a working tag
  fitness_per_box <- filter(fitness_per_box,enough_data == 2)
  #keep only the succeful box
  fitness_per_box <- filter(fitness_per_box,fledglings>0)
  
  
  ##as factor####
  fitness_per_box$mass_dev <- as.numeric(fitness_per_box$mass_dev)
  fitness_per_box$site <- as.factor(fitness_per_box$site)
  fitness_per_box$year <- as.factor(fitness_per_box$year)
  fitness_per_box$total_prospect_rate <- as.numeric(fitness_per_box$total_prospect_rate)
  fitness_per_box$mean_age <- as.numeric(fitness_per_box$mean_age)
  fitness_per_box$site <- relevel(as.factor(fitness_per_box$site),"Y")
  
  return(fitness_per_box)
}

bird_failure_data <- function(d){
  FA_day = d
  
  ##filters####
  #keep only the relevant periods
  FA_day =filter(FA_day,phase == "Provisioning" | phase == "Incubation" | phase == "Fail" | phase == "Fledge")
  #keep only the box-owners
  FA_day = filter(FA_day,!is.na(owned_box))
  
  ## add info####
  FA_day$relative_day = as.numeric(as.Date(FA_day$date) - as.Date(FA_day$site_median_hatch))
  FA_day$breeding_attempt = paste(FA_day$owned_box,FA_day$year) 
  #FA: failure_analysis. three levels: failed, success or will fail
  FA_day$already_failed = (FA_day$phase == "Fail")
  FA_day$overall_failed = (FA_day$fledglings == 0)
  FA_day$FA = "Success"
  FA_day$FA[FA_day$already_failed] = "Failed"
  FA_day$FA[FA_day$overall_failed & !FA_day$already_failed] = "Will fail"
  FA_day$FA = relevel(as.factor(FA_day$FA),"Success")
  ##as factor####
  FA_day$FA = as.factor(FA_day$FA)
  FA_day$owned_box = as.factor(FA_day$owned_box)
  FA_day$breeding_attempt = as.factor(FA_day$breeding_attempt)
  FA_day$year = as.factor(FA_day$year)
  FA_day$JID = as.factor(FA_day$JID)
  FA_day$sex = as.factor(FA_day$sex)
  FA_day$site = as.factor(FA_day$site)
  FA_day$site <- relevel(as.factor(FA_day$site),"Y")

  return(FA_day)
  
}

box_phase_comparison <- function(d){
  phase_data = d
  ##filters#####
  #keep only the relevant periods
  phase_data =filter(phase_data,phase == "Incubation" | phase == "Provisioning" | phase == "Fledge")
  #keep only the breeding attempt with enough data
  phase_data = filter(phase_data,enough_prospect_data)
  #keep only the full phases
  phase_data = filter(phase_data,!(phase == "Incubation" & is.na(Hatch)))
  phase_data = filter(phase_data,!(phase == "Provisioning" & fledglings == 0))

  ##as.Factor#####
  phase_data$year = as.factor(phase_data$year)
  phase_data$phase = as.factor(phase_data$phase)
  phase_data$phase = relevel(phase_data$phase,"Provisioning") #set provisioning as the base phase for comparison
  phase_data$breeding_attempt = as.factor(paste(phase_data$box,phase_data$year))
 phase_data$site = relevel(as.factor(phase_data$site),"Y")
  
  return(phase_data)
  
  
}

day_by_day_data <- function(d){
  day_data = d
  
  ###add info#####
  day_data$relative_day = as.Date(day_data$date) - as.Date(day_data$site_median_hatch)
  day_data$relative_day = as.numeric(day_data$relative_day)
  day_data$breeding_attempt = paste(day_data$box,day_data$year)
  
  ##filter#####
  #keep only the relevant periods
  day_data =filter(day_data,phase == "Incubation" | phase == "Provisioning" | phase == "Fledge" |phase == "Fail")
  
  ##as factor####
  day_data$breeding_attempt = as.factor(day_data$breeding_attempt)
  day_data$year = as.factor(day_data$year)
  day_data$phase = as.factor(day_data$phase)
  day_data$phase = relevel(day_data$phase, "Provisioning" )
  day_data$box = as.factor(day_data$box)
  day_data$site = relevel(as.factor(day_data$site),"Y")
  
  return(day_data)
}

parental_box_data <- function(d){
  
  parental_data = d
  
  ##filters#####
  #keep only the relevant period
  parental_data =filter(parental_data,phase == "Provisioning")
  #keep only the successful nests
  parental_data = filter(parental_data,fledglings>0)
  #keep only the breeding attempt with enough data
  parental_data = filter(parental_data,enough_parental_data)
  
  ##add info####
  all_dates<- read.csv("Data/all_dates.csv")[,-1]
  dates_prov <- filter(all_dates, phase == "Provisioning")
  dates_prov$year <- year(dates_prov$date)
  
  mean_chicks <-  dates_prov %>% 
    group_by(year,box) %>% 
    summarise(
      mean_chicks = mean(chicks),
      median_chicks = median(chicks)
    )
  
  parental_data$median_chick <- mean_chicks$median_chicks[match(paste(parental_data$box,parental_data$year),
                                                                paste(mean_chicks$box,mean_chicks$year))]
  
  ##as.Factor#####
  parental_data$year = as.factor(parental_data$year)
  parental_data$phase = as.factor(parental_data$phase)
  parental_data$parental_rate = as.numeric(parental_data$parental_rate)
  parental_data$median_chick <- as.numeric(parental_data$median_chick)
  
  parental_data$site = relevel(as.factor(parental_data$site),"Y")
  
  return(parental_data)
}


box_failure_data <- function(d){
  
  
  FA_day = d
  ##add info####
  FA_day$relative_day = as.numeric(as.Date(FA_day$date) - as.Date(FA_day$site_median_hatch))
  FA_day$breeding_attempt = paste(FA_day$box,FA_day$year) 
  #FA: failure_analysis. three levels: failed, success or will fail
  FA_day$already_failed = (FA_day$phase == "Fail")
  FA_day$overall_failed = (FA_day$fledglings == 0)
  FA_day$FA = "Success"
  FA_day$FA[FA_day$already_failed] = "Failed"
  FA_day$FA[FA_day$overall_failed & !FA_day$already_failed] = "Will fail"
  FA_day$FA = relevel(as.factor(FA_day$FA),"Success")
  #obs: is the failure observable? (i.e. should there be chicks/eggs in the nest)
  FA_day$obs <- T
  FA_day$obs[FA_day$site_phase == "Fledge"] <- F
  ##filters####
  #keep only the relevant periods
  FA_day =filter(FA_day,phase == "Provisioning" | phase == "Incubation" | phase == "Fail" | phase == "Fledge")
  #remove the two days of post-fledge will fail
  FA_day = filter(FA_day, !(FA == "Will fail" & !obs))
  ##as.factor####
  FA_day$FA = as.factor(FA_day$FA)
  FA_day$box = as.factor(FA_day$box)
  FA_day$breeding_attempt = as.factor(FA_day$breeding_attempt)
  FA_day$year = as.factor(FA_day$year)
  FA_day$site_phase = relevel(as.factor(FA_day$site_phase), "Provisioning")
  FA_day$prospect_visits = as.numeric(FA_day$prospect_visits)
  FA_day$obs <- as.factor(FA_day$obs)
  FA_day$site = relevel(as.factor(FA_day$site),"Y")

  
  return(FA_day)
}







