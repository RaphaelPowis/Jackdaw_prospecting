

failure_pred <- function(model){
 out <-  plot_model(model,type = "pred", terms = c("relative_day [all]","sex"), show.data = F)
 ggsave(out, filename = "figures/bird_failure.pdf")
 return(out)
} 

failure_pred_full_plot <- function(FA_day,FA_model,important_dates){
  
  tukey <-  summary(glht(FA_model, linfct = mcp(FA = "Tukey")))
  tukey2 <- as.data.frame(tukey$test$coefficients)
  groups <- rownames(tukey2)
  pvalues <- as.numeric(tukey$test$pvalues)
  for(i in 1:length(pvalues)){
    if(as.numeric(pvalues[i]) > 0.001){
      pvalues[i] <- paste("p = ",round(as.numeric(pvalues[i]),digits = 3))  
    } else{
      pvalues[i] <- "p < 0.001"
    }
  }
  formated_tukey <- data.frame(p.adj = pvalues, groups = groups)
  formated_tukey <- separate(formated_tukey, col = "groups", into = c("group1", "group2"), sep = " - ")
  formated_tukey$group1 <- factor(formated_tukey$group1, levels = c("Success", "Will fail","Failed"))
  formated_tukey$group2 <- factor(formated_tukey$group2, levels = c("Success", "Will fail","Failed"))
  
  
  important_dates <- read.csv("Data/important_dates.csv")[,-1]
  important_dates$ year = as.factor(important_dates$year)
  FA_day <- FA_day %>%
    left_join(important_dates, by = c("owned_box" = "box","year" = "year"))
  
  FA_day$relative_hatch = as.Date(FA_day$Hatch) - as.Date(FA_day$site_median_hatch)
  FA_day$relative_hatch = as.numeric(FA_day$relative_hatch)
  median_hatching_date <- FA_day %>%
    group_by(owned_box) %>%
    summarize(hatching_date = first(relative_hatch)) %>%
    summarize(median_hatching_date = median(hatching_date, na.rm = T))
  median_hatching_date
  
  FA_day$relative_fledge = as.Date(FA_day$Fledge) - as.Date(FA_day$site_median_hatch)
  FA_day$relative_fledge = as.numeric(FA_day$relative_fledge)
  median_fledgling_date <- FA_day %>%
    group_by(owned_box) %>%
    summarize(fledge_date = first(relative_fledge)) %>%
    summarize(median_fledgling_date = median(fledge_date, na.rm = T))
  median_fledgling_date
  
  FA_day <- FA_day
  
  vlines <- data.frame(xintercept = c(as.numeric(median_hatching_date), as.numeric(median_fledgling_date)), 
                       line_name = c("Median\nhatching", "Median\nfledgling"),levels = c("Median hatching", "Median fledgling"))
  
  trend <- plot_model(FA_model, type = "pred", terms = c("relative_day [all]","sex"), show.data = F) +
    geom_vline(data = vlines, aes(xintercept = xintercept, color = line_name), 
               linetype = "dashed", linewidth = 1, alpha = 0.75) +
    labs(title = "",
         x = "Time relative to median hatching date of the site (day)",
         y = "Prospecting visits") +
    scale_color_manual(
      values = c("Median\nhatching" = "#56B4E9", "Median\nfledgling" = "#E69F00", "M" = "blue", "F" = "red"),
      breaks = c("Median\nhatching", "Median\nfledgling") # Specify the order
    )+
    theme_minimal() + 
    guides(color = guide_legend(override.aes = list(linetype = 2)))+
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.key.size = unit(1.5, "lines"),
      legend.text = element_text(size = 14)
    )+scale_x_continuous(breaks = c(-25,0,25,35,50), labels = c(-25,0,25,"fledg.",50))
  
  trend
  legend_t <- cowplot::get_legend(trend)
  trend <- trend+theme( legend.position = "none")
  
  ###pred####

  FA_pred <- plot_model(FA_model,type = "pred", terms = "FA [all]")+ 
    theme_minimal()+
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 14))+
    ggtitle("")+
    xlab("") + 
    ylab("Prospecting visits / day \n")+
    stat_pvalue_manual( data = formated_tukey,
                        label = "p.adj",
                        step.increase = 0.1,
                        y.position = 0.9,
                        size = 4
    ) +  scale_x_discrete(labels = c("Succesful", "Failed", "Will fail")) 
  FA_pred
  
  FA_data <- ggplot(FA_day, aes(x = FA, y = prospect_visits, fill = FA) )+
    # geom_violin(draw_quantiles = c(0.25,0.5,0.75),show.legend	= F, 
    #            linewidth = 0.7, linetype =2)+
    geom_violin(draw_quantiles = c(0.5),show.legend	= F,
                linewidth = 0.705)+
    #geom_boxplot(outliers  = F, fill = c("#56B4E9", "#E69F00","#CC79A7") ) +
    theme_minimal() +
    theme(text = element_text(size = 16),
          axis.text = element_text(size = 14))+
    ggtitle("") +
    xlab("") + 
    ylab("Prospecting visits / day \n") +
    scale_fill_manual(values = c("grey","grey","grey"))   +  
    scale_x_discrete(labels = c("Succesful", "Failed", "Will fail")) 
  FA_data
  
  
  grid = grid.arrange(FA_pred,trend,nrow = 1,widths = c(1,1.5))
  ggsave(grid, filename = "figures/failure_bird_full.pdf",height = 8, width = 13)
  return(grid)
  
}

box_failure_pred_full <- function(FA_day1,FA_model1,FA_tukey1,FA_day2,FA_model2,FA_tukey2){
  
  tukey <-  summary(glht(FA_model1, linfct = mcp(FA = "Tukey")))
  tukey2 <- as.data.frame(tukey$test$coefficients)
  groups <- rownames(tukey2)
  pvalues <- as.numeric(tukey$test$pvalues)
  for(i in 1:length(pvalues)){
    if(as.numeric(pvalues[i]) > 0.001){
      pvalues[i] <- paste("p = ",round(as.numeric(pvalues[i]),digits = 3))  
    } else{
      pvalues[i] <- "p < 0.001"
    }
  }
  formated_tukey <- data.frame(p.adj =  c(pvalues[[2]],pvalues[[3]],pvalues[[1]]), groups = groups)
  formated_tukey <- separate(formated_tukey, col = "groups", into = c("group1", "group2"), sep = " - ")
  formated_tukey$group1 <- factor(formated_tukey$group1, levels =  c("Success","Failed","Will fail" ))
  formated_tukey$group2 <- factor(formated_tukey$group2, levels =  c("Success","Failed","Will fail" ))
  
  
  #IP for incubation-provisoning. pred for model predictions
  IP_pred <- plot_model(FA_model1,type = "pred", terms = "FA [all]",show.data = F)+ 
    theme_minimal()+
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 15))+
    ggtitle("")+
    xlab("") + 
    ylab("Prospecting visits / day \n")+
    stat_pvalue_manual( data = formated_tukey,
                        label = "p.adj",
                        step.increase = 0.17,
                        y.position = 4.95,
                        size = 4
    ) +  scale_x_discrete(labels = c("Succesful", "Failed", "Will fail"))+
    coord_cartesian(ylim = c(0, 6.5))  
  IP_pred
  
  #PF for post-fledged. pred for model predictions
  pvalues <- as.numeric(FA_tukey2$test$pvalues)
  
  for(i in 1:length(pvalues)){
    if(as.numeric(pvalues[i]) > 0.001){
      pvalues[i] <- paste("p = ",round(as.numeric(pvalues[i]),digits = 3))  
    } else{
      pvalues[i] <- "p < 0.001"
    }
  }
  FA_day2$FA <- droplevels(FA_day2$FA)
  tukey_test_results <- FA_day2 %>% 
    tukey_hsd(prospect_visits ~ FA)
  tukey_test_results$p.adj <-pvalues
  
  
  PF_pred <- plot_model(FA_model2,type = "pred", terms = "FA [all]",show.data = F)+ 
    theme_minimal()+
    theme(text = element_text(size = 15),
          axis.text = element_text(size = 15))+
    ggtitle("")+
    xlab("") + 
    ylab("Prospecting visits / day \n")+
    scale_x_discrete(labels = c("Success", "Failed"))+
    stat_pvalue_manual( data = tukey_test_results,
                        label = "p.adj",
                        step.increase = 0.1,
                        y.position = 18.3,
                        size = 4
    )+ scale_y_continuous(limits = c(3,19))
  
  PF_pred
  
  #general figure
  IP_pred_t <- IP_pred+ggtitle("Pre-fledgling: model predictions")+
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  PF_pred_t <- PF_pred+ggtitle("Post-fledgling: model predictions")+
    theme(plot.title = element_text(hjust = 0.5, size = 10))
  
  IP_pred_t <- IP_pred+ggtitle("(A) Pre-fledgling")+
    theme(plot.title = element_text(hjust = 0.5, size = 13))
  PF_pred_t <- PF_pred+ggtitle("(B) Post-fledgling")+
    theme(plot.title = element_text(hjust = 0.5, size = 13))
  
 out <-  grid.arrange(IP_pred_t,PF_pred_t, nrow = 1)
 ggsave(out, filename = "figures/box_failure_full.pdf",width = 9, height = 6)
 return(out)
  
}


phase_comp_plot <- function(d,tukey){
  #re-ordering for the figure
  d$phase <- factor(d$phase, levels = c("Incubation","Provisioning","Fledge"))
  

  d$phase =relevel(d$phase,"Incubation")
  pvalues <- summary(tukey)$p.value
  for(i in 1:length(pvalues)){
    if(as.numeric(pvalues[i]) > 0.001){
      pvalues[i] <- paste("p = ",round(as.numeric(pvalues[i]),digits = 3))  
    } else{
      pvalues[i] <- "p < 0.001"
    }
  }
  tukey_test_results <- data.frame(
    group1 = c("Incubation", "Incubation", "Provisioning"),
    group2 = c("Provisioning", "Fledge", "Fledge"),
    p.adj = pvalues
  )
  
  ni = nrow(filter(d, phase == "Incubation"))
  np = nrow(filter(d, phase == "Provisioning"))
  nf = nrow(filter(d, phase == "Fledge"))
  
  
  phase_plot <- ggplot(d, aes(x = phase, y = prospect_visits)) +
    geom_boxplot(
      fill = c("#009E73","#56B4E9","#E69F00")) +
    theme_minimal() +
    labs(x = "", y = "Prospecting visits") +
    ggtitle("")+
    stat_pvalue_manual( data = tukey_test_results,
                        label = "p.adj",
                        step.increase = 0.1,
                        y.position = 1250,
                        size = 5
    )+
    scale_x_discrete(labels = c(paste("Incubation\nN = ",ni), 
                                paste("Provisioning\nN = ",np),
                                paste("Post-flegling\nN = ",nf))) +
    theme(legend.position = "bottom",
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)    
    )
 return(phase_plot)
}

phase_aggregated_plot <- function(day_data,day_day_model,phase_plot){

  day_data$relative_hatch = as.Date(day_data$Hatch) - as.Date(day_data$site_median_hatch)
  day_data$relative_hatch = as.numeric(day_data$relative_hatch)
  median_hatching_date <- day_data %>%
    group_by(box) %>%
    summarize(hatching_date = first(relative_hatch)) %>%
    summarize(median_hatching_date = median(hatching_date, na.rm = T))
  median_hatching_date
  
  day_data$relative_fledge = as.Date(day_data$Fledge) - as.Date(day_data$site_median_hatch)
  day_data$relative_fledge = as.numeric(day_data$relative_fledge)
  median_fledgling_date <- day_data %>%
    group_by(box) %>%
    summarize(fledge_date = first(relative_fledge)) %>%
    summarize(median_fledgling_date = median(fledge_date, na.rm = T))
  median_fledgling_date
  
  
  
  vlines <- data.frame(xintercept = c(as.numeric(median_hatching_date), as.numeric(median_fledgling_date)), 
                       line_name = c("Median\nhatching", "Median\nfledgling"),levels = c("Median hatching", "Median fledgling"))
  
  trend <- plot_model(day_day_model, type = "pred", terms = c("relative_day [all]"), show.data = F, color = "black") +
    geom_vline(data = vlines, aes(xintercept = xintercept, color = line_name), 
               linetype = "dashed", linewidth = 1, alpha = 0.75) +
    labs(title = "",
         x = "Time relative to median hatching date of the site (day)",
         y = "Prospecting visits",
         color = "") +
    scale_color_manual(
      values = c("Median\nhatching" = "#56B4E9", "Median\nfledgling" = "#E69F00"),
      breaks = c("Median\nhatching", "Median\nfledgling") # Specify the order
    )+
    theme_minimal() + 
    guides(color = guide_legend(override.aes = list(linetype = 2)))+
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.key.size = unit(1.5, "lines"),
      legend.text = element_text(size = 14)
    )+scale_x_continuous(breaks = c(-25,0,25,35,50), labels = c(-25,0,25,"fledg.",50))
  
  trend
  legend_t <- cowplot::get_legend(trend)
  trend <- trend+theme( legend.position = "none")
  
  
  plot_model(day_day_model, type = "pred",terms="relative_day [all]", show.data = T) +
    geom_vline(xintercept = 0,colour =  "#56B4E9" ) + geom_vline(xintercept = 36,colour = "#E69F00" )
  
  
  qplot <- qplot(x = relative_day, y = prospect_visits, data = day_data, color=phase) +
    #geom_smooth() +
    #geom_jitter(width = 0.5, height = 0, size = 1.5) +
    labs(y = "Prospecting visits", x = "Time relative to median hatching date of the site (day)")+
    theme_minimal()+theme(    axis.title = element_text(size = 16),
                              axis.text = element_text(size = 14),
                              legend.text = element_text(size = 14)
                              ,legend.key.size = unit(17, "points"),
                              legend.title = element_text(size = 15)
    ) +
    scale_color_manual(values = c("Incubation" = "#009E73", "Provisioning" = "#56B4E9" , "Fledge" = "#E69F00", "Fail" = "grey50"),
                       breaks = c("Incubation","Provisioning", "Fledge", "Fail" ),
                       name = "Phase")+
    guides(color = guide_legend(override.aes = list(size = 3)))+
    scale_x_continuous(breaks = c(-25,0,25,50), labels = c(-25,0,25,50))
  qplot
  legend_q <- cowplot::get_legend(qplot)
  qplot <- qplot+theme( legend.position = "none")
  
  trend_t <- trend + ggtitle(" (A)")
  qplot_t <- qplot + ggtitle(" (B)")
  phase_plot_t <- phase_plot + ggtitle(" (C)")
  
  #grid = grid.arrange(trend_t,legend_t,qplot_t,legend_q,nrow = 2,widths = c(5,1))
  grid = grid.arrange(trend_t,qplot_t,nrow = 2)
  final <- grid.arrange(grid, phase_plot_t, nrow = 1, widths = c(2,1.3))
  final
ggsave(final, filename = "figures/phenology.pdf",width = 12,height = 8)
  return(final)
  
}

age_plot <- function(d,model,phase){
  
  y_name = paste("Prospecting visits undertaken\nduring the ",phase, "phase")
  if(phase == "general"){
    y_name == "prospecting visits undertaken\nper breeding stage"
  }
  
  age_pred <- plot_model(model, type = "pred", terms = "age",show.data = F,
                         condition = c(functional_days = median(d$functional_days)))+
    labs(title = "A", x = "Age", y =y_name)+
    theme(
      plot.title = element_text(size = 16),       
      axis.title.x = element_text(size = 14),    
      axis.title.y = element_text(size = 14),    
      axis.text = element_text(size = 12)        
    )
  age_pred
  
  data_plot <- ggplot(data = d, aes(x = age, y = prospect_visits)) + 
    geom_jitter(width = 0.3, height = 0, alpha = 0.5, size = 1.7)+
    labs(title = "B", x = "Age", y = y_name)+
    theme(
      plot.title = element_text(size = 16),       
      axis.title.x = element_text(size = 14),    
      axis.title.y = element_text(size = 14),    
      axis.text = element_text(size = 12)        
    )
  
  out <- grid.arrange(age_pred,data_plot,nrow = 2)
  ggsave(out, filename = paste("figures/age_plot_",phase,".pdf"),height = 10, width = 9)
  return(out)
}

sex_plot <- function(d,phase){
  ggplot(d, aes(x = sex, y = prospect_visits)) +
    geom_boxplot(
      fill = c("#009E73","grey","#E69F00")) +
    theme_minimal() +
    labs(x = "", y = paste("Prospecting visits undertaken\nduring the ",phase," phase")) +
    ggtitle("")
}

parental_plot <- function(model){
  parental_plot <- plot_model(model, type = "pred", terms = "parental_rate", show.data = T)+theme_minimal()+
    labs(title = " ", x = "Parental activity (visits/hour)", y = "Prospecting visits received\nduring the provisioning phase")+
    theme(  axis.title.x = element_text(size = 14),    
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 12))
  ggsave(parental_plot, filename = "figures/parental_plot.pdf")
  return(parental_plot)
}

chick_plot <- function(model){
  chicks_plot <-  plot_model(model, type = "pred", terms = "median_chick", show.data = T)+theme_minimal()+
    labs(title = " ", x = "Median number of nestlings", y = "Prospecting visits received\nduring the provisioning phase")+
    theme(  axis.title.x = element_text(size = 14),    
              axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 12) )
  ggsave(chicks_plot, filename = "figures/chicks_plot.pdf")
  return(chicks_plot)
  
}

box_failure_plot <- function(model){
  plot <- plot_model(model,type = "pred", terms = c("relative_day [all]","FA"), show.data =F)
  ggsave(box_failure_plot, filename = "figures/box_failure.pdf")
  return(plot)
}

fitness_plot <- function(model){
plot <- plot_model(model, type = "pred", terms = "total_prospect_rate", show.data = T)+theme_minimal()+
    labs(title = " ", x = "Combined prospecting rate of\nboth parents (visits/hour)", y = "Relative mass of fledglings (g)")+
    theme(  axis.title.x = element_text(size = 14),    
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 12) )
  ggsave(plot, filename = "figures/fitness_plot.pdf")
  return(plot)
}


trade_off_plot <- function(d,model){
  plot <- plot_model(model, type = "pred", terms = "prospect_rate_loggerOK", show.data = T,
                     condition = c(total_logger_duration = median(d$total_logger_duration)))+theme_minimal()+
    labs(title = " ", x = "Prospecting rate (visits/hour)", y = "Parental visits undertaken\nduring the provisioning phase")+
    theme(  axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text = element_text(size = 12) )
  ggsave(plot, filename = "figures/trade_off_plot.pdf", height = 7, width = 7)
  return(plot)
}



