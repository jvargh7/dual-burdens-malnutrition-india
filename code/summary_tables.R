
###1. Ordering columns----
order_cols <- function(names_df){
  dual_index <- match("dual",names_df)
  neither_index <- match("neither",names_df)
  
  order_index <- c(dual_index,c(1:4)[!c(1:4) %in% c(dual_index,neither_index)][1],
                   c(1:4)[!c(1:4) %in% c(dual_index,neither_index)][2],neither_index)
  
  
  return(order_index)
}

###2. Chi-square test----
chisq_df <- function(df_x,df_p=NULL,district=FALSE){
  
  df_x[is.na(df_x)] <- 0
  
  col_index = c(2:5)
  if(district==TRUE) {
    col_index = c(3:6)
  }
  print(col_index)
  
  order_index = order_cols(colnames(df_x)[col_index])
  df_x$chisq_statistic <- NA
  df_x$chisq_p <- NA
  
  
  for (s in c(1:nrow(df_x))){
    
    mat_x <- df_x[s,col_index][order_index]
    mat_x <- as.numeric(mat_x)
    continuity_correction <- match(TRUE,mat_x<5)
    
    if(is.na(continuity_correction)){
      mat_x <- as.numeric(lapply(mat_x, function(x) round(x,0)))
      # mat_p <- df_p[s,2:5][order_index]
      # mat_p <- as.numeric(mat_p)
      
      mat_x <- matrix(mat_x,nrow=2,byrow=TRUE)
      # print(s)
      
      csq <- chisq.test(mat_x)
      df_x[s,]$chisq_statistic <- csq$statistic
      df_x[s,]$chisq_p <- csq$p.value
      
    }
    
    
  }
  
  df_x$chisq_statistic <- round(as.numeric(df_x$chisq_statistic),3)
  df_x$chisq_p <- round(as.numeric(df_x$chisq_p),3)
  return(df_x)
  
}


###3. Plotting graph for dual burden----

plot_dual_graph <- function(path_image,dual_burden,
                            title=NULL, rsq_pos = NULL){
  text_size=13
  state_format_df <- get(paste0("format_by_",dual_burden))
  district_format_df <- get(paste0("dist_format_by_",dual_burden))
  
  state_wtd_obs_noNA_index <- match("d_weighted_observations_noNA",colnames(state_format_df))
  district_wtd_obs_noNA_index <- match("d_weighted_observations_noNA",colnames(district_format_df))
  
  #Burden 1 Prevalence
  burden1_name <- colnames(state_format_df)[state_wtd_obs_noNA_index+1]
  state_format_df[,"burden1"] <- state_format_df[,burden1_name] + state_format_df[,"dual"]
  district_format_df[,"burden1"] <- district_format_df[,burden1_name] + district_format_df[,"dual"]
  
  #Burden 2 Prevalence
  burden2_name <- colnames(state_format_df)[state_wtd_obs_noNA_index+2]
  state_format_df[,"burden2"] <- state_format_df[,burden2_name] + state_format_df[,"dual"]
  district_format_df[,"burden2"] <- district_format_df[,burden2_name] + district_format_df[,"dual"]
  
  ####SCATTER-----
  state_title <- "A. Joint Prevalence, by State (n=36)"
  district_title <- "B. Joint Prevalence, by District (n=640)"
  
  file_name <- paste0(burden1_name," vs ",burden2_name)
  if(is.null(title)){
    state_title <- file_name
    district_title <- file_name
  }
  
  title <- paste0(title,", India NFHS-4")
  
  if(is.null(rsq_pos)| rsq_pos=="tr"){
    mult_factor_x = 0.8
    mult_factor_y = 0.9
  }
  
  if(rsq_pos=="tl"){
    mult_factor_x = 0.2
    mult_factor_y = 0.9
  }
  
  if(rsq_pos=="bl"){
    mult_factor_x = 0.2
    mult_factor_y = 0.1
  }
  
  if(rsq_pos=="br"){
    mult_factor_x = 0.8
    mult_factor_y = 0.1
  }
  
  
  
  state_x_max = round(max(state_format_df[,"burden1"]),-1)
  state_y_max = round(max(state_format_df[,"burden2"]),-1)
  
  # if(state_x_max<100){
  #   state_x_max = state_x_max+5
  # }
  # 
  # if(state_y_max<100){
  #   state_y_max = state_y_max + 5
  # }
  
  state_x_limits <- c(0,state_x_max)
  state_y_limits <- c(0,state_y_max)
  
  
  
  district_x_max = round(max(district_format_df[,"burden1"]),-1)
  district_y_max = round(max(district_format_df[,"burden2"]),-1)
  
  # if(district_x_max<100){
  #   district_x_max = district_x_max+5
  # }
  # 
  # if(district_y_max<100){
  #   district_y_max = district_y_max +5
  # }
  
  district_x_limits <- c(0,district_x_max)
  district_y_limits <- c(0,district_y_max)
  
  
  
  state_model_df <- lm(state_format_df[,"burden2"]~state_format_df[,"burden1"])
  district_model_df <- lm(district_format_df[,"burden2"]~district_format_df[,"burden1"])
  
  state_plot_caption <- paste0("R-squared = ",
                               round(summary(state_model_df)$r.squared,2)
  )
  district_plot_caption <- paste0("R-squared = ",
                                  round(summary(district_model_df)$r.squared,2)
  )
  x_label = paste0(str_to_title(str_replace(burden1_name,"_"," ")),
                   " %")
  x_label = str_replace(x_label,"Women","Woman")
  
  y_label = paste0(str_to_title(str_replace(burden2_name,"_"," ")),
                   " %")
  y_label = str_replace(y_label,"Women","Woman")
  
  ####Plotting
  state_dual_plot <- ggplot(data=state_format_df,aes_string(x="burden1",
                                                            y="burden2")) + theme_bw()
  state_dual_plot <- state_dual_plot + geom_point(size=3) + geom_smooth(method="lm",colour="black")
  
  state_dual_plot <- state_dual_plot  + labs(x=x_label,
                                             y= y_label)
  state_dual_plot <- state_dual_plot + annotate("text",label=state_plot_caption,
                                                x=state_x_max*mult_factor_x,
                                                y=state_y_max*mult_factor_y,
                                                size=5)
  state_dual_plot <- state_dual_plot + theme(axis.title.x = element_text(size=text_size),
                                             axis.title.y = element_text(size=text_size),
                                             plot.title = element_text(size=text_size))
  state_dual_plot <- state_dual_plot + ggtitle(state_title)
  state_dual_plot <- state_dual_plot + expand_limits(x=0,y=0)
  state_dual_plot <- state_dual_plot + scale_x_continuous(expand=c(0,0),limits = state_x_limits) 
  state_dual_plot <- state_dual_plot + scale_y_continuous(expand=c(0,0),limits = state_y_limits)
  
  district_dual_plot <- ggplot(data=district_format_df,aes_string(x="burden1",
                                                                  y="burden2")) + theme_bw()
  district_dual_plot <- district_dual_plot + geom_point(size=1) + geom_smooth(method="lm",colour="black")
  
  district_dual_plot <- district_dual_plot  + labs(x=x_label,
                                                   y= y_label)
  district_dual_plot <- district_dual_plot + annotate("text",label=district_plot_caption,
                                                      x=district_x_max*mult_factor_x,
                                                      y=district_y_max*mult_factor_y,
                                                      size=5)
  
  district_dual_plot <- district_dual_plot + theme(axis.title.x = element_text(size=text_size),
                                                   axis.title.y = element_text(size=text_size),
                                                   plot.title = element_text(size=text_size))
  district_dual_plot <- district_dual_plot + ggtitle(district_title)
  district_dual_plot <- district_dual_plot + expand_limits(x=0,y=0)
  district_dual_plot <- district_dual_plot + scale_x_continuous(expand=c(0,0),limits = district_x_limits) 
  district_dual_plot <- district_dual_plot + scale_y_continuous(expand=c(0,0),limits = district_y_limits)
  
  ####HISTOGRAM-----
  
  
  state_format_df$difference <- with(state_format_df, dual- expected_dual_proportion)
  district_format_df$difference <- with(district_format_df, dual- expected_dual_proportion)
  
  ####Plot Parameters
  state_hist_title <- paste0("C. Difference between observed and expected joint prevalence, by State (n=36)")
  district_hist_title <- paste0("D. Difference between observed and expected joint prevalence, by District (n=640)")
  
  
  hist_x_label <- "Observed Prevalence (%) - Expected Prevalence (%)"
  hist_y_label <- "Density (%)"
  
  state_hist_plot <- ggplot(data=state_format_df,aes(x=difference)) + theme_bw()
  state_hist_plot <- state_hist_plot + geom_histogram(aes(y=100*..count../sum(..count..))) 
  state_hist_plot <- state_hist_plot + xlab(hist_x_label) + ylab(hist_y_label) 
  state_hist_plot <- state_hist_plot + scale_x_continuous(limits=c(-10,10))
  
  state_hist_plot <- state_hist_plot + theme(axis.title.x = element_text(size=text_size),
                                             axis.title.y = element_text(size=text_size),
                                             plot.title = element_text(size=text_size))
  state_hist_plot <- state_hist_plot + ggtitle(state_hist_title)
  
  district_hist_plot <- ggplot(data=district_format_df,aes(x=difference)) + theme_bw()
  district_hist_plot <- district_hist_plot + geom_histogram(aes(y=100*..count../sum(..count..))) 
  district_hist_plot <- district_hist_plot + xlab(hist_x_label) + ylab(hist_y_label) 
  district_hist_plot <- district_hist_plot + scale_x_continuous(limits=c(-10,10))
  
  district_hist_plot <- district_hist_plot + theme(axis.title.x = element_text(size=text_size),
                                                   axis.title.y = element_text(size=text_size),
                                                   plot.title = element_text(size=text_size))
  district_hist_plot <- district_hist_plot + ggtitle(district_hist_title)
  
  
  dual_plot <- grid.arrange(state_dual_plot,district_dual_plot,
                            state_hist_plot,district_hist_plot,
                            ncol=2,
                            top=textGrob(title,gp=gpar(fontsize=text_size+5)))
  print(dual_plot)
  ggsave(paste0(file_name,".png"), plot=dual_plot,device="png",path = path_image,scale=2.2)
  
}



#####4. Formatting into table structure-----

# State (and district, for district-level analyses) identifiers
# Number of individuals (unweighted)
# Burden 1 Only (weighted %)
# Burden 2 Only (weighted %)
# Neither burden (weighted %)
# Both burdens (weighted %)
# Expected both burdens (weighted %)
# Expected both burdens (95% CI)
# Chisq p value for E-O

order_cols_format <- function(names_df){
  dual_index <- match("dual",names_df)
  neither_index <- match("neither",names_df)
  # na_index <- match("NA",names_df)
  
  order_index <- c(c(1:4)[!c(1:4) %in% c(dual_index,neither_index)][1],
                   c(1:4)[!c(1:4) %in% c(dual_index,neither_index)][2],
                   neither_index,dual_index)
  
  
  return(order_index)
}

format_structure <- function(count_df, summary_df,uw_df,district=FALSE){
  count_vars <- c("v024",
                  "expected_dual","difference_observed_expected"
                  # ,"chisq_statistic"
                  # ,"chisq_p"
  )
  
  
  if(district==TRUE){
    count_vars <- c("sdistri",count_vars)
  }
  
  count_index <- match(count_vars,colnames(count_df))
  na_index <- match("NA",colnames(count_df))
  
  if(is.na(na_index)){
    count_df[,"NA"] <- 0 
    na_index <- match("NA",colnames(count_df))
  }
  
  count_df$d_weighted_observations <- rowSums(count_df[,-count_index],na.rm=TRUE)
  
  weighted_observations_index <- match("d_weighted_observations",colnames(count_df))
  
  count_df$d_weighted_observations_noNA <- rowSums(count_df[,-c(na_index,
                                                                weighted_observations_index,
                                                                count_index)],na.rm=TRUE)
  count_df$expected_dual_proportion <- with(count_df,expected_dual/d_weighted_observations_noNA)
  
  format_vars <- summary_df %>% select(v024:expected_dual) %>% colnames()
  format_vars <- format_vars[!format_vars %in% c("v024","sdistri","NA","expected_dual")]
  
  #Retrieve index and then reorder the items
  format_index <- order_cols_format(format_vars)
  format_vars <- format_vars[format_index]
  # print(format_vars)
  
  # summary_vars <- c("v024",format_vars,"chisq_p")
  # if(district==TRUE){
  #   summary_vars <- c("sdistri",summary_vars)
  # }
  # print(summary_vars)
  
  uw_df$d_unweighted_observations_noNA <- rowSums(uw_df[,format_vars],na.rm=TRUE)
  uw_merge_var <- ifelse(district==TRUE,"sdistri","v024")
  # print(uw_merge_var)
  
  id_var <- "v024"
  if(district==TRUE) {
    id_var = c("v024","sdistri")
  }
  
  count_df[,format_vars] <- count_df[,format_vars]/count_df$d_weighted_observations_noNA
  
  
  format_df <- merge(uw_df[,c(uw_merge_var,
                              "d_unweighted_observations_noNA",
                              "chisq_p")],
                     count_df[,c(id_var,
                                 "d_weighted_observations",
                                 "NA",
                                 "d_weighted_observations_noNA",
                                 format_vars,
                                 "expected_dual_proportion"
                                 # ,"chisq_p"
                     )],
                     by=uw_merge_var)
  
  
  ###Normal Approximation CI
  # format_df$u_ci <- with(format_df,expected_dual_proportion +
  #                          2* sqrt(expected_dual_proportion*(1-expected_dual_proportion)/
  #                                    d_weighted_observations))
  # 
  # format_df$l_ci <- with(format_df,expected_dual_proportion -
  #                          2* sqrt(expected_dual_proportion*(1-expected_dual_proportion)/
  #                                    d_weighted_observations))
  
  ###WILSON CI
  format_df$u_ci <- with(format_df,binom.confint(expected_dual_proportion*
                                                   d_unweighted_observations_noNA,
                                                 d_unweighted_observations_noNA,
                                                 methods="wilson")$upper)
  
  format_df$l_ci <- with(format_df,binom.confint(expected_dual_proportion*
                                                   d_unweighted_observations_noNA,
                                                 d_unweighted_observations_noNA,
                                                 methods="wilson")$lower)
  
  format_df$ci <- with(format_df,paste0("(",round(l_ci*100,2),",",round(u_ci*100,2),")"))
  
  format_df$check <- with(format_df,ifelse(dual >=l_ci & 
                                             dual <=u_ci &
                                             chisq_p <0.05,"CHECK",NA))
  
  format_df[,c("u_ci","l_ci")] <- NULL
  
  
  format_df[,format_vars] <- lapply(format_df[,format_vars],
                                    function(x) { round(x,4)*100})
  
  format_df$expected_dual_proportion <- with(format_df, round(expected_dual_proportion,4)*100)
  
  format_df <- format_df[,c(id_var,"d_unweighted_observations_noNA",
                            "d_weighted_observations",
                            "NA",
                            "d_weighted_observations_noNA",
                            format_vars,
                            "expected_dual_proportion","ci",
                            "chisq_p","check")]
  
  
  
  return(format_df)
  
}


###5. Writing separate excel----

write_format_excel <- function(df_name,save_path,date_version){
  df <- get(df_name)
  
  is_district <- ifelse(regexpr("dist_",df_name)>0,TRUE,FALSE)
  
  file_name <- paste0("state_summary_",
                      substr(df_name,regexpr("by_",df_name)+3,
                             str_length(df_name)),
                      date_version)
  
  if(is_district){
    file_name <- paste0("district_summary_",
                        substr(df_name,regexpr("by_",df_name)+3,
                               str_length(df_name)),
                        date_version)
    
  }
  
  
  xlsx::write.xlsx(df,
                   file=paste0(save_path,"/",file_name,".xlsx"),
                   append=TRUE,
                   sheetName=df_name,
                   row.names=FALSE)
  
}


###6. Summary table format-----

percentage_format <- function(x){
  v <- format(round(x, 1), nsmall = 1)
  return(v)
}

###7. Wealth Index Plots----
plot_wealth_graph <- function(df,format_df_name,path_image,only_significant=FALSE,var_name=NULL){
  
  region_format <- ifelse(regexpr("dist_",format_df_name)>0,
                          "dist_format_by_",
                          "format_by_")
  
  grouping_var <- ifelse(regexpr("dist_",format_df_name)>0,"sdistri","v024")
  if(is.null(var_name)){
    var_name <- ifelse(regexpr("_m",format_df_name)>0,
                       str_replace_all(format_df_name,region_format,"d_"),
                       str_replace_all(format_df_name,region_format,"c_d_"))
    
    
    
    
  }
  df$filter_var <- df[,get(var_name)]  
  format_df <- get(format_df_name)
  format_df$difference <- with(format_df, dual-expected_dual_proportion)
  
  
  
  wealth_df <- df[!is.na(df$filter_var),] %>% 
    group_by_(grouping_var) %>% 
    summarize(mean_wi = mean(d_wealth_index_score,na.rm=TRUE),
              median_wi = median(d_wealth_index_score,na.rm=TRUE))
  # View(wealth_df)
  
  wtd_obs_noNA_index <- match("d_weighted_observations_noNA",colnames(format_df))
  
  #Burden 1 Prevalence
  burden1_name <- colnames(format_df)[wtd_obs_noNA_index+1]
  format_df[,"burden1"] <- format_df[,burden1_name] + format_df[,"dual"]
  
  #Burden 2 Prevalence
  burden2_name <- colnames(format_df)[wtd_obs_noNA_index+2]
  format_df[,"burden2"] <- format_df[,burden2_name] + format_df[,"dual"]
  
  if(only_significant==TRUE){
    format_df <- format_df[format_df$chisq_p<0.05 & !is.na(format_df$chisq_p),]
  }
  
  
  plot_df <- merge(wealth_df[,c(grouping_var,"mean_wi","median_wi")],
                   format_df[, c(grouping_var,"burden1","burden2","dual","difference")],
                   by=grouping_var)
  
  plot_title <- str_replace_all(format_df_name,region_format,"")
  file_name <- paste0("wealth_plot_",plot_title," by ",grouping_var)
  
  
  size_points = 1
  if(grouping_var=="v024"){
    size_points=3
  }
  
  wealth_plot <- wealth_lm_scatter(plot_df,plot_title,burden1_name,burden2_name,size_points)
  
  print(wealth_plot)
  ggsave(paste0(file_name,".png"), plot=wealth_plot,device="png",path = path_image,scale=2)
  
  
}

wealth_lm_scatter <- function(plot_df,plot_title,burden1_name,burden2_name,size_points){
  ####Plot Parameters
  plot_title <- str_replace_all(plot_title,"_"," - ")
  
  
  y_burden1_max <- max(plot_df$burden1)
  y_burden2_max <- max(plot_df$burden2)
  y_dual_max <- max(plot_df$dual)
  y_difference_max <- max(plot_df$difference)
  
  x_max <- max(plot_df$mean_wi)
  
  model_df_burden1 <- lm(burden1~mean_wi,data=plot_df)
  plot_caption_burden1 <- paste0("R-squared= ",
                                 round(summary(model_df_burden1)$r.squared,2))
  
  model_df_burden2 <- lm(burden2~mean_wi,data=plot_df)
  plot_caption_burden2 <- paste0("R-squared= ",
                                 round(summary(model_df_burden2)$r.squared,2))
  
  model_df_dual <- lm(dual~mean_wi,data=plot_df)
  plot_caption_dual <- paste0("R-squared= ",
                              round(summary(model_df_dual)$r.squared,2))
  
  model_df_difference <- lm(difference~mean_wi,data=plot_df)
  plot_caption_difference <- paste0("R-squared= ",
                                    round(summary(model_df_difference)$r.squared,2))
  
  
  ####Burden 1
  wealth_plot_burden1 <- ggplot(data=plot_df,aes(x=mean_wi,y=burden1)) + theme_bw()
  wealth_plot_burden1 <- wealth_plot_burden1 + geom_point(size=size_points) + geom_smooth(method="lm",colour="black")
  
  wealth_plot_burden1 <- wealth_plot_burden1  + labs(x="Mean Wealth Index",
                                                     y=paste0(str_to_title(str_replace(burden1_name,"_"," ")),
                                                              " (%)"))
  wealth_plot_burden1 <- wealth_plot_burden1 + annotate("text",size=5,
                                                        x = x_max*0.8,
                                                        y = y_burden1_max*0.9,
                                                        label= plot_caption_burden1)
  wealth_plot_burden1 <- wealth_plot_burden1 + theme(axis.title.x = element_text(size=14),
                                                     axis.title.y = element_text(size=14))
  # wealth_plot_burden1 <- wealth_plot_burden1 + ggtitle(plot_title)
  wealth_plot_burden1 <- wealth_plot_burden1 + expand_limits(x=0,y=0)
  
  #Burden 2
  wealth_plot_burden2 <- ggplot(data=plot_df,aes(x=mean_wi,y=burden2)) + theme_bw()
  wealth_plot_burden2 <- wealth_plot_burden2 + geom_point(size=size_points) + geom_smooth(method="lm",colour="black")
  
  wealth_plot_burden2 <- wealth_plot_burden2  + labs(x="Mean Wealth Index",
                                                     y= paste0(str_to_title(str_replace(burden2_name,"_"," ")),
                                                               " (%)"))
  wealth_plot_burden2 <- wealth_plot_burden2 + annotate("text",size=5,
                                                        x = x_max*0.8,
                                                        y = y_burden2_max*0.9,
                                                        label= plot_caption_burden2)
  wealth_plot_burden2 <- wealth_plot_burden2 + theme(axis.title.x = element_text(size=14),
                                                     axis.title.y = element_text(size=14))
  # wealth_plot_burden2 <- wealth_plot_burden2 + ggtitle(plot_title)
  wealth_plot_burden2 <- wealth_plot_burden2 + expand_limits(x=0,y=0)
  
  #Difference
  wealth_plot_dual <- ggplot(data=plot_df,aes(x=mean_wi,y=dual)) + theme_bw()
  wealth_plot_dual <- wealth_plot_dual + geom_point(size=size_points) + geom_smooth(method="lm",colour="black")
  
  wealth_plot_dual <- wealth_plot_dual  + labs(x="Mean Wealth Index",
                                               y= "Observed Dual Burden Prevalence (%)")
  wealth_plot_dual <- wealth_plot_dual + annotate("text",size=5,
                                                  x = x_max*0.8,
                                                  y = y_dual_max*0.9,
                                                  label= plot_caption_dual)
  wealth_plot_dual <- wealth_plot_dual + theme(axis.title.x = element_text(size=14),
                                               axis.title.y = element_text(size=14))
  # wealth_plot_dual <- wealth_plot_dual + ggtitle(plot_title)
  wealth_plot_dual <- wealth_plot_dual + expand_limits(x=0,y=0)
  
  #Difference
  wealth_plot_difference <- ggplot(data=plot_df,aes(x=mean_wi,y=difference)) + theme_bw()
  wealth_plot_difference <- wealth_plot_difference + geom_point(size=size_points) + geom_smooth(method="lm",colour="black")
  
  wealth_plot_difference <- wealth_plot_difference  + labs(x= "Mean Wealth Index",
                                                           y= "Observed-Expected (%)")
  wealth_plot_difference <- wealth_plot_difference + annotate("text",size=5,
                                                              x = x_max*0.8,
                                                              y = y_difference_max*0.9,
                                                              label= plot_caption_difference)
  
  wealth_plot_difference <- wealth_plot_difference + theme(axis.title.x = element_text(size=14),
                                                           axis.title.y = element_text(size=14))
  # wealth_plot_difference <- wealth_plot_difference + ggtitle(plot_title)
  wealth_plot_difference <- wealth_plot_difference + expand_limits(x=0,y=0)
  
  plot_wealth <- grid.arrange(wealth_plot_burden1,wealth_plot_burden2,
                              wealth_plot_dual,wealth_plot_difference,
                              ncol=2)
  return(plot_wealth)
  
}
