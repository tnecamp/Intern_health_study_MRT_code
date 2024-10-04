## step 1, run multiple imputation on data 
  ## this gave me a full complete dataset (no missingness) over the number of imputations

## step 2, estimate treatment effects using multiple imputation data.
  ## that is what we do here

load(file = "imputed data file") ## file contains a list of imputed data that will be itersted through. 
## each imputed data has baseline data (baseline information), full data (original data set), and full data complete (imputed data set)

## lists for storing results
mood_results = list()
mood_agg_results = list()
step_results = list()
step_agg_results = list()
sleep_results = list()
sleep_agg_results = list()

library(geepack)
library(mitml)

num_impute = impute_list$num_impute
for(impute_iter in 1:num_impute){
  cur_list = impute_list[[impute_iter]] ## get current data
  
  all_baseline = cur_list$all_baseline ## baseline data
  full_data = cur_list$full_data ## full trial data
  full_data_complete = cur_list$full_data_complete ## full trial data with imputation
  
  full_data_complete$STEP_COUNT = sqrt(full_data_complete$STEP_COUNT^3)  ##  get square root of raw step count (which was originaly cube root transformed)
  all_baseline[,c(10,13,16)] = sqrt((all_baseline[,c(10,13,16)])^3) ##  get square root of raw step count in baseline (which was originaly cube root transformed)
  
  ## get baseline aggregats to put in model
  baseline_step = apply(all_baseline[,c(10,13,16)], MARGIN = 1, FUN = mean) 
  baseline_sleep = apply(all_baseline[,c(11,14,17)], MARGIN = 1, FUN = mean)
  baseline_mood = apply(all_baseline[,c(12,15,18)], MARGIN = 1, FUN = mean)
  baseline_average = cbind(all_baseline$USERID, baseline_step, baseline_sleep, baseline_mood)
  baseline_average = data.frame(baseline_average)
  names(baseline_average)[1] = 'USERID'
  baseline_average$study_week = 1
  names(baseline_average)[2:4] = c('STEP_COUNTprev', 'SLEEP_COUNTprev', 'MOODprev')
  baseline_average$week_categoryprev = NA
  
  ## data is currently at the daily level. We now aggreagate it at the weekly level becase that was our level of randomization
  aggregate_weekly = aggregate(full_data_complete[, 5:7], by = full_data_complete[,c(1,3,4)], FUN = mean) ## aggregrates step, sleep and mood by study week and user
  aggregate_weekly2 = aggregate_weekly
  
  ## get previous weeks mood, sleep and steps, which will also be used in the model as covariates
  names(aggregate_weekly2)[3:6] = paste(names(aggregate_weekly2)[3:6], "prev", sep = '')
  aggregate_weekly2$study_week = aggregate_weekly2$study_week + 1
  aggregate_weekly2 = rbind(aggregate_weekly2, baseline_average)
  aggregate_weekly_new = merge(aggregate_weekly, aggregate_weekly2, by = c('USERID', 'study_week'), all.x = TRUE)
  
  aggregate_weekly_new1 = merge(aggregate_weekly_new, all_baseline[,1:9], by = 'USERID', all.x = TRUE)
  
  analysis_dat = aggregate_weekly_new1[, -7] ## remove week_category_prev
  
  ## categorize the intervention groups for each week, and set No messages as baseline. Week category is intervention group for 
  ## each individual and each week
  analysis_dat = analysis_dat[analysis_dat$week_category != 'unsure', ]  ## get intervention groups and remove tech errors
  analysis_dat$week_category_new = as.numeric(analysis_dat$week_category != 'None')  
  analysis_dat$week_category = relevel(analysis_dat$week_category, ref = 'None')
  
  ## order and prep for gee
  analysis_dat_gee = analysis_dat[order(analysis_dat$USERID, analysis_dat$study_week), ]
  analysis_dat_gee$week_category= droplevels(analysis_dat_gee$week_category)
  
  ## get results for mood as outcome and previous mood as moderator (exploratory analysis)
  gee_result_mood = geeglm(MOOD ~ week_category + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                             Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category:MOODprev, data = analysis_dat_gee, id = USERID, scale.fix = T)
  mood_results[[impute_iter]] = gee_result_mood
  
  ## get main effects results with mood as outcome and any message as treatment, and mood as result (effect of -.052)
  gee_result_mood_agg = geeglm(MOOD ~ week_category_new + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                                 Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step +  week_category_new:MOODprev, data = analysis_dat_gee, id = USERID, scale.fix = T)
  mood_agg_results[[impute_iter]] = gee_result_mood_agg
  
  ## get results with steps as outcome, step messages as treatment, and previous steps as moderator (-.039)
  gee_result = geeglm(STEP_COUNT ~ week_category + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category:STEP_COUNTprev, data = analysis_dat_gee, id = USERID, scale.fix = T)
  step_results[[impute_iter]] = gee_result
  
  ## get results with sleep as outcoms, sleep messages as treatment, and previous sleep as moderator (-.075)
  gee_result = geeglm(SLEEP_COUNT ~ week_category + STEP_COUNTprev + SLEEP_COUNTprev + MOODprev + Sex + PHQtot0 +
                        Neu0 + depr0 + pre_intern_mood + pre_intern_sleep +  pre_intern_sqrt_step + week_category:SLEEP_COUNTprev, data = analysis_dat_gee, id = USERID, scale.fix = T)
  sleep_results[[impute_iter]] = gee_result
  
  print(impute_iter)
}

## results
testEstimates(mood_agg_results) ## this contains the main results in the paper of moderation effect of -.052
testEstimates(step_results) ## this contains the effect of step messages on step count (-.039)
testEstimates(sleep_results) ## this contains the effect of sleep messages on sleep  (-.075)
testEstimates(mood_results) ## this contains the results of the exploratory subaim
#get 95% confidence intervals
