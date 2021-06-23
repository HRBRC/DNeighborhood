library(tidyverse)
library(psych)

diabetes_raw_data_col_num = 2
bp_raw_data_col_num = 3
poor_physical_health_raw_data_col_num = 1

to_return_data_value_col_num = 3

mileage_start_value = 0.001
mileage_increment_value = 0.25


raw_data = read.csv("data/faNorfolkData.csv", header=T)
labeled_data = read.csv("data/norfolkDataForScript.csv", header=T)
cor_matrix = cor(raw_data, method="spearman")


factor_analysis = psych::fa(cor_matrix, 3, n.obs = nrow(raw_data))
old_factor_scores = factor.scores(raw_data, factor_analysis)


pretty_bundle_values_from_last_iter = function(cur_diabetes, min_diabetes, med_diabetes, max_diabetes,
                                               cur_bp, min_bp, med_bp, max_bp, 
                                               cur_phsy_health, min_phys_health, med_phys_health, max_phys_health)
{
  to_return = matrix(nrow=12, ncol=3)
  
  to_return[1,] = c("Diabetes", "Current State", cur_diabetes)
  to_return[2,] = c("Diabetes", "Min Improvement", min_diabetes)
  to_return[3,] = c("Diabetes", "Median Improvement", med_diabetes)
  to_return[4,] = c("Diabetes", "Max Improvement", max_diabetes)
  
  to_return[5,] = c("High BP", "Current State", cur_bp)
  to_return[6,] = c("High BP", "Min Improvement", min_bp)
  to_return[7,] = c("High BP", "Median Improvement", med_bp)
  to_return[8,] = c("High BP", "Max Improvement", max_bp)
  
  to_return[9,] = c("Poor Phys. Health", "Current State", cur_phsy_health)
  to_return[10,] = c("Poor Phys. Health", "Min Improvement", min_phys_health)
  to_return[11,] = c("Poor Phys. Health", "Median Improvement", med_phys_health)
  to_return[12,] = c("Poor Phys. Health", "Max Improvement", max_phys_health)
  
  to_return = as.data.frame(to_return)
  colnames(to_return) = c("HealthMeasure", "ForecastType", "PrctofPopulationWithoutHealthIssue")
  forecast_type_levels_in_order = c("Current State", "Min Improvement", "Median Improvement", "Max Improvement")
  to_return$ForecastType = factor(to_return$ForecastType, levels=forecast_type_levels_in_order)
  to_return[,3] = as.numeric(as.character(to_return[,3]))
  return(to_return)
}


bundle_values_to_return_into_df = function(index_of_tract, diabetes_scores, bp_scores, poor_physical_health_scores)
{
  
  to_return = matrix(nrow=12, ncol=3)
  
  to_return[1,] = c("Diabetes", "Current State", raw_data[index_of_tract, diabetes_raw_data_col_num])
  to_return[2,] = c("Diabetes", "Min Improvement", min(diabetes_scores, na.rm=T))
  to_return[3,] = c("Diabetes", "Median Improvement", median(diabetes_scores, na.rm=T))
  to_return[4,] = c("Diabetes", "Max Improvement", max(diabetes_scores, na.rm=T))
  
  to_return[5,] = c("High BP", "Current State", raw_data[index_of_tract, bp_raw_data_col_num])
  to_return[6,] = c("High BP", "Min Improvement", min(bp_scores, na.rm=T))
  to_return[7,] = c("High BP", "Median Improvement", median(bp_scores, na.rm=T))
  to_return[8,] = c("High BP", "Max Improvement", max(bp_scores, na.rm=T))
  
  to_return[9,]  = c("Poor Phys. Health", "Current State", raw_data[index_of_tract, poor_physical_health_raw_data_col_num])
  to_return[10,] = c("Poor Phys. Health", "Min Improvement", min(poor_physical_health_scores, na.rm=T))
  to_return[11,] = c("Poor Phys. Health", "Median Improvement", median(poor_physical_health_scores, na.rm=T))
  to_return[12,] = c("Poor Phys. Health", "Max Improvement", max(poor_physical_health_scores, na.rm=T))
  
  to_return = as.data.frame(to_return)
  colnames(to_return) = c("HealthMeasure", "ForecastType", "PrctofPopulationWithoutHealthIssue")
  forecast_type_levels_in_order = c("Current State", "Min Improvement", "Median Improvement", "Max Improvement")
  to_return$ForecastType = factor(to_return$ForecastType, levels=forecast_type_levels_in_order)
  to_return[,3] = as.numeric(as.character(to_return[,3]))
  
  return(to_return)
}


get_index_of_requested_census_tract = function(census_tract_to_edit)
{
  tract_diffs = abs(as.numeric(as.character(census_tract_to_edit)) - as.numeric(as.character(labeled_data$TractFIPS)))
  index_of_tract = which.min(tract_diffs)
  return(index_of_tract)
}

get_bike_factor_census_tract_comps = function(new_data_factor_scores, index_of_tract)
{
  newBikeScore = new_data_factor_scores$scores[52,3]
  healthBackComp = labeled_data$MR1[index_of_tract]
  higherHealthComps = subset(labeled_data, MR1 > healthBackComp)
  
  bikeDiff = abs(newBikeScore - higherHealthComps$MR2)
  biking_factor_comparables = c(higherHealthComps$MR1[which.min(bikeDiff)])
  bikeDiff[which.min(bikeDiff)] = NA
  biking_factor_comparables = c(biking_factor_comparables, higherHealthComps$MR1[which.min(bikeDiff)])
  bikeDiff[which.min(bikeDiff)] = NA
  biking_factor_comparables = c(biking_factor_comparables, higherHealthComps$MR1[which.min(bikeDiff)])
  bikeDiff[which.min(bikeDiff)] = NA
  biking_factor_comparables = c(biking_factor_comparables, higherHealthComps$MR1[which.min(bikeDiff)])
  bikeDiff[which.min(bikeDiff)] = NA
  biking_factor_comparables = c(biking_factor_comparables, higherHealthComps$MR1[which.min(bikeDiff)])
  
  healthBikeComp2 = new_data_factor_scores$scores[52,3]
  biking_factor_comparables = c(biking_factor_comparables, max(healthBikeComp2, healthBackComp))
}


get_improvement_distribution_for_census_tract = function(census_tract_to_edit, mileage_to_add)
{
  
  # values that go in the df getting initialized
  cur_diabetes = 0
  min_diabetes = 0
  med_diabetes = 0
  max_diabetes = 0
  
  cur_bp = 0
  min_bp = 0
  med_bp = 0
  max_bp = 0
  
  cur_phsy_health = 0
  min_phys_health = 0
  med_phys_health = 0
  max_phys_health = 0
  
  
  i= mileage_start_value
  
  while ( i <= mileage_to_add)
  {
    index_of_tract = get_index_of_requested_census_tract(census_tract_to_edit)
    hypothetical_tract_to_add = raw_data[index_of_tract,]
    
    hypothetical_tract_to_add[1,10] = hypothetical_tract_to_add[1,10] + i
    
    new_data_with_hypothetical_tract = rbind(raw_data, hypothetical_tract_to_add)
    new_data_factor_scores = factor.scores(new_data_with_hypothetical_tract, factor_analysis)
    
    biking_factor_comparables = get_bike_factor_census_tract_comps(new_data_factor_scores, index_of_tract)
    
    index_of_new_max_biking_factor_score = which.min(abs(max(biking_factor_comparables, na.rm=T) - labeled_data$MR1))
    index_of_new_median_biking_factor_score = which.min(abs(median(biking_factor_comparables, na.rm=T) - labeled_data$MR1))
    index_of_new_min_biking_factor_score = which.min(abs(min(biking_factor_comparables, na.rm=T) - labeled_data$MR1))
    
    diabetes_scores = c(max(raw_data[index_of_new_max_biking_factor_score, 2], raw_data[index_of_tract, 2])) #2
    bp_scores = c(max(raw_data[index_of_new_max_biking_factor_score, 3], raw_data[index_of_tract, 3])) #3
    poor_physical_health_scores = c(max(raw_data[index_of_new_max_biking_factor_score, 1], raw_data[index_of_tract, 1])) #1
    
    diabetes_scores = c(diabetes_scores, max(raw_data[index_of_new_median_biking_factor_score, 2], raw_data[index_of_tract, 2]))
    bp_scores = c(bp_scores, max(raw_data[index_of_new_median_biking_factor_score, 3], raw_data[index_of_tract, 3]))
    poor_physical_health_scores = c(poor_physical_health_scores, max(raw_data[index_of_new_median_biking_factor_score, 1], raw_data[index_of_tract, 1]))
    
    diabetes_scores = c(diabetes_scores, max(raw_data[index_of_new_min_biking_factor_score, 2], raw_data[index_of_tract, 2]))
    bp_scores = c(bp_scores, max(raw_data[index_of_new_min_biking_factor_score, 3], raw_data[index_of_tract, 3]))
    poor_physical_health_scores = c(poor_physical_health_scores, max(raw_data[index_of_new_min_biking_factor_score, 1], raw_data[index_of_tract, 1]))
    
    iter_value_df = bundle_values_to_return_into_df(index_of_tract, diabetes_scores, bp_scores, poor_physical_health_scores)
    
    # ensure we didn't go backwards by adding mileage
    this_iter_cur_diabetes = iter_value_df[1,to_return_data_value_col_num]
    this_iter_min_diabetes = iter_value_df[2,to_return_data_value_col_num]
    this_iter_med_diabetes = iter_value_df[3,to_return_data_value_col_num]
    this_iter_max_diabetes = iter_value_df[4,to_return_data_value_col_num]
    
    this_iter_cur_bp = iter_value_df[5,to_return_data_value_col_num]
    this_iter_min_bp = iter_value_df[6,to_return_data_value_col_num]
    this_iter_med_bp = iter_value_df[7,to_return_data_value_col_num]
    this_iter_max_bp = iter_value_df[8,to_return_data_value_col_num]
    
    this_iter_cur_phys_health = iter_value_df[9,to_return_data_value_col_num]
    this_iter_min_phys_health = iter_value_df[10,to_return_data_value_col_num]
    this_iter_med_phys_health = iter_value_df[11,to_return_data_value_col_num]
    this_iter_max_phys_health = iter_value_df[12,to_return_data_value_col_num]
    
    cur_diabetes = this_iter_cur_diabetes
    min_diabetes = max(min_diabetes, this_iter_min_diabetes)
    med_diabetes = max(med_diabetes, this_iter_med_diabetes)
    max_diabetes = max(max_diabetes, this_iter_max_diabetes)
    
    cur_bp = this_iter_cur_bp
    min_bp = max(min_bp, this_iter_min_bp)
    med_bp = max(med_bp, this_iter_med_bp)
    max_bp = max(max_bp, this_iter_max_bp)
    
    cur_phsy_health = this_iter_cur_phys_health
    min_phys_health = max(min_phys_health, this_iter_min_phys_health)
    med_phys_health = max(med_phys_health, this_iter_med_phys_health)
    max_phys_health = max(max_phys_health, this_iter_max_phys_health)
    
    i=i+mileage_increment_value
  }
  
  # when loop ends values these values have our "sanity checked" answers
  final_value_df = pretty_bundle_values_from_last_iter(cur_diabetes, min_diabetes, med_diabetes, max_diabetes,
                                                       cur_bp, min_bp, med_bp, max_bp, 
                                                       cur_phsy_health, min_phys_health, med_phys_health, max_phys_health)
  
  return(final_value_df)
}
