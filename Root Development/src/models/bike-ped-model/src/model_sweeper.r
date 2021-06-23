library(tidyverse)
library(janitor)
source("src/factor_model.r")

# loop over list of census tracts and a common set of mileages to create a .csv with a lot of data in it from the model
# get_improvement_distribution_for_census_tract("51710000100", 1)

df_with_results = NULL
census_tract_vector = unique(as.character(labeled_data$TractFIPS))
common_mileage_vector = seq(from=0.25, to=2, by=0.05)

for (iter_number in 1:length(census_tract_vector))
{
  cur_census_tract = census_tract_vector[iter_number]
  for (j in 1:length(common_mileage_vector))
  {
    cur_mileage = common_mileage_vector[j]
    
    inner_iter_result = get_improvement_distribution_for_census_tract(cur_census_tract, cur_mileage)
    inner_iter_result$CensusTract = cur_census_tract
    inner_iter_result = inner_iter_result %>% as_tibble()
    inner_iter_result = inner_iter_result %>% mutate(ForecastType = ForecastType %>% str_replace_all(" ", "_"))
    inner_iter_result = inner_iter_result %>% mutate(HealthMeasure = HealthMeasure %>% str_replace_all(" ", "_"))
    inner_iter_result = inner_iter_result %>% mutate(HealthMeasure = HealthMeasure %>% str_remove_all("\\."))
    inner_iter_result = inner_iter_result %>% unite( "Measure", c(ForecastType, HealthMeasure), sep="-")
    inner_iter_result = inner_iter_result %>% pivot_wider(names_from = Measure, values_from = PrctofPopulationWithoutHealthIssue)
    inner_iter_result$Mileage_Added = cur_mileage
    if (df_with_results %>% is.null())
    {
      df_with_results = inner_iter_result
    } else {
      df_with_results = df_with_results %>% bind_rows(inner_iter_result)
    }
  }
}

df_with_results = df_with_results %>% janitor::clean_names()

df_with_results = df_with_results %>% mutate(min_improvement_diabetes = min_improvement_diabetes - current_state_diabetes)
df_with_results = df_with_results %>% mutate(median_improvement_diabetes = median_improvement_diabetes - current_state_diabetes)
df_with_results = df_with_results %>% mutate(max_improvement_diabetes = max_improvement_diabetes - current_state_diabetes)

df_with_results = df_with_results %>% mutate(min_improvement_high_bp = min_improvement_high_bp - current_state_high_bp)
df_with_results = df_with_results %>% mutate(median_improvement_high_bp = median_improvement_high_bp - current_state_high_bp)
df_with_results = df_with_results %>% mutate(max_improvement_high_bp = max_improvement_high_bp - current_state_high_bp)

df_with_results = df_with_results %>% mutate(min_improvement_poor_phys_health = min_improvement_poor_phys_health - current_state_poor_phys_health)
df_with_results = df_with_results %>% mutate(median_improvement_poor_phys_health = median_improvement_poor_phys_health - current_state_poor_phys_health)
df_with_results = df_with_results %>% mutate(max_improvement_poor_phys_health = max_improvement_poor_phys_health - current_state_poor_phys_health)

write_csv(df_with_results, path = "output/bike_ped_model_sweep.csv")