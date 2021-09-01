# data wrangling library in r
library(tidyverse)
library(RcppAlgos)
# a libraries to help with generating the possible combinations of census tracts and bike/ped mile additions
library(gtools)

start_time <- Sys.time()

# read in the data
data = read_csv("../output/bike_ped_model_sweep.csv")

# this function generates all census tract combinations for number_of_cts from our data files
get_all_census_tract_combinations = function(number_of_cts)
{
  
  bigVec = data$census_tract %>% unique()
  bigUni <- unique(bigVec)
  bigRep <- rle(bigVec)$lengths
  to_return =  comboGeneral(bigUni, number_of_cts, freqs =  bigRep)
  return(to_return)
}

# this_function_generates all valid combinations of mileage from our data files
get_all_valid_mileage_addition_combinations = function(total, number_of_cts){
  
  x = data$mileage_added %>% unique()
  x = x %>% rep(number_of_cts)
  bigVec = x %>% rep()
  bigUni <- unique(bigVec)
  bigRep <- rle(bigVec)$lengths
  bigAns <- total
  len <- number_of_cts
  
  bigTest <- comboGeneral(bigUni, len, freqs = bigRep,
                          constraintFun = "sum",
                          comparisonFun = "==",
                          limitConstraints = bigAns)
  return(bigTest)
}

t <- get_all_census_tract_combinations(3)

r <- get_all_valid_mileage_addition_combinations(2, 3)

# given a set of census tracts each with a specified amount of mileage use or data to find the total health improvement
score_a_census_tract_row = function(ct_row, mileage_df, health_outcome)
{
  
  needed_colname =   "median_improvement_high_bp"
  
  # decode the health outcome
  if (health_outcome == "diabetes")
  {
    needed_colname = "median_improvement_diabetes"
  }
  
  if (health_outcome == "poor physical health")
  {
    needed_colname = "median_improvement_poor_phys_health"
  }
  
  # identify the subset of data set need from our larger set
  colname_to_select = c("census_tract", "mileage_added", needed_colname)
  unique_mileage_additions = unique(c(mileage_df))
  working_df = data %>% select(all_of(colname_to_select)) %>% filter(census_tract %in% ct_row) %>% filter(mileage_added %in% unique_mileage_additions)
  
  # set up a vector to store the improvements
  answers = c()
  
  #tibble with all possible combinations
  comb = expand.grid(ct_row, mileage_df)
  
  row_needed = left_join(working_df, comb, by = c("census_tract" = "Var1", "mileage_added" = "Var2"))
  
  this_answer = row_needed[, 3] %>% unlist()
  
  answers = c(answers, this_answer)
  
  # find the best set of mileage additions
  best_mileage_row = which.max(answers)
  
  # it has the best improvement
  best_improvement = max(answers)
  
  # bundle these up and return them
  to_return = NULL
  to_return$ct_mileage_index = best_mileage_row
  to_return$total_improvement = best_improvement
  return(to_return)
}

get_bike_ped_path_porfolio = function(health_outcome, total_miles, number_of_census_tract_entries_in_portfolio)
{
  # use our function to get all combinations for a certain number of census tracts
  unique_ct_combinations = get_all_census_tract_combinations(number_of_census_tract_entries_in_portfolio)
  
  # use our function to get all valid combinations of mileage
  unique_mileage_addition_combinations = get_all_valid_mileage_addition_combinations(total_miles, number_of_census_tract_entries_in_portfolio)
  
  # set up a structure to store the best improvement and index of each set of mileage additions for each census tract combination
  returned_vals = tibble(improvement = double(), ct_mileage_index = integer())
  
  
  # go through all the census tract combinations
  for (i in 1:nrow(unique_ct_combinations))
  {
    cur_row = unique_ct_combinations[i,]
    
    # use our function to get the best improvement from the current row across all the valid milege additions
    x = score_a_census_tract_row(cur_row, unique_mileage_addition_combinations, health_outcome)
    
    # add it into our return structure
    returned_vals = returned_vals %>% add_row(improvement=x$total_improvement, ct_mileage_index=x$ct_mileage_index)
  }
  
  # pretty up and join the output with the actual mileage additions
  unique_ct_combinations = unique_ct_combinations %>% as.data.frame()
  colnames(unique_ct_combinations) = paste0("portfolio_census_tract_", rep(1:number_of_census_tract_entries_in_portfolio))
  unique_ct_combinations = tibble(unique_ct_combinations)
  unique_ct_combinations = unique_ct_combinations %>% bind_cols(returned_vals)
  
  unique_mileage_addition_combinations = unique_mileage_addition_combinations %>% as.data.frame()
  colnames(unique_mileage_addition_combinations) = paste0("portfolio_mile_added_", rep(1:number_of_census_tract_entries_in_portfolio))
  unique_mileage_addition_combinations = tibble(unique_mileage_addition_combinations)
  unique_mileage_addition_combinations = unique_mileage_addition_combinations %>% rowid_to_column("ct_mileage_index")
  
  # the result is a data frame that contains each potential census tract combination for each valid mileage addition combination
  to_return = unique_ct_combinations %>% inner_join(unique_mileage_addition_combinations, by="ct_mileage_index")
  to_return = to_return %>% select(-ct_mileage_index)
  
  #find the best improvement and isolate that row
  max_improvement = max(to_return$improvement)
  to_return = to_return %>% filter(improvement == max_improvement)
  to_return = to_return[1,]
  
  # return the isolated row
  return(to_return)
}

results_for_three_census_tracts_one_mile = get_bike_ped_path_porfolio(health_outcome="diabetes", total_miles=1, number_of_census_tract_entries_in_portfolio=3)
results_for_three_census_tracts_one_mile
end_time <- Sys.time()
time.taken <- end_time - start_time

time.taken