#importing necessary libraries
import numpy as np
import pandas as pd

#creating a function to return median value for the input health outcome
def h_o(health_outcome, filename):
    df = pd.read_csv(filename)
    if health_outcome == 'diabetes':
        df = df[['census_tract', 'current_state_diabetes', 'median_improvement_diabetes', 'mileage_added']]
        return df
    elif health_outcome == 'high bp':
        df = df[['census_tract', 'current_state_high_bp', 'median_improvement_high_bp', 'mileage_added']]
        return df
    elif health_outcome == 'poor physical health':
        df = df[['census_tract', 'current_state_poor_phys_health', 'median_improvement_poor_phys_health', 'mileage_added']]
        return df
    else:
        return 'Invalid Input'
    
#function that returns max value and min miles added
def min_mileage_for_max_outcome(census_tract, health_outcome, filename):
    df = pd.read_csv(filename)
    #applying function to return the median value for the given health outcome
    df = h_o(health_outcome, filename)
    #filtering only the values corresponding to the input census tract value
    df = df[(df['census_tract'] == census_tract)]
    #returning the max value from the median column
    df = df[df.iloc[:, 2] == df.iloc[:, 2].max()]
    #returning the min miles added for max improvement
    return df.iloc[0, 2], df['mileage_added'].min()

#function that returns min miles added and any improvement value
def min_mileage_any_outcome_improvement(census_tract, health_outcome, filename):
    df = pd.read_csv(filename)
    #applying function to return the median value for the given health outcome
    df = h_o(health_outcome, filename)
    #filtering only the values corresponding to the input census tract value
    df = df[df['census_tract'] == census_tract]
    #returning all values in the median column greater than 0
    df = df[df.iloc[:, 2] > 0]
    #conditional to return min mileage for any improvement
    if df.iloc[:, 2].any() != 0:
        return df.iloc[0, 2], df['mileage_added'].min()
    else:
        return 0, 0.25 
    
#function that returns the improvement values in between the top prct and the bottom prct
def bottom_top_diff_improvement(top_prct, bottom_prct, health_outcome, filename):
    df = pd.read_csv(filename)
    #applying function to return the values corresponding to the given health outcome
    df = h_o(health_outcome, filename)
    #considering the health outcome values in the top range
    k = df.iloc[:, 1].quantile(top_prct)
    #considering the health outcome values in the bottom range
    l = df.iloc[:, 1].quantile(bottom_prct)
    k_1 =df[df.iloc[:, 1] == k]
    l_1 =df[df.iloc[:, 1] == l]
    #max value in the top range
    max1 = k_1.iloc[:, 2].max()
    #max value in the bottom range
    max2 = l_1.iloc[:, 2].max()
    #returning their difference
    return abs(np.round(max1 - max2, 2)) 


