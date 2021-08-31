import pandas as pd
from census_tract_analysis_of_intervention_improvement_api import min_mileage_for_max_outcome, min_mileage_any_outcome_improvement

#specifying the path and the filename
filename = '../output/bike_ped_model_sweep.csv'

df = pd.read_csv(filename)

unique_census_tract = df['census_tract'].unique()

k = []
for c in unique_census_tract:
    r1 = min_mileage_any_outcome_improvement(c, 'diabetes', filename)
    r2 = min_mileage_any_outcome_improvement(c, 'high bp', filename)
    r3 = min_mileage_any_outcome_improvement(c, 'poor physical health', filename)
    k.append(
        {
            'census_tract': c,
            'diabetes_health_improvement': r1[0],
            'diabetes_mileage_increase': r1[1],
            'high_blood_pressure_health_improvement': r2[0],
            'high_blood_pressure_mileage_increase': r2[1],
            'poor_physical_health_health_improvement': r3[0],
            'poor_physical_health_mileage_increase': r3[1],
            'analysis_type': 'mileage_added_for_any_improvement'
        }
    )
    
    
k = pd.DataFrame(k)

#exporting as a csv
k.to_csv('../output/min_mileage_any_outcome_improvement.csv')

l = []
for c in unique_census_tract:
    s1 = min_mileage_for_max_outcome(c, 'diabetes', filename)
    s2 = min_mileage_for_max_outcome(c, 'high bp', filename)
    s3 = min_mileage_for_max_outcome(c, 'poor physical health', filename)
    l.append(
        {
            'census_tract': c,
            'diabetes_health_improvement': s1[0],
            'diabetes_mileage_increase': s1[1],
            'high_blood_pressure_health_improvement': s2[0],
            'high_blood_pressure_mileage_increase': s2[1],
            'poor_physical_health_health_improvement': s3[0],
            'poor_physical_health_mileage_increase': s3[1],
            'analysis_type': 'mileage_added_for_max_improvement'
        }
    )

l = pd.DataFrame(l)

#exporting as a csv
l.to_csv('../output/min_mileage_for_max_outcome.csv')
