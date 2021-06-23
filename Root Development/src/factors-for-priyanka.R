library(tidyverse)

data = read_csv("output/wide/norfolk-only-digital-neighborhoods-data-wide.csv")

# now filter each so that its only norfolk census tracts.
norfolk_cts = read_csv("output/medicare-analysis/norfolk_only_ho_data_for_analysis.csv")
norfolk_cts = norfolk_cts %>% rename(CensusTract = CENSUSTRACT)

data = data %>% filter(CensusTract %in% norfolk_cts$CensusTract)

# demographics
demographic_data = read_csv("output/medicare-analysis/demographic-data.csv")
demographic_data = demographic_data %>% filter(CensusTract %in% norfolk_cts$CensusTract)
demographic_data = demographic_data %>% mutate(PRCT_AGE_20_24 = AGE_20_24 / Total_Population,
                                               PRCT_AGE_25_35 = AGE_25_35 / Total_Population,
                                               PRCT_AGE_35_44 = AGE_35_44 / Total_Population,
                                               PRCT_AGE_45_54 = AGE_45_54 / Total_Population,
                                               PRCT_AGE_55_59 = AGE_55_59 / Total_Population,
                                               PRCT_AGE_60_64 = AGE_60_64 / Total_Population,
                                               PRCT_AGE_65_74 = AGE_65_74 / Total_Population,
                                               PRCT_AGE_75_84 = AGE_75_84 / Total_Population,
                                               PRCT_AGE_85_years_plus = AGE_85_years_plus / Total_Population,
                                               PRCT_HISPANIC = Hispanic_Population / Total_Population,
                                               PRCT_WHITE = White_Pop / Total_Population,
                                               PRCT_BLACK = Black_Pop / Total_Population)

demographic_data = demographic_data %>% select(CensusTract, Males_To_Female, PRCT_AGE_20_24, PRCT_AGE_25_35, PRCT_AGE_35_44, PRCT_AGE_45_54, 
                                               PRCT_AGE_55_59, PRCT_AGE_60_64, PRCT_AGE_65_74, PRCT_AGE_75_84, PRCT_AGE_85_years_plus, PRCT_HISPANIC, PRCT_WHITE, PRCT_BLACK)

population = data %>% select(DP05_0001E)




# income
income_data = data %>% select(CensusTract, DP03_0062E, DP03_0074E, DP03_0086E, DP03_0128E)
colnames(income_data) = c("CensusTract", "MEDIAN_INCOME_TOTAL_HOUSEHOLDS", "TOTAL_HOUSEHOLDS_WITH_SNAP_FOODSTAMPS", "FAMILIES_MEDIAN_INCOME", "PRCT_INCOME_BELOW_POVERTY_LINE")
income_data = income_data %>% mutate(TOTAL_HOUSEHOLDS_WITH_SNAP_FOODSTAMPS = TOTAL_HOUSEHOLDS_WITH_SNAP_FOODSTAMPS / population$DP05_0001E)


# housing
housing_data = data  %>% select(CensusTract, DP04_0046E, DP04_0047E)
colnames(housing_data) = c("CensusTract","PRCT_OWNER_OCCUPIED", "PRCT_RENTER_OCCUPIED")
housing_data = housing_data %>% mutate(PRCT_OWNER_OCCUPIED = PRCT_OWNER_OCCUPIED / population$DP05_0001E)
housing_data = housing_data %>% mutate(PRCT_RENTER_OCCUPIED = PRCT_RENTER_OCCUPIED / population$DP05_0001E)

# employment
employment_data = data %>% select(CensusTract, DP03_0005E, DP03_0027E, DP03_0028E, DP03_0029E, DP03_0030E, DP03_0031E)
colnames(employment_data) = c("CensusTract","PRCT_UNEMPLOYED", "PRCT_MGMT_BUS_SCI_ART_JOB", "PRCT_SERVICE_JOB", "PRCT_SALES_JOB", "PRCT_CONSTRUCTION_JOB", "PRCT_PROD_TRANS_MOVING_JOB")

employment_data = employment_data %>% mutate(CensusTract, PRCT_UNEMPLOYED = PRCT_UNEMPLOYED / population$DP05_0001E,
                                             PRCT_MGMT_BUS_SCI_ART_JOB = PRCT_MGMT_BUS_SCI_ART_JOB / population$DP05_0001E,
                                             PRCT_SERVICE_JOB = PRCT_SERVICE_JOB / population$DP05_0001E,
                                             PRCT_SALES_JOB = PRCT_SALES_JOB / population$DP05_0001E,
                                             PRCT_CONSTRUCTION_JOB = PRCT_CONSTRUCTION_JOB / population$DP05_0001E,
                                             PRCT_PROD_TRANS_MOVING_JOB = PRCT_PROD_TRANS_MOVING_JOB / population$DP05_0001E)


write_csv(employment_data, "output/norfolk_factor_data/employment_data_for_analysis.csv")
write_csv(demographic_data, "output/norfolk_factor_data/demographic_data_for_analysis.csv")
write_csv(housing_data, "output/norfolk_factor_data/housing_data_for_analysis.csv")
write_csv(income_data, "output/norfolk_factor_data/income_data_for_analysis.csv")
