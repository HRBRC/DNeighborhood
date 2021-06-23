library(tidyverse)
library(ggcorrplot)

demo_data = read_csv("output/medicare-analysis/demographic-data.csv")
medicare_opiate_data = read_csv("output/medicare-analysis/medicare-opiate-data.csv")
medicare_opiate_data = medicare_opiate_data %>% rename(CENSUSTRACT = census_tract)
medicare_opiate_data = medicare_opiate_data %>% rename(TOTAL_CLAIM_COUNT = CT_Total_Claim_Count,
                                                       OPIOD_CLAIM_COUNT = CT_Opioid_Claim_Count,
                                                       LONG_ACTING_OPIOD_CLAIM_COUNT = CT_Long_Acting_Opioid_Claims)

medicare_opiate_data = medicare_opiate_data %>% mutate(OPIOD_CLAIM_RATE = OPIOD_CLAIM_COUNT/TOTAL_CLAIM_COUNT)
medicare_opiate_data = medicare_opiate_data %>% mutate(LONG_ACTING_OPIOD_CLAIM_RATE = LONG_ACTING_OPIOD_CLAIM_COUNT/TOTAL_CLAIM_COUNT)

ho_data = read_csv("output/medicare-analysis/ho_data_for_analysis.csv")
ho_data = ho_data %>% rename(CENSUSTRACT = CensusTract)
prevention_data = read_csv("output/medicare-analysis/prevention_data_for_analysis.csv")
prevention_data = prevention_data %>% rename(CENSUSTRACT = CensusTract)
ub_data = read_csv("output/medicare-analysis/ub_data_for_analysis.csv")
ub_data = ub_data %>% rename(CENSUSTRACT = CensusTract)

# select(c(CensusTract, BINGE, CSMOKING, LPA, OBESITY))

ct_names = read_csv("output/medicare-analysis/census_tract_names.csv")
colnames(ct_names) = c("CENSUSTRACT", "CENSUSTRACT_NAME")

agg_demo_data = demo_data
agg_demo_data = agg_demo_data %>% mutate(PRCT_20_24 = (AGE_20_24) / Total_Population)
agg_demo_data = agg_demo_data %>% mutate(PRCT_25_34 = (AGE_25_35) / Total_Population)
agg_demo_data = agg_demo_data %>% mutate(PRCT_35_44 = (AGE_35_44) / Total_Population)
agg_demo_data = agg_demo_data %>% mutate(PRCT_45_54 = (AGE_45_54) / Total_Population)
agg_demo_data = agg_demo_data %>% mutate(PRCT_55_59 = (AGE_55_59) / Total_Population)
agg_demo_data = agg_demo_data %>% mutate(PRCT_60_64 = (AGE_60_64) / Total_Population)

agg_demo_data = agg_demo_data %>% mutate(PRCT_65_74 = (AGE_65_74) / Total_Population)
agg_demo_data = agg_demo_data %>% mutate(PRCT_75_84 = (AGE_75_84) / Total_Population)
agg_demo_data = agg_demo_data %>% mutate(PRCT_85_years_plus = (AGE_85_years_plus) / Total_Population)
agg_demo_data = agg_demo_data %>% mutate(PRCT_45_PLUS = (AGE_45_54 + AGE_55_59 + AGE_60_64 + AGE_65_74 + AGE_75_84 + AGE_85_years_plus) / Total_Population)
agg_demo_data = agg_demo_data %>% mutate(PRCT_45_64 = (AGE_45_54 + AGE_55_59 + AGE_60_64) / Total_Population)

agg_demo_data = agg_demo_data %>% mutate(PRCT_WHITE = (White_Pop / Total_Population))
agg_demo_data = agg_demo_data %>% mutate(PRCT_BLACK = (Black_Pop / Total_Population))
agg_demo_data = agg_demo_data %>% mutate(PRCT_HISPANIC = (Hispanic_Population / Total_Population))
agg_demo_data = agg_demo_data %>% mutate(MALE_TO_FEMALE_RATIO = (Males_To_Female / Total_Population))
# agg_demo_data = agg_demo_data %>% select(CensusTract, PRCT_20_24, PRCT_25_34, PRCT_35_44, PRCT_45_54, PRCT_55_59, PRCT_60_64, 
#                                          PRCT_65_74, PRCT_75_84, PRCT_85_years_plus, MALE_TO_FEMALE_RATIO, PRCT_WHITE, PRCT_BLACK, PRCT_HISPANIC)
agg_demo_data = agg_demo_data %>% rename(CENSUSTRACT = CensusTract)

joined_data = ct_names %>% inner_join(agg_demo_data) %>% inner_join(ho_data) %>% inner_join(prevention_data) %>% inner_join(ub_data) %>% inner_join(medicare_opiate_data)


# corr_plots
demo_corr = ct_names %>% inner_join(agg_demo_data) %>% inner_join(medicare_opiate_data)
demo_corr = demo_corr %>% select(-c(CENSUSTRACT, CENSUSTRACT_NAME))
demo_corr = demo_corr %>% select(PRCT_25_34, PRCT_45_PLUS, MALE_TO_FEMALE_RATIO, PRCT_WHITE, PRCT_BLACK, PRCT_HISPANIC, OPIOD_CLAIM_RATE)
p.mat <- cor_pmat(demo_corr)
corr_obs = "pairwise.complete.obs"
corr_method = "pearson"
demo_corr = demo_corr %>% cor(use=corr_obs, method = corr_method) %>% round(2)
demo_corrplot_opiate_rate = demo_corr %>% ggcorrplot(type="lower", lab = TRUE, p.mat = p.mat, sig.level = .05) + ggtitle("Correlation Among Different Census Tract \nDemographics & Medicare Data Statistics")

demo_corr = ct_names %>% inner_join(agg_demo_data) %>% inner_join(medicare_opiate_data)
demo_corr = demo_corr %>% select(-c(CENSUSTRACT, CENSUSTRACT_NAME))
demo_corr = demo_corr %>% select(PRCT_45_64, PRCT_85_years_plus, TOTAL_CLAIM_COUNT, OPIOD_CLAIM_COUNT)
p.mat <- cor_pmat(demo_corr)
corr_obs = "pairwise.complete.obs"
corr_method = "pearson"
demo_corr = demo_corr %>% cor(use=corr_obs, method = corr_method) %>% round(2)
demo_corrplot_total_claims = demo_corr %>% ggcorrplot(type="lower", lab = TRUE, p.mat = p.mat, sig.level = .05) + ggtitle("Correlation Among Different Census Tract \nDemographics & Medicare Data Statistics")


# corr_plots - ub_data
ub_data_corr = ct_names  %>% inner_join(ub_data) %>% inner_join(medicare_opiate_data)
ub_data_corr = ub_data_corr %>% select(-c(CENSUSTRACT, CENSUSTRACT_NAME))
p.mat <- cor_pmat(ub_data_corr)
corr_obs = "pairwise.complete.obs"
corr_method = "pearson"
ub_data_corr = ub_data_corr %>% cor(use=corr_obs, method=corr_method) %>% round(2)
ub_data_corrplot = ub_data_corr %>% ggcorrplot(type="lower", lab = TRUE, p.mat = p.mat, sig.level = .35) + ggtitle("Correlation Among Different Census Tract Unhealthy Behaviors\n & Medicare Data Statistics")


# corr_plots - prevention_data
prevention_data_corr = ct_names  %>% inner_join(prevention_data) %>% inner_join(medicare_opiate_data)
prevention_data_corr = prevention_data_corr %>% select(-c(CENSUSTRACT, CENSUSTRACT_NAME))
prevention_data_corr = prevention_data_corr %>% select(ACCESS2, TOTAL_CLAIM_COUNT, OPIOD_CLAIM_COUNT, OPIOD_CLAIM_RATE, LONG_ACTING_OPIOD_CLAIM_RATE)
p.mat <- cor_pmat(prevention_data_corr)
corr_obs = "pairwise.complete.obs"
corr_method = "pearson"
prevention_data_corr = prevention_data_corr %>% cor(use=corr_obs, method=corr_method) %>% round(2)
prevention_data_corrplot = prevention_data_corr %>% ggcorrplot(type="lower", lab = TRUE, p.mat = p.mat, sig.level = .35) + ggtitle("Correlation Among Different Census Tract Preventative Measures\n & Medicare Data Statistics")

# corr_plots - prevention_data
ho_data_corr = ct_names  %>% inner_join(ho_data) %>% inner_join(medicare_opiate_data)
ho_data_corr = ho_data_corr %>% select(-c(CENSUSTRACT, CENSUSTRACT_NAME))
ho_data_corr = ho_data_corr %>% select(c(PHLTH, MHLTH, DIABETES, COPD, BPHIGH, TOTAL_CLAIM_COUNT, OPIOD_CLAIM_COUNT, OPIOD_CLAIM_RATE, LONG_ACTING_OPIOD_CLAIM_RATE))
p.mat <- cor_pmat(ho_data_corr)
corr_obs = "pairwise.complete.obs"
corr_method = "pearson"
ho_data_corr = ho_data_corr%>% cor(use=corr_obs, method=corr_method)  %>% round(2)
ho_data_corrplot = ho_data_corr %>% ggcorrplot(type="lower", lab = TRUE, p.mat = p.mat, sig.level = .35) + ggtitle("Correlation Among Different Census Tract Health Outcome\n & Medicare Data Statistics")

