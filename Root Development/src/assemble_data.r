library(tidyverse)


# # ACS codes and names
acs_codes_names_demo <- read_csv("data/acs/demographic-characteristics-acs-metadata.csv")
colnames(acs_codes_names_demo) =  c("Measure_Id", "Measure")
acs_codes_names_econ <- read_csv("data/acs/economic-characteristics-acs-metadata.csv")
colnames(acs_codes_names_econ) =  c("Measure_Id", "Measure")
acs_codes_names_hous <- read_csv("data/acs/housing-characteristics-acs-metadata.csv")
colnames(acs_codes_names_hous) =  c("Measure_Id", "Measure")
acs_codes_names_social <- read_csv("data/acs/social-characteristics-acs-metadata.csv")
colnames(acs_codes_names_social) =  c("Measure_Id", "Measure")
acs_codes_names <- bind_rows(acs_codes_names_demo, acs_codes_names_econ, acs_codes_names_hous, acs_codes_names_social)
acs_codes_names = acs_codes_names %>% mutate(Measure = Measure %>% str_remove_all("\\(")) %>% mutate(Measure = Measure %>% str_remove_all("\\)")) %>% mutate(Measure = Measure %>% str_replace_all("-", " "))
acs_codes_names = acs_codes_names %>% select(contains("Measure")) %>% mutate(MeasureType = Measure %>% str_extract("[a-zA-Z0-9 ]+!!")) %>%  mutate(Measure = Measure %>% str_remove("[a-zA-Z0-9 ]+!!"))
acs_codes_names = acs_codes_names %>% mutate(Variable_Family = Measure %>% str_extract("[a-zA-Z0-9 ]+!!") %>% str_remove_all("!!"))
acs_codes_names = acs_codes_names %>% mutate(Measure = Measure %>% str_remove("[a-zA-Z0-9 ]+!!") %>% str_replace_all("!!", ", "))
acs_codes_names = acs_codes_names %>% drop_na()
acs_codes_names = acs_codes_names %>% mutate(MeasureType = MeasureType %>% str_remove_all("!!"))

tidy_acs_vars = function(acs_df)
{
  geo_cols = c("GEO_ID", "NAME")
  to_return = acs_df %>% mutate_at(vars(-geo_cols), as.double)
  to_return = to_return %>% pivot_longer(cols = !geo_cols, names_to = "Measure_Id", values_to = "Data_Value")
  to_return = to_return %>% left_join(acs_codes_names, by=c("Measure_Id")) %>% filter(MeasureType == "Estimate")
  to_return = to_return %>% rename(CensusTract = GEO_ID, CensusTractName = NAME) %>% 
    select(c("CensusTract", "Measure_Id", "Variable_Family", "Measure", "Data_Value")) %>% 
    mutate(CensusTract = CensusTract %>% str_remove_all("1400000US"))
  return(to_return)
}

fill_in_missing_cts = function(data_df, ct_tbl, filter_var)
{
  to_return = data_df %>% filter(Measure_Id == filter_var)
  to_return = ct_tbl %>% left_join(to_return, by="CensusTract")
  to_return = to_return %>% fill(Measure, .direction = "downup") %>% 
    fill(Measure_Id, .direction = "downup") %>% fill(Variable_Family, .direction = "downup")
  return(to_return)
}


# ACS Data
#################################################

# All ACS Data
va_demo_data_long = read_csv("data/acs/demographic-characteristics-acs.csv", na = c("-", "*****", "**"))
va_econ_data_long = read_csv("data/acs/economic-characteristics-acs.csv", na = c("-", "*****", "**"))
va_hous_data_long = read_csv("data/acs/housing-characteristics-acs.csv", na = c("-", "*****", "**"))
va_social_data_long = read_csv("data/acs/social-characteristics-acs.csv", na = c("-", "*****", "**"))
va_acs = va_demo_data_long %>% inner_join(va_econ_data_long, by=c("GEO_ID", "NAME")) %>%
  inner_join(va_hous_data_long, by=c("GEO_ID", "NAME")) %>% inner_join(va_social_data_long, by=c("GEO_ID", "NAME"))

va_acs = va_acs %>% tidy_acs_vars()

# 500 cities data
five_hundred_cities_data = readRDS("data/500-cities/va-specific-500-cities.RDS")
five_hundred_cities_data = five_hundred_cities_data %>% mutate(Esimate_Value = PopulationCount*(Data_Value/100))
five_hundred_cities_data = five_hundred_cities_data %>% mutate(Data_Value = Esimate_Value)
census_tract_populations = five_hundred_cities_data %>% select(c(CensusTract, PopulationCount)) %>% distinct()
census_tract_names = va_demo_data_long %>% select(c(GEO_ID, NAME)) %>% rename(CensusTract = GEO_ID, CensusTractName = NAME) %>% mutate(CensusTract = CensusTract %>% str_remove_all("1400000US"))

va_500_census_tract = five_hundred_cities_data %>% select(-c(PopulationCount, CategoryID, Short_Question_Text)) %>% 
  select(CensusTract, MeasureId, Category, Measure, Data_Value) %>% rename(Variable_Family = Category, Measure_Id = MeasureId)
va_500_census_tract = va_500_census_tract %>% mutate(Data_Value = Data_Value %>% round())


# CCVI Vulnerability Data
va_vulnerability_index = read_csv("data/ccvi/ccvi-va-tract.csv", na="NULL")
va_vulnerability_index = va_vulnerability_index %>% select(FIPS, ccvi) %>% rename(CensusTract = FIPS, Data_Value = ccvi)
va_vulnerability_index = va_vulnerability_index %>% mutate(CensusTract = CensusTract %>% as.character())
va_vulnerability_index = va_vulnerability_index %>% mutate(Measure_Id = "CCVI",
                                                           Variable_Family = "Vulnerability",
                                                           Measure = "COVID-19 Community Vulnerability Index")
va_vulnerability_index = va_vulnerability_index %>% select(c("CensusTract", "Measure_Id", "Variable_Family", "Measure", "Data_Value"))

#Norfolk Bike Pedestrian Path Data
va_bike_ped_path_miles = readRDS("data/bike-ped/norfolk-bike-ped-path-miles-by-census-tract.RDS")
va_bike_ped_path_miles = va_bike_ped_path_miles %>% mutate(CensusTract = CensusTract %>% as.character())
va_bike_ped_path_miles = va_bike_ped_path_miles %>% rename(Measure_Id = MeasureId, Variable_Family = Category)
census_tracts = five_hundred_cities_data %>% select(CensusTract)
va_bike_ped_path_miles = census_tracts %>% left_join(va_bike_ped_path_miles, by="CensusTract")
va_bike_ped_path_miles = va_bike_ped_path_miles %>%  select(c("CensusTract", "Measure_Id", "Variable_Family", "Measure", "Data_Value"))
va_bike_ped_path_miles = va_bike_ped_path_miles %>% mutate(Measure_Id = "BIKE PED MILES", Variable_Family = "Environmental Infrastructure", Measure = "Bike Pedestrian Path Mileage")
va_bike_ped_path_miles = va_bike_ped_path_miles %>% mutate(Data_Value = round(Data_Value, 2))

va_acs = census_tract_names %>% inner_join(va_acs) %>% mutate(DataSet = "ACS-5YR-2015-2019")

va_vulnerability_index = census_tract_names %>% left_join(va_vulnerability_index)  %>% mutate(DataSet = "COVID-19CommunityVulnerabilityIndex-2021") %>% fill(Measure, .direction = "downup") %>% 
  fill(Measure_Id, .direction = "downup") %>% fill(Variable_Family, .direction = "downup")
va_bike_ped_path_miles = census_tract_names %>% left_join(va_bike_ped_path_miles)  %>% mutate(DataSet = "NorfolkBikeAndPedestrianPaths-2017") %>% fill(Measure, .direction = "downup") %>% 
  fill(Measure_Id, .direction = "downup") %>% fill(Variable_Family, .direction = "downup")

unique_va_500_measure_ids = va_500_census_tract$Measure_Id %>% unique()

full_va_500_census_tract = va_500_census_tract %>% fill_in_missing_cts(census_tract_names, unique_va_500_measure_ids[1])
for (i in 2:length(unique_va_500_measure_ids))
{
  temp_df = va_500_census_tract %>% fill_in_missing_cts(census_tract_names, unique_va_500_measure_ids[i])
  full_va_500_census_tract = full_va_500_census_tract %>% bind_rows(temp_df)
}
full_va_500_census_tract = full_va_500_census_tract %>% mutate(DataSet = "CDC500CitiesProject-2017")

# bric stuff
va_bric = readRDS("data/bric/bric-indicies.RDS")
va_bric = va_bric %>% mutate(Hazard_Index = Hazard_Index %>% str_remove_all("%") %>% as.double(),
                             Pop_Vulnerability_Index = Pop_Vulnerability_Index %>% str_remove_all("%") %>% as.double(),
                             MatchingFIPS = FIPS %>% substr(1, 5))

va_bric_ct = census_tract_names %>% mutate(MatchingFIPS = CensusTract %>% substr(1, 5))
va_bric_ct_hazard = va_bric_ct %>% left_join(va_bric, by="MatchingFIPS") %>% select(CensusTract, CensusTractName, Hazard_Index)
va_bric_ct_hazard = va_bric_ct_hazard %>% rename(Data_Value = Hazard_Index) %>% mutate(Measure_Id = "BRIC_HAZARD_INDX",
                                                                                       Variable_Family = "ENVIRONMENTAL HAZARDS",
                                                                                       Measure = "Baseline Resilience Indicators for Communities, Hazard Index")
va_bric_ct_hazard = va_bric_ct_hazard %>%  select(c("CensusTract", "CensusTractName", "Measure_Id", "Variable_Family", "Measure", "Data_Value"))
va_bric_ct_hazard = va_bric_ct_hazard %>% mutate(DataSet = "BaselineResilienceIndicatorsforCommunities2020")

# "Measure_Id", "Variable_Family", "Measure", "Data_Value"
va_bric_ct_pop = va_bric_ct %>% left_join(va_bric, by="MatchingFIPS") %>% select(CensusTract, CensusTractName, Pop_Vulnerability_Index)
va_bric_ct_pop = va_bric_ct_pop %>% rename(Data_Value = Pop_Vulnerability_Index) %>% mutate(Measure_Id = "BRIC_POP_INDX",
                                                                                       Variable_Family = "VULNERABILITY",
                                                                                       Measure = "Baseline Resilience Indicators for Communities,  Population Vulnverability Index")
va_bric_ct_pop = va_bric_ct_pop %>%  select(c("CensusTract", "CensusTractName", "Measure_Id", "Variable_Family", "Measure", "Data_Value"))
va_bric_ct_pop = va_bric_ct_pop %>% mutate(DataSet = "BaselineResilienceIndicatorsforCommunities2020")

va_digital_twin = bind_rows(va_acs, full_va_500_census_tract, va_vulnerability_index, va_bike_ped_path_miles, va_bric_ct_pop, va_bric_ct_hazard) %>% 
  mutate(Variable_Family = Variable_Family %>% toupper() %>% 
           trimws()) %>% filter(CensusTractName != "Virginia")


# data_for_emily_query = va_digital_twin %>% filter(Variable_Family == "PREVENTION" | Variable_Family == "HEALTH INSURANCE COVERAGE") %>% filter(CensusTractName %>% str_detect("Norfolk")) 

va_digital_twin_categories = read_csv("data/category-mapping/mapping-from-variable-family-to-category-and-subcategory.csv")
va_digital_twin = va_digital_twin %>% inner_join(va_digital_twin_categories, by="Variable_Family") %>% mutate(Measure = paste0(Variable_Family, ": ", Measure)) %>% select(-Variable_Family) %>% arrange(Category, Subcategory)
write_csv(va_digital_twin, "output/va-digital-twin.csv")