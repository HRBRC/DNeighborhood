library(tidyverse)
library(zipcodeR)
db = zip_code_db

catchment_analysis = FALSE

not_all_na <- function(x) {!all(is.na(x))}

acs_data = read_csv("output/wide/va-digital-neighborhoods-data-wide.csv")
church_school_data = read_csv("data/church-school-data/churches_public_schools.csv")
needed_cts = c()

church_school_data = church_school_data %>% mutate(is_church = as.logical(is_church))
church_school_data = church_school_data %>% mutate(is_school = as.logical(is_school))

church_school_data = church_school_data %>% mutate(lon = as.numeric(as.character(lon)))
church_school_data = church_school_data %>% mutate(lat = as.numeric(as.character(lat)))

if (catchment_analysis)
{
  zip_codes = church_school_data$zip_code %>% unique()
  
  for (i in 1:length(zip_codes))
  {
    cur_zip_code = as.character(zip_codes[i])
    res = get_tracts(cur_zip_code)
    needed_cts = c(needed_cts, res$GEOID) %>% unique()
  }
  db = db %>% filter(zipcode %in% zip_codes) %>% select(zipcode, population, population_density, housing_units, occupied_housing_units, median_home_value, median_household_income)
} else
{
  needed_cts = church_school_data$census_tract %>% unique()
}




acs_data = acs_data %>% filter(CensusTract %in% needed_cts)

#add landarea to acs
buffer = church_school_data %>% select(census_tract, census_tract_landarea)
buffer = buffer %>% rename(CensusTract = census_tract) %>% distinct()
acs_data = acs_data %>% inner_join(buffer)


# ethnicity_df
ethnicity_colnames = c("ct_id", "pop", "white", "black", "asian", "hisp_pop", "hispanic")
ethnicity_vars = c("CensusTract", "DP05_0033E", "DP05_0037E", "DP05_0038E", "DP05_0044E", "DP05_0070E", "DP05_0071E")
ethnicity_data = acs_data %>% select(ethnicity_vars %>% all_of())
colnames(ethnicity_data) = ethnicity_colnames
ethnicity_data = ethnicity_data %>% mutate(ct_prct_white = white/pop,
                                           ct_prct_black = black/pop,
                                           ct_prct_asian = asian/pop,
                                           ct_prct_hispanic = hispanic/hisp_pop)

ethnicity_data = ethnicity_data %>% select(contains("ct"))

#education_df
education_colnames = c("ct_id", "pop", "less_than_9th", 
                       "ninth_12th", "high_school", "some_college", "associates", 
                       "bachelors", "grad", "high_school_plus", "bachelors_plus")
education_vars = c("CensusTract", "DP02_0059E", "DP02_0060E", "DP02_0061E", "DP02_0062E", "DP02_0063E", "DP02_0064E", "DP02_0065E", "DP02_0066E", "DP02_0067E", "DP02_0068E")
education_data = acs_data %>% select(education_vars %>% all_of())
colnames(education_data) = education_colnames
education_data = education_data %>% mutate(ct_prct_less_than_9th = less_than_9th/pop,
                                           ct_prct_9th_12th = ninth_12th/pop,
                                           ct_prct_high_school = high_school/pop,
                                           ct_prct_some_college = some_college/pop,
                                           ct_prct_associates = associates/pop,
                                           ct_prct_bachelors = bachelors/pop,
                                           ct_prct_grad = grad/pop,
                                           ct_prct_high_school_plus = high_school_plus/pop,
                                           ct_prct_bachelors_plus = bachelors_plus/pop)
education_data = education_data %>% select(contains("ct"))

#age df
age_colnames = c("ct_id", "pop", "age_5_9", "age_10_14", "age_15_19", "age_20_24", "age_25_34",
                 "age_35_44", "age_45_54", "age_55_59", "age_60_64", "age_65_74", "age_75_84", 
                 "age_85_plus")
age_vars = c("CensusTract", "DP05_0001E", "DP05_0005E", "DP05_0006E", "DP05_0007E", "DP05_0008E", 
             "DP05_0009E", "DP05_0010E", "DP05_0011E", "DP05_0012E", "DP05_0013E", "DP05_0014E", 
             "DP05_0015E", "DP05_0016E")
age_data = acs_data %>% select(age_vars %>% all_of())
colnames(age_data) = age_colnames
age_data = age_data %>% mutate(ct_prct_age_5_9 = age_5_9/pop,
                               ct_prct_age_10_14 = age_10_14/pop,
                               ct_prct_age_15_19 = age_15_19/pop,
                               ct_prct_age_20_24 = age_20_24/pop,
                               ct_prct_age_25_34 = age_25_34/pop,
                               ct_prct_age_35_44 = age_35_44/pop,
                               ct_prct_age_45_54 = age_45_54/pop,
                               ct_prct_age_55_59 = age_55_59/pop,
                               ct_prct_age_60_64 = age_60_64/pop,
                               ct_prct_age_65_74 = age_65_74/pop,
                               ct_prct_age_75_84 = age_75_84/pop,
                               ct_prct_age_85_plus = age_85_plus/pop)

age_data = age_data %>% select(contains("ct"))

# income df
income_colnames = c("ct_id", "pop", "inc_less_than_10", "inc_10_15", "inc_15_25", "inc_25_35", "inc_35_50", "inc_50_75", "inc_75_100",
                    "inc_100_150", "inc_150_200", "inc_200_plus", "ct_median_inc")

income_vars = c("CensusTract", "DP03_0051E", "DP03_0052E", "DP03_0053E", "DP03_0054E", "DP03_0055E", "DP03_0056E", "DP03_0057E", 
                "DP03_0058E", "DP03_0059E", "DP03_0060E", "DP03_0061E", "DP03_0062E")

income_data = acs_data %>% select(income_vars %>% all_of())
colnames(income_data) = income_colnames
income_data = income_data %>% mutate(ct_prct_inc_less_than_10 = inc_less_than_10/pop,
                                     ct_prct_inc_10_15 = inc_10_15/pop,
                                     ct_prct_inc_15_25 = inc_15_25/pop,
                                     ct_prct_inc_25_35 = inc_25_35/pop,
                                     ct_prct_inc_35_50 = inc_35_50/pop,
                                     ct_prct_inc_50_75 = inc_50_75/pop,
                                     ct_prct_inc_75_100 = inc_75_100/pop,
                                     ct_prct_inc_100_150 = inc_100_150/pop,
                                     ct_prct_inc_150_200 = inc_150_200/pop,
                                     ct_prct_inc_200_plus = inc_200_plus/pop)

income_data = income_data %>% select(contains("ct"))

# housing_df
housing_colnames = c("ct_id", "pop", "occ_housing_units", "vac_housing_units", "home_owner_vacancy_rate", 
                     "renter_vacancy_rate", "occ_pop", "ho_occ", "renter_occ")

housing_vars = c("CensusTract", "DP04_0001E", "DP04_0002E", "DP04_0003E", "DP04_0004E", 
                 "DP04_0005E", "DP04_0045E", "DP04_0046E", "DP04_0047E")

housing_data = acs_data %>% select(housing_vars %>% all_of())
colnames(housing_data) = housing_colnames
housing_data = housing_data %>% mutate(ct_prct_occupied = occ_housing_units/pop,
                                       ct_prct_vacant = vac_housing_units/pop,
                                       ct_prct_home_owner_vacancy = home_owner_vacancy_rate/100,
                                       ct_prct_renter_vacancy = renter_vacancy_rate/100,
                                       ct_prct_home_owner_occupied = ho_occ/occ_pop,
                                       ct_prct_renter_occupied = renter_occ/occ_pop) 
housing_data = housing_data %>% select(contains("ct"))

# population df
pop_colnames = c("ct_id", "ct_population", "land_area")
pop_vars =c("CensusTract", "DP05_0001E", "census_tract_landarea")
pop_data = acs_data %>% select(pop_vars %>% all_of())
colnames(pop_data) = pop_colnames
pop_data = pop_data %>% mutate(ct_pop_density = ct_population/land_area)
pop_data = pop_data %>% select(contains("ct"))

ct_data = pop_data %>% inner_join(ethnicity_data) %>% inner_join(age_data) %>% inner_join(income_data) %>% inner_join(education_data) %>% inner_join(housing_data)
ct_data = ct_data %>% rename(census_tract = ct_id)

if (catchment_analysis == FALSE)
{
  chuch_school_ct_data_pull = church_school_data %>% inner_join(ct_data)
  
  # maps
  library("ggmap")
  options(ggplot2.continuous.fill="viridis")
  chuch_school_ct_data_map = chuch_school_ct_data_pull %>% select(lon, lat, ct_median_inc, is_school)
  chuch_school_ct_data_map = chuch_school_ct_data_map %>% mutate(is_school = as.factor(is_school))
  chuch_school_ct_data_map$is_school <- factor(chuch_school_ct_data_map$is_school, levels = c("FALSE", "TRUE"), 
                                               labels = c("Churches", "Schools"))
  # use qmplot to make a scatterplot on a map
  
  map_plot = qmplot(lon, lat, data = chuch_school_ct_data_map, zoom = 13)
  # +geom_point(aes(fill=id), colour="black",pch=21, size=5  
  map_plot = map_plot + geom_point(aes(fill=ct_median_inc), colour="black",pch=21, size=8) + facet_wrap(~ is_school, nrow=1) + scale_fill_continuous(name = "Census Tract Household\n Median Income in Dollars", 
                                                                                                        labels = c("$20,000", "$40,000", "$60,000", "$80,000"), 
                                                                                                        breaks = c(20000, 40000, 60000, 80000))
  map_plot = map_plot +theme( strip.text.x = element_text( size = 16, color = "black", face = "bold"),
                              strip.text.y = element_text(size = 16, color = "black", face = "bold"),
                              legend.title = element_text(size = 16, color = "black", face = "bold"),
                              legend.text = element_text(size = 16, color = "black")
  )
  #+ labs(color )
  #+ scale_size_area(breaks = c(20000, 40000, 60000, 80000),
  #                                      labels = c("$20,000", "$40,000", "$60,000", "$80,000"))
}
