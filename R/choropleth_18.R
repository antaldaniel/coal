library(eurobarometer)
library(tidyverse)
ZA7488_raw <- haven::read_spss(file.path("not_included", "ZA7488_v1-0-0.sav"))
ZA7488_metadata <- gesis_metadata_create(dat = ZA7488_raw)

ZA7488 <- ZA7488_raw %>%
  purrr::set_names ( as.character(ZA7488_metadata$canonical_name)) %>%
  mutate ( region_nuts_names  = haven::as_factor(region_nuts_codes)) %>%
  mutate ( region_nuts_codes =  as.character(region_nuts_codes)) 

source(file.path("R", "which_coal_region.R"))

# Change to eval=TRUE if you want to run this code
eb18 <- ZA7488 %>%
  select ( one_of("eu_env_policy_statements_more_pub_fin_support_for_clean_energy_even_if_fossil_subsidies_reduced",
                  "region_nuts_names", "region_nuts_codes",
                  "age_exact", "age_education", "type_of_community",
                  "age_education_recoded_5_cat",
                  "country_code_iso_3166", 
                  "weight_result_from_target_redressment"),
           contains("occupation")) %>%
  dplyr::rename ( eu_env_policy  = eu_env_policy_statements_more_pub_fin_support_for_clean_energy_even_if_fossil_subsidies_reduced, 
                  w1 = weight_result_from_target_redressment ) %>%
  mutate ( eu_env_policy = haven::as_factor ( eu_env_policy )) %>%
  mutate_at ( vars(starts_with("type"),
                   contains("recoded"),
                   contains("occupation")), haven::as_factor) %>%
  mutate ( eu_env_policy_numeric = case_when (
    grepl("Totally agree|Tend to agree",
          as.character(eu_env_policy))    ~ 1,
    grepl("disagree", as.character(eu_env_policy)) ~ 0,
    TRUE ~ NA_real_ )
  ) %>%
  mutate ( is_eu_env_policy_totally = case_when (
    grepl("Totally agree",
          as.character(eu_env_policy))    ~ 1,
    grepl("Tend to|disagree", as.character(eu_env_policy)) ~ 0,
    TRUE ~ NA_real_ )
  ) %>%
  mutate ( total_agreement_weighted = w1*is_eu_env_policy_totally) %>%
  mutate ( age_education = recode_age_education(var = age_education,
                                                age_exact = age_exact )
  ) %>%
  mutate  ( is_rural = case_when (
    grepl ( "rural", tolower(as.character(type_of_community))) ~ 1,
    grepl ( "town", tolower(as.character(type_of_community)))  ~ 0,
    tolower(as.character(type_of_community)) == "dk" ~ NA_real_,
    TRUE ~ NA_real_)
  ) %>%
  mutate  ( is_student = case_when (
    grepl ( "studying", tolower(as.character(age_education_recoded_5_cat))) ~ 1,
    grepl ( "refuse", tolower(as.character(type_of_community)))  ~ NA_real_,
    TRUE ~ 0)
  ) %>%
  mutate ( year_survey = 2018 ) %>%
  mutate ( coal_region = which_coal_region(region_nuts_codes)) %>%
  mutate ( is_coal_region = ifelse (is.na(coal_region), 0, 1))

saveRDS(eb18,
        file.path("data", "eb18.rds"), 
        version = 2) # backward compatiblity

to_ch <- eb18 %>%
  select ( region_nuts_codes, total_agreement_weighted  ) %>%
  rename ( geo = region_nuts_codes ) %>%
  group_by ( geo ) %>%
  summarize ( values   = mean(total_agreement_weighted, na.rm=TRUE) ) %>%
  ungroup ( ) %>%
  harmonize_geo_code() %>%
  recode_to_nuts_2016() 

dat <- to_ch
%>%
  regdata::impute_nuts_1_values(dat = .)
  satellitereport::create_choropleth(., level = 2, 
                                     n = 5, 
                                     geo_var = "region_nuts_codes", 
                                     values_var = "total_agreement_weighted", 
                                     type = 'discrete')
