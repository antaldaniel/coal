Processing Eurobarometer 80.2
================
Daniel Antal, CFA
4/27/2020

  - [Setup The Eurobarometer Package](#setup-the-eurobarometer-package)
  - [Read In Data](#read-in-data)
      - [Preprocessing the Eurobarometer 80.2
        Data](#preprocessing-the-eurobarometer-80.2-data)
  - [Simple Models](#simple-models)
      - [Simple GLM model outside
        Poland](#simple-glm-model-outside-poland)
      - [Simple GLM model for Poland](#simple-glm-model-for-poland)
      - [Simple Model With Country
        Effects](#simple-model-with-country-effects)

## Setup The Eurobarometer Package

## Read In Data

You can read in the SPSS file with `haven`, which is part of
`tidyverse`.

``` r
# Change to eval=TRUE if you want to run this code
ZA5877_raw <- haven::read_spss(file.path("not_included", "ZA5877_v2-0-0.sav"))
```

You can analyze the SPSS file with `gesis_metadata_create`.

``` r
# Change to eval=TRUE if you want to run this code
ZA5877_metadata <- gesis_metadata_create(dat = ZA5877_raw)

ZA5877 <- ZA5877_raw %>%
  purrr::set_names ( as.character(ZA5877_metadata$canonical_name)) %>%
  mutate ( region_nuts_names  = sjlabelled::as_label(region_nuts_codes)
           ) %>%
  mutate ( region_nuts_codes =  as.character(sjlabelled::as_labelled(region_nuts_codes)
           ))
```

### Preprocessing the Eurobarometer 80.2 Data

``` r
# Change to eval=TRUE if you want to run this code
eb13_no_coal <- ZA5877 %>%
  select ( one_of("serious_world_problems_climate_change",
                  "serious_world_problems_climate_change_sum" ,
                  "climate_change_action_generate_renewable_energy",
                  "gvrnmnt_targets_for_renewable_energy_importance",
                  "region_nuts_names", "region_nuts_codes",
                  "age_exact", "age_education", "type_of_community",
                  "age_education_5_cat_recoded",
                  "country_code_iso_3166"),
           contains("occupation")) %>%
  mutate_at( vars(starts_with("serious"),
                  starts_with("climate")), recode_mentioned) %>%
  mutate_at ( vars(starts_with("type"),
                   contains("recoded"),
                   contains("occupation")), haven::as_factor) %>%
  mutate ( government_targets = haven::as_factor(gvrnmnt_targets_for_renewable_energy_importance)) %>%
  mutate ( government_targets_numeric = as.numeric(gvrnmnt_targets_for_renewable_energy_importance),
           government_targets_important = case_when (
           government_targets_numeric < 3 ~ 1,
           government_targets_numeric >= 3 ~ 0,
           is.na(government_targets_numeric ) ~ NA_real_ )
           ) %>%
  mutate ( age_education = recode_age_education(var = age_education,
                                                age_exact = age_exact )) %>%
  mutate  ( is_rural = case_when (
    grepl ( "rural", tolower(as.character(type_of_community))) ~ 1,
    grepl ( "town", tolower(as.character(type_of_community)))  ~ 0,
    TRUE ~ NA_real_)
) %>%
  mutate  ( is_student = case_when (
    grepl ( "studying", tolower(as.character(age_education_5_cat_recoded))) ~ 1,
    grepl ( "refuse", tolower(as.character(type_of_community)))  ~ NA_real_,
    TRUE ~ 0)
  ) %>%
  mutate ( year_survey = 2013 )

saveRDS(eb13_no_coal,
        file.path("data-raw", "eb13_no_coal.rds"), version = 2)
```

I created a coal region proxy with this document: [Coal regions in
transition](https://ec.europa.eu/energy/topics/oil-gas-and-coal/EU-coal-regions/coal-regions-transition_en)

``` r
coal_regions <- readRDS( file.path("data-raw", "eb13_no_coal.rds")
                         ) %>% 
  select ( starts_with( "country_code"),
           starts_with ("region")) %>%
  distinct_all() %>%
  mutate ( coal_region = case_when (
    region_nuts_codes == "DE4" ~ "Brandenburg",
    region_nuts_codes == "DED" ~ "Saxony",
    region_nuts_codes == "DEE" ~ "Saxony-Anhalt",
    region_nuts_codes == "DEA" ~ "Nord-Rhein-Westphalia",
    region_nuts_codes == "RO42" ~ "Jiu Valley [part]",
    region_nuts_codes == "ES12" ~ "Asturias",
    region_nuts_codes == "ES24" ~ "Aragón",
    region_nuts_codes == "ES41" ~ "Castilla-y-León",
    region_nuts_codes == "SK02" ~ "Upper Nitra [part]",
    region_nuts_codes == "CZ04" ~ "Karlovy Vary & Usti",
    region_nuts_codes == "CZ08" ~ "Moravia-Silesia",
    region_nuts_codes %in% c("EL13", "EL53" ) ~ "Western Macedonia",
    region_nuts_codes %in% c("SI015", "SI035" ) ~ "Zasavska",
    region_nuts_codes %in% c("SI014", "SI004" ) ~ "Savinjsko-Šaleška",
    region_nuts_codes == "PL21"  ~ "Lesser Poland",
    region_nuts_codes == "PL22"  ~ "Silesia",
    region_nuts_codes == "PL41"  ~ "Greater Poland",
    region_nuts_codes == "PL51"  ~ "Lower Silesia",
    TRUE ~ NA_character_
    )) %>%
  mutate ( is_coal_region = ifelse (is.na(coal_region), 0, 1))

saveRDS(coal_regions, file.path("data", "coal_regions.rds"), 
        version = 2) #backward compatibilty
```

``` r
eb13_no_coal <- readRDS(file.path("data-raw", "eb13_no_coal.rds"))

eb13 <- eb13_no_coal %>%
  left_join ( readRDS(file.path("data", "coal_regions.rds")), 
              by = c("region_nuts_names", 
                     "region_nuts_codes", "country_code_iso_3166"))
```

    ## Warning: Column `region_nuts_names` has different attributes on LHS and RHS of
    ## join

``` r
saveRDS(eb13, file.path("data", "eb13.rds"))
```

## Simple Models

### Simple GLM model outside Poland

``` r
eb13 <- readRDS(file.path("data", "eb13.rds"))
summary ( glm ( government_targets_important ~ age_exact +
                  is_rural + is_student  +
                  is_coal_region,
                data = filter ( eb13,
                                country_code_iso_3166!= "PL"),
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = government_targets_important ~ age_exact + is_rural + 
    ##     is_student + is_coal_region, family = "binomial", data = filter(eb13, 
    ##     country_code_iso_3166 != "PL"))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4344   0.3864   0.4047   0.4216   0.5551  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     2.733582   0.079247  34.495  < 2e-16 ***
    ## age_exact      -0.005343   0.001398  -3.821 0.000133 ***
    ## is_rural       -0.032760   0.047853  -0.685 0.493591    
    ## is_student      0.256570   0.115155   2.228 0.025878 *  
    ## is_coal_region -0.433696   0.083472  -5.196 2.04e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 14628  on 26174  degrees of freedom
    ## Residual deviance: 14570  on 26170  degrees of freedom
    ##   (744 observations deleted due to missingness)
    ## AIC: 14580
    ## 
    ## Number of Fisher Scoring iterations: 5

Support for the target variable `GVRNMNT TARGETS FOR RENEWABLE ENERGY`
coded to binary variable (important, not important) \* is shrinking with
age. \* is more likely to be supported by full-time students \* less
likely to be supported in rural areas, but this is not a significant
variable \* less likely to be supported in coal areas.

### Simple GLM model for Poland

The model would be similar in Poland, but the variables are not
significant.

``` r
summary ( glm ( government_targets_important ~ age_exact +
                  is_rural + is_student +
                  is_coal_region,
                data = filter ( eb13,
                                country_code_iso_3166 == "PL"),
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = government_targets_important ~ age_exact + is_rural + 
    ##     is_student + is_coal_region, family = "binomial", data = filter(eb13, 
    ##     country_code_iso_3166 == "PL"))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3898   0.3705   0.4203   0.4685   0.6033  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      1.941442   0.377428   5.144 2.69e-07 ***
    ## age_exact        0.011712   0.007156   1.637    0.102    
    ## is_rural        -0.372760   0.231633  -1.609    0.108    
    ## is_student      15.571814 557.720695   0.028    0.978    
    ## is_coal_region  -0.191377   0.233531  -0.819    0.413    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 568.41  on 952  degrees of freedom
    ## Residual deviance: 553.15  on 948  degrees of freedom
    ##   (47 observations deleted due to missingness)
    ## AIC: 563.15
    ## 
    ## Number of Fisher Scoring iterations: 16

### Simple Model With Country Effects

One explanation for the difference is that support for the measure in
Poland is overall smaller than in the EU.

``` r
summary ( glm ( government_targets_important ~ age_exact +
                  is_student +
                  country_code_iso_3166,
                data = eb13,
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = government_targets_important ~ age_exact + is_student + 
    ##     country_code_iso_3166, family = "binomial", data = eb13)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.3160   0.3123   0.3781   0.4362   0.7370  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  2.980192   0.147593  20.192  < 2e-16 ***
    ## age_exact                   -0.006161   0.001407  -4.380 1.19e-05 ***
    ## is_student                   0.276079   0.115591   2.388  0.01692 *  
    ## country_code_iso_3166BE     -0.154952   0.174358  -0.889  0.37416    
    ## country_code_iso_3166BG     -0.968582   0.158574  -6.108 1.01e-09 ***
    ## country_code_iso_3166CY      1.887314   0.467775   4.035 5.47e-05 ***
    ## country_code_iso_3166CZ     -0.859932   0.158670  -5.420 5.97e-08 ***
    ## country_code_iso_3166DE-E   -1.261078   0.168706  -7.475 7.72e-14 ***
    ## country_code_iso_3166DE-W   -0.421373   0.167800  -2.511  0.01203 *  
    ## country_code_iso_3166DK     -0.107735   0.177812  -0.606  0.54459    
    ## country_code_iso_3166EE     -1.001835   0.156809  -6.389 1.67e-10 ***
    ## country_code_iso_3166ES      0.567721   0.213437   2.660  0.00782 ** 
    ## country_code_iso_3166FI      0.051511   0.186150   0.277  0.78200    
    ## country_code_iso_3166FR     -0.509389   0.166003  -3.069  0.00215 ** 
    ## country_code_iso_3166GB-GBN -0.689061   0.162271  -4.246 2.17e-05 ***
    ## country_code_iso_3166GB-NIR -0.415407   0.231610  -1.794  0.07288 .  
    ## country_code_iso_3166GR      0.505725   0.208339   2.427  0.01521 *  
    ## country_code_iso_3166HR     -0.186839   0.178147  -1.049  0.29427    
    ## country_code_iso_3166HU      0.262593   0.194330   1.351  0.17661    
    ## country_code_iso_3166IE     -0.074045   0.182961  -0.405  0.68569    
    ## country_code_iso_3166IT     -0.223161   0.175483  -1.272  0.20348    
    ## country_code_iso_3166LT     -0.202573   0.177623  -1.140  0.25409    
    ## country_code_iso_3166LU     -0.123197   0.216109  -0.570  0.56863    
    ## country_code_iso_3166LV     -0.828793   0.160869  -5.152 2.58e-07 ***
    ## country_code_iso_3166MT      2.858576   0.718892   3.976 7.00e-05 ***
    ## country_code_iso_3166NL      0.395546   0.198823   1.989  0.04665 *  
    ## country_code_iso_3166PL     -0.355899   0.172547  -2.063  0.03915 *  
    ## country_code_iso_3166PT      0.158923   0.188797   0.842  0.39992    
    ## country_code_iso_3166RO     -0.185797   0.177596  -1.046  0.29548    
    ## country_code_iso_3166SE      0.249739   0.191839   1.302  0.19298    
    ## country_code_iso_3166SI      0.235425   0.188610   1.248  0.21195    
    ## country_code_iso_3166SK     -0.300689   0.174700  -1.721  0.08522 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 15204  on 27140  degrees of freedom
    ## Residual deviance: 14624  on 27109  degrees of freedom
    ##   (778 observations deleted due to missingness)
    ## AIC: 14688
    ## 
    ## Number of Fisher Scoring iterations: 7
