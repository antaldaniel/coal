Processing Eurobarometer 90.2 (October-November 2018)
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
ZA7488_raw <- haven::read_spss(file.path("not_included", "ZA7488_v1-0-0.sav"))
```

You can analyze the SPSS file with `gesis_metadata_create`.

``` r
# Change to eval=TRUE if you want to run this code
ZA7488_metadata <- gesis_metadata_create(dat = ZA7488_raw)

ZA7488 <- ZA7488_raw %>%
  purrr::set_names ( as.character(ZA7488_metadata$canonical_name)) %>%
  mutate ( region_nuts_names  = haven::as_factor(region_nuts_codes)) %>%
  mutate ( region_nuts_codes =  as.character(region_nuts_codes)) 
```

### Preprocessing the Eurobarometer 80.2 Data

``` r
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
  )  %>%
  mutate  ( is_highly_educated = case_when (
    grepl ( "20 years", tolower(as.character(age_education_recoded_5_cat))) ~ 1,
    grepl ( "refuse", tolower(as.character(type_of_community)))  ~ NA_real_,
    TRUE ~ 0)
  ) %>%
  mutate ( year_survey = 2018 ) %>%
  mutate ( coal_region = which_coal_region(region_nuts_codes)) %>%
  mutate ( is_coal_region = ifelse (is.na(coal_region), 0, 1))

saveRDS(eb18,
        file.path("data", "eb18.rds"), 
        version = 2) # backward compatiblity
```

I created a coal region proxy with this document: [Coal regions in
transition](https://ec.europa.eu/energy/topics/oil-gas-and-coal/EU-coal-regions/coal-regions-transition_en)

## Simple Models

The problem with this variable is that it has very little variance.
Therefore the target variable is not `agreement` but `total agreement`.

``` r
library(ggplot2)

eb18 <- readRDS(file.path("data", "eb18.rds"))

eb18 %>%
  ggplot( data = ., 
          aes ( x= as.factor(eu_env_policy_numeric) )) +
  geom_histogram( stat = "count")
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](Eurobarometer_18_files/figure-gfm/histogram-1.png)<!-- -->

``` r
eb18 %>%
  ggplot( data = ., 
          aes ( x= as.factor(is_eu_env_policy_totally ) )) +
  geom_histogram( stat = "count")
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](Eurobarometer_18_files/figure-gfm/histogram-2.png)<!-- -->

### Simple GLM model outside Poland

``` r
summary ( glm ( is_eu_env_policy_totally ~
                  is_rural + is_highly_educated +
                  is_coal_region,
                data = filter ( eb18,
                                country_code_iso_3166!= "PL"),
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = is_eu_env_policy_totally ~ is_rural + is_highly_educated + 
    ##     is_coal_region, family = "binomial", data = filter(eb18, 
    ##     country_code_iso_3166 != "PL"))
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.171  -1.121  -1.037   1.235   1.402  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -0.13532    0.01958  -6.910 4.84e-12 ***
    ## is_rural           -0.05335    0.02752  -1.938   0.0526 .  
    ## is_highly_educated  0.12020    0.02732   4.401 1.08e-05 ***
    ## is_coal_region     -0.32482    0.05758  -5.641 1.69e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32913  on 23812  degrees of freedom
    ## Residual deviance: 32852  on 23809  degrees of freedom
    ##   (2492 observations deleted due to missingness)
    ## AIC: 32860
    ## 
    ## Number of Fisher Scoring iterations: 4

Support for the target variable
`eu_env_policy_statements_more_pub_fin_support_for_clean_energy_even_if_fossil_subsidies_reduced`
coded to binary variable (agree, disagree) \* is shrinking with age. \*
less likely to be supported in rural areas, but this is not a
significant variable \* less likely to be supported in coal areas.

### Simple GLM model for Poland

The `coal regions` in Poland are not significant and do not have a
negative coefficient.

``` r
summary ( glm ( is_eu_env_policy_totally ~ 
                  is_rural + is_highly_educated +
                  is_coal_region,
                data = filter ( eb18,
                                country_code_iso_3166 == "PL"),
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = is_eu_env_policy_totally ~ is_rural + is_highly_educated + 
    ##     is_coal_region, family = "binomial", data = filter(eb18, 
    ##     country_code_iso_3166 == "PL"))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1118  -0.9736  -0.9224   1.3154   1.4686  
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -0.66271    0.12879  -5.146 2.66e-07 ***
    ## is_rural            0.02824    0.14133   0.200    0.842    
    ## is_highly_educated  0.34396    0.13998   2.457    0.014 *  
    ## is_coal_region      0.13413    0.13969   0.960    0.337    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1231.7  on 921  degrees of freedom
    ## Residual deviance: 1225.1  on 918  degrees of freedom
    ##   (112 observations deleted due to missingness)
    ## AIC: 1233.1
    ## 
    ## Number of Fisher Scoring iterations: 4

### Simple Model With Country Effects

The Poland-only model is counterintuitive, becuase in Poland the level
of total agreement is 29% less likely than in the average EU country.

``` r
country_effects_18 <- glm ( is_eu_env_policy_totally ~
                              is_rural + is_highly_educated +
                              country_code_iso_3166,
                data = eb18,
                family = 'binomial')

summary ( country_effects_18 )
```

    ## 
    ## Call:
    ## glm(formula = is_eu_env_policy_totally ~ is_rural + is_highly_educated + 
    ##     country_code_iso_3166, family = "binomial", data = eb18)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5563  -1.1021  -0.9144   1.2044   1.5644  
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)               -0.16095    0.06503  -2.475 0.013320 *  
    ## is_rural                  -0.09200    0.02818  -3.265 0.001096 ** 
    ## is_highly_educated         0.12758    0.02848   4.479 7.49e-06 ***
    ## country_code_iso_3166BE   -0.22867    0.09026  -2.533 0.011297 *  
    ## country_code_iso_3166BG   -0.25889    0.09574  -2.704 0.006852 ** 
    ## country_code_iso_3166CY    0.89071    0.11734   7.591 3.18e-14 ***
    ## country_code_iso_3166CZ   -0.62239    0.09579  -6.497 8.18e-11 ***
    ## country_code_iso_3166DE-E -0.29134    0.11375  -2.561 0.010429 *  
    ## country_code_iso_3166DE-W -0.12243    0.09173  -1.335 0.181976    
    ## country_code_iso_3166DK    0.06199    0.09292   0.667 0.504739    
    ## country_code_iso_3166EE   -0.48839    0.09771  -4.998 5.79e-07 ***
    ## country_code_iso_3166ES    0.61642    0.09305   6.625 3.48e-11 ***
    ## country_code_iso_3166FI   -0.08179    0.09217  -0.887 0.374912    
    ## country_code_iso_3166FR   -0.14043    0.09247  -1.519 0.128874    
    ## country_code_iso_3166GB    0.20783    0.09244   2.248 0.024557 *  
    ## country_code_iso_3166GR   -0.02523    0.09171  -0.275 0.783264    
    ## country_code_iso_3166HR    0.12398    0.08977   1.381 0.167282    
    ## country_code_iso_3166HU    0.17083    0.09048   1.888 0.059029 .  
    ## country_code_iso_3166IE    0.46509    0.09140   5.088 3.61e-07 ***
    ## country_code_iso_3166IT   -0.15898    0.09245  -1.720 0.085505 .  
    ## country_code_iso_3166LT   -0.02014    0.09397  -0.214 0.830319    
    ## country_code_iso_3166LU   -0.06602    0.11488  -0.575 0.565505    
    ## country_code_iso_3166LV   -0.35451    0.09667  -3.667 0.000245 ***
    ## country_code_iso_3166MT    0.29449    0.11332   2.599 0.009356 ** 
    ## country_code_iso_3166NL    0.27578    0.09039   3.051 0.002280 ** 
    ## country_code_iso_3166PL   -0.31041    0.09289  -3.342 0.000833 ***
    ## country_code_iso_3166PT    0.28735    0.09182   3.129 0.001752 ** 
    ## country_code_iso_3166RO   -0.01866    0.09169  -0.203 0.838769    
    ## country_code_iso_3166SE    0.34364    0.09218   3.728 0.000193 ***
    ## country_code_iso_3166SI    0.15740    0.09174   1.716 0.086226 .  
    ## country_code_iso_3166SK   -0.43116    0.09683  -4.453 8.47e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 34168  on 24734  degrees of freedom
    ## Residual deviance: 33552  on 24704  degrees of freedom
    ##   (2604 observations deleted due to missingness)
    ## AIC: 33614
    ## 
    ## Number of Fisher Scoring iterations: 4

A relatively good model takes the rural regions, coal regions, high
education level and countries. In Poland, even after controlling for
high education level, lack of subjective urbanization and coal regions,
the country effect is significantly negative.

``` r
summary ( glm ( is_eu_env_policy_totally ~
                              is_rural + 
                              is_coal_region + 
                              country_code_iso_3166,
                data = eb18,
                family = 'binomial') 
          )
```

    ## 
    ## Call:
    ## glm(formula = is_eu_env_policy_totally ~ is_rural + is_coal_region + 
    ##     country_code_iso_3166, family = "binomial", data = eb18)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5205  -1.1058  -0.9041   1.2223   1.5930  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)               -0.122988   0.064440  -1.909 0.056317 .  
    ## is_rural                  -0.101264   0.028087  -3.605 0.000312 ***
    ## is_coal_region            -0.120392   0.059271  -2.031 0.042233 *  
    ## country_code_iso_3166BE   -0.207460   0.090098  -2.303 0.021300 *  
    ## country_code_iso_3166BG   -0.259157   0.095704  -2.708 0.006771 ** 
    ## country_code_iso_3166CY    0.901048   0.117269   7.684 1.55e-14 ***
    ## country_code_iso_3166CZ   -0.593976   0.096559  -6.151 7.68e-10 ***
    ## country_code_iso_3166DE-E -0.237901   0.117575  -2.023 0.043032 *  
    ## country_code_iso_3166DE-W -0.091983   0.092580  -0.994 0.320440    
    ## country_code_iso_3166DK    0.119243   0.092007   1.296 0.194971    
    ## country_code_iso_3166EE   -0.459315   0.097445  -4.714 2.43e-06 ***
    ## country_code_iso_3166ES    0.625353   0.093222   6.708 1.97e-11 ***
    ## country_code_iso_3166FI   -0.047813   0.091822  -0.521 0.602566    
    ## country_code_iso_3166FR   -0.120312   0.092323  -1.303 0.192520    
    ## country_code_iso_3166GB    0.212264   0.092395   2.297 0.021598 *  
    ## country_code_iso_3166GR   -0.015546   0.091661  -0.170 0.865322    
    ## country_code_iso_3166HR    0.115405   0.089720   1.286 0.198342    
    ## country_code_iso_3166HU    0.158081   0.090404   1.749 0.080361 .  
    ## country_code_iso_3166IE    0.487028   0.091408   5.328 9.93e-08 ***
    ## country_code_iso_3166IT   -0.168270   0.092397  -1.821 0.068582 .  
    ## country_code_iso_3166LT   -0.003804   0.093850  -0.041 0.967671    
    ## country_code_iso_3166LU   -0.027308   0.114513  -0.238 0.811518    
    ## country_code_iso_3166LV   -0.342057   0.096583  -3.542 0.000398 ***
    ## country_code_iso_3166MT    0.282212   0.113251   2.492 0.012706 *  
    ## country_code_iso_3166NL    0.311002   0.090012   3.455 0.000550 ***
    ## country_code_iso_3166PL   -0.245354   0.095473  -2.570 0.010174 *  
    ## country_code_iso_3166PT    0.277113   0.091764   3.020 0.002529 ** 
    ## country_code_iso_3166RO   -0.010868   0.091901  -0.118 0.905866    
    ## country_code_iso_3166SE    0.398627   0.091339   4.364 1.28e-05 ***
    ## country_code_iso_3166SI    0.180236   0.092179   1.955 0.050549 .  
    ## country_code_iso_3166SK   -0.395727   0.098865  -4.003 6.26e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 34168  on 24734  degrees of freedom
    ## Residual deviance: 33568  on 24704  degrees of freedom
    ##   (2604 observations deleted due to missingness)
    ## AIC: 33630
    ## 
    ## Number of Fisher Scoring iterations: 4
