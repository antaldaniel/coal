Processing Eurobarometer 91.3 (April 2019)
================
Daniel Antal, CFA
4/27/2020

  - [Setup The Eurobarometer Package](#setup-the-eurobarometer-package)
  - [Read In Data](#read-in-data)
      - [Preprocessing The Eurobarometer 91.3 (April
        2019)](#preprocessing-the-eurobarometer-91.3-april-2019)
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
ZA7572_raw <- haven::read_spss(file.path("not_included", "ZA7572_v1-0-0.sav"))
```

You can analyze the SPSS file with `gesis_metadata_create`.

``` r
# Change to eval=TRUE if you want to run this code
ZA7572_metadata <- gesis_metadata_create(dat = ZA7572_raw)

ZA7572 <- ZA7572_raw %>%
  purrr::set_names ( as.character(ZA7572_metadata$canonical_name)) %>%
  mutate ( region_nuts_names  = haven::as_factor(region_nuts_codes)) %>%
  mutate ( region_nuts_codes =  as.character(region_nuts_codes)) 
```

### Preprocessing The Eurobarometer 91.3 (April 2019)

``` r
source(file.path("R", "which_coal_region.R"))
# Change to eval=TRUE if you want to run this code
eb19 <- ZA7572 %>%
  select ( one_of("more_pub_fin_support_for_transition_even_if_cut_subsidies_fossil_fuels",
                  "region_nuts_names", "region_nuts_codes",
                  "age_exact", "age_education", "type_of_community",
                  "age_education_recoded_5_cat",
                  "country_code_iso_3166", 
                  "weight_result_from_target_redressment"),
           contains("occupation")) %>%
  dplyr::rename ( transition_policy  =  more_pub_fin_support_for_transition_even_if_cut_subsidies_fossil_fuels, 
                  w1 = weight_result_from_target_redressment ) %>%
  mutate ( transition_policy = haven::as_factor ( transition_policy )) %>%
  mutate_at ( vars(starts_with("type"),
                   contains("recoded"),
                   contains("occupation")), haven::as_factor) %>%
  mutate ( transition_policy_numeric = case_when (
           grepl("Totally agree|Tend to agree",
                 as.character(transition_policy))    ~ 1,
           grepl("disagree", as.character(transition_policy)) ~ 0,
           TRUE ~ NA_real_ )
           ) %>%
  mutate ( is_transition_policy_totally = case_when (
           grepl("Totally agree",
                 as.character(transition_policy))    ~ 1,
           grepl("Tend to|disagree", as.character(transition_policy)) ~ 0,
           TRUE ~ NA_real_ )
           ) %>%
  mutate ( total_agreement_weighted = w1*is_transition_policy_totally) %>%
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
  mutate  ( is_highly_educated = case_when (
    grepl ( "20 years", tolower(as.character(age_education_recoded_5_cat))) ~ 1,
    grepl ( "refuse", tolower(as.character(type_of_community)))  ~ NA_real_,
    TRUE ~ 0)
  ) %>%
  mutate ( year_survey = 2019 ) %>%
  mutate ( coal_region = which_coal_region(region_nuts_codes)) %>%
  mutate ( is_coal_region = ifelse (is.na(coal_region), 0, 1))

saveRDS(eb19,
        file.path("data", "eb19.rds"), 
        version = 2) # backward compatiblity
```

I created a coal region proxy with this document: [Coal regions in
transition](https://ec.europa.eu/energy/topics/oil-gas-and-coal/EU-coal-regions/coal-regions-transition_en)

## Simple Models

The problem with this variable is that it has very little variance.

``` r
library(ggplot2)

eb19 <- readRDS(file.path("data", "eb19.rds"))

eb19 %>%
  ggplot( data = ., 
          aes ( x= as.factor(transition_policy_numeric) )) +
  geom_histogram( stat = "count")
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](Eurobarometer_19_files/figure-gfm/histogram-1.png)<!-- -->

``` r
eb19 %>%
  ggplot( data = ., 
          aes ( x= as.factor(is_transition_policy_totally ) )) +
  geom_histogram( stat = "count")
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](Eurobarometer_19_files/figure-gfm/histogram-2.png)<!-- -->

### Simple GLM model outside Poland

``` r
summary ( glm ( is_transition_policy_totally ~ age_exact +
                  is_rural +
                  is_coal_region + is_highly_educated,
                data = filter ( eb19,
                                country_code_iso_3166!= "PL"),
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = is_transition_policy_totally ~ age_exact + is_rural + 
    ##     is_coal_region + is_highly_educated, family = "binomial", 
    ##     data = filter(eb19, country_code_iso_3166 != "PL"))
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.243  -1.128  -1.031   1.217   1.484  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         0.0931685  0.0408101   2.283  0.02243 *  
    ## age_exact          -0.0045943  0.0007118  -6.455 1.08e-10 ***
    ## is_rural           -0.0799021  0.0276722  -2.887  0.00388 ** 
    ## is_coal_region     -0.2956219  0.0560292  -5.276 1.32e-07 ***
    ## is_highly_educated  0.1559016  0.0268201   5.813 6.14e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 33917  on 24537  degrees of freedom
    ## Residual deviance: 33795  on 24533  degrees of freedom
    ##   (2104 observations deleted due to missingness)
    ## AIC: 33805
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
summary ( glm ( is_transition_policy_totally ~ age_exact +
                  is_rural + is_highly_educated +
                  is_coal_region,
                data = filter ( eb19,
                                country_code_iso_3166 == "PL"),
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = is_transition_policy_totally ~ age_exact + is_rural + 
    ##     is_highly_educated + is_coal_region, family = "binomial", 
    ##     data = filter(eb19, country_code_iso_3166 == "PL"))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1820  -0.9595  -0.8145   1.3326   1.6931  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -0.272120   0.242803  -1.121 0.262395    
    ## age_exact          -0.010315   0.004143  -2.490 0.012786 *  
    ## is_rural           -0.011873   0.141094  -0.084 0.932940    
    ## is_highly_educated  0.511336   0.142610   3.586 0.000336 ***
    ## is_coal_region     -0.025920   0.141451  -0.183 0.854607    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1207.4  on 925  degrees of freedom
    ## Residual deviance: 1186.4  on 921  degrees of freedom
    ##   (87 observations deleted due to missingness)
    ## AIC: 1196.4
    ## 
    ## Number of Fisher Scoring iterations: 4

### Simple Model With Country Effects

The Poland-only model is again counterintuitive, becuase in Poland the
level of total agreement is 32.6% less likely than in the average EU
country.

``` r
country_effects_18 <- glm ( is_transition_policy_totally ~
                              age_exact +
                              is_rural + 
                              is_highly_educated +
                              country_code_iso_3166,
                data = eb19,
                family = 'binomial')

summary ( country_effects_18 )
```

    ## 
    ## Call:
    ## glm(formula = is_transition_policy_totally ~ age_exact + is_rural + 
    ##     is_highly_educated + country_code_iso_3166, family = "binomial", 
    ##     data = eb19)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7445  -1.0879  -0.8766   1.2022   1.7040  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                0.003442   0.074190   0.046 0.963000    
    ## age_exact                 -0.005382   0.000734  -7.332 2.27e-13 ***
    ## is_rural                  -0.106666   0.028432  -3.752 0.000176 ***
    ## is_highly_educated         0.200575   0.028189   7.115 1.12e-12 ***
    ## country_code_iso_3166BE   -0.321287   0.092009  -3.492 0.000480 ***
    ## country_code_iso_3166BG   -0.141081   0.094518  -1.493 0.135535    
    ## country_code_iso_3166CY    1.158055   0.120465   9.613  < 2e-16 ***
    ## country_code_iso_3166CZ   -0.481278   0.095536  -5.038 4.71e-07 ***
    ## country_code_iso_3166DE-E -0.344540   0.116652  -2.954 0.003141 ** 
    ## country_code_iso_3166DE-W  0.100787   0.091210   1.105 0.269160    
    ## country_code_iso_3166DK    0.185638   0.092633   2.004 0.045067 *  
    ## country_code_iso_3166EE   -0.597406   0.101399  -5.892 3.82e-09 ***
    ## country_code_iso_3166ES    1.189634   0.096801  12.289  < 2e-16 ***
    ## country_code_iso_3166FI   -0.301119   0.093954  -3.205 0.001351 ** 
    ## country_code_iso_3166FR   -0.092018   0.092553  -0.994 0.320118    
    ## country_code_iso_3166GB    0.371394   0.092009   4.037 5.43e-05 ***
    ## country_code_iso_3166GR    0.211028   0.091352   2.310 0.020885 *  
    ## country_code_iso_3166HR    0.033915   0.090946   0.373 0.709213    
    ## country_code_iso_3166HU    0.467313   0.090926   5.139 2.75e-07 ***
    ## country_code_iso_3166IE    0.405231   0.090365   4.484 7.31e-06 ***
    ## country_code_iso_3166IT   -0.023770   0.091880  -0.259 0.795857    
    ## country_code_iso_3166LT    0.040742   0.093903   0.434 0.664381    
    ## country_code_iso_3166LU   -0.265363   0.115861  -2.290 0.022000 *  
    ## country_code_iso_3166LV   -0.337653   0.096762  -3.490 0.000484 ***
    ## country_code_iso_3166MT    0.655422   0.114886   5.705 1.16e-08 ***
    ## country_code_iso_3166NL    0.152368   0.091220   1.670 0.094853 .  
    ## country_code_iso_3166PL   -0.356621   0.094110  -3.789 0.000151 ***
    ## country_code_iso_3166PT    0.241119   0.092616   2.603 0.009230 ** 
    ## country_code_iso_3166RO    0.061674   0.090526   0.681 0.495690    
    ## country_code_iso_3166SE    0.486293   0.092910   5.234 1.66e-07 ***
    ## country_code_iso_3166SI    0.327731   0.090936   3.604 0.000313 ***
    ## country_code_iso_3166SK   -0.109938   0.092377  -1.190 0.234005    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 35169  on 25463  degrees of freedom
    ## Residual deviance: 34128  on 25432  degrees of freedom
    ##   (2191 observations deleted due to missingness)
    ## AIC: 34192
    ## 
    ## Number of Fisher Scoring iterations: 4

A relatively good model takes the age, rural regions, coal regions and
countries. In Poland, even after controlling for age, lack of subjective
urbanization and coal regions, the country effect is significantly
negative.

``` r
summary ( glm ( is_transition_policy_totally ~
                              age_exact +
                              is_rural + 
                              is_highly_educated + 
                              is_coal_region + 
                              country_code_iso_3166,
                data = eb19,
                family = 'binomial') 
          )
```

    ## 
    ## Call:
    ## glm(formula = is_transition_policy_totally ~ age_exact + is_rural + 
    ##     is_highly_educated + is_coal_region + country_code_iso_3166, 
    ##     family = "binomial", data = eb19)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7543  -1.0881  -0.8727   1.2012   1.7034  
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                0.0039162  0.0741927   0.053 0.957904    
    ## age_exact                 -0.0053695  0.0007341  -7.314 2.59e-13 ***
    ## is_rural                  -0.1074049  0.0284401  -3.777 0.000159 ***
    ## is_highly_educated         0.1977715  0.0282040   7.012 2.35e-12 ***
    ## is_coal_region            -0.2068658  0.0589987  -3.506 0.000454 ***
    ## country_code_iso_3166BE   -0.3208473  0.0920069  -3.487 0.000488 ***
    ## country_code_iso_3166BG   -0.1412213  0.0945159  -1.494 0.135136    
    ## country_code_iso_3166CY    1.1581102  0.1204607   9.614  < 2e-16 ***
    ## country_code_iso_3166CZ   -0.4332011  0.0965100  -4.489 7.17e-06 ***
    ## country_code_iso_3166DE-E -0.2382541  0.1205583  -1.976 0.048126 *  
    ## country_code_iso_3166DE-W  0.1549239  0.0925116   1.675 0.094004 .  
    ## country_code_iso_3166DK    0.1867040  0.0926313   2.016 0.043846 *  
    ## country_code_iso_3166EE   -0.5969953  0.1013957  -5.888 3.91e-09 ***
    ## country_code_iso_3166ES    1.2135954  0.0970927  12.499  < 2e-16 ***
    ## country_code_iso_3166FI   -0.3003716  0.0939515  -3.197 0.001388 ** 
    ## country_code_iso_3166FR   -0.0917374  0.0925500  -0.991 0.321578    
    ## country_code_iso_3166GB    0.3713458  0.0920062   4.036 5.43e-05 ***
    ## country_code_iso_3166GR    0.2111954  0.0913493   2.312 0.020780 *  
    ## country_code_iso_3166HR    0.0337826  0.0909443   0.371 0.710291    
    ## country_code_iso_3166HU    0.4669307  0.0909240   5.135 2.82e-07 ***
    ## country_code_iso_3166IE    0.4125133  0.0903953   4.563 5.03e-06 ***
    ## country_code_iso_3166IT   -0.0240277  0.0918779  -0.262 0.793694    
    ## country_code_iso_3166LT    0.0409949  0.0939000   0.437 0.662416    
    ## country_code_iso_3166LU   -0.2647189  0.1158573  -2.285 0.022320 *  
    ## country_code_iso_3166LV   -0.3373375  0.0967593  -3.486 0.000490 ***
    ## country_code_iso_3166MT    0.6549490  0.1148828   5.701 1.19e-08 ***
    ## country_code_iso_3166NL    0.1529912  0.0912178   1.677 0.093502 .  
    ## country_code_iso_3166PL   -0.2723101  0.0970903  -2.805 0.005036 ** 
    ## country_code_iso_3166PT    0.2408984  0.0926140   2.601 0.009293 ** 
    ## country_code_iso_3166RO    0.0776186  0.0906430   0.856 0.391826    
    ## country_code_iso_3166SE    0.4872015  0.0929091   5.244 1.57e-07 ***
    ## country_code_iso_3166SI    0.3626256  0.0915201   3.962 7.42e-05 ***
    ## country_code_iso_3166SK   -0.0398653  0.0945210  -0.422 0.673200    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 35169  on 25463  degrees of freedom
    ## Residual deviance: 34116  on 25431  degrees of freedom
    ##   (2191 observations deleted due to missingness)
    ## AIC: 34182
    ## 
    ## Number of Fisher Scoring iterations: 4
