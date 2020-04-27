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
source(file.path("R", "which_coal_region.R"))
# Change to eval=TRUE if you want to run this code
eb13 <- ZA5877 %>%
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
  mutate ( government_targets_numeric =  case_when (
           grepl("Not very|Not at all", 
                 as.character(government_targets)) ~ 0,
           grepl("Fairly|Very", as.character(government_targets)) ~ 1,
           government_targets == "DK" ~ NA_real_,
           TRUE ~ NA_real_ )
           ) %>%
  mutate ( age_education = recode_age_education(var = age_education,
                                                age_exact = age_exact )
           ) %>%
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
  mutate ( year_survey = 2013 ) %>%
  mutate ( coal_region = which_coal_region(region_nuts_codes)) %>%
  mutate ( is_coal_region = ifelse (is.na(coal_region), 0, 1))

saveRDS(eb13, file.path("data", "eb13.rds"), 
        version = 2 )
```

I created a coal region proxy with this document: [Coal regions in
transition](https://ec.europa.eu/energy/topics/oil-gas-and-coal/EU-coal-regions/coal-regions-transition_en)

## Simple Models

### Simple GLM model outside Poland

``` r
eb13 <- readRDS(file.path("data", "eb13.rds"))

no_poland_model_13 <- glm ( government_targets_numeric ~ 
                              age_exact +
                              is_student  + 
                              is_rural +
                              is_coal_region,
                data = filter ( eb13,
                                country_code_iso_3166!= "PL"),
                family = 'binomial')

summary (no_poland_model_13)
```

    ## 
    ## Call:
    ## glm(formula = government_targets_numeric ~ age_exact + is_student + 
    ##     is_rural + is_coal_region, family = "binomial", data = filter(eb13, 
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
    ## is_student      0.256570   0.115155   2.228 0.025878 *  
    ## is_rural       -0.032760   0.047853  -0.685 0.493591    
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
poland_model_13 <- glm ( government_targets_numeric ~ 
                              age_exact +
                              is_student +
                              is_rural +
                              is_coal_region,
                data = filter ( eb13,
                                country_code_iso_3166 == "PL"),
                family = 'binomial')

summary(poland_model_13)
```

    ## 
    ## Call:
    ## glm(formula = government_targets_numeric ~ age_exact + is_student + 
    ##     is_rural + is_coal_region, family = "binomial", data = filter(eb13, 
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
    ## is_student      15.571814 557.720695   0.028    0.978    
    ## is_rural        -0.372760   0.231633  -1.609    0.108    
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
summary ( glm ( government_targets_numeric ~ age_exact +
                  is_student + is_rural +
                  country_code_iso_3166,
                data = eb13,
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = government_targets_numeric ~ age_exact + is_student + 
    ##     is_rural + country_code_iso_3166, family = "binomial", data = eb13)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.3016   0.3113   0.3776   0.4370   0.7526  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  3.018018   0.148589  20.311  < 2e-16 ***
    ## age_exact                   -0.006111   0.001409  -4.336 1.45e-05 ***
    ## is_student                   0.269418   0.115648   2.330  0.01982 *  
    ## is_rural                    -0.104483   0.048040  -2.175  0.02964 *  
    ## country_code_iso_3166BE     -0.143891   0.174452  -0.825  0.40948    
    ## country_code_iso_3166BG     -0.977812   0.158660  -6.163 7.14e-10 ***
    ## country_code_iso_3166CY      1.882617   0.467785   4.025 5.71e-05 ***
    ## country_code_iso_3166CZ     -0.867706   0.158730  -5.467 4.59e-08 ***
    ## country_code_iso_3166DE-E   -1.283697   0.169066  -7.593 3.13e-14 ***
    ## country_code_iso_3166DE-W   -0.425205   0.167831  -2.534  0.01129 *  
    ## country_code_iso_3166DK     -0.113203   0.177846  -0.637  0.52444    
    ## country_code_iso_3166EE     -1.005507   0.156841  -6.411 1.45e-10 ***
    ## country_code_iso_3166ES      0.579817   0.213525   2.715  0.00662 ** 
    ## country_code_iso_3166FI      0.037474   0.186281   0.201  0.84056    
    ## country_code_iso_3166FR     -0.508698   0.166016  -3.064  0.00218 ** 
    ## country_code_iso_3166GB-GBN -0.701008   0.162401  -4.317 1.59e-05 ***
    ## country_code_iso_3166GB-NIR -0.419467   0.231630  -1.811  0.07015 .  
    ## country_code_iso_3166GR      0.495554   0.208407   2.378  0.01742 *  
    ## country_code_iso_3166HR     -0.173853   0.178722  -0.973  0.33067    
    ## country_code_iso_3166HU      0.258571   0.194351   1.330  0.18338    
    ## country_code_iso_3166IE     -0.076411   0.182983  -0.418  0.67625    
    ## country_code_iso_3166IT     -0.246117   0.175802  -1.400  0.16152    
    ## country_code_iso_3166LT     -0.216291   0.177756  -1.217  0.22369    
    ## country_code_iso_3166LU     -0.124785   0.216132  -0.577  0.56370    
    ## country_code_iso_3166LV     -0.829202   0.160890  -5.154 2.55e-07 ***
    ## country_code_iso_3166MT      2.874690   0.718985   3.998 6.38e-05 ***
    ## country_code_iso_3166NL      0.397150   0.198836   1.997  0.04578 *  
    ## country_code_iso_3166PL     -0.356117   0.172566  -2.064  0.03905 *  
    ## country_code_iso_3166PT      0.152461   0.188828   0.807  0.41943    
    ## country_code_iso_3166RO     -0.176287   0.177669  -0.992  0.32109    
    ## country_code_iso_3166SE      0.231647   0.192047   1.206  0.22774    
    ## country_code_iso_3166SI      0.238612   0.188631   1.265  0.20588    
    ## country_code_iso_3166SK     -0.294798   0.174739  -1.687  0.09159 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 15197  on 27127  degrees of freedom
    ## Residual deviance: 14612  on 27095  degrees of freedom
    ##   (791 observations deleted due to missingness)
    ## AIC: 14678
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
summary ( glm ( government_targets_numeric ~ age_exact +
                  is_coal_region +
                  country_code_iso_3166,
                data = eb13,
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = government_targets_numeric ~ age_exact + is_coal_region + 
    ##     country_code_iso_3166, family = "binomial", data = eb13)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.3185   0.3132   0.3752   0.4366   0.7690  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  3.060946   0.144000  21.257  < 2e-16 ***
    ## age_exact                   -0.007564   0.001284  -5.890 3.87e-09 ***
    ## is_coal_region              -0.140272   0.092993  -1.508  0.13145    
    ## country_code_iso_3166BE     -0.148628   0.174330  -0.853  0.39390    
    ## country_code_iso_3166BG     -0.966140   0.158561  -6.093 1.11e-09 ***
    ## country_code_iso_3166CY      1.893709   0.467763   4.048 5.16e-05 ***
    ## country_code_iso_3166CZ     -0.829371   0.160412  -5.170 2.34e-07 ***
    ## country_code_iso_3166DE-E   -1.172960   0.176919  -6.630 3.36e-11 ***
    ## country_code_iso_3166DE-W   -0.373421   0.170039  -2.196  0.02809 *  
    ## country_code_iso_3166DK     -0.094339   0.177710  -0.531  0.59551    
    ## country_code_iso_3166EE     -0.996224   0.156783  -6.354 2.10e-10 ***
    ## country_code_iso_3166ES      0.589847   0.213769   2.759  0.00579 ** 
    ## country_code_iso_3166FI      0.068704   0.185997   0.369  0.71184    
    ## country_code_iso_3166FR     -0.505635   0.165995  -3.046  0.00232 ** 
    ## country_code_iso_3166GB-GBN -0.681801   0.162246  -4.202 2.64e-05 ***
    ## country_code_iso_3166GB-NIR -0.407414   0.231587  -1.759  0.07854 .  
    ## country_code_iso_3166GR      0.516052   0.208340   2.477  0.01325 *  
    ## country_code_iso_3166HR     -0.182838   0.178127  -1.026  0.30468    
    ## country_code_iso_3166HU      0.261058   0.194328   1.343  0.17915    
    ## country_code_iso_3166IE     -0.070486   0.182946  -0.385  0.70003    
    ## country_code_iso_3166IT     -0.221441   0.175471  -1.262  0.20696    
    ## country_code_iso_3166LT     -0.192618   0.177563  -1.085  0.27802    
    ## country_code_iso_3166LU     -0.118756   0.216095  -0.550  0.58262    
    ## country_code_iso_3166LV     -0.826420   0.160852  -5.138 2.78e-07 ***
    ## country_code_iso_3166MT      2.864928   0.718888   3.985 6.74e-05 ***
    ## country_code_iso_3166NL      0.405883   0.198765   2.042  0.04115 *  
    ## country_code_iso_3166PL     -0.302176   0.176490  -1.712  0.08687 .  
    ## country_code_iso_3166PT      0.161313   0.188798   0.854  0.39287    
    ## country_code_iso_3166RO     -0.170030   0.177911  -0.956  0.33922    
    ## country_code_iso_3166SE      0.260507   0.191777   1.358  0.17434    
    ## country_code_iso_3166SI      0.265945   0.189190   1.406  0.15981    
    ## country_code_iso_3166SK     -0.248244   0.177892  -1.395  0.16287    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 15204  on 27140  degrees of freedom
    ## Residual deviance: 14628  on 27109  degrees of freedom
    ##   (778 observations deleted due to missingness)
    ## AIC: 14692
    ## 
    ## Number of Fisher Scoring iterations: 7
