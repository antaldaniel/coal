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
                  "country_code_iso_3166"),
           contains("occupation")) %>%
  dplyr::rename ( eu_env_policy  = eu_env_policy_statements_more_pub_fin_support_for_clean_energy_even_if_fossil_subsidies_reduced) %>%
  mutate ( eu_env_policy = haven::as_factor ( eu_env_policy )) %>%
  mutate_at ( vars(starts_with("type"),
                   contains("recoded"),
                   contains("occupation")), haven::as_factor) %>%
  mutate ( eu_env_policy_numeric = case_when (
           grepl("agree", as.character(eu_env_policy))    ~ 1,
           grepl("disagree", as.character(eu_env_policy)) ~ 0,
           TRUE ~ NA_real_ )
           ) %>%
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
```

I created a coal region proxy with this document: [Coal regions in
transition](https://ec.europa.eu/energy/topics/oil-gas-and-coal/EU-coal-regions/coal-regions-transition_en)

## Simple Models

### Simple GLM model outside Poland

``` r
eb18 <- readRDS(file.path("data", "eb18.rds"))
summary ( glm ( eu_env_policy ~ age_exact +
                  is_rural + is_student  +
                  is_coal_region,
                data = filter ( eb18,
                                country_code_iso_3166!= "PL"),
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = eu_env_policy ~ age_exact + is_rural + is_student + 
    ##     is_coal_region, family = "binomial", data = filter(eb18, 
    ##     country_code_iso_3166 != "PL"))
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.547  -1.295   1.014   1.061   1.118  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    0.0952835  0.0437910   2.176  0.02956 *  
    ## age_exact      0.0030919  0.0007638   4.048 5.17e-05 ***
    ## is_rural       0.0844655  0.0263362   3.207  0.00134 ** 
    ## is_student     0.0246811  0.0566067   0.436  0.66283    
    ## is_coal_region 0.3828422  0.0552942   6.924 4.40e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 35829  on 26287  degrees of freedom
    ## Residual deviance: 35749  on 26283  degrees of freedom
    ##   (17 observations deleted due to missingness)
    ## AIC: 35759
    ## 
    ## Number of Fisher Scoring iterations: 4

Support for the target variable
`eu_env_policy_statements_more_pub_fin_support_for_clean_energy_even_if_fossil_subsidies_reduced`
coded to binary variable (agree, disagree) \* is shrinking with age. \*
less likely to be supported in rural areas, but this is not a
significant variable \* less likely to be supported in coal areas.

### Simple GLM model for Poland

The model would be similar in Poland, but the variables are not
significant.

``` r
summary ( glm ( eu_env_policy ~ is_coal_region,
                data = filter ( eb18,
                                country_code_iso_3166 == "PL"),
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = eu_env_policy ~ is_coal_region, family = "binomial", 
    ##     data = filter(eb18, country_code_iso_3166 == "PL"))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4823  -1.4152   0.9005   0.9005   0.9569  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     0.69315    0.08372   8.279   <2e-16 ***
    ## is_coal_region -0.14953    0.13411  -1.115    0.265    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1334.0  on 1033  degrees of freedom
    ## Residual deviance: 1332.8  on 1032  degrees of freedom
    ## AIC: 1336.8
    ## 
    ## Number of Fisher Scoring iterations: 4

### Simple Model With Country Effects

One explanation for the difference is that support for the measure in
Poland is overall smaller than in the EU. But in this case the
difference is not very prononunced.

``` r
summary ( glm ( eu_env_policy ~ age_exact +
                  is_student +
                  country_code_iso_3166,
                data = eb18,
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = eu_env_policy ~ age_exact + is_student + country_code_iso_3166, 
    ##     family = "binomial", data = eb18)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.663  -1.249   0.842   1.047   1.468  
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                0.0475664  0.0736829   0.646 0.518568    
    ## age_exact                  0.0037201  0.0007777   4.783 1.72e-06 ***
    ## is_student                 0.0331531  0.0566834   0.585 0.558628    
    ## country_code_iso_3166BE    0.1706207  0.0892505   1.912 0.055914 .  
    ## country_code_iso_3166BG    0.5243954  0.0918125   5.712 1.12e-08 ***
    ## country_code_iso_3166CY   -0.7717582  0.1114201  -6.927 4.31e-12 ***
    ## country_code_iso_3166CZ    0.7049074  0.0937192   7.521 5.42e-14 ***
    ## country_code_iso_3166DE-E  0.3127020  0.1113719   2.808 0.004989 ** 
    ## country_code_iso_3166DE-W  0.1495923  0.0900293   1.662 0.096594 .  
    ## country_code_iso_3166DK   -0.0585378  0.0895180  -0.654 0.513162    
    ## country_code_iso_3166EE    0.7008549  0.0941461   7.444 9.74e-14 ***
    ## country_code_iso_3166ES   -0.4411332  0.0890432  -4.954 7.26e-07 ***
    ## country_code_iso_3166FI    0.0830834  0.0899315   0.924 0.355563    
    ## country_code_iso_3166FR    0.2229019  0.0899399   2.478 0.013199 *  
    ## country_code_iso_3166GB   -0.0822623  0.0891132  -0.923 0.355944    
    ## country_code_iso_3166GR    0.0864538  0.0895131   0.966 0.334133    
    ## country_code_iso_3166HR   -0.1302264  0.0887030  -1.468 0.142072    
    ## country_code_iso_3166HU   -0.1437550  0.0886748  -1.621 0.104986    
    ## country_code_iso_3166IE   -0.4296912  0.0892248  -4.816 1.47e-06 ***
    ## country_code_iso_3166IT    0.2225640  0.0899152   2.475 0.013314 *  
    ## country_code_iso_3166LT    0.1974155  0.0903987   2.184 0.028975 *  
    ## country_code_iso_3166LU    0.1729987  0.1103143   1.568 0.116826    
    ## country_code_iso_3166LV    0.5979523  0.0930551   6.426 1.31e-10 ***
    ## country_code_iso_3166MT   -0.1678659  0.1091070  -1.539 0.123916    
    ## country_code_iso_3166NL   -0.3297082  0.0886860  -3.718 0.000201 ***
    ## country_code_iso_3166PL    0.4036743  0.0905292   4.459 8.23e-06 ***
    ## country_code_iso_3166PT   -0.1685798  0.0889691  -1.895 0.058117 .  
    ## country_code_iso_3166RO    0.1193201  0.0894711   1.334 0.182329    
    ## country_code_iso_3166SE   -0.4270714  0.0893343  -4.781 1.75e-06 ***
    ## country_code_iso_3166SI   -0.0304578  0.0888536  -0.343 0.731759    
    ## country_code_iso_3166SK    0.6490991  0.0937713   6.922 4.45e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 37212  on 27338  degrees of freedom
    ## Residual deviance: 36404  on 27308  degrees of freedom
    ## AIC: 36466
    ## 
    ## Number of Fisher Scoring iterations: 4

A relatively good model takes the age, rural regions, coal regions and
countries.

``` r
summary ( glm ( eu_env_policy ~ age_exact + is_rural +
                  is_coal_region + 
                  country_code_iso_3166,
                data = eb18,
                family = 'binomial'))
```

    ## 
    ## Call:
    ## glm(formula = eu_env_policy ~ age_exact + is_rural + is_coal_region + 
    ##     country_code_iso_3166, family = "binomial", data = eb18)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7072  -1.2503   0.8463   1.0479   1.4809  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                0.021865   0.071348   0.306 0.759254    
    ## age_exact                  0.003283   0.000697   4.711 2.47e-06 ***
    ## is_rural                   0.124809   0.027114   4.603 4.16e-06 ***
    ## is_coal_region             0.152422   0.057169   2.666 0.007672 ** 
    ## country_code_iso_3166BE    0.171616   0.089216   1.924 0.054405 .  
    ## country_code_iso_3166BG    0.542022   0.091928   5.896 3.72e-09 ***
    ## country_code_iso_3166CY   -0.767716   0.111464  -6.888 5.68e-12 ***
    ## country_code_iso_3166CZ    0.692816   0.094624   7.322 2.45e-13 ***
    ## country_code_iso_3166DE-E  0.265159   0.115291   2.300 0.021453 *  
    ## country_code_iso_3166DE-W  0.123084   0.090874   1.354 0.175597    
    ## country_code_iso_3166DK   -0.027846   0.089699  -0.310 0.756228    
    ## country_code_iso_3166EE    0.707856   0.094179   7.516 5.65e-14 ***
    ## country_code_iso_3166ES   -0.461315   0.089319  -5.165 2.41e-07 ***
    ## country_code_iso_3166FI    0.103974   0.089947   1.156 0.247703    
    ## country_code_iso_3166FR    0.231058   0.089981   2.568 0.010233 *  
    ## country_code_iso_3166GB   -0.069278   0.089414  -0.775 0.438455    
    ## country_code_iso_3166GR    0.092173   0.089572   1.029 0.303461    
    ## country_code_iso_3166HR   -0.130298   0.088734  -1.468 0.141994    
    ## country_code_iso_3166HU   -0.133311   0.088734  -1.502 0.133002    
    ## country_code_iso_3166IE   -0.454947   0.089428  -5.087 3.63e-07 ***
    ## country_code_iso_3166IT    0.262491   0.090305   2.907 0.003652 ** 
    ## country_code_iso_3166LT    0.206298   0.090439   2.281 0.022544 *  
    ## country_code_iso_3166LU    0.173036   0.110517   1.566 0.117421    
    ## country_code_iso_3166LV    0.598943   0.093092   6.434 1.24e-10 ***
    ## country_code_iso_3166MT   -0.181764   0.109191  -1.665 0.095984 .  
    ## country_code_iso_3166NL   -0.327898   0.088715  -3.696 0.000219 ***
    ## country_code_iso_3166PL    0.342178   0.093014   3.679 0.000234 ***
    ## country_code_iso_3166PT   -0.176382   0.089092  -1.980 0.047728 *  
    ## country_code_iso_3166RO    0.100593   0.089719   1.121 0.262202    
    ## country_code_iso_3166SE   -0.385753   0.089787  -4.296 1.74e-05 ***
    ## country_code_iso_3166SI   -0.078249   0.089536  -0.874 0.382151    
    ## country_code_iso_3166SK    0.584199   0.096123   6.078 1.22e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 37187  on 27320  degrees of freedom
    ## Residual deviance: 36351  on 27289  degrees of freedom
    ##   (18 observations deleted due to missingness)
    ## AIC: 36415
    ## 
    ## Number of Fisher Scoring iterations: 4
