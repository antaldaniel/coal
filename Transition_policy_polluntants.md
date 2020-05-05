Simple Analysis Of Policy Agreement & Air Pollutants
================
Daniel Antal, CFA
5/5/2020

  - [Data Preparation](#data-preparation)
  - [PCA](#pca)
  - [Agreement](#agreement)
  - [Disagreement](#disagreement)

## Data Preparation

``` r
transition_policy <- eb19_raw %>%
  rowid_to_column() %>%
  mutate ( transition_policy = normalize_text(transition_policy)) %>%
  fastDummies::dummy_cols(select_columns = 'transition_policy') %>%
  mutate ( transition_policy_agree = case_when(
    transition_policy_totally_agree + transition_policy_tend_to_agree > 0 ~ 1, 
    TRUE ~ 0
  )) %>%
  mutate ( transition_policy_disagree = case_when(
    transition_policy_totally_disagree + transition_policy_tend_to_disagree > 0 ~ 1, 
    TRUE ~ 0
  )) 
```

## PCA

If later needed, calculate the principal components:

![](Transition_policy_polluntants_files/figure-gfm/pca-1.png)<!-- -->

``` r
eb19_df  <- transition_policy %>% 
  left_join ( air_pollutants, by = 'region_nuts_codes' ) %>%
  mutate ( is_poland = ifelse ( country_code == "PL", 1, 0))
```

## Agreement

Significantly more people agree where - there are more polutants - who
are younger - more educated - in many countries, including from V4
Hungary

Significantly less people agree - in rural areas - older - less educated
- less polluted - coal regions - Belgium (coal), Bulgaria, Croatia,
Czechia, Estonia, Italy, Lativa, Luxembourg (coal), Finland, Poland,
Romania, Slovakia

``` r
c("transition_policy_totally_agree" , "pm10", "so2", "age_exact", "is_highly_educated" , "is_rural")
```

    ## [1] "transition_policy_totally_agree" "pm10"                           
    ## [3] "so2"                             "age_exact"                      
    ## [5] "is_highly_educated"              "is_rural"

``` r
summary( glm ( transition_policy_totally_agree ~ pm10 + so2 + 
                 age_exact +
                 is_highly_educated + is_rural + is_coal_region +
                 country_code, 
               data = eb19_df, 
               family = binomial ))
```

    ## 
    ## Call:
    ## glm(formula = transition_policy_totally_agree ~ pm10 + so2 + 
    ##     age_exact + is_highly_educated + is_rural + is_coal_region + 
    ##     country_code, family = binomial, data = eb19_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7690  -1.0253  -0.8165   1.2264   1.9085  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -0.1975096  0.0921551  -2.143 0.032095 *  
    ## pm10                0.0068505  0.0017445   3.927 8.60e-05 ***
    ## so2                 0.1381994  0.0405867   3.405 0.000662 ***
    ## age_exact          -0.0075018  0.0007873  -9.529  < 2e-16 ***
    ## is_highly_educated  0.2953905  0.0311127   9.494  < 2e-16 ***
    ## is_rural           -0.1277983  0.0313321  -4.079 4.53e-05 ***
    ## is_coal_region     -0.2624005  0.0640233  -4.099 4.16e-05 ***
    ## country_codeBE     -0.3290891  0.0916117  -3.592 0.000328 ***
    ## country_codeBG     -0.6470116  0.1125114  -5.751 8.89e-09 ***
    ## country_codeCY      0.8471483  0.1273306   6.653 2.87e-11 ***
    ## country_codeCZ     -0.5754008  0.0965974  -5.957 2.57e-09 ***
    ## country_codeDE      0.0106430  0.0856322   0.124 0.901088    
    ## country_codeDK      0.0577724  0.0925391   0.624 0.532429    
    ## country_codeEE     -0.8041188  0.0989047  -8.130 4.28e-16 ***
    ## country_codeES      1.1266903  0.0941495  11.967  < 2e-16 ***
    ## country_codeFI     -0.2617501  0.0946837  -2.764 0.005702 ** 
    ## country_codeFR      0.0130239  0.1639339   0.079 0.936678    
    ## country_codeGB      0.2454631  0.0891845   2.752 0.005918 ** 
    ## country_codeGR      0.2169278  0.1209199   1.794 0.072816 .  
    ## country_codeHR     -0.1632727  0.1001563  -1.630 0.103064    
    ## country_codeHU      0.5779928  0.1020987   5.661 1.50e-08 ***
    ## country_codeIT     -0.1427249  0.0940144  -1.518 0.128985    
    ## country_codeLU     -0.3111627  0.1140426  -2.728 0.006363 ** 
    ## country_codeLV     -0.6246590  0.0963526  -6.483 8.99e-11 ***
    ## country_codeMT      0.3303363  0.1228611   2.689 0.007173 ** 
    ## country_codeNL      0.1707080  0.0902189   1.892 0.058470 .  
    ## country_codePL     -0.2843198  0.1228657  -2.314 0.020664 *  
    ## country_codePT      0.1447295  0.0899079   1.610 0.107452    
    ## country_codeRO     -0.0479674  0.0930433  -0.516 0.606177    
    ## country_codeSE      0.4865939  0.0922486   5.275 1.33e-07 ***
    ## country_codeSK     -0.2427307  0.0964652  -2.516 0.011861 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 30568  on 22401  degrees of freedom
    ## Residual deviance: 29313  on 22371  degrees of freedom
    ##   (5253 observations deleted due to missingness)
    ## AIC: 29375
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
summary( glm ( transition_policy_agree ~ pm10 + so2 + age_exact +
                 is_highly_educated + is_rural, 
               data = eb19_df, 
               family = binomial ))
```

    ## 
    ## Call:
    ## glm(formula = transition_policy_agree ~ pm10 + so2 + age_exact + 
    ##     is_highly_educated + is_rural, family = binomial, data = eb19_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1970   0.5035   0.5803   0.6495   0.8465  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         1.807823   0.079297  22.798  < 2e-16 ***
    ## pm10                0.005092   0.001239   4.108 3.99e-05 ***
    ## so2                 0.003274   0.051410   0.064  0.94922    
    ## age_exact          -0.009781   0.000988  -9.900  < 2e-16 ***
    ## is_highly_educated  0.396743   0.039735   9.985  < 2e-16 ***
    ## is_rural           -0.107448   0.037953  -2.831  0.00464 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 20488  on 22401  degrees of freedom
    ## Residual deviance: 20250  on 22396  degrees of freedom
    ##   (5253 observations deleted due to missingness)
    ## AIC: 20262
    ## 
    ## Number of Fisher Scoring iterations: 4

## Disagreement

The disagreement is consistent with the agreement variables.

Explicit disagreement is higher when - there is coal - less pollutants

Polish people are significantly less likely to agree. Disagreement is
more difficult to model, because the vast majority of the people tend to
agree.

``` r
summary( glm ( transition_policy_totally_disagree ~ 
                 pm10 + so2  + is_coal_region,  
               data = eb19_df,
               family = binomial ))
```

    ## 
    ## Call:
    ## glm(formula = transition_policy_totally_disagree ~ pm10 + so2 + 
    ##     is_coal_region, family = binomial, data = eb19_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.2720  -0.2129  -0.2102  -0.2012   3.1684  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -3.636131   0.129608 -28.055  < 2e-16 ***
    ## pm10           -0.005208   0.003261  -1.597 0.110258    
    ## so2            -0.529464   0.310996  -1.702 0.088665 .  
    ## is_coal_region  0.487126   0.142218   3.425 0.000614 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4715.7  on 22412  degrees of freedom
    ## Residual deviance: 4696.2  on 22409  degrees of freedom
    ##   (5242 observations deleted due to missingness)
    ## AIC: 4704.2
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
summary( glm ( transition_policy_disagree ~ pm10 + so2 + is_coal_region +
                 is_poland, 
               data = eb19_df, 
               family = binomial ))
```

    ## 
    ## Call:
    ## glm(formula = transition_policy_disagree ~ pm10 + so2 + is_coal_region + 
    ##     is_poland, family = binomial, data = eb19_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5095  -0.4533  -0.4410  -0.4037   2.8323  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    -1.978855   0.067070 -29.504  < 2e-16 ***
    ## pm10           -0.008933   0.001725  -5.178 2.24e-07 ***
    ## so2            -0.348535   0.118776  -2.934  0.00334 ** 
    ## is_coal_region  0.224955   0.084672   2.657  0.00789 ** 
    ## is_poland       0.250349   0.144970   1.727  0.08418 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 13662  on 22412  degrees of freedom
    ## Residual deviance: 13605  on 22408  degrees of freedom
    ##   (5242 observations deleted due to missingness)
    ## AIC: 13615
    ## 
    ## Number of Fisher Scoring iterations: 6
