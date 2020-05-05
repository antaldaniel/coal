library(tidyverse)
library(fastDummies)
library(factoextra)

population <- eurostat::get_eurostat('demo_r_pjangrp3',
                                     time_format = 'num') %>%
  filter ( age %in% c("TOTAL", "Y_LT5",	"Y5-9", "Y10-14") ) %>%
  filter ( sex == "T") %>%
  filter ( time == 2018 ) %>%
  select ( geo, time, values, age ) %>%
  mutate ( age = tolower(as.character(age))) %>%
  mutate ( age = gsub("-", "_", age)) %>%
  pivot_wider ( names_from =  age, 
                values_from = values ) %>%
  mutate ( population_y_gt15 = total - y_lt5 - y5_9 - y10_14 ) %>%
  mutate_if ( is.factor, as.character ) %>%
  rename ( nuts3 = geo )

nuts_2_population <- population %>% 
  mutate ( nuts2 = str_sub(nuts3, 1, 4) ) %>%
  filter ( nchar(nuts2) == 4) %>%
  select ( nuts2, population_y_gt15  ) %>%
  group_by (nuts2) %>%
  summarize ( w2 = sum(population_y_gt15)) %>%
  ungroup()

sum(nuts_2_population$nuts2 %in% air_pollutants_csv$nuts2)

  
nuts_1_population <- population %>% 
  mutate ( nuts1 = str_sub(nuts3, 1, 3) ) %>%
  filter ( nchar(nuts1) == 3) %>%
  select ( nuts1, population_y_gt15  ) %>%
  group_by (nuts1) %>%
  summarize ( w1 = sum(population_y_gt15)) %>%
  ungroup()


air_pollutants_csv <- read_csv(file.path('data-raw',
                                     'air_pollutants_2016_all.csv')) %>%
  rename ( nuts3 = NUTS_ID) %>%
  mutate ( nuts2 = str_sub(nuts3,1,4)) %>%
  mutate ( nuts1 = str_sub(nuts3,1,3)) %>%
  left_join ( population %>%
                select ( nuts3, population_y_gt15), 
              by = "nuts3") %>%
  mutate ( country_code = regions::get_country_code (nuts3)) %>%
  select ( country_code, nuts1, nuts2, nuts3, 
           population_y_gt15, pm2_5, pm10, o3, BaP, so2 ) 

air_pollutants <- air_pollutants_csv %>%
  mutate_if (is.factor,as.character) %>%
  mutate_at (vars(c("pm2_5", "pm10", "o3", "BaP", "so2")), 
             ~ ./population_y_gt15) %>%
  select (-nuts3, -nuts1) %>%
  group_by (nuts2) %>%
  mutate_if ( is.numeric, sum ) %>%
  mutate_at (vars (c("pm2_5", "pm10", "o3", "BaP", "so2")),
             ~.*population_y_gt15)

air_pollutants_2 <- air_pollutants_csv %>%
  mutate_if (is.factor,as.character) %>%
  select ( -nuts1, -nuts3 ) %>%
  filter ( !is.na(population_y_gt15)) %>%
  mutate ( w2 = population_y_gt15 ) %>%
  group_by ( country_code, nuts2 )  %>%
  summarize_at(vars(c("pm2_5", "pm10", "o3", "BaP", "so2")), 
            funs(weighted.mean(., w2))) %>%
  ungroup() %>%
  rename ( region_nuts_codes = nuts2 )

air_pollutants_1 <- air_pollutants_csv %>%
  mutate_if (is.factor,as.character) %>%
  select ( -nuts2, -nuts3 ) %>%
  filter ( !is.na(population_y_gt15)) %>%
  mutate ( w1 = population_y_gt15 ) %>%
  group_by ( country_code, nuts1 )  %>%
  summarize_at(vars(c("pm2_5", "pm10", "o3", "BaP", "so2")), 
               funs(weighted.mean(., w1))) %>%
  ungroup() %>%
  rename ( region_nuts_codes = nuts1 )

air_pollutants_0 <- air_pollutants_csv %>%
  mutate_if (is.factor,as.character) %>%
  select ( -nuts2, -nuts3, -nuts1 ) %>%
  filter ( !is.na(population_y_gt15)) %>%
  mutate ( w0 = population_y_gt15 ) %>%
  group_by ( country_code )  %>%
  summarize_at(vars(c("pm2_5", "pm10", "o3", "BaP", "so2")), 
               funs(weighted.mean(., w0))) %>%
  ungroup() %>%
  mutate ( region_nuts_codes = country_code )

air_pollutants_3 <- air_pollutants_csv %>%
  mutate_if (is.factor,as.character) %>%
  select ( -nuts2,  -nuts1 )  %>%
  rename ( region_nuts_codes = nuts3 ) %>%
  select ( -population_y_gt15)

pollutant_names <- c('pm2_5', 'pm10', 'o3', 'BaP', 'so2')

air_pollutants <- bind_rows ( air_pollutants_0, air_pollutants_1, 
                              air_pollutants_2, air_pollutants_3) %>%
  select ( all_of(c("country_code", "region_nuts_codes", pollutant_names)))

pollutant.pca <- prcomp(air_pollutants[,-c(1:2)], scale = TRUE)
fviz_eig(pollutant.pca)

air_pollutants <- air_pollutants  %>%
  bind_cols( 
    pollutant.pca$x %>%
        as_tibble() %>%
        purrr::set_names ( ., paste0("ap_", tolower(names(.))))
  )

save ( air_pollutants, file = "data/air_pollutants.rda")

library(corrr)

air_pollutants %>%
  select( -all_of(c("country_code", "region_nuts_codes")), 
          -starts_with('ap')) %>%
  correlate() %>%
  rearrange() %>%
  shave() %>%
  rplot(shape = 15, colours = c("darkorange", "white", "darkcyan"))

#Only PM are correlated
              