library(tidyverse)
library(fastDummies)
library(factoextra)
eb19_raw <- readRDS( "data/eb19.rds") 

normalize_text <- function(x) {
  
  x <- as.character(x)
  x <- tolower(x)
  x <- str_trim(x, side = "both")
  x <- gsub("\\s", "_", x)
  x <- gsub(":|!|,|;|%","", x)
  x <- gsub("____|___|__", "_", x)
  x
}

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

names ( transition_policy)

transition_policy_pca <- prcomp(transition_policy [,c(25:29)], scale = TRUE)
fviz_eig(transition_policy_pca)

transition_policy <- transition_policy  %>%
  bind_cols( 
    transition_policy_pca$x %>%
      as_tibble() %>%
      purrr::set_names ( ., paste0("tp_", tolower(names(.))))
  )

eb19_df  <- transition_policy %>% 
  left_join ( air_pollutants ) %>%
  left_join ( demography )

names ( eb19_df )
summary( glm ( transition_policy_totally_agree ~ pm10 + so2 + age_exact +
                 is_highly_educated + is_rural, 
               data = eb19_df, 
      family = binomial ))



summary( lm ( tp_pc4 ~ pm10 + so2, 
               data = eb19_df))

library(corrr)

eb19_df %>%
  select( all_of(c("transition_policy_totally_agree" ,
                   "pm10", "so2", "age_exact", 
                   "is_highly_educated" , "is_rural"))) %>%
  correlate() %>%
  rearrange() %>%
  shave() %>%
  rplot(shape = 15, colours = c("darkorange", "white", "darkcyan"))



