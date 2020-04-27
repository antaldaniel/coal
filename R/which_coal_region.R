which_coal_region <- function(region_nuts_codes) {
  dplyr::case_when (
    region_nuts_codes == "IE012"  ~ "Midland",
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
  )
}
