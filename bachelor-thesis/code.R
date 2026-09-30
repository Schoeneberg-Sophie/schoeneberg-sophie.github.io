################################################################################
#                                                                              #
#                               Bachelor Thesis                                #
#           Security Dilemma in Space:  Quantitative Analysis of               #
#             Worldwide Procurement of Dual-Use Satellites                     #
#                                                                              #
#                         Autor: Sophie Schöneberg                             #
#                            INFORMATION REDACTED                              #
#                                                                              #
################################################################################


library(tidyverse)
library(readr)
library(nlme)
library(purrr)
library(scales)
library(modelsummary)
library(ggplot2)
library(strucchange)
library(lubridate)
library(hrbrthemes)
library(ggrepel)
library(plm)
library(lmtest)
library(dplyr)
library(countrycode)
library(panelvar)
library(tidyr)
library(readxl)

# load dataset
satcat <- read_csv("satcat.csv")


payload_growth <- satcat %>%
  mutate(
    launch_date_fixed = as.Date(LAUNCH_DATE),
    launch_year_pay = year(launch_date_fixed)
  ) %>%
  filter(OBJECT_TYPE == "PAY") %>%
  filter(launch_year_pay >= 1957 & launch_year_pay <= 2025) %>%
  group_by(launch_year_pay) %>%
  summarise(payload_count = n())
payload_plot <- ggplot(payload_growth, aes(x = launch_year_pay, y = payload_count)) +
  theme_ipsum_rc(grid="Y") +
  geom_area(fill = "#2e86c1", alpha = 0.2) + 
  geom_line(color = "#1b4f72", size = 1) +
  labs(
    title = "Annual Functional Payload Launches (1957 - 2025)",
    subtitle = "Yearly aggregated count of payload objects",
    x = "Year of Launch",
    y = "Number of Payloads",
    caption = "Source:CelesTrak Satellite Catalogue."
  ) +
  scale_x_continuous(breaks = seq(1950, 2030, by = 10)) +
  theme(
    plot.title = element_text(color = "black", face = "bold"),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )
print(payload_plot)
ggsave("payload_plot.png", payload_plot, width = 11, height = 7, dpi = 600)


satcat <- satcat %>% 
  filter(OBJECT_TYPE == "PAY") 
satcat <- satcat %>% 
  filter(LAUNCH_DATE >= as.Date("2000-01-01") & 
           LAUNCH_DATE <= as.Date("2025-12-31"))

# CREATING THE DATASET
################################################################################


year_range <- as.character(2000:2025)
satcat[, year_range] <- NA_real_
satcat$`dual_use_country` <- NA
View(satcat)


update_satcat <- function(df, sat_name, country, start_year) {
  clean_input <- trimws(gsub("[[:space:]]", " ", sat_name))
  
  row_idx <- which(toupper(df$name) == toupper(clean_input))
  
  if (length(row_idx) == 0) {
    row_idx <- which(grepl(toupper(clean_input), toupper(df$name), fixed = TRUE))
  }
  
  if (length(row_idx) == 0) {
    message(paste("Satellite", sat_name, "still not found. Check spelling."))
    return(df)
  }
  df[row_idx, "dual_use_country"] <- country
  if (start_year <= 2025) {
    target_years <- as.character(start_year:2025)
    df[row_idx, target_years] <- 1
    df[row_idx, as.character(2000:(start_year-1))] <- 0
  }
  return(df)
}
satcat <- update_satcat(satcat, "NUSAT-1 (FRESCO)", "Brazil", 2025)

#-------------------------------------------------------------------------------
colnames(satcat)[colnames(satcat) == "OBJECT_NAME"] <- "name"
update_satcat <- function(df, sat_name, country, start_year) {
  target_name <- toupper(sat_name)
  row_idx <- which(grepl(target_name, df$name))
  if (length(row_idx) == 0) {
    message(paste("Satellite", sat_name, "not found. Check for typos."))
    return(df)
  }
  df[row_idx, "dual_use_country"] <- country
  if (start_year <= 2025) {
    target_years <- as.character(start_year:2025)
    df[row_idx, target_years] <- 1
    pre_years <- as.character(2000:(start_year - 1))
    df[row_idx, pre_years] <- 0
  } else {
    df[row_idx, as.character(2000:2025)] <- 0
  }
  return(df)
}

satcat <- update_satcat(satcat, "ARABSAT-6A", "Saudi Arabia", 2019)
satcat <- update_satcat(satcat, "ARABSAT-7B.+(BADR-8)", "Saudi Arabia", 2023)
satcat <- update_satcat(satcat, "ES'HAIL 1", "Qatar", 2013)
satcat <- update_satcat(satcat, "ES'HAIL 2", "Qatar", 2018)
satcat <- update_satcat(satcat, "NUSAT-1.+(FRESCO)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-10.+(CAROLINE)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-11.+(CORA)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-12.+(DOROTHY)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-13.+(EMMY)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-14.+(HEDY)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-15.+(KATHERINE)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-16.+(LISE)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-17.+(MARY)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-18.+(VERA)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-19.+(ROSALIND)", "Brazil", 2025)
satcat <- update_satcat(satcat, "SNUSAT-2", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-20.+(GRACE)", "Brazil", 2025)
satcat <- update_satcat(satcat, "GEO-IK 2", "Russia",2011 )
satcat <- update_satcat(satcat, "NUSAT-21.+(ELISA)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-22.+(SOFYA)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-23.+(ANNIE MAUNDER)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-24.+(KALPANA CHAWLA)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-25.+(MARIA TELKES)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-26.+(M SOMERVILLE)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-27.+(SALLY RIDE)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-28.+(ALICE LEE)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-29.+(EDITH CLARKE)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-30.+(MARGHERITA)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-3.+(MILANESAT)", "Brazil", 2025)
satcat <- update_satcat(satcat, "NUSAT-32.+(ALBANIA-1)", "Albania", 2022)
satcat <- update_satcat(satcat, "NUSAT-33.+(ALBANIA-2)", "Albania", 2022)
update_satcat_batch <- function(df, sat_name, country, start_year) {
  clean_input <- trimws(gsub(",$", "", sat_name))
  if (grepl("\\(", clean_input) && !grepl("\\)", clean_input)) {
    clean_input <- paste0(clean_input, ")")
  }
  target <- toupper(clean_input)
  row_idx <- which(toupper(df$name) == target)
  if (length(row_idx) == 0) {
    row_idx <- which(grepl(target, toupper(df$name), fixed = TRUE))
  }
  if (length(row_idx) > 0) {
    df[row_idx, "dual_use_country"] <- country
    target_years <- as.character(start_year:2025)
    df[row_idx, target_years] <- 1
    pre_years <- as.character(2000:(start_year-1))
    df[row_idx, pre_years] <- 0
  } else {
    message(paste("Not found:", clean_input))
  }
  return(df)
}
brazil_sats <- list(
  "NUSAT-31 (RUBY PAYNE-S)", "NUSAT-34 (AMELIA EARHART)", "NUSAT-35 (WILLIAMINA)",
  "NUSAT-36 (ANNIE CANNON)", "NUSAT-37 (JOAN CLARKE)", "NUSAT-38 (MARIA AGNESI)",
  "NUSAT-39 (TIKVAH ALPER)", "NUSAT-4 (ADA)", "NUSAT-40 (C SHOEMAKER)",
  "NUSAT-41 (C PAYNE-G)", "NUSAT-42 (M WONENBURGER)", "NUSAT-43 (R DIENG-KUNTZ)",
  "NUSAT-44 (MARIA MITCHELL)", "NUSAT-45 (UZMASAT-1)", "NUSAT-47",
  "NUSAT-48 (HENRIETTA LEAVITT)", "NUSAT-49 (KLARA DAN VON NEUMANN)",
  "NUSAT-5 (MARYAM)", "NUSAT-50 (NANCY ROMAN)", "NUSAT-51 (YVONNE BRILL)",
  "NUSAT-52", "NUSAT-6 (HYPATIA)", "NUSAT-7 (SOPHIE)", "NUSAT-8 (MARIE)",
  "NUSAT-9 (ALICE)")
for (s in brazil_sats) {
  satcat <- update_satcat_batch(satcat, s, "Brazil", 2025)
}
satcat <- update_satcat(satcat, "OPTUS C1", "Australia", 2003)
satcat <- update_satcat_batch(satcat, "OPTUS-X (ADS-01)", "Australia", 2024)
satcat <- update_satcat_batch(satcat, "CARCARA 1 (ICEYE-X18)", "Brazil", 2022)
satcat <- update_satcat_batch(satcat, "CARCARA 2 (ICEYE-X19)", "Brazil", 2022)
satcat <- update_satcat(satcat, "RADARSAT-2", "Canada", 2004)
satcat <- update_satcat(satcat, "RCM-1", "Canada", 2019)
satcat <- update_satcat(satcat, "RCM-2", "Canada", 2019)
satcat <- update_satcat(satcat, "RCM-3", "Canada", 2019)
satcat <- update_satcat(satcat, "ZORKIY-2M", "Russia", 2023)
satcat <- update_satcat(satcat, "SITRO-AIS", "Russia", 2023)
satcat <- update_satcat(satcat, "EXPRESS-AM3", "Russia", 2013)
satcat <- update_satcat(satcat, "EXPRESS-AM7", "Russia", 2013)
satcat <- update_satcat(satcat, "YAMAL 401", "Russia", 2009)
satcat <- update_satcat(satcat, "YAMAL 402", "Russia", 2009)
satcat <- update_satcat(satcat, "GONETS-M", "Russia", 2017)
satcat <- update_satcat(satcat, "CBERS 4A", "China", 2019)
satcat <- update_satcat(satcat, "Luch.+(Olymp-K)", "Russia", 2012)
satcat <- update_satcat(satcat, "FACSAT-1", "Colombia", 2014)
satcat <- update_satcat(satcat, "FACSAT-2", "Colombia", 2020)
satcat <- update_satcat(satcat, "VZLUSAT-2", "Czech Republic", 2021)
satcat <- update_satcat(satcat, "EDRS-C", "European Space Agency", 2016)
satcat <- update_satcat(satcat, "TIBA-1", "Egypt", 2016)
satcat <- update_satcat(satcat, "ICEYE", "Sweden", 2026)
satcat <- update_satcat(satcat, "GOMX-4A", "Denmark", 2016)
satcat <- update_satcat(satcat, "EGYPTSAT 2", "Egypt", 2009)

satcat <- update_satcat(satcat, "BRO.+1", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+2", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+3", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+4", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+5", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+6", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+7", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+8", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+9", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+10", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+11", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+12", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+13", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+14", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+15", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+16", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+17", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+18", "France", 2019)
satcat <- update_satcat(satcat, "BRO.+20", "France", 2019)

satcat <- update_satcat(satcat, "ICEYE-X1", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X2", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X3", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X4", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X5", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X6", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X7", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X8", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X9", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X10", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X11", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X12", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X13", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X14", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X15", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X16", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X17", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X18", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X19", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X20", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X21", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X23", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X24", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X25", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X26", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X27", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X30", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X31", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X32", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X33", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X34", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X35", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X36", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X37", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X38", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X39", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X40", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X41", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X42", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X43", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X44", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X45", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X46", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X47", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X48", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X49", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X50", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X51", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X52", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X53", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X54", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X55", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X56", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X57", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X58", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X59", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X60", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X61", "Germany", 2025)
satcat <- update_satcat(satcat, "ICEYE-X62", "Germany", 2025)

satcat <- update_satcat(satcat,"PLEIADES NEO 3", "France", 2021) 
satcat <- update_satcat(satcat,"PLEIADES NEO 4", "France", 2021) 

satcat <- update_satcat(satcat,"CO3D 1", "France", 2019) 
satcat <- update_satcat(satcat,"CO3D 2", "France", 2019) 
satcat <- update_satcat(satcat,"CO3D 3", "France", 2019) 
satcat <- update_satcat(satcat,"CO3D 4", "France", 2019) 

satcat <- update_satcat(satcat,"TERRASAR-X", "Germany", 2022)
satcat <- update_satcat(satcat, "TANDEM-X", "Germany", 2022)
satcat <- update_satcat(satcat, "RAPIDEYE 1", "Germany", 2022)
satcat <- update_satcat(satcat, "RAPIDEYE 2", "Germany", 2022)
satcat <- update_satcat(satcat, "RAPIDEYE 3", "Germany", 2022)
satcat <- update_satcat(satcat, "RAPIDEYE 4", "Germany", 2022)
satcat <- update_satcat(satcat, "RAPIDEYE 5", "Germany", 2022)
satcat <- update_satcat(satcat, "SARAH-1", "Germany", 2022)
satcat <- update_satcat(satcat, "SARAH-2", "Germany", 2022)
satcat <- update_satcat(satcat, "SARAH-3", "Germany", 2022)

satcat <- update_satcat(satcat, "HELLAS-SAT 4 & SGS-1", "Saudi Arabia", 2015)
satcat <- update_satcat(satcat, "HELLAS-SAT 2", "Greece", 2001)
satcat <- update_satcat(satcat, "INMARSAT 5-F1", "USA", 2022)
satcat <- update_satcat(satcat, "INMARSAT 5-F2", "USA", 2022)
satcat <- update_satcat(satcat, "INMARSAT 5-F3", "USA", 2022)
satcat <- update_satcat(satcat, "INMARSAT 5-F4", "USA", 2022)
satcat <- update_satcat(satcat, "INMARSAT GX5", "USA", 2022)

# note not all Globalstar sats are dual-use
satcat <- update_satcat(satcat, "GLOBALSTAR M060", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M062", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M063", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M064", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M065", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M066", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M067", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M068", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M069", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M070", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M071", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M072", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M073", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M074", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M075", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M076", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M077", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M078", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M079", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M080", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M081", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M082", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M083", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M084", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M085", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M086", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M087", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M088", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M089", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M090", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M091", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M092", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M093", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M094", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M095", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M096", "USA", 2025)
satcat <- update_satcat(satcat, "GLOBALSTAR M097", "USA", 2025)

satcat <- update_satcat(satcat, "TSAT-1A", "India", 2023)
satcat <- update_satcat(satcat, "SHAKUNTALA", "India", 2024)
satcat <- update_satcat(satcat, "MERAH PUTIH 2", "Indonesia", 2025)
satcat <- update_satcat(satcat, "KOWSAR 1.5", "Iran", 2024)
satcat <- update_satcat(satcat, "EROS.+A1", "Israel", 2001)
satcat[grepl("EROS.+A1", satcat$name, ignore.case = TRUE), "2000"] <- 1


satcat <- update_satcat(satcat, "OPTSAT-3000", "Italy", 2012)
satcat <- update_satcat(satcat, "DSN-2", "Japan", 2013)
satcat <- update_satcat(satcat, "DSN-3", "Japan", 2013)
satcat <- update_satcat(satcat, "KAZSAT-3", "Kazakhstan", 2025)
satcat <- update_satcat(satcat, "MP42", "United Kingdom", 2022)
satcat <- update_satcat(satcat, "M6P", "United Kingdom", 2022)
satcat <- update_satcat(satcat, "O3B MPOWER F1", "USA", 2023)
satcat <- update_satcat(satcat, "O3B MPOWER F2", "USA", 2023)
satcat <- update_satcat(satcat, "O3B MPOWER F3", "USA", 2023)
satcat <- update_satcat(satcat, "O3B MPOWER F4", "USA", 2023)
satcat <- update_satcat(satcat, "O3B MPOWER F5", "USA", 2023)
satcat <- update_satcat(satcat, "O3B MPOWER F6", "USA", 2023)
satcat <- update_satcat(satcat, "O3B MPOWER F7", "USA", 2023)
satcat <- update_satcat(satcat, "O3B MPOWER F8", "USA", 2023)
satcat <- update_satcat(satcat, "O3B MPOWER F9", "USA", 2023)
satcat <- update_satcat(satcat, "O3B MPOWER F10", "USA", 2023)

satcat <- update_satcat(satcat, "MEASAT-3", "Malaysia", 2025)
satcat <- update_satcat(satcat, "MEASAT-3A", "Malaysia", 2025)
satcat <- update_satcat(satcat, "MEASAT-3B", "Malaysia", 2025)
satcat <- update_satcat(satcat, "MEASAT 3D", "Malaysia", 2025)
satcat <- update_satcat(satcat, "BRIK-II", "Netherlands", 2017)
satcat <- update_satcat(satcat, "O3B", "USA", 2023)

satcat <- update_satcat(satcat, "PIAST-M", "Poland", 2025)
satcat <- update_satcat(satcat, "PIAST-S1", "Poland", 2025)
satcat <- update_satcat(satcat, "PIAST-S2", "Poland", 2025)
satcat <- update_satcat(satcat, "PERUSAT 1", "Peru", 2014)
satcat <- update_satcat(satcat, "POSAT-2", "Portugal", 2025)
satcat <- update_satcat(satcat, "JILIN-1", "China", 2021)
satcat <- update_satcat(satcat, "APSTAR-6", "China", 2018)
satcat <- update_satcat(satcat, "TIANQI", "China", 2023)
satcat <- update_satcat(satcat, "XINGYUN-2", "China", 2020)

satcat <- update_satcat(satcat, "PIAST-M", "Poland", 2025)
satcat <- update_satcat(satcat, "SAUDISAT 5A", "Saudi Arabia", 2018)
satcat <- update_satcat(satcat, "SAUDISAT 5B", "Saudi Arabia", 2018)
satcat <- update_satcat(satcat, "SES-16", "USA", 2025)
satcat <- update_satcat(satcat, "TELEOS-2", "Singapore", 2023)
satcat <- update_satcat(satcat, "KOREASAT 5", "South Korea", 2006)
satcat <- update_satcat(satcat, "KOMPSAT-5", "South Korea", 2013)

satcat <- update_satcat(satcat, "SPAINSAT NG I", "Spain", 2019)
satcat <- update_satcat(satcat, "SPAINSAT NG II", "Spain", 2019)
satcat <- update_satcat(satcat, "PAZ", "Spain", 2007)
satcat <- update_satcat(satcat, "OVZON-3", "Sweden", 2024)

satcat <- update_satcat(satcat, "THAICOM 4", "Thailand", 2021)
satcat <- update_satcat(satcat, "THAICOM 6", "Thailand", 2021)
satcat <- update_satcat(satcat, "THAICOM 8", "Thailand", 2021)
satcat <- update_satcat(satcat, "TURKMENALEM52E/MONACOSAT", "Turkmenistan", 2011)
satcat <- update_satcat(satcat, "GOKTURK 1A", "Turkey", 2009)

satcat <- update_satcat(satcat, "YAHSAT 1A", "United Arab Emirates", 2008)
satcat <- update_satcat(satcat, "YAHSAT 1B", "United Arab Emirates", 2008)
satcat <- update_satcat(satcat, "FALCON EYE 2", "United Arab Emirates", 2014)

satcat <- update_satcat(satcat, "CAPELLA-", "USA", 2022)
satcat <- update_satcat(satcat, "IRIDIUM", "USA", 2019)
satcat <- update_satcat(satcat, "SKYSAT-C", "USA", 2022)
satcat <- update_satcat(satcat, "BLACKJACK", "USA", 2020)
satcat <- update_satcat(satcat, "STARLINK", "USA", 2023)
satcat <- update_satcat(satcat, "UMBRA-0", "USA", 2022)
satcat <- update_satcat(satcat, "UMBRA-10", "USA", 2022)
satcat <- update_satcat(satcat, "UMBRA-11", "USA", 2022)
satcat <- update_satcat(satcat, "VIASAT", "USA", 2024)
satcat <- update_satcat(satcat, "TOMORROW", "USA", 2023)
satcat <- update_satcat(satcat, "UMBRA-10", "USA", 2022)
satcat <- update_satcat(satcat, "COSMO-SkyMed", "Italy", 2017)
satcat <- update_satcat(satcat, "Beidou", "China", 2003)

#-------------------------------------------------------------------------------

plot_data_truncated <- satcat %>%
  mutate(
    launch_year = as.numeric(format(as.Date(LAUNCH_DATE), "%Y")),
    is_dual = !is.na(`dual_use_country`)
  ) %>%
  filter(!is.na(launch_year) & launch_year >= 1990 & launch_year <= 2025) %>%
  group_by(launch_year) %>%
  summarise(
    new_total = n(),
    new_dual = sum(is_dual),
    .groups = "drop"
  ) %>%
  complete(launch_year = 1990:2025, fill = list(new_total = 0, new_dual = 0)) %>%
  arrange(launch_year) %>%
  mutate(
    cum_total = cumsum(new_total),
    cum_dual = cumsum(new_dual),
    share_pct = (cum_dual / cum_total) * 100
  ) %>%
  filter(launch_year >= 2000)

max_y <- max(plot_data_truncated$cum_total, na.rm = TRUE)
scaling_factor <- max_y / 100

plot_panel <- ggplot(plot_data_truncated, aes(x = launch_year)) +
  geom_area(aes(y = cum_total, fill = "All Satellites"), color = "black", size = 0.2, alpha = 0.15) +
  geom_area(aes(y = cum_dual, fill = "Dual-Use Satellites (Absolute)"), color = "black", size = 0.2, alpha = 0.4) +
  
  geom_line(aes(y = share_pct * scaling_factor, linetype = "Dual-Use Satellites (Relative)"), color = "black", size = 0.8) +
  geom_point(aes(y = share_pct * scaling_factor), color = "black", size = 1) +
  
  scale_y_continuous(
    name = "Cumulative Satellite Count",
    labels = scales::comma, 
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / scaling_factor, 
                        name = "Proportion of Dual-Use Assets (%)", 
                        breaks = seq(0, 100, 20))
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(2000, 2025, 5),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c("All Satellites" = "gray80", "Dual-Use Satellites (Absolute)" = "gray40")) +
  scale_linetype_manual(values = c("Dual-Use Satellites (Relative)" = "solid")) +
  
  theme_bw() +
  theme(
    text = element_text(family = "serif", color = "black"), 
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Longitudinal Analysis of Global Satellites and Dual-Use Proportionality",
    subtitle = "Yearly aggregated data (2000–2025). Absolute counts and relative dual-use shares.",
    caption = "Calculations include baseline payloads launched post-1990. Redundancy and decay are not modelled.",
    fill = NULL,      
    linetype = NULL   
  )
plot_panel
ggsave("plot_panel.png", plot_panel,  width = 10, height = 5, dpi = 80)
#-------------------------------------------------------------------------------
satcat$`military` <- 0

satcat$military[grepl("BUCCANEER MM", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("BUCCANEER RMM", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("M2-A", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("M2-B", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("RAAF M1", satcat$name, ignore.case = TRUE)] <- 1

satcat$military[grepl("MERIDIAN", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SSOT", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SAPPHIRE", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("GLONASS", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("LUCH", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("FACSAT", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("CERES", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("HELIOS", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("CSO", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SYRACUSE", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SICRAL", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("COMSATBW", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("ERNST", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SAR-LUPE", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("EMISAT", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("\\bGSAT-7\\b", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("RISAT-2BR1", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SINE (SINDHUNETRA)", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("NOOR-1", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("NOOR-2", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("OFEQ", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("TECSAR", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SICRAL-1B", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("KIRAMEKI-2", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("IGS", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("MALLIGYONG-1", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("ARVAKER", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("BIRKELAND", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("PRSS-1", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("LKW", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("TIANHUI", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("TIANLIN", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("FENGYUN", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("GAOFEN", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("YAOGAN", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("JILIN", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("HONGYAN", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("ZHONGXING", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("KONDOR-E", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("TELEOS-2", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("KORSAT-7", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SPAINSAT NG", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("TYCHE", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("NAPA-1", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("NAPA-2", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SKYNET", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("AEHF", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("NAVSTAR", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("DSP", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("MILSTAR", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("SIBRIS GEO", satcat$name, ignore.case = TRUE)] <- 1
satcat$military[grepl("WGS F", satcat$name, ignore.case = TRUE)] <- 1

#-------------------------------------------------------------------------------
####################### regression estimates #######################


################################################################################
# HYPOTHESIS 1
################################################################################

dyad_geopolitical_scores_Major24xAll <- read_csv("dyad_geopolitical_scores_Major24xAll.csv")

V_Dem_CY_Core_v16 <- read_csv("V-Dem-CY-Core-v16.csv")
SIPRI_Milex_data_1949_2025_v1_2 <- read_excel("SIPRI-Milex-data-1949-2025_v1.2.xlsx", 
                                              sheet = "Constant (2024) US$", skip = 5)


country_lookup <- c(
  "USA" = "USA",
  "United States" = "USA",
  "China" = "CHN",
  "Russia" = "RUS",
  "Israel" = "ISR",
  "South Korea" = "KOR",
  "South Korea" = "KOR",   
  "Korea, South" = "KOR",
  "North Korea" = "PRK",
  "China/Brazil" = "CHN",  
  "USA/Brazil" = "USA",
  "United Arab Emirates" = "ARE",
  "Germany" = "DEU",
  "Italy" = "ITA",
  "United Kingdom" = "GBR",
  "Japan" = "JPN",
  "Australia" = "AUS",
  "Thailand" = "THA",
  "France" = "FRA",
  "Canada" = "CAN",
  "India" = "IND",
  "Brazil" = "BRA",
  "Turkey" = "TUR",
  "Spain" = "ESP",
  "Greece" = "GRC",
  "Egypt" = "EGY",
  "Saudi Arabia" = "SAU",
  "Netherlands" = "NLD",
  "Sweden" = "SWE",
  "Indonesia" = "IDN",
  "Portugal" = "PRT",
  "Poland" = "POL",
  "Iran" = "IRN",
  "Malaysia" = "MYS",
  "Norway" = "NOR",
  "Ukraine" = "UKR",
  "Denmark" = "DNK",
  "Czech Republic" = "CZE",
  "Singapore" = "SGP",
  "Kazakhstan" = "KAZ",
  "Turkmenistan" = "TKM"
)


master_lookup <- c(
  "Russia" = "RUS", "CIS" = "RUS", "USSR" = "RUS", "Belarus" = "BLR", "Ukraine" = "UKR",
  "United States" = "USA", "United States of America" = "USA", "USA" = "USA", "USA/Brazil" = "USA",
  "Canada" = "CAN", "Mexico" = "MEX", "Brazil" = "BRA", "Argentina" = "ARG", 
  "Chile" = "CHL", "Colombia" = "COL", "Peru" = "PER", "Venezuela" = "VEN", 
  "Ecuador" = "ECU", "Bolivia" = "BOL", "Uruguay" = "URY",
  "China" = "CHN", "China/Brazil" = "CHN", "Taiwan" = "TWN",
  "Japan" = "JPN", "Singapore/Japan" = "JPN",
  "South Korea" = "KOR", "South Korea" = "KOR", "Korea, South" = "KOR", "Singapore/Taiwan (ST-1)" = "KOR",
  "North Korea" = "PRK", "Korea, North" = "PRK",
  "India" = "IND", "Pakistan" = "PAK", "Bangladesh" = "BGD",
  "Indonesia" = "IDN", "Malaysia" = "MYS", "Thailand" = "THA", "Thaicom" = "THA",
  "Vietnam" = "VNM", "Viet Nam" = "VNM", "Philippines" = "PHL", "Singapore" = "SGP",
  "Australia" = "AUS", "New Zealand" = "NZL", "Kazakhstan" = "KAZ", "Uzbekistan" = "UZB",
  "Turkmenistan" = "TKM", "Laos" = "LAO",
  "Russia" = "RUS", "Ukraine" = "UKR", "Belarus" = "BLR",
  "Germany" = "DEU", "United Kingdom" = "GBR", "France" = "FRA", "Italy" = "ITA", 
  "Spain" = "ESP", "Netherlands" = "NLD", "Belgium" = "BEL", "Switzerland" = "CHE", 
  "Sweden" = "SWE", "Norway" = "NOR", "Denmark" = "DNK", "Finland" = "FIN", 
  "Greece" = "GRC", "Turkey" = "TUR", "Türkiye" = "TUR", "Poland" = "POL", 
  "Israel" = "ISR", "Saudi Arabia" = "SAU", "United Arab Emirates" = "ARE",
  "Iran" = "IRN", "Iraq" = "IRQ", "Egypt" = "EGY", "Qatar" = "QAT", "Kuwait" = "KWT",
  "Algeria" = "DZA", "Morocco" = "MAR", "Nigeria" = "NGA", "South Africa" = "ZAF",
  "Angola" = "AGO", "Ethiopia" = "ETH", "Sudan" = "SDN", "Kenya" = "KEN", "Albania" = "ALB"
)

us_allies <- c("USA", "GBR", "FRA", "ESP","FIN","NLD", "BEL","GRC","POL", "CZE","HUN","NOR",
               "DEU", "JPN", "CAN", "AUS", "NZL", "ITA", "KOR", "BGR", "AUT", "PRT", "ROU", "SWE","TUR")

sat_long_clean <- satcat %>%
  dplyr::select(dual_use_country, `2000`:`2024`) %>%
  tidyr::pivot_longer(
    cols = `2000`:`2024`,
    names_to = "year",
    values_to = "strat_presence"
  ) %>%
  dplyr::mutate(
    year = as.integer(year),
    iso3 = master_lookup[as.character(dual_use_country)]
  ) %>%
  dplyr::filter(!is.na(iso3))

sat_counts <- sat_long_clean %>%
  dplyr::group_by(iso3, year) %>%
  dplyr::summarise(
    strategic_count = sum(strat_presence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(iso3 = toupper(trimws(iso3)))

allied_power_timeline <- sat_counts %>%
  dplyr::filter(iso3 %in% us_allies) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(global_allied_count = sum(strategic_count, na.rm = TRUE), .groups = "drop")

panel_final <- sat_counts %>%
  dplyr::filter(!(iso3 %in% us_allies)) %>%
  dplyr::left_join(allied_power_timeline, by = "year") %>%
  dplyr::mutate(
    allied_threat = tidyr::replace_na(global_allied_count, 0),
    log_cap = log(strategic_count + 1),
    log_threat = log(allied_threat + 1)
  )

##########################################################################################
# weighting
##########################################################################################
rivalry_weights <- dyad_geopolitical_scores_Major24xAll %>%
  filter(year >= 2000) %>%
  select(country1_code, country2_code, year, geo_score_ma) %>%
  rename(target_iso = country1_code, rival_iso = country2_code)

dyadic_threat_df <- rivalry_weights %>%
  left_join(sat_counts, by = c("rival_iso" = "iso3", "year" = "year")) %>%
  mutate(strategic_count = replace_na(strategic_count, 0)) %>%
  mutate(weighted_threat_contribution = strategic_count * geo_score_ma) %>%
  group_by(target_iso, year) %>%
  summarise(
    total_dyadic_threat = sum(weighted_threat_contribution, na.rm = TRUE),
    .groups = "drop"
  )

panel_global_dyadic <- sat_counts %>%
  left_join(dyadic_threat_df, by = c("iso3" = "target_iso", "year" = "year")) %>%
  left_join(milex_clean, by = c("iso3", "year")) %>%
  left_join(vdem_clean, by = c("iso3", "year")) %>%
  mutate(
    log_cap = log(strategic_count + 1),
    log_threat = log(replace_na(total_dyadic_threat, 0) + 1),
    log_milex = log(replace_na(milex, 1) + 1),
    democracy_score = replace_na(v2x_polyarchy, 0.5)
  )
p_df_global <- plm::pdata.frame(as.data.frame(panel_global_dyadic), index = c("iso3", "year"))

model_global_static <- plm::plm(
  log_cap ~ plm::lag(log_threat, 3) + 
    plm::lag(log_milex, 3) + 
    democracy_score,
  data = p_df_global, 
  model = "pooling"
)

summary(model_global_static)



model_g1 <- plm::plm(log_cap ~ log_threat, 
                     data = p_df_global, model = "pooling")

model_g2 <- plm::plm(log_cap ~ log_threat + log_milex + democracy_score, 
                     data = p_df_global, model = "within", effect = "individual")

model_g3 <- plm::plm(log_cap ~ plm::lag(log_threat, 1) + log_milex + democracy_score, 
                     data = p_df_global, model = "within", effect = "individual")

model_g4 <- plm::plm(log_cap ~ plm::lag(log_cap, 1) + log_threat + log_milex + democracy_score, 
                     data = p_df_global, model = "within", effect = "individual")

model_g5 <- plm::plm(log_cap ~ plm::lag(log_cap, 1) + plm::lag(log_threat, 3) + log_milex + democracy_score, 
                     data = p_df_global, model = "within", effect = "individual")


##
# prior both military and dual-use
# this regression is for dual-use only
##
master_lookup <- c(
  "Russia" = "RUS", "CIS" = "RUS", "USSR" = "RUS", "Belarus" = "BLR", "Ukraine" = "UKR",
  "United States" = "USA", "United States of America" = "USA", "USA" = "USA",
  "Canada" = "CAN", "Mexico" = "MEX", "Brazil" = "BRA", "Argentina" = "ARG", 
  "China" = "CHN", "Japan" = "JPN", "South Korea" = "KOR", "India" = "IND",
  "Germany" = "DEU", "United Kingdom" = "GBR", "France" = "FRA", "Italy" = "ITA", 
  "Israel" = "ISR", "Saudi Arabia" = "SAU", "Iran" = "IRN", "Australia" = "AUS",
  "North Korea" = "PRK", "Spain" = "ESP", "Turkey" = "TUR"
)

sat_long_clean <- satcat %>%
  dplyr::filter(!is.na(dual_use_country)) %>% 
  dplyr::select(dual_use_country, `2000`:`2024`) %>%
  tidyr::pivot_longer(
    cols = `2000`:`2024`,
    names_to = "year",
    values_to = "strat_presence"
  ) %>%
  dplyr::mutate(
    year = as.integer(year),
    iso3 = master_lookup[as.character(dual_use_country)]
  ) %>%
  dplyr::filter(!is.na(iso3))

sat_counts <- sat_long_clean %>%
  dplyr::group_by(iso3, year) %>%
  dplyr::summarise(dual_use_count = sum(strat_presence, na.rm = TRUE), .groups = "drop")

milex_clean <- SIPRI_Milex_data_1949_2025_v1_2 %>%
  tidyr::pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = "milex") %>%
  dplyr::mutate(year = as.integer(year), iso3 = master_lookup[Country]) %>%
  dplyr::filter(!is.na(iso3), year >= 2000) %>%
  dplyr::group_by(iso3, year) %>%
  dplyr::summarise(milex = mean(as.numeric(milex), na.rm = TRUE), .groups = "drop")

vdem_clean <- V_Dem_CY_Core_v16 %>%
  dplyr::mutate(iso3 = country_text_id, year = as.integer(year)) %>%
  dplyr::select(iso3, year, v2x_polyarchy) %>%
  dplyr::filter(year >= 2000)


#threat index & panel construction

rivalry_weights <- dyad_geopolitical_scores_Major24xAll %>%
  dplyr::filter(year >= 2000) %>%
  dplyr::select(target_iso = country1_code, rival_iso = country2_code, year, geo_score_ma)

dyadic_threat_df <- rivalry_weights %>%
  dplyr::left_join(sat_counts, by = c("rival_iso" = "iso3", "year" = "year")) %>%
  dplyr::mutate(dual_use_count = tidyr::replace_na(dual_use_count, 0)) %>%
  dplyr::mutate(weighted_contribution = dual_use_count * geo_score_ma) %>%
  dplyr::group_by(target_iso, year) %>%
  dplyr::summarise(total_threat = sum(weighted_contribution, na.rm = TRUE), .groups = "drop")

panel_final <- sat_counts %>%
  dplyr::left_join(dyadic_threat_df, by = c("iso3" = "target_iso", "year" = "year")) %>%
  dplyr::left_join(milex_clean, by = c("iso3", "year")) %>%
  dplyr::left_join(vdem_clean, by = c("iso3", "year")) %>%
  dplyr::mutate(
    log_cap = log(dual_use_count + 1),
    log_threat = log(tidyr::replace_na(total_threat, 0) + 1),
    log_milex = log(tidyr::replace_na(milex, 1) + 1),
    democracy_score = tidyr::replace_na(v2x_polyarchy, 0.5)
  )
p_df_dual <- plm::pdata.frame(as.data.frame(panel_final), index = c("iso3", "year"))


# nested regression (one-way fixed effects)

model_1 <- plm::plm(log_cap ~ log_threat, 
                    data = p_df_dual, model = "pooling")

model_2 <- plm::plm(log_cap ~ log_threat + log_milex + democracy_score, 
                    data = p_df_dual, model = "within", effect = "individual")

model_3 <- plm::plm(log_cap ~ plm::lag(log_threat, 1) + log_milex + democracy_score, 
                    data = p_df_dual, model = "within", effect = "individual")

model_4 <- plm::plm(log_cap ~ plm::lag(log_cap, 1) + log_threat + log_milex + democracy_score, 
                    data = p_df_dual, model = "within", effect = "individual")

model_5 <- plm::plm(log_cap ~ plm::lag(log_cap, 1) + plm::lag(log_threat, 3) + log_milex + democracy_score, 
                    data = p_df_dual, model = "within", effect = "individual")


################################################################################
# HYPOTHESIS 2
################################################################################
satcat <- satcat %>%
  mutate(
    `2000` = ifelse(is.na(`2000`), 0, `2000`),
    `2001` = ifelse(is.na(`2001`), 0, `2001`),
    `2002` = ifelse(is.na(`2002`), 0, `2002`),
    `2003` = ifelse(is.na(`2003`), 0, `2003`),
    `2004` = ifelse(is.na(`2004`), 0, `2004`),
    `2005` = ifelse(is.na(`2005`), 0, `2005`),
    `2006` = ifelse(is.na(`2006`), 0, `2006`),
    `2007` = ifelse(is.na(`2007`), 0, `2007`),
    `2008` = ifelse(is.na(`2008`), 0, `2008`),
    `2009` = ifelse(is.na(`2009`), 0, `2009`),
    `2010` = ifelse(is.na(`2010`), 0, `2010`),
    `2011` = ifelse(is.na(`2011`), 0, `2011`),
    `2012` = ifelse(is.na(`2012`), 0, `2012`),
    `2013` = ifelse(is.na(`2013`), 0, `2013`),
    `2014` = ifelse(is.na(`2014`), 0, `2014`),
    `2015` = ifelse(is.na(`2015`), 0, `2015`),
    `2016` = ifelse(is.na(`2016`), 0, `2016`),
    `2017` = ifelse(is.na(`2017`), 0, `2017`),
    `2018` = ifelse(is.na(`2018`), 0, `2018`),
    `2019` = ifelse(is.na(`2019`), 0, `2019`),
    `2020` = ifelse(is.na(`2020`), 0, `2020`),
    `2021` = ifelse(is.na(`2021`), 0, `2021`),
    `2022` = ifelse(is.na(`2022`), 0, `2022`),
    `2023` = ifelse(is.na(`2023`), 0, `2023`),
    `2024` = ifelse(is.na(`2024`), 0, `2024`),
    `2025` = ifelse(is.na(`2025`), 0, `2025`)
  ) %>%
  mutate(launch_year = year(as.Date(LAUNCH_DATE)))


ts_data <- satcat %>%
  pivot_longer(cols = matches("^20\\d{2}$"), names_to = "year", values_to = "is_dual") %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year) %>%
  summarize(total_dual = sum(is_dual, na.rm = TRUE))
dual_ts <- ts(ts_data$total_dual, start = 2000, end = 2025)

bp_test <- breakpoints(dual_ts ~ 1)
summary(bp_test)
break_year <- ts_data$year[bp_test$breakpoints]
print(paste("Structural break detected at year:", break_year))


years_to_test <- 2001:2020
results <- data.frame(year = years_to_test, f_stat = NA, p_value = NA)
for (i in seq_along(years_to_test)) {
  current_year <- years_to_test[i]
  idx <- which(time(dual_ts) == current_year)
  test <- sctest(dual_ts ~ 1, type = "Chow", point = idx)
  results$f_stat[i] <- test$statistic
  results$p_value[i] <- test$p.value
}
print(results)

results$sig_level <- cut(results$p_value, 
                         breaks = c(-Inf, 0.01, 0.05, 0.1, Inf), 
                         labels = c("p < 0.01", 
                                    "p < 0.05", 
                                    "p < 0.10", 
                                    "p > 0.10"))

scientific_blue_plot <- ggplot(results, aes(x = year, y = f_stat)) +
  theme_ipsum_rc(grid="Y", axis_title_size = 12) +
  geom_hline(yintercept = 3.84, linetype = "dashed", color = "#5dade2", alpha = 0.5) +
  geom_line(color = "#1b4f72", size = 0.8, alpha = 0.3) +
  geom_point(aes(color = sig_level, size = (year == 2015)), alpha = 0.9) +
  geom_label_repel(data = subset(results, year == 2015),
                   aes(label = paste0("2015 Pivot\n", "p = ", round(p_value, 4))),
                   nudge_y = 7, nudge_x = -2, segment.color = "#1b4f72",
                   fill = "white", color = "#1b4f72", fontface = "bold", size = 3.5) +
  scale_color_manual(values = c("p < 0.01" = "#08306b",      
                                "p < 0.05" = "#2171b5", 
                                "p < 0.10" = "#6baed6",    
                                "p > 0.10" = "#deebf7"),        
                     name = "Significance") +
  scale_size_manual(values = c("FALSE" = 3, "TRUE" = 6), guide = "none") +
  labs(
    title = "Structural Stability Analysis: Dual-Use Satellites",
    subtitle = "Chow Test F-Statistics Categorised by P-Value Significance Levels",
    x = "Candidate Break Year",
    y = "F-Statistic",
    caption = "Note: Points above the dashed line (F > 3.84) indicate statistically significant structural breaks. 
               Darker blue tones denote higher statistical confidence."
  ) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "#1b4f72"),
    plot.title = element_text(color = "black", face = "bold"),
    legend.title = element_text(face = "bold")
  )
scientific_blue_plot
ggsave("chow_p_value_blue_legend.png", scientific_blue_plot, width = 11, height = 7, dpi = 600)


proliferation_plot <- ggplot(ts_data, aes(x = year, y = total_dual)) +
  theme_ipsum_rc(grid="Y") +
  geom_area(fill = "#5dade2", alpha = 0.2) + 
  geom_line(color = "#1b4f72", size = 1.2) +
  geom_point(color = "#1b4f72", size = 2.5, alpha = 0.8) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "#1b4f72", alpha = 0.7) + 
  labs(
    title = "Dual-Use Satellite Proliferation (2000-2025)",
    subtitle = "Annual count of active dual-use assets identified",
    x = "Year",
    y = "Total Active Satellites",
    caption = "Data Source: SATCAT Analysis. Note the acceleration post-2015."
  ) +
  theme(
    plot.title = element_text(color = "black", face = "bold"),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)
  )
print(proliferation_plot)
ggsave("proliferation_plot.png", proliferation_plot, width = 11, height = 7, dpi = 600)

####
# by country analysis //2015// (as per theory)
####

country_ts_data <- satcat %>%
  pivot_longer(cols = matches("^20\\d{2}$"), 
               names_to = "year", 
               values_to = "is_dual") %>%
  mutate(year = as.numeric(year)) %>%
  group_by(dual_use_country, year) %>%
  summarize(total_dual = sum(is_dual, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    time = year - min(year) + 1,
    interruption = ifelse(year >= 2015, 1, 0),
    post_trend = pmax(0, year - 2015)
  )

major_countries <- country_ts_data %>%
  group_by(dual_use_country) %>%
  summarise(max_sat = max(total_dual)) %>%
  filter(max_sat > 2) %>% 
  pull(dual_use_country)

run_its_country <- function(df) {
  tryCatch({
    gls(total_dual ~ time + interruption + post_trend,
        data = df,
        correlation = corAR1(form = ~ time),
        method = "ML")
  }, error = function(e) return(NULL))
}

country_models <- country_ts_data %>%
  filter(dual_use_country %in% major_countries) %>%
  split(.$dual_use_country) %>%
  map(run_its_country) %>%
  compact()

results_comparison_2015 <- map_df(country_models, ~{
  coeffs <- summary(.x)$tTable
  data.frame(
    Level_Shift_P = coeffs["interruption", "p-value"],
    Slope_Change_Beta = coeffs["post_trend", "Value"],
    Slope_Change_SE = coeffs["post_trend", "Std.Error"], 
    Slope_Change_T = coeffs["post_trend", "t-value"],    
    Slope_Change_P = coeffs["post_trend", "p-value"]
  )
}, .id = "Country")

print(results_comparison_2015)

####
# by country analysis //2023// (as data suggests)
####
data_2022 <- country_ts_data %>%
  group_by(dual_use_country) %>%
  mutate(
    time = year - min(year) + 1,
    interruption = ifelse(year >= 2023, 1, 0), 
    post_trend = pmax(0, year - 2022)          
  ) %>%
  ungroup()

results_comparison_2022 <- data_2022 %>%
  split(.$dual_use_country) %>%
  map(~ tryCatch({
    gls(total_dual ~ time + interruption + post_trend, 
        data = .x, correlation = corAR1(form = ~ time), method = "ML")
  }, error = function(e) NULL)) %>%
  compact() %>%
  map_df(~ {
    coeffs <- summary(.x)$tTable
    data.frame(
      Level_Shift_P = coeffs[results"interruption", "p-value"],
      Slope_Change_Beta = coeffs["post_trend", "Value"],
      Slope_Change_SE = coeffs["post_trend", "Std.Error"],
      Slope_Change_T = coeffs["post_trend", "t-value"],   
      Slope_Change_P = coeffs["post_trend", "p-value"]
    )
  }, .id = "Country")

print(results_comparison_2022)
#####
# plots
#####
plot_data_final <- results_comparison_2015 %>%
  mutate(
    Country = case_when(
      grepl("KOREA", Country, ignore.case = TRUE) ~ "South Korea",
      grepl("EMIRATES|UAE", Country, ignore.case = TRUE) ~ "UAE",
      TRUE ~ Country
    )
  ) %>%
  filter(Slope_Change_Beta > 0 | Country %in% c("South Korea", "UAE")) %>%
  mutate(
    Slope_Change_Beta = ifelse(Slope_Change_Beta <= 0.1, 0.1, Slope_Change_Beta),
    
    Country = reorder(Country, Slope_Change_Beta),
    sig_level = case_when(
      Slope_Change_P < 0.01 ~ "p < 0.01",
      Slope_Change_P < 0.05 ~ "p < 0.05",
      Slope_Change_P < 0.10 ~ "p < 0.10",
      TRUE ~ "p > 0.10"
    ),
    sig_level = factor(sig_level, levels = c("p < 0.01", "p < 0.05", "p < 0.10", "p > 0.10"))
  )

all_minor_breaks <- c(
  seq(0.1, 1, 0.1), 
  seq(1, 10, 1), 
  seq(10, 100, 10), 
  seq(100, 1000, 100)
)

plot_model2015 <- ggplot(plot_data_final, aes(x = Country, y = Slope_Change_Beta)) +
  theme_ipsum_rc(grid="X", axis_title_size = 12) +
  geom_segment(aes(x = Country, xend = Country, y = 0.1, yend = Slope_Change_Beta), 
               color = "#5dade2", size = 0.8, alpha = 0.3) +
  geom_point(aes(fill = sig_level), 
             shape = 21, color = "#1b4f72", stroke = 1.2, size = 4, alpha = 0.9) +
  scale_y_log10(
    breaks = c(0.1, 1, 10, 100, 1000), 
    minor_breaks = all_minor_breaks,
    labels = c("0.1", "1", "10", "100", "1,000"),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  coord_flip() +
  scale_fill_manual(values = c("p < 0.01" = "black",      
                               "p < 0.05" = "#1b4f72", 
                               "p < 0.10" = "white",    
                               "p > 0.10" = "#deebf7"),      
                    name = "Significance") +
  labs(
    title = "Structural Slope Coefficients Post-2015 Intervention",
    subtitle = "Interrupted Time Series of Dual-Use Assets per Country",
    x = NULL,
    y = "Slope Change Beta (Logarithmic Scale)",
    caption = "Black = p < 0.01 | Blue = p < 0.05 | White (Blue Border) = p < 0.10\nSource: SATCAT Analysis via CelesTrak Satellite Catalogue."
  ) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "#1b4f72"),
    panel.grid.minor.x = element_line(color = "gray95", size = 0.4),
    plot.title = element_text(color = "black", face = "bold"),
    legend.title = element_text(face = "bold"),
    axis.text.y = element_text(color = "#1b4f72", face = "bold")
  )

print(plot_model2015)
ggsave("plot_model2015.png", plot_model2015, width = 11, height = 7, dpi = 600)



baseline_trends <- map_df(country_models, ~{
  coeffs <- summary(.x)$tTable
  data.frame(
    Pre_2015_Trend = coeffs["time", "Value"]
  )
}, .id = "Country") <=
  


percentage_results_2015 <- results_comparison_2015 %>%
  left_join(baseline_trends, by = "Country") %>%
  mutate(
    effective_baseline = ifelse(Pre_2015_Trend <= 0, 0.1, Pre_2015_Trend),
    Pct_Slope_Increase = (Slope_Change_Beta / effective_baseline) * 100
  ) %>%
  select(Country, Pre_2015_Trend, Slope_Change_Beta, Pct_Slope_Increase, Slope_Change_P) %>%
  arrange(desc(Pct_Slope_Increase))
print(percentage_results_2015)