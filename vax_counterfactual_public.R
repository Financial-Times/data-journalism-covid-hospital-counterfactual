# Load in data on European vaccination by age
ecdc_vax_age_raw <- read_csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv") %>% clean_names()

# Process vax rates by age over time for selected countries
ecdc_vax_cleaned <- ecdc_vax_age_raw %>%
  filter(grepl("Age", target_group) & !target_group %in% c("AgeUNK")) %>%
  filter(reporting_country == region) %>%
  filter(reporting_country %in% c("PT", "DK", "ES", "IT", "AT", "FR", "PL", "CZ")) %>% 
  group_by(
    date = ISOweek2date(paste0(year_week_iso,"-7")),
    country = reporting_country,
    age = target_group
  ) %>%
  summarise(
    `1` = sum(first_dose)/max(denominator)*100,
    `2` = sum(second_dose)/max(denominator)*100,
    `3` = sum(dose_additional1)/max(denominator)*100,
    pop = max(denominator)
  ) %>% 
  pivot_longer(4:6, names_to = "dose", values_to = "rate") %>%
  group_by(country, dose, age) %>%
  mutate(rate = cumsum(rate)) %>%
  ungroup() %>%
  transmute(
    date,
    age,
    pop,
    dose,
    rate,
    country = case_when(
      country == "AT" ~ "Austria",
      country == "CZ" ~ "Czech Republic",
      country == "DK" ~ "Denmark",
      country == "ES" ~ "Spain",
      country == "FR" ~ "France",
      country == "IT" ~ "Italy",
      country == "PL" ~ "Poland",
      country == "PT" ~ "Portugal",
      T ~ country
    )
  ) %>%
  group_by(country, dose, age, pop) %>%
  complete(date = full_seq(date, period = 1)) %>%
  mutate(rate = na.approx(rate)) %>%
  # Remove age categories that duplicate more granular breaks
  filter(!(country == "AT" & age == "Age<18")) %>%
  filter(!(country == "CZ" & age == "Age<18")) %>%
  filter(!(country == "ES" & age == "Age<18"))

# Load and clean English vaccination demographics
source("process_english_vax_data.R")

# Process vax rates over time for England
eng_vax_cleaned <- nations_vax_age_cleaned %>%
  filter(area_name == "England") %>%
  group_by(
    date,
    age = case_when(
      age %in% c("12_15") ~ age,
      age %in% c("16_17", "18_24") ~ "16_24",
      age %in% c("25_29", "30_34", "35_39", "40_44") ~ "25_44",
      age %in% c("45_49", "50_54") ~ "45_54",
      age %in% c("55_59", "60_64") ~ "55_64",
      age %in% c("65_69", "70_74") ~ "65_74",
      age %in% c("75_79", "80_84") ~ "75_84",
      age %in% c("85_89", "90+") ~ "85_100",
      T ~ NA_character_
    )
  ) %>%
  drop_na(age) %>%
  summarise(
    `1` = sum(d1)/sum(pop)*100,
    `2` = sum(d2)/sum(pop)*100,
    `3` = sum(d3)/sum(pop)*100,
    pop = sum(pop)
  ) %>%
  pivot_longer(3:5, names_to = "dose", values_to = "rate") %>%
  ungroup() %>%
  transmute(
    date,
    age,
    pop,
    dose,
    rate = case_when(
      # Adjust dates for oldest populations where reported data is believed to understate uptake. Adjustments are based on finding a compromise between the reported data and the antibody-based estimates from ONS https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurveyantibodyandvaccinationdatafortheuk/26january2022#by-age-group
      age == "85_100" ~ rate * 1.152941 * 0.96,
      age == "75_84" ~ rate * 1.042553 * 0.96,
      age == "65_74" ~ rate * 1.077778 * 0.96,
      age == "55_64" ~ rate * 1.071429 * 0.96,
      T ~ rate
    ),
    rate,
    country = "England"
  )

# Load in data on US vaccination by age
us_vax_demos_nat <- read_csv("https://data.cdc.gov/api/views/km4m-vcsb/rows.csv?accessType=DOWNLOAD") %>% 
  clean_names() %>%
  transmute(
    date = as.Date(date, format = "%m/%d/%Y"),
    group = demographic_category,
    d1 = administered_dose1,
    d2 = series_complete_yes,
    d3 = booster_doses_yes
  ) %>%
  arrange(date) %>%
  group_by(date) %>% 
  filter(grepl("Age",group)) %>%
  filter(!group %in% c("Age_unknown", "Age_known", "Ages_<12yrs", "Ages_12-15_yrs", "Ages_16-17_yrs")) %>%
  mutate(
    pop = case_when(
      # Latest population estimates (more up-to-date than CDC)
      group == "Ages_<5yrs" ~ 19301292,
      group == "Ages_5-11_yrs" ~ 28384878,
      group == "Ages_12-17_yrs" ~ 25135943,
      group == "Ages_18-24_yrs" ~ 30026997,
      group == "Ages_25-39_yrs" ~ 67897950,
      group == "Ages_40-49_yrs" ~ 40278494,
      group == "Ages_50-64_yrs" ~ 62799204,
      group == "Ages_65-74_yrs" ~ 32549398,
      group == "Ages_75+_yrs" ~ 23109967
    )
  ) %>% 
  group_by(
    date,
    age = case_when(
      group == "Ages_<5yrs" ~ "00_05",
      group == "Ages_5-11_yrs" ~ "05_11",
      group == "Ages_12-17_yrs" ~ "12_17",
      group == "Ages_18-24_yrs" ~ "18_24",
      group == "Ages_25-39_yrs" ~ "25_39",
      group == "Ages_40-49_yrs" ~ "40_49",
      group == "Ages_50-64_yrs" ~ "50_64",
      group == "Ages_65-74_yrs" ~ "65_74",
      group == "Ages_75+_yrs" ~ "75_100" 
    ),
    pop
  ) %>%
  summarise(
    rate1 = (sum(d1)/sum(pop))*100,
    rate2 = (sum(d2)/sum(pop))*100,
    rate3 = (sum(d3)/sum(pop))*100
  ) %>%
  ungroup() %>%
  pivot_longer(4:6, names_to = "dose", values_to = "rate") %>%
  mutate(country = "US")

# Process vax rates over time for US
us_vax_cleaned <- us_vax_demos_nat %>%
  left_join(
    us_vax_demos_nat %>% 
      filter(dose == "rate1") %>%
      filter(age %in% c("18_24", "25_39", "40_49", "50_64", "65_74")) %>%
      filter(date >= "2021-01-14") %>%
      group_by(date) %>%
      # Booster uptake data is not available for under-65s, so this is modelled based on how first dose uptake for other age groups related to over-65s
      mutate(relative = rate/rate[age == "65_74"]) %>%
      filter(age != "65_74") %>%
      mutate(date = date + 255) %>%
      transmute(date, age, dose = "rate3", relative) %>%
      {
        this <- as_tibble(.)
        bind_rows(
          this %>%
            filter(date <= "2021-10-26") %>%
            mutate(date = date-31) %>%
            group_by(age) %>%
            mutate(relative = head(relative,1)),
          this
        )
      }
  ) %>%
  group_by(date) %>%
  mutate(
    rate = case_when(
      age %in% c("18_24", "25_39", "40_49", "50_64") & !is.na(relative) ~ rate[age == "65_74"]*relative*0.9,
      T ~ rate
    )
  ) %>% 
  select(-relative) %>%
  ungroup() %>%
  mutate(
    dose = parse_number(dose),
    rate
  )

# Combine all data on vaccination demographics and calculate weighted hospitalisation exposure
vax_age_risk_master <- bind_rows(
  ecdc_vax_cleaned %>% mutate(dose = as.numeric(dose)),
  eng_vax_cleaned %>% mutate(dose = as.numeric(dose)),
  us_vax_cleaned %>% mutate(dose = as.numeric(dose)),
  # Adds scenario for England without boosters
  eng_vax_cleaned %>%
    mutate(
      dose = as.numeric(dose),
      rate = if_else(dose == 3, 0, rate),
      country = "England no boost"
    )
) %>%
  ungroup() %>% 
  # Cap uptake at 99 per cent (deals with any over-counting due to dodgy population data)
  mutate(rate = pmin(rate, 99)) %>%
  {
    # Add in implied values for unvaccinated
    this <- as_tibble(.)
    bind_rows(
      this %>% 
        group_by(country, date, age, pop) %>%
        summarise(rate = 100-rate[dose==1], dose = 0),
      this
    )
  } %>% 
  arrange(country, date, age, dose) %>%
  pivot_wider(names_from = dose, values_from = rate) %>%
  group_by(country, age) %>%
  mutate(
    `1` = `1` - `2`,
    `2` = `2` - `3`,
    # Calculate how many people’s 2nd and 3rd doses had waned sufficiently to affect vaccine efficacy
    `2w` = pmax(replace_na(lag(`2`, 175),0)-`3`,0),
    `2` = `2` - `2w`,
    `3ww` = replace_na(lag(`3`, 70),0),
    `3w` = replace_na(pmax(0,lag(`3`, 35) - `3ww`),0),
    `3` = `3` - (`3w`+`3ww`)
  ) %>% 
  pivot_longer(5:11, names_to = "dose", values_to = "rate") %>%
  mutate(
    # Raw pre-vaccine hospitalisation risks, calculated from UKHSA flu and Covid vaccine surveillance reports in January 2021
    # 100x increase in risk between age 16 and age 85
    raw_risk = case_when(
      age %in% c("Age0_4", "00_05") ~ 2.44,
      age %in% c("Age<18","Age10_14","12_15","12_17","05_11","Age5_9") ~ 0.465,
      age %in% c("Age15_17") ~ 0.691,
      age %in% c("Age18_24", "16_24", "18_24") ~ 0.917,
      age %in% c("Age25_49", "25_44", "25_39") ~ 2.05,
      age %in% c("40_49") ~ 3.9,
      age %in% c("45_54") ~ 4.8,
      age %in% c("Age50_59") ~ 7.55,
      age %in% c("50_64") ~ 8.5,
      age %in% c("55_64") ~ 10.3,
      age %in% c("Age60_69") ~ 19.9,
      age %in% c("65_74") ~ 29.4,
      age %in% c("Age70_79") ~ 45.4,
      age %in% c("75_84") ~ 61.3,
      age %in% c("75_100") ~ 64,
      age %in% c("Age80+") ~ 64.5,
      age %in% c("85_100") ~ 65
    ),
    # Vaccine efficacy against Omicron, plus a uniform 0.3333x downward adjustment of hospitalisation risk due to Omicron reduced intrinsic severity
    vaxxed_risk_omi = case_when(
      dose == "0" ~ raw_risk * 2 * 0.3333,  # `* 2` here is a crude approximation for increased likelihood of breakthrough infections with Omicron: a portion of the unvaccinated have prior infection, meaning Omicron’s immune evasion increases their exposure in the same way it increases risk of breakthrough infections for the vaccinated
      dose == "1" ~ raw_risk * 0.42 * 0.3333,
      dose == "2" ~ raw_risk * 0.36 * 0.3333,
      dose == "2w" ~ raw_risk * 0.66 * 0.3333, # was 0.54 in original UKHSA esimates
      dose == "3" ~ raw_risk * 0.11 * 0.3333, # was 0.08
      dose == "3w" ~ raw_risk * 0.15 * 0.3333, # was 0.12
      dose == "3ww" ~ raw_risk * 0.21 * 0.3333 # was 0.17
    ),
    # Vaccine efficacy against Delta
    # First dose efficacy from here https://khub.net/web/phe-national/public-library/-/document_library/v2WsRK3ZlEig/view_file/479607329?_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_v2WsRK3ZlEig_redirect=https%3A%2F%2Fkhub.net%3A443%2Fweb%2Fphe-national%2Fpublic-library%2F-%2Fdocument_library%2Fv2WsRK3ZlEig%2Fview%2F479607266
    # Second dose efficacy from here https://www.nejm.org/doi/full/10.1056/NEJMoa2115481
    # Third dose efficacy from here https://twitter.com/DevanSinha/status/1473270012639129601
    # Before booster, English data adjusted downwards due to greater use of AstraZeneca
    vaxxed_risk_delta = case_when(
      dose == "0" ~ raw_risk,
      dose == "1" & grepl("England", country) ~ raw_risk * 0.2,
      dose == "1" ~ raw_risk * 0.06,
      dose == "2" & grepl("England", country) ~ raw_risk * 0.07, # 0.05
      dose == "2" ~ raw_risk * 0.03,
      dose == "2w" & grepl("England", country) ~ raw_risk * 0.13, # 0.12
      dose == "2w" ~ raw_risk * 0.08,
      dose == "3" ~ raw_risk * 0.01,
      dose == "3w" ~ raw_risk * 0.02,
      dose == "3ww" ~ raw_risk * 0.04
    ),
    # Simplistic representation of Omicron taking over from Delta from mid Dec to early Jan, to allow for gradual transition from using Delta VE to Omicron VE
    omi_share = case_when(
      date < "2021-12-16" ~ 0,
      date > "2022-01-09" ~ 1,
      T ~ 1/(1+exp(-as.numeric(date-as.Date("2021-12-28")))),
    )
  ) %>% 
  group_by(country, date) %>%
  summarise(
    weighted_risk = sum((vaxxed_risk_omi * omi_share * (rate/100) * pop) + (vaxxed_risk_delta * (1-omi_share) * (rate/100) * pop)) / sum(pop)
  ) %>%
  ungroup()