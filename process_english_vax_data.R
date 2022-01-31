# Read in raw data on vaccination demographics for UK nations
nations_vax_age_raw <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&metric=vaccinationsAgeDemographics&format=csv") %>% clean_names()

# Load latest UK population estimates
load("uk_pop_by_age_raw_2020_master.Rdata")

# Clean latest UK population estimates
uk_pop_age_clean <- uk_pop_by_age_raw_2020_master %>%
  rename(area_code = Code) %>%
  select(c(1, ncol(.), 5:(ncol(.)-1))) %>%
  pivot_longer(3:ncol(.), names_to = "age", values_to = "pop") %>%
  mutate(age = parse_number(age))

# Clean and process vaccination demographics
nations_vax_age_cleaned <- nations_vax_age_raw %>%
  group_by(area_code, age) %>%
  nest() %>%
  mutate(
    age_min = as.numeric(str_sub(age, 0, 2)),
    age_max = as.numeric(str_sub(age, -2, -1)) %>% replace_na(200)
  ) %>%
  rowwise() %>%
  mutate(
    pop_ons = sum(uk_pop_age_clean$pop[uk_pop_age_clean$age >= age_min & uk_pop_age_clean$age <= age_max & uk_pop_age_clean$area_code == area_code])
  ) %>% 
  unnest() %>% 
  select(
    area_code, area_name, date, age,
    pop = pop_ons,
    d1 = cum_people_vaccinated_first_dose_by_vaccination_date,
    d2 = cum_people_vaccinated_second_dose_by_vaccination_date,
    d3 = cum_people_vaccinated_third_injection_by_vaccination_date
  ) %>%
  group_by(area_code, area_name, date) %>%
  do({
    this <- as_tibble(.)
    all_adults <- this %>% 
      group_by(age = "18+") %>%
      summarise(across(one_of(c("pop", "d1", "d2", "d3")), sum))
    bind_rows(this, all_adults) %>%
      arrange(age)
  }) %>%
  ungroup() %>%
  fill(area_code, area_name, date, .direction = "down") %>%
  mutate(rate1 = d1/pop*100, rate2 = d2/pop*100, rate3 = d3/pop*100)