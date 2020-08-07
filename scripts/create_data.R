packages <- c("tidyverse", "covid19us", "tidyselect", "plotly", "TTR", "glue", 
              "readxl", "httr", "coronavirus", "COVID19")
xfun::pkg_attach2(packages, message = F)

# population values ----
# CA 2019, estimated from:
# https://www.census.gov/quickfacts/fact/table/CA/PST045219
population_ca <- 39512223  

# Los Angeles County, CA 2019, estimated from:
# https://www.census.gov/quickfacts/fact/table/losangelescountycalifornia,TX/PST045219
population_la_county <- 10039107

# TX 2019, estimated from:
# https://www.census.gov/quickfacts/fact/table/TX/PST045219
population_tx <- 28995881

# Dallas, TX 2019, estimated from:
# https://www.census.gov/quickfacts/fact/table/dallascountytexas,US/PST045219
population_dallas_county <- 2635516

# Guatemala 2018, estimated from:
# https://en.wikipedia.org/wiki/Guatemala#cite_note-IMF-4
population_guatemala <- 17263239

# Thailand 2019, estimated from:
# https://en.wikipedia.org/wiki/Thailand#cite_note-2019PopEst-4
population_thailand <- 69758935

df_all <- NULL

# state data ----
df_temp_orig <- get_states_daily() %>%
  filter(state %in% c("CA", "TX"))
df_temp <- df_temp_orig %>%
  arrange(date) %>%
  select(date, 
         region = state,
         daily_deaths = death_increase,
         daily_total_hospitalizations = hospitalized_currently,
         daily_total_icu = in_icu_currently) %>%
  pivot_longer(., -c(date, region), names_to = "metric", values_to = "value")
df_all <- rbind(df_all, df_temp)

# la county data ----
df_temp_orig <- read.csv("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/cdph-hospital-patient-county-totals.csv")

df_temp <- df_temp_orig %>%
  filter(county == "Los Angeles") %>%
  select(-county, -fips) %>%
  as_tibble() %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  rowwise() %>%
  mutate(daily_total_hospitalizations = sum(c(positive_patients, suspected_patients)),
         daily_total_icu = sum(c(icu_positive_patients, icu_suspected_patients))) %>%
  select(date, daily_total_hospitalizations, daily_total_icu, daily_total_icu_beds_available = icu_available_beds) %>%
  pivot_longer(., -date, names_to = 'metric', values_to = 'value') %>%
  mutate(region = 'LA County')
df_all <- rbind(df_all, df_temp)

df_temp_orig <- read.csv("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv")

df_temp <- df_temp_orig %>%
  filter(county == "Los Angeles") %>%
  select(-county, -fips) %>%
  as_tibble() %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  select(date, daily_deaths = new_deaths) %>%
  na.omit() %>%
  pivot_longer(., -date, names_to = "metric", values_to = "value") %>%
  mutate(region = 'LA County')
df_all <- rbind(df_all, df_temp)

# dallas county data ----
download.file("https://dshs.texas.gov/coronavirus/TexasCOVID19DailyCountyFatalityCountData.xlsx", "data/tx_daily_deaths.xlsx")
df_temp <- read_excel(glue("data/tx_daily_deaths.xlsx"), skip = 2, n_max = 254) %>%
  filter(`County Name` == 'DALLAS') %>%
  pivot_longer(., -`County Name`, names_to = 'date', values_to = 'value') %>%
  select(-`County Name`) %>%
  arrange(date) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         metric = 'daily_deaths',
         region = 'Dallas County',
         value = c(NA, diff(value))) %>%
  na.omit()
df_all <- rbind(df_all, df_temp)

# world data ----
df_temp <- covid19() %>% 
  filter(id %in% c("GTM", "THA")) %>%
  select(date, region = id, daily_deaths = deaths, daily_total_hospitalizations = hosp, daily_total_icu = icu) %>%
  mutate(daily_deaths = c(NA, diff(daily_deaths))) %>%
  ungroup() %>%
  mutate(region = case_when(
    region == "GTM" ~ "Guatemala",
    region == "THA" ~ "Thailand"
  )) %>%
  pivot_longer(., -c(date, region), names_to = 'metric', values_to = 'value')
df_all <- rbind(df_all, df_temp)

# 7-day moving average
df <- df_all %>%
  arrange(date) %>%
  group_by(region, metric) %>%
  mutate(value_sma7 = SMA(value, 7)) %>%
  pivot_longer(., c(value, value_sma7), names_to = 'smooth', values_to = 'value') %>%
  mutate(smooth = ifelse(smooth == 'value_sma7', T, F),
         proportion = NA)

# population proportion ----
for (i in 1:nrow(df)) {
  population_code <- df$region[i] %>%
    tolower() %>%
    str_replace(., " ", "_") %>%
    paste0("population_", .)
  df$proportion[i] <- df$value[i] / eval(as.name(population_code))
}

df <- df %>%
  pivot_longer(., c(value, proportion), names_to = 'proportion', values_to = 'value') %>%
  mutate(proportion = ifelse(proportion == 'proportion', T, F)) %>%
  na.omit()

out_file <- glue("data/data.csv")
write.csv(df, out_file, row.names = F)
