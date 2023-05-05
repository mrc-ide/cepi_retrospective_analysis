flatten_name <- function(x, name){
  nms <- names(x)
  for(i in seq_along(x)) {
    x[[i]][[name]] <- nms[i]
  }
  do.call(rbind, x)
}

# get our scenario match table
source("src/run_simulations/funcs.R")
scenarios <- read.csv("src/run_simulations/scenarios.csv") %>%
  mutate(scenario = as.integer(rownames(.))) %>%
  describe_scenarios() %>%
  select(scenario, Rt, Vaccine)

# -------------------------------------------------- #
# Health Outcome Totals
# -------------------------------------------------- #

# Get out health outcomes
deaths <- readRDS("analysis/data_out/total_deaths_global.rds")
deaths_income <- readRDS("analysis/data_out/total_deaths_income.rds")

hosp <- readRDS("analysis/data_out/total_hospitalisations_global.rds")
hosp_income <- readRDS("analysis/data_out/total_hospitalisations_income.rds")

infections <- readRDS("analysis/data_out/total_infections_global.rds")
infections_income <- readRDS("analysis/data_out/total_infections_income.rds")

# Function to nice format ranges
create_range <- function(x, var = "averted", scale = 1e-6, unit = "", accuracy = 0.01, prefix = "", scen = 1) {
grab <- grep(paste0(paste0(var, "_", c("025","med","975")), collapse = "|"), names(x))
nums <- as.numeric((x %>% filter(scenario == scen))[,grab])

mid <- scales::unit_format(prefix = prefix, scale = scale, unit = unit, accuracy = accuracy)(sort(nums)[2])
range <- paste(scales::unit_format(scale = scale, unit = "", suffix = "", accuracy = accuracy)(sort(nums)[c(1, 3)]), collapse = " - ")
range <- paste0("(", range, ")")

combined <- paste(mid, range)
combined
}

# Combine together
income <- c("HIC", "UMIC", "LMIC", "LIC")

global_deaths <- create_range(deaths)
income_deaths <- do.call(rbind, lapply(income, function(x){deaths_income %>% filter(income == x) %>% create_range()}))

global_hosp <- create_range(hosp)
income_hosp <- do.call(rbind, lapply(income, function(x){hosp_income %>% filter(income == x) %>% create_range()}))

global_infections <- create_range(infections, var = "averted", scale = 1e-9, unit = "")
income_infections <- do.call(rbind, lapply(income, function(x){infections_income %>% filter(income == x) %>% create_range(var = "averted", scale = 1e-9, "")}))

# Bring together into one table
table_1 <- cbind(
  rbind(global_deaths, income_deaths),
  rbind(global_hosp, income_hosp),
  rbind(global_infections, income_infections)
  )
rownames(table_1) <- c("Worldwide", "High-Income", "Upper-middle-Income", "Lower-middle-Income","Low-Income")
colnames(table_1) <- c("Deaths Averted (Millions)",
                       "Hospitalisations Averted (Millions)",
                       "Infections Averted (Billions)")

write.csv(table_1, file = "analysis/data_out/table_one.csv")

# -------------------------------------------------- #
# Health Economic Outcome Totals
# -------------------------------------------------- #

vsl_total <- readRDS("analysis/data_out/total_lifeyears_global.rds")
vsl_income <- readRDS("analysis/data_out/total_lifeyears_income.rds")

hospcosts_total <- readRDS("analysis/data_out/total_hospcosts_global.rds")
hospcosts_income <- readRDS("analysis/data_out/total_hospcosts_income.rds")

# vsl
global_vsl <- create_range(vsl_total, "economic_lives_saved", scale = 1e-12, unit = "", prefix = "", accuracy = 0.01)
income_vsl <- do.call(rbind, lapply(income, function(x){
  accuracy <- ifelse(x == "LIC", 0.001, 0.01)
  vsl_income %>% filter(income == x) %>%
    create_range("economic_lives_saved", scale = 1e-12, unit = "", prefix = "", accuracy = accuracy)
  }))

# vsly
global_vsly <- create_range(vsl_total, "economic_life_years_saved", scale = 1e-12, unit = "", prefix = "", accuracy = 0.01)
income_vsly <- do.call(rbind, lapply(income, function(x){
  accuracy <- ifelse(x == "LIC", 0.001, 0.01)
  vsl_income %>% filter(income == x) %>%
    create_range("economic_life_years_saved", scale = 1e-12, unit = "", prefix = "", accuracy = accuracy)
}))

# productivity
global_productivity <- create_range(vsl_total, "economic_productive_loss", scale = 1e-12, unit = "", prefix = "", accuracy = 0.01)
income_productivity <- do.call(rbind, lapply(income, function(x){
  accuracy <- ifelse(x == "LIC", 0.0001, 0.01)
  vsl_income %>% filter(income == x) %>%
    create_range("economic_productive_loss", scale = 1e-12, unit = "", prefix = "", accuracy = accuracy)
}))

# hosp costs
global_hospcosts <- create_range(hospcosts_total, "hospitalisation_costs_averted", scale = 1e-9, unit = "", prefix = "", accuracy = 0.01)
income_hospcosts <- do.call(rbind, lapply(income, function(x){
  accuracy <- ifelse(x == "LIC", 0.001, 0.01)
  hospcosts_income %>% filter(income == x) %>%
    create_range("hospitalisation_costs_averted", scale = 1e-9, unit = "", prefix = "", accuracy = accuracy)
}))

# Bring together into one table
table_2 <- cbind(
  rbind(global_vsl, income_vsl),
  rbind(global_vsly, income_vsly),
  rbind(global_productivity, income_productivity),
  rbind(global_hospcosts, income_hospcosts)
)
rownames(table_2) <- c("Worldwide", "High-Income", "Upper-middle-Income", "Lower-middle-Income","Low-Income")
colnames(table_2) <- c("Value of Statistical Life ($, Trillions)",
                       "Value of Statistical Life Years ($, Trillions)",
                       "Productivity Losses Averted ($, Trillions)",
                       "Cost of Hospitalizations Averted ($, Billions)")

write.csv(table_2, file = "analysis/data_out/table_two.csv")


###

# Health Impacts Table
create_table_deaths_averted_for_scenario <- function(scen) {

# Combine together
income <- c("HIC", "UMIC", "LMIC", "LIC")

global_deaths <- create_range(deaths, scen = scen)
income_deaths <- do.call(rbind, lapply(income, function(x){deaths_income %>% filter(income == x) %>% create_range(scen = scen)}))

global_hosp <- create_range(hosp, scen = scen)
income_hosp <- do.call(rbind, lapply(income, function(x){hosp_income %>% filter(income == x) %>% create_range(scen = scen)}))

global_infections <- create_range(infections, var = "averted", scale = 1e-9, "", scen = scen)
income_infections <- do.call(rbind, lapply(income, function(x){infections_income %>% filter(income == x) %>% create_range(var = "averted",  scale = 1e-9, "", scen = scen)}))

# Bring together into one table
table_1 <- cbind(
  rbind(global_deaths, income_deaths),
  rbind(global_hosp, income_hosp),
  rbind(global_infections, income_infections)
)
rownames(table_1) <- c("Worldwide", "High-Income", "Upper-middle-Income", "Lower-middle-Income","Low-Income")
colnames(table_1) <- c("Deaths Averted (Millions)",
                       "Hospitalisations Averted (Millions)",
                       "Infections Averted (Billions)")

table_1

}

# Health Economic Impacts Table
create_table_vsl_averted_for_scenario <- function(scen) {

global_vsl <- create_range(vsl_total, "economic_lives_saved", scale = 1e-12, unit = "", prefix = "$", accuracy = 0.01, scen = scen)
income_vsl <- do.call(rbind, lapply(income, function(x){
  accuracy <- ifelse(x == "LIC", 0.001, 0.01)
  vsl_income %>% filter(income == x) %>%
    create_range("economic_lives_saved", scale = 1e-12, unit = "", prefix = "$", accuracy = accuracy, scen = scen)
}))

# vsly
global_vsly <- create_range(vsl_total, "economic_life_years_saved", scale = 1e-12, unit = "", prefix = "$", accuracy = 0.01, scen = scen)
income_vsly <- do.call(rbind, lapply(income, function(x){
  accuracy <- ifelse(x == "LIC", 0.001, 0.01)
  vsl_income %>% filter(income == x) %>%
    create_range("economic_life_years_saved", scale = 1e-12, unit = "", prefix = "$", accuracy = accuracy, scen = scen)
}))

# productivity
global_productivity <- create_range(vsl_total, "economic_productive_loss", scale = 1e-12, unit = "", prefix = "$", accuracy = 0.01, scen = scen)
income_productivity <- do.call(rbind, lapply(income, function(x){
  accuracy <- ifelse(x == "LIC", 0.0001, 0.01)
  vsl_income %>% filter(income == x) %>%
    create_range("economic_productive_loss", scale = 1e-12, unit = "", prefix = "$", accuracy = accuracy, scen = scen)
}))

# hosp costs
global_hospcosts <- create_range(hospcosts_total, "hospitalisation_costs_averted", scale = 1e-9, unit = "", prefix = "$", accuracy = 0.01, scen = scen)
income_hospcosts <- do.call(rbind, lapply(income, function(x){
  accuracy <- ifelse(x == "LIC", 0.001, 0.01)
  hospcosts_income %>% filter(income == x) %>%
    create_range("hospitalisation_costs_averted", scale = 1e-9, unit = "", prefix = "$", accuracy = accuracy, scen = scen)
}))

# Bring together into one table
table_2 <- cbind(
  rbind(global_vsl, income_vsl),
  rbind(global_vsly, income_vsly),
  rbind(global_productivity, income_productivity),
  rbind(global_hospcosts, income_hospcosts)
)
rownames(table_2) <- c("Worldwide", "High-Income", "Upper-middle-Income", "Lower-middle-Income","Low-Income")
colnames(table_2) <- c("Value of Statistical Life ($, Trillions)",
                       "Value of Statistical Life Years ($, Trillions)",
                       "Productivity Losses Averted ($, Trillions)",
                       "Cost of Hospitalizations Averted ($, Billions)")

table_2

}

# Economic Impacts Table
npi_total <- readRDS("analysis/data_out/total_npigains_global.rds")
npi_income <- readRDS("analysis/data_out/total_npigains_income.rds")
school_total <- readRDS("analysis/data_out/total_schoolweeks_global.rds")
school_income <- readRDS("analysis/data_out/total_schoolweeks_income.rds")

create_table_npis_averted_for_scenario <- function(scen) {

  global_npi <- create_range(npi_total, "gain_in_openness", scale = 1, unit = "", prefix = "", accuracy = 100, scen = scen)
  income_npi <- do.call(rbind, lapply(income, function(x){
    accuracy <- ifelse(x == "LIC", 1, 1)
    npi_income %>% filter(income == x) %>%
      create_range("gain_in_openness", scale = 1, unit = "", prefix = "", accuracy = 100, scen = scen)
  }))

  # schools
  global_school <- school_total %>%
    filter(scenario == scen) %>%
    select(extra_full_school_weeks_total, extra_partial_school_weeks_total) %>%
    mutate_all(.funs = function(x){scales::unit_format(accuracy = 10, scale = 1, unit = "", prefix = "", )(x)})

  income_school <- school_income %>%
    filter(scenario == scen) %>%
    select(extra_full_school_weeks_total, extra_partial_school_weeks_total) %>%
    mutate_all(.funs = function(x){scales::unit_format(accuracy = 10, scale = 1, unit = "", prefix = "", )(x)})

  # Bring together into one table
  table_2 <- cbind(
    rbind(global_npi, income_npi),
    rbind(global_school, income_school)
  )
  rownames(table_2) <- c("Worldwide", "High-Income", "Upper-middle-Income", "Lower-middle-Income","Low-Income")
  colnames(table_2) <- c("Additional Days Without NPIs",
                         "Extra Weeks of Schools Being Fully Open",
                         "Extra Weeks of Schools Being Partially Open")

  table_2

}

# Bring these all together
all_death_tables <- lapply(1:12, create_table_deaths_averted_for_scenario) %>%
  lapply(as.data.frame) %>%
  setNames(paste(scenarios$Rt, "and", scenarios$Vaccine)) %>%
  flatten_name("scenario") %>%
  mutate(`NPI Scenario` = gsub("(.*)( and )(.*)", "\\1", scenario), .before = `Deaths Averted (Millions)`) %>%
  mutate(`Vaccine Scenario` = gsub("(.*)( and )(.*)", "\\3", scenario), .before = `Deaths Averted (Millions)`) %>%
  select(-scenario)
all_death_tables <- all_death_tables %>% mutate(
  Income = strsplit(rownames(all_death_tables), ".", fixed = TRUE) %>% map_chr(.f = function(x){tail(x,1)}), .before = `NPI Scenario`
) %>% `rownames<-`(NULL)

all_vsl_tables <- lapply(1:12, create_table_vsl_averted_for_scenario) %>%
  lapply(as.data.frame) %>%
  setNames(paste(scenarios$Rt, "and", scenarios$Vaccine)) %>%
  flatten_name("scenario") %>%
  mutate(`NPI Scenario` = gsub("(.*)( and )(.*)", "\\1", scenario), .before = `Value of Statistical Life ($, Trillions)`) %>%
  mutate(`Vaccine Scenario` = gsub("(.*)( and )(.*)", "\\3", scenario), .before = `Value of Statistical Life ($, Trillions)`) %>%
  select(-scenario)
all_vsl_tables <- all_vsl_tables %>% mutate(
  Income = strsplit(rownames(all_vsl_tables), ".", fixed = TRUE) %>% map_chr(.f = function(x){tail(x,1)}), .before = `NPI Scenario`
) %>% `rownames<-`(NULL)

all_npi_tables <- lapply(1:12, create_table_npis_averted_for_scenario) %>%
  lapply(as.data.frame) %>%
  setNames(paste(scenarios$Rt, "and", scenarios$Vaccine)) %>%
  flatten_name("scenario") %>%
  mutate(`NPI Scenario` = gsub("(.*)( and )(.*)", "\\1", scenario), .before = `Additional Days Without NPIs`) %>%
  mutate(`Vaccine Scenario` = gsub("(.*)( and )(.*)", "\\3", scenario), .before = `Additional Days Without NPIs`) %>%
  select(-scenario)
all_npi_tables <- all_npi_tables %>% mutate(
  Income = strsplit(rownames(all_npi_tables), ".", fixed = TRUE) %>% map_chr(.f = function(x){tail(x,1)}), .before = `NPI Scenario`
) %>% `rownames<-`(NULL)
all_npi_tables <- all_npi_tables %>% filter(`NPI Scenario` != "History Based")

write.csv(all_death_tables, "analysis/data_out/appendix_scenario_health_tables.csv", row.names = FALSE)
write.csv(all_vsl_tables, "analysis/data_out/appendix_scenario_vsl_tables.csv", row.names = FALSE)
write.csv(all_npi_tables, "analysis/data_out/appendix_scenario_npi_tables.csv", row.names = FALSE)


