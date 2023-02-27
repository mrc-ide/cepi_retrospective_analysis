iso3c <- "GBR"
excess_mortality <- FALSE
booster <- TRUE

## Get fit from github
fit <- grab_fit(iso3c, excess_mortality, booster)
original_out <- squire.page::generate_draws(fit, pars.list = NULL, draws = NULL, parallel = TRUE)

## Setup Scenarios
scenarios <- read_csv("scenarios.csv")

# Note have just set to the default here
scenario_objects <- implement_scenarios(fit, scenarios[1,])

## Run simulations
scenario_out <- squire.page::generate_draws(scenario_objects[[1]], pars.list = NULL, draws = NULL, parallel = TRUE)

## Format and export
# Note have been testing this in demo_check_baseline at the moment
