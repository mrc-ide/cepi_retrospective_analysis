iso3c <- "GBR"
excess_mortality <- TRUE

## Get fit from github
fit <- grab_fit(iso3c, excess_mortality)
## Setup Scenarios
scenarios <- read_csv("scenarios.csv")
scenario_objects <- implement_scenarios(fit, scenarios)
## Run simulations

## Format and export