orderly::orderly_develop_start("run_simulations", list(iso3c = "KEN", excess_mortality = TRUE))
booster <- TRUE

## Get fit from github
fit <- grab_fit(iso3c, excess_mortality, booster)
original_out <- squire.page::generate_draws(fit, pars.list = NULL, draws = NULL, parallel = TRUE)

## Setup Scenarios
scenarios <- read_csv("scenarios.csv")

# Note have just set to the default here for Rt
scenario_objects <- implement_scenarios(fit, scenarios, iso3c)

# Plot of our vaccine scenarios
vacc_gg <- vacc_allocation_plot(scenarios, scenario_objects, fit)
ggsave("scenario_plot.pdf", vac_gg) #update this to the diagnostic plot + scenario output plot


## Run simulations and export (roughly 3 minutes per scenario?)
dir.create("data")
walk(seq_len(nrow(scenarios)), function(i){
  out <- squire.page::generate_draws(scenario_objects[[i]])
  save_scenario(out, i)
})

#recombine into a single file
walk(c("time", "age"), function(name){
  map_dfr(seq_len(nrow(scenarios)), function(scenario){
    out <- readRDS(paste0("data/", name, "_", scenario, ".Rds")) %>%
      mutate(scenario = scenario)
    unlink(paste0("data/", name, "_", scenario, ".Rds"))
    out
  }) %>%
    saveRDS(paste0("data/", name, "_scenarios.Rds"))
})

#export baseline
save_scenario(original_out, "baseline")
