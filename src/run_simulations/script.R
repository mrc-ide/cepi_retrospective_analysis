orderly::orderly_develop_start("run_simulations", list(iso3c = "KEN", excess_mortality = TRUE))
booster <- TRUE
future::plan(future::multisession()) #not sure what the best way to do this in an orderly task is

## Get fit from github
fit <- grab_fit(iso3c, excess_mortality, booster)
original_out <- squire.page::generate_draws(fit)

## Setup Scenarios
scenarios <- read_csv("scenarios.csv")

# Note have just set to the default here for Rt
scenario_objects <- implement_scenarios(fit, scenarios, iso3c)

# Plot of our vaccine scenarios
output_plot <- vacc_allocation_plot(scenarios, scenario_objects, fit)

if(simulate_counterfactuals){
  ## Run simulations and export (roughly ~<1 minute per scenario on a 12 core machine)
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

  #Add counterfactual comparisons
  output_plot <- cowplot::plot_grid(
    plot_deaths(scenarios), output_plot

  )
} else {
  walk(paste0("data/", c("age_baseline", "age_scenarios", "time_baseline", "time_scenarios"), ".Rds")
       ~saveRDS(NULL, .x))
}


#plot output
ggsave("scenario_plot.pdf", output_plot, width = 17, height = 10) #need to add Rt diagnostics to this too
