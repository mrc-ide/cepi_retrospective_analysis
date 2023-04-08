# ---------------------------------------------------------------------------- #
# 1. Set and read in global variables/data from previous runs
# ---------------------------------------------------------------------------- #

# Files to be read in
income_boosts <- readRDS("income_boosts.Rds")

# Make sure provided parameters are the correct class
cepi_start_date <- as.Date(cepi_start_date)
equity_speed <- as.numeric(equity_speed)
booster <- as.logical(booster)
simulate_counterfactuals <- as.logical(simulate_counterfactuals)
excess_mortality <- as.logical(excess_mortality)
iso3c <- as.character(iso3c)

end_date <- as.Date("2022-01-01")

# ---------------------------------------------------------------------------- #
# 2. Get fit and prepare scenarios
# ---------------------------------------------------------------------------- #

## Get fit from github
fit <- grab_fit(iso3c, excess_mortality, booster)
fit$inputs$data <- fit$inputs$data %>% filter((date_start <= end_date))

## Setup Scenarios
scenarios <- read_csv("scenarios.csv")

# Note have just set to the default here for Rt
scenario_objects <- implement_scenarios(fit, scenarios, iso3c, force_opening)

# Plot of our vaccine and Rt scenarios
vacc_plot <- vacc_allocation_plot(scenarios, scenario_objects, fit, combine = FALSE, end_date)
#rt_plot <- rt_scenario_plot(scenarios, scenario_objects, fit)
rt_plot <- rt_complex_scenario_plot(scenarios, scenario_objects, fit, end_date)

#calculate a measure openness gained over the baseline in each scenario
calculate_openness(fit, scenario_objects, end_date) %>%
  saveRDS("gain_in_openness.Rds")

# ---------------------------------------------------------------------------- #
# 3. Run new scenarios
# ---------------------------------------------------------------------------- #

# start results creation in data directory
dir.create("data")
if(simulate_counterfactuals){

  # future::plan(future::multisession()) #not sure what the best way to do this in an orderly task is

  original_out <- squire.page::generate_draws(fit)

  ## Run simulations and export (roughly ~<1 minute per scenario on a 12 core machine)
  walk(seq_len(nrow(scenarios)), function(i){
    early_vacc <- any(scenario_objects[[i]]$parameters$tt_primary_doses < 0)
    if(early_vacc){
      out <- simulate_early_vaccinations(scenario_objects[[i]]) %>%
        squire.page::generate_draws(project_forwards = TRUE)
    } else {
      out <- squire.page::generate_draws(scenario_objects[[i]])
    }
    save_scenario(out, i)
  }, .progress = TRUE)

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
  death_plot <- plot_deaths(scenarios, FALSE)
  death_averted_plot <- plot_deaths_averted(scenarios, FALSE)

} else {
  walk(paste0("data/", c("age_baseline", "age_scenarios", "time_baseline", "time_scenarios"), ".Rds"),
       ~saveRDS(NULL, .x))
  death_plot <- NULL
  death_averted_plot <- NULL
}

# ---------------------------------------------------------------------------- #
# 4. Process outputs
# ---------------------------------------------------------------------------- #

outplot <- combine_plot_outputs(vacc_plot, rt_plot, death_plot, death_averted_plot)

#plot output
ggsave("scenario_plot.pdf", outplot, width = 19, height = 16)

# close the session
# future::plan(future::sequential())
