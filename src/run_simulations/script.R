orderly::orderly_develop_start("run_simulations", list(iso3c = "KEN", excess_mortality = TRUE))
booster <- TRUE
future::plan(future::multisession()) #not sure what the best way to do this in an orderly task is
cepi_start_date <- as.Date("2020-04-20")

## Get fit from github
fit <- grab_fit(iso3c, excess_mortality, booster)
original_out <- squire.page::generate_draws(fit)

## Setup Scenarios
scenarios <- read_csv("scenarios.csv")

# Note have just set to the default here for Rt
scenario_objects <- implement_scenarios(fit, scenarios, iso3c)

# Plot of our vaccine and Rt scenarios
output_plot <- vacc_allocation_plot(scenarios, scenario_objects, fit, combine = FALSE)
rt_plot <- rt_scenario_plot(scenarios, scenario_objects, fit)
input_plot <- cowplot::plot_grid(
  output_plot[[1]],
  output_plot[[2]] + theme(legend.position = "none"),
  rt_plot, ncol = 1, align = "v",
  rel_heights = c(1,0.8,1))

if(simulate_counterfactuals){
  ## Run simulations and export (roughly ~<1 minute per scenario on a 12 core machine)
  dir.create("data")
  walk(seq_len(nrow(scenarios)), function(i){
    out <- squire.page::generate_draws(scenario_objects[[i]])
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
  walk(paste0("data/", c("age_baseline", "age_scenarios", "time_baseline", "time_scenarios"), ".Rds")
       ~saveRDS(NULL, .x))
  death_plot <- NULL
  deaths_averted_plot <- NULL
}

# vertical line to separate plots
line_v <- ggplot() + cowplot::draw_line(x = 0,y=1:10, colour = "grey") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# horizintal line to separate plots
line_h <- ggplot() + cowplot::draw_line(x = 1:10,y=0, colour = "grey") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# title for the plot
title <- cowplot::ggdraw() +
  cowplot::draw_label(
    paste0(fit$parameters$country, " - 100 Day Mission Scenarios"),
    fontface = 'bold',
    x = 0.5
  )


outplot <- cowplot::plot_grid(input_plot, line_v, death_plot, line_v, death_averted_plot,
                              ncol = 5, rel_widths = c(1,0.1,1,0.1,1))


outplot <- cowplot::plot_grid(title, line_h, outplot,
                              ncol = 1, rel_heights = c(0.05,0.03,1))

#plot output
ggsave("scenario_plot.pdf", outplot, width = 30, height = 14)
