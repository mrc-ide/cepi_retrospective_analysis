vacc_allocation_plot <- function(scenarios, scenario_objects, fit, combine = TRUE) {

  # create our vaccine allocation dataframe
  vacc_list <- list()
  for(i in seq_along(unique(scenarios$Vaccine))) {
    ii <- match(unique(scenarios$Vaccine), scenarios$Vaccine)[i]
    vacc_list[[i]] <- data.frame(
      "date" = scenario_objects[[ii]]$parameters$tt_booster_doses + scenario_objects[[ii]]$inputs$start_date,
      "primary" = scenario_objects[[ii]]$parameters$primary_doses,
      "booster" = scenario_objects[[ii]]$parameters$booster_doses,
      "scenario" = scenarios$Vaccine[ii]
    )
  }
  vacc_list[[length(vacc_list) + 1]] <- data.frame(
    "date" = fit$parameters$tt_booster_doses + fit$inputs$start_date,
    "primary" = fit$parameters$primary_doses,
    "booster" = fit$parameters$booster_doses,
    "scenario" = "baseline"
  )

  df <- do.call(rbind, vacc_list)

  # Demonstration Plot of it all together
  gg2 <- df %>%
    group_by(scenario) %>%
    mutate(doses = cumsum(primary)) %>%
    complete(date = seq.Date(min(date), max(df$date), 1)) %>%
    fill(doses, .direction = "down") %>%
    filter(date > as.Date("2020-01-08")) %>%
    ggplot(aes(x = as.integer(date - cepi_start_date)+100, doses, color = scenario)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "right",
          legend.key = element_rect(fill = "white")) +
    scale_color_discrete(name = "Scenario:") +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Cumulative Primary Vaccine Doses")

  gg1 <- df %>%
    group_by(scenario) %>%
    mutate(doses = (primary)) %>%
    complete(date = seq.Date(min(date), max(df$date), 1)) %>%
    fill(doses, .direction = "down") %>%
    filter(date > as.Date("2020-01-08")) %>%
    ggplot(aes(x = as.integer(date - cepi_start_date)+100, doses, color = scenario)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "right",
          legend.key = element_rect(fill = "white")) +
    scale_color_discrete(name = "Scenario:") +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Daily Primary Vaccine Doses")

  if(combine) {
  cowplot::plot_grid(gg1, gg2, ncol = 1, align = "v")
  } else {
  list(gg1, gg2)
  }
}

rt_scenario_plot <- function(scenarios, scenario_objects, fit) {

  # create our vaccine allocation dataframe
  rt_list <- list()
  for(i in seq_along(unique(scenarios$Rt))) {
    ii <- match(unique(scenarios$Rt), scenarios$Rt)[i]
    rt <- squire.page:::get_Rt(scenario_objects[[ii]])
    rt$scenario <- scenarios$Rt[ii]
    rt <-
    rt_list[[i]] <- rt
  }
  rt_list[[length(rt_list) + 1]] <- squire.page:::get_Rt(fit)
  rt_list[[length(rt_list)]]$scenario <- "baseline"

  # bind and summarise
  df <- do.call(rbind, rt_list)
  df <- df %>%
    filter(date > as.Date("2020-01-08")) %>%
    group_by(scenario, date) %>%
    summarise(Rt = median(Rt))

  # Demonstration Plot of it all together
  gg <- df %>%
    ggplot(aes(x = as.integer(date - cepi_start_date) + 100, Rt, color = scenario)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    #geom_line(lwd = 1) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "loess", span = 0.02, se = FALSE) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "right",
          legend.key = element_rect(fill = "white")) +
    scale_color_discrete(name = "Scenario:") +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Rt")

  gg

}
