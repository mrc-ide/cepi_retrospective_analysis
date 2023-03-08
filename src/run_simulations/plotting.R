vacc_allocation_plot <- function(scenarios, scenario_objects, fit, combine = TRUE) {

  # create our vaccine allocation dataframe
  vacc_list <- list()
  for(i in seq_along(unique(scenarios$Vaccine))) {
    ii <- match(unique(scenarios$Vaccine), scenarios$Vaccine)[i]
    vacc_list[[i]] <- data.frame(
      "date" = scenario_objects[[ii]]$parameters$tt_booster_doses + scenario_objects[[ii]]$inputs$start_date,
      "primary" = scenario_objects[[ii]]$parameters$primary_doses,
      "booster" = scenario_objects[[ii]]$parameters$booster_doses,
      "Vaccine" = scenarios$Vaccine[ii]
    )
  }
  df <- do.call(rbind, vacc_list)
  df <- df %>% describe_scenarios()
  df <- rbind(df,
              data.frame(
    "date" = fit$parameters$tt_booster_doses + fit$inputs$start_date,
    "primary" = fit$parameters$primary_doses,
    "booster" = fit$parameters$booster_doses,
    "Vaccine" = "Baseline"
  ))
  df$Vaccine <- factor(df$Vaccine, levels = c("Baseline",
                                              "Science",
                                              "Science & Infrastructure",
                                              "Science & Manufacturing",
                                              "Science Total (Manu. & Infr.)"))

  # Demonstration Plot of it all together
  gg2 <- df %>%
    group_by(Vaccine) %>%
    mutate(doses = cumsum(primary)) %>%
    complete(date = seq.Date(min(date), max(df$date), 1)) %>%
    fill(doses, .direction = "down") %>%
    filter(date > as.Date("2020-01-08")) %>%
    ggplot(aes(x = as.integer(date - cepi_start_date)+100, doses, color = Vaccine)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "top",
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name = "", values = c("Black", pals::stepped3()[c(1,5,9,13)])) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14)) +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Cumulative Primary Vaccine Doses")

  gg1 <- df %>%
    group_by(Vaccine) %>%
    mutate(doses = (primary)) %>%
    complete(date = seq.Date(min(date), max(df$date), 1)) %>%
    fill(doses, .direction = "down") %>%
    filter(date > as.Date("2020-01-08")) %>%
    ggplot(aes(x = as.integer(date - cepi_start_date)+100, doses, color = Vaccine)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "top",
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name = "", values = c("Black", pals::stepped3()[c(1,5,9,13)])) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14)) +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Daily Primary Vaccine Doses")

  if(combine) {
  cowplot::plot_grid(gg1, gg2 + theme(legend.position = "none"), ncol = 1, align = "v", rel_heights = c(1,0.8))
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
    summarise(Rt = median(Rt)) %>%
    rename(rt = Rt,
           Rt = scenario) %>%
    describe_scenarios()

  # Demonstration Plot of it all together
  gg <- df %>%
    ggplot(aes(x = as.integer(date - cepi_start_date) + 100, rt, color = Rt)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    #geom_line(lwd = 1) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "loess", span = 0.02, se = FALSE) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(),
          panel.grid.major = element_line(),
          legend.key = element_rect(fill = "white")) +
    scale_color_discrete(name = "Scenario:") +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Rt") +
    scale_color_manual(name = "", values = c(pals::stepped2()[c(13,1,5)])) +
    guides(color=guide_legend(nrow=1, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14), legend.position = "top")

  gg

}
