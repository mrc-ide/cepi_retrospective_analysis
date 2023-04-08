vacc_allocation_plot <- function(scenarios, scenario_objects, fit, combine = TRUE, end_date) {

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
  )) %>%
    filter(date <= end_date)
  df$Vaccine <- factor(df$Vaccine, levels = c("Baseline",
                                              "Science",
                                              "Science & Infrastructure",
                                              "Science & Manufacturing",
                                              "Science Total (Manu. & Infr.)"))

  # get forty_vac
  pop_size <- squire::get_population(fit$parameters$country)$n
  forty_vac <- (sum(pop_size[-(1:3)])*0.4)

  # Demonstration Plot of it all together

  # daily primary doses
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

  # cumulative primary doses
  gg2 <- df %>%
    group_by(Vaccine) %>%
    mutate(doses = cumsum(primary)) %>%
    complete(date = seq.Date(min(date), max(df$date), 1)) %>%
    fill(doses, .direction = "down") %>%
    filter(date > as.Date("2020-01-08")) %>%
    ggplot(aes(x = as.integer(date - cepi_start_date)+100, doses, color = Vaccine)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    #geom_vline(xintercept = as.integer(as.Date("2022-01-01") - scenario_objects[[ii]]$inputs$start_date)) +
    #geom_hline(yintercept = forty_vac) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "top",
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name = "", values = c("Black", pals::stepped3()[c(1,5,9,13)])) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14)) +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Cumulative Primary Vaccinations")

  # daily booster doses
  gg3 <- df %>%
    group_by(Vaccine) %>%
    mutate(doses = (booster)) %>%
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
    ylab("Daily Booster Vaccine Doses")

  # cumulative primary doses
  gg4 <- df %>%
    group_by(Vaccine) %>%
    mutate(doses = cumsum(booster)) %>%
    complete(date = seq.Date(min(date), max(df$date), 1)) %>%
    fill(doses, .direction = "down") %>%
    filter(date > as.Date("2020-01-08")) %>%
    ggplot(aes(x = as.integer(date - cepi_start_date)+100, doses, color = Vaccine)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    #geom_vline(xintercept = as.integer(as.Date("2022-01-01") - scenario_objects[[ii]]$inputs$start_date)) +
    #geom_hline(yintercept = forty_vac) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "top",
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name = "", values = c("Black", pals::stepped3()[c(1,5,9,13)])) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14)) +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Cumulative Booster Vaccinations")

  if(combine) {
  cowplot::plot_grid(gg1, gg2 + theme(legend.position = "none"),
                     gg3, gg4 + theme(legend.position = "none"),
                     ncol = 2, align = "v", rel_heights = c(1,0.8), byrow = FALSE)
  } else {
  list(gg1, gg2, gg3, gg4)
  }
}

rt_scenario_plot <- function(scenarios, scenario_objects, fit, end_date) {

  # create our vaccine allocation dataframe
  rt_list <- list()
  for(i in seq_along(unique(scenarios$Rt))) {
    ii <- match(unique(scenarios$Rt), scenarios$Rt)[i]
    rt <- squire.page:::get_Rt(scenario_objects[[ii]])
    rt$scenario <- scenarios$Rt[ii]
    rt_list[[i]] <- rt
  }
  rt_list[[length(rt_list) + 1]] <- squire.page:::get_Rt(fit)
  rt_list[[length(rt_list)]]$scenario <- "baseline"

  # bind and summarise
  df <- do.call(rbind, rt_list)
  df <- df %>%
    filter(date > as.Date("2020-01-08")) %>%
    group_by(scenario, date) %>%
    summarise(Rt = median(Rt, na.rm=TRUE)) %>%
    rename(rt = Rt,
           Rt = scenario) %>%
    describe_scenarios() %>%
    filter(date <= end_date)

  # Demonstration Plot of it all together
  gg <- df %>%
    ggplot(aes(x = as.integer(date - cepi_start_date) + 100, rt, color = Rt)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    geom_smooth(method = "loess", span = 0.02, se = FALSE) +
    geom_smooth(method = "loess", span = 0.02, se = FALSE, data = . %>% filter(Rt == "Public Health Optimum")) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(),
          panel.grid.major = element_line(),
          legend.key = element_rect(fill = "white")) +
    scale_color_discrete(name = "Scenario:") +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Rt (Science Vaccine Scenario)") +
    scale_color_manual(name = "", values = c("Black", "yellow3", "pink3")) +
    guides(color=guide_legend(nrow=1, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14), legend.position = "top")

  gg

}

rt_complex_scenario_plot <- function(scenarios, scenario_objects, fit, end_date) {

  # create our vaccine allocation dataframe
  rt_list <- list()
  for(i in seq_along(scenarios$Rt)) {
    rt <- squire.page:::get_Rt(scenario_objects[[i]])
    rt$scenario <- scenarios$Rt[i]
    rt$Vaccine <- scenarios$Vaccine[i]
    rt_list[[i]] <- rt
  }
  rt_list[[length(rt_list) + 1]] <- squire.page:::get_Rt(fit)
  rt_list[[length(rt_list)]]$scenario <- "baseline"
  rt_list[[length(rt_list)]]$Vaccine <- unique(scenarios$Vaccine)[1]

  for(i in seq_along(unique(scenarios$Vaccine))[-1]) {
  rt_list[[length(rt_list) + 1]] <- rt_list[[length(rt_list)]]
  rt_list[[length(rt_list)]]$Vaccine <- unique(scenarios$Vaccine)[i]
  }

  # bind and summarise
  df <- do.call(rbind, rt_list)
  df <- df %>%
    filter(date > as.Date("2020-01-08")) %>%
    group_by(scenario, Vaccine, date) %>%
    summarise(Rt = median(Rt, na.rm=TRUE)) %>%
    rename(rt = Rt,
           Rt = scenario) %>%
    describe_scenarios() %>%
    filter(date <= end_date)

  # Demonstration Plot of it all together
  gg <- df %>%
    ggplot(aes(x = as.integer(date - cepi_start_date) + 100, rt, color = Rt)) +
    geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
    geom_smooth(method = "loess", span = 0.02, se = FALSE) +
    geom_smooth(method = "loess", span = 0.02, se = FALSE, data = . %>% filter(Rt == "Public Health Optimum")) +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(),
          panel.grid.major = element_line(),
          legend.key = element_rect(fill = "white")) +
    scale_color_discrete(name = "Scenario:") +
    xlab("Days Since Recognition of COVID-19") +
    ylab("Rt") +
    scale_color_manual(name = "", values = c("Black", "yellow3", "pink3")) +
    guides(color=guide_legend(nrow=1, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14), legend.position = "top") +
    facet_wrap(~Vaccine) +
    theme(panel.border = element_rect(color = "black", fill = NA))

  gg

}

combine_plot_outputs <- function(vacc_plot, rt_plot, death_plot, death_averted_plot){

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
      paste0(fit$parameters$country, " (", iso3c, ") - 100 Day Mission Scenarios"),
      fontface = 'bold',
      x = 0.5
    )

  row1 <- cowplot::plot_grid(death_plot, line_v, death_averted_plot,
                             ncol = 3, rel_widths = c(1,0.02,1))

  row2a <- cowplot::plot_grid(vacc_plot[[2]], vacc_plot[[4]] + theme(legend.position = "none"),
                             ncol = 1, rel_heights = c(1,0.8), align = "v")

  row2 <- cowplot::plot_grid(row2a, line_v, rt_plot,
                             ncol = 3, rel_widths = c(1,0.02,1))

  outplot <-  cowplot::plot_grid(row1, line_h, row2,
                                  ncol = 1, rel_heights = c(1.1, 0.05, 1))

  final <- cowplot::plot_grid(title, line_h, outplot,
                                ncol = 1, rel_heights = c(0.05,0.02,1))

  return(final)

}
