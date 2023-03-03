cdppl <- function (res, extra_df = NULL)
{
  date_0 <- squire.page:::get_data_end_date_inner(res)
  data <- squire.page:::get_data(res)
  data$date <- squire.page:::get_dates_greater(res)
  data$deaths <- cumsum(data$deaths)
  d <- nimue:::odin_index(res$model)
  pd <- do.call(rbind, lapply(seq_len(dim(res$output)[3]), function(x) {
    df <- data.frame(value = rowSums(res$output[,d$D,x]), compartment = "D")
    df$t <- seq_len(nrow(df)) - nrow(df)
    df$replicate <- x
    df$date <- df$t + date_0
    return(df)
  }))
  pds <- pd %>% dplyr::group_by(date) %>%
    dplyr::summarise(ymin = stats::quantile(.data$value, 0.025, na.rm = TRUE),
                     ymax = stats::quantile(.data$value, 0.975, na.rm = TRUE),
                     y = median(.data$value, na.rm = TRUE))
  p <- ggplot2::ggplot()
  p <- p + ggplot2::geom_line(data = pds, ggplot2::aes(x = .data$date,
                                                       y = .data$y),
                              color = "red")
  p <- p + ggplot2::geom_ribbon(data = pds, ggplot2::aes(x = .data$date,
                                                         ymin = .data$ymin, ymax = .data$ymax),
                                fill = "red",
                                alpha = 0.25, col = NA)
  suppressWarnings(cdp <- p +
                     ggplot2::theme_bw() + ggplot2::theme(legend.position = "none",
                                                          axis.title.x = ggplot2::element_blank()) + ggplot2::ylab("Cumulative Deaths") +
                     ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
                     ggplot2::xlab("") + ggplot2::geom_line(data = data, ggplot2::aes(x = .data$date,
                                                                                      y = .data$deaths), linetype = "dashed"))
  if (!is.null(extra_df)) {
    cdp <- cdp + ggplot2::geom_line(data = extra_df, ggplot2::aes(x = .data$date,
                                                                  y = cumsum(.data$deaths)), alpha = 0.5, colour = "green") +
      ggplot2::geom_ribbon(data = extra_df, ggplot2::aes(x = .data$date,
                                                         ymin = cumsum(.data$bot), ymax = cumsum(.data$top)),
                           alpha = 0.25, fill = "green")
  }
  cdp + geomtextpath::geom_textvline(xintercept = pds$date[seq(15,nrow(pds), 14)], label = seq_along(seq(15,nrow(pds), 14)), hjust = 0.9)
}

quick_format <- function(x, var_select, date_0) {

  d <- nimue:::odin_index(x$model)

  do.call(rbind,lapply(var_select, function(i){
  do.call(rbind, lapply(seq_len(dim(x$output)[3]), function(y) {
    df <- data.frame(y = rowSums(x$output[,d[[i]],y]), compartment = i)
    df$t <- seq_len(nrow(df)) - nrow(df)
    df$replicate <- y
    df$date <- df$t + date_0
    return(df)
  }))
  }))

}

nimue_simulation_plot_prep <- function (x, var_select, q = c(0.025, 0.975), summary_f = mean,
          x_var = "t", ...)
{
  pd <- quick_format(x, var_select = var_select, ...)
  pd <- pd %>% dplyr::mutate(x = .data[[x_var]])
  if (x_var == "t") {
    pd$x <- round(pd$x, ceiling(log10(1/x$parameters$dt)))
  }
  if (sum(is.na(pd$t) | is.na(pd$y)) > 0) {
    pd <- pd[-which(is.na(pd$t) | is.na(pd$y)), ]
  }
  pds <- pd %>% dplyr::group_by(.data$x, .data$compartment) %>%
    dplyr::summarise(ymin = stats::quantile(.data$y, q[1]),
                     ymax = stats::quantile(.data$y, q[2]), y = summary_f(.data$y))
  return(list(pd = pd, pds = pds))
}


comp_plot <- function (r_list, scenarios,
                       var_select = NULL,
                       replicates = FALSE,
                       summarise = TRUE,
                       ci = TRUE,
                       q = c(0.025, 0.975),
                       summary_f = median,
                       date_0 = squire.page:::get_data_end_date(r_list[[1]]),
                       x_var = "date"){
  squire:::assert_list(r_list)
  squire:::assert_string(scenarios)
  squire:::assert_same_length(r_list, scenarios)

  pd_list <- lapply(r_list, FUN = nimue_simulation_plot_prep,
                    var_select = var_select, x_var = x_var, q = q, summary_f = summary_f,
                    date_0 = date_0)

  for (i in seq_along(scenarios)) {
    pd_list[[i]]$pd$Scenario <- scenarios[i]
    pd_list[[i]]$pds$Scenario <- scenarios[i]
  }
  pds <- do.call(rbind, lapply(pd_list, "[[", "pds"))
  pd <- do.call(rbind, lapply(pd_list, "[[", "pd"))
  p <- ggplot2::ggplot()
  if (replicates) {
    p <- p + ggplot2::geom_line(data = pd, ggplot2::aes(x = .data$x,
                                                        y = .data$y, col = .data$Scenario, linetype = .data$compartment,
                                                        group = interaction(.data$compartment, .data$replicate,
                                                                            .data$Scenario)), alpha = max(0.2, 1/r_list[[1]]$parameters$replicates))
  }
  if (summarise) {
    p <- p + ggplot2::geom_line(data = pds, ggplot2::aes(x = .data$x,
                                                         y = .data$y, col = .data$Scenario, linetype = .data$compartment))
  }
  if (ci) {
    p <- p + ggplot2::geom_ribbon(data = pds, ggplot2::aes(x = .data$x,
                                                           ymin = .data$ymin, ymax = .data$ymax, fill = .data$Scenario,
                                                           linetype = .data$compartment), alpha = 0.25, col = "black")
  }
  p <- p + ggplot2::scale_color_discrete(name = "") + ggplot2::scale_fill_discrete(guide = FALSE) +
    ggplot2::xlab("Time") + ggplot2::ylab("N") + ggplot2::theme_bw() +
    guides(linetype = "none")

  if(length(var_select) == 1) {
    p <- p + ylab(var_select)
  }

  return(p)
}

r_list <- list(scenario_out, original_out)

pv <- comp_plot(r_list = r_list,
          scenarios = c("100-day","Real World"),
          var_select = c("vaccinated_second_waned"),
          ci = FALSE,
          summarise = TRUE)

pb <- comp_plot(r_list = r_list,
                scenarios = c("100-day","Real World"),
                #var_select = c("priorvaccinated"),
                var_select = c("vaccinated_booster_waned"),
                ci = FALSE,
                summarise = TRUE)

v <- comp_plot(r_list = r_list,
          scenarios = c("100-day","Real World"),
          var_select = c("vaccinated_second_dose"),
          ci = FALSE,
          summarise = TRUE) +
  geomtextpath::geom_texthline(yintercept = 53091328, linetype = "dashed", label = "95% adults vaccinated")

bv <- comp_plot(r_list = r_list,
          scenarios = c("100-day","Real World"),
          var_select = c("vaccinated_booster_dose"),
          ci = FALSE,
          summarise = TRUE)

inf <- comp_plot(r_list = r_list,
               scenarios = c("100-day","Real World"),
               var_select = c("infections_cumu"),
               ci = TRUE,
               summarise = TRUE)

d <- comp_plot(r_list = r_list,
                              scenarios = c("100-day","Real World"),
                              var_select = c("D"),
                              ci = TRUE,
                              summarise = TRUE) +
  geomtextpath::geom_textvline(label = "Dec 8, 2021", xintercept = as.Date("2021-12-08"),
                               hjust = 0.1, size = 3) +
  geom_line(data = r_list[[1]]$inputs$data %>%
              mutate(cumu = cumsum(deaths),
                     date_mid = date_start + 3),
            aes(x = date_start, y = cumu), inherit.aes = FALSE,
            linetype = "dashed", lwd = 1)


gg <- cowplot::plot_grid(d + theme(legend.position = "none"),
                         inf + theme(legend.position = "none"),
                         v + theme(legend.position = "none"),
                         bv + theme(legend.position = "none"),
                         pv + theme(legend.position = "none"),
                         pb + theme(legend.position = "none"),
                         ncol = 2)

cowplot::plot_grid(
  gg,
  cowplot::get_legend(
    d + scale_color_discrete(name = "Scenario") +
      theme(legend.text = element_text(size = 12), legend.title = element_text(size = 14))
    ), ncol = 2, rel_widths = c(1,0.2))
