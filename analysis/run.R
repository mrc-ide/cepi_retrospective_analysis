iso3cs <- xml2::read_html("https://github.com/mrc-ide/nimue_global_fits/tree/main/excess_mortality")
iso3cs <- gsub("\\.Rds","",rvest::html_text(rvest::html_nodes(iso3cs,".js-navigation-open.Link--primary")))

# remove ERI and FSM for lack of vaccines at the moment
iso3cs <- iso3cs[-which(iso3cs %in% c("ERI", "FSM"))]

# ---------------------------------------------- #
# 1. Demo Runs ------------------------------------------------------------
# ---------------------------------------------- #

# orderly_ids <- vector("character", length(iso3cs))
pb <- progress::progress_bar$new(
  format = "  Orderly Running [:bar] :percent eta: :eta",
  total = length(orderly_ids), clear = FALSE, width= 60)
for(i in seq_along(iso3cs)[which(is.na(orderly_ids))]){
  #pb$tick()
  message(i)
  suppressWarnings(suppressMessages(
    orderly_ids[i] <- tryCatch(
      orderly::orderly_run(
        "run_simulations", parameters = list(iso3c = iso3cs[i], excess_mortality = TRUE, simulate_counterfactuals = FALSE),
        echo = FALSE
      ),
      error = function(e){NA}
    )
  ))
}

committed <- lapply(orderly_ids, orderly::orderly_commit)
pdfs <- file.path(list.files(file.path(here::here(), "archive/run_simulations"), full.names = TRUE), "scenario_plot.pdf")
pdftools::pdf_combine(pdfs[-c(1:2)], file.path(here::here(), "analysis/plots/scenario_plots.pdf"))

# ---------------------------------------------- #
# 2. Real Runs ------------------------------------------------------------
# ---------------------------------------------- #

library(foreach)
library(doParallel)
library(orderly)
library(tidyverse)

# Make sure we don't runs we have already run
orderly_ids <- data.frame("iso3cs" = iso3cs, "orderly_id" = NA)
seach_fn <- function(iso3c){
  paste0("latest(parameter:iso3c == '", iso3c,"' && parameter:simulate_counterfactuals == TRUE)")
}
# get the data frame of ones we need to run
orderly_ids <- orderly_ids %>% mutate(
  orderly_id = map_chr(iso3cs, ~orderly_search(seach_fn(.x), name = "run_simulations", draft = TRUE))
)

iso3c_to_run <- orderly_ids$iso3cs[is.na(orderly_ids$orderly_id)]
Sys.setenv("SQUIRE_PARALLEL_DEBUG" = "TRUE")

# set up cluster
n.cores <- 4
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()

finished_orderly_ids <- foreach(
  i = seq_along(iso3c_to_run),
  .combine = 'c'
) %dopar% {

  suppressWarnings(suppressMessages(
    tryCatch(
      orderly::orderly_run(
        "run_simulations", parameters = list(iso3c = iso3c_to_run[i], excess_mortality = TRUE, simulate_counterfactuals = TRUE),
        echo = FALSE
      ),
      error = function(e){NA}
    )
  ))

}

parallel::stopCluster(my.cluster)
orderly_ids$orderly_id[is.na(orderly_ids$orderly_id)] <- finished_orderly_ids
committed <- lapply(orderly_ids$orderly_id, orderly_commit)
saveRDS(orderly_ids, "analysis/data_out/orderly_ids.rds")

# ------------------------------------------------------------------------- #
# 3. Gather real run outputs ------------------------------------------------------------
# ------------------------------------------------------------------------- #

orderly_ids <- readRDS("analysis/data_out/orderly_ids.rds")
names(orderly_ids) <- c("iso3c", "id")
library(furrr)

collate_outputs <- function(orderly_ids, grouping = NULL, replicates = 100, type = "deaths", over_time = TRUE, max_date = as.Date("2022-01-01")){

  if(is.null(grouping)){
    grouping <- function(x){"temp"}
  }
  iso3cs <- mutate(orderly_ids, group = grouping(iso3c))
  #get the replicates for each country using the age df (smaller)
  iso3cs <- mutate(iso3cs, replicates = map(id, function(id){
    readRDS(file.path("archive", "run_simulations", id, "data", "age_baseline.Rds")) %>%
      pull(replicate) %>%
      unique()
  }))

  #generate random combinations
  if(over_time) {
    indv_country_func <- function(id, replicates, type){
      rep <- sample(replicates, 1)
      df <- readRDS(file.path("archive", "run_simulations", id, "data", "time_baseline.Rds")) %>%
        filter(replicate == rep) %>%
        filter(date < max_date) %>%
        select(date, all_of(type)) %>%
        full_join(
          readRDS(file.path("archive", "run_simulations", id, "data", "time_scenarios.Rds")) %>%
            filter(replicate == rep) %>%
            select(date, all_of(type), scenario),
          by = "date", multiple = "all"
        )
      df[[paste0(type, "_baseline")]] <- df[[paste0(type, ".x")]]
      df[[paste0(type)]] <- df[[paste0(type, ".y")]]
      df[[paste0(type, "_averted")]] <- df[[paste0(type, ".x")]] - df[[paste0(type, ".y")]]
      df[[paste0(type, ".x")]] <- NULL
      df[[paste0(type, ".y")]] <- NULL
      df
    }
    combine_func <- function(x){
      map_dfr(x$deaths_averted, function(y){y}) %>%
        group_by(scenario, date) %>%
        summarise(
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), sum),
          .groups = "drop"
        )
    }
    summarise_func <- function(g, totals){
      map_dfr(totals, function(df, g){
        df[[which(names(df) == g)]]
      }, g = g) %>%
        group_by(scenario, date) %>%
        summarise(
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~median(.x, na.rm=TRUE), .names = "{col}_med"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.025, na.rm=TRUE), .names = "{col}_025"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.25, na.rm=TRUE), .names = "{col}_25"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.75, na.rm=TRUE), .names = "{col}_75"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.975, na.rm=TRUE), .names = "{col}_975"),
          .groups = "drop"
        )
    }
  } else {
    indv_country_func <- function(id, replicates, type){
      rep <- sample(replicates, 1)
      df <- readRDS(file.path("archive", "run_simulations", id, "data", "age_scenarios.Rds")) %>%
        filter(replicate == rep) %>%
        group_by(scenario) %>%
        summarise(
          across(all_of(type), sum)
        )
      df[[paste0(type, "_baseline")]] <- readRDS(file.path("archive", "run_simulations", id, "data", "age_baseline.Rds")) %>%
        filter(replicate == rep) %>%
        pull(all_of(type)) %>%
        sum()
      df[[paste0(type, "_averted")]] <- df[[paste0(type, "_baseline")]] - df[[type]]

      df
    }
    combine_func <- function(x){
      map_dfr(x$deaths_averted, function(y){y}) %>%
        group_by(scenario) %>%
        summarise(
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), sum),
          .groups = "drop"
        )
    }
    summarise_func <- function(g, totals){
      map_dfr(totals, function(df, g){
        df[[which(names(df) == g)]]
      }, g = g) %>%
        group_by(scenario) %>%
        summarise(
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~median(.x, na.rm=TRUE), .names = "{col}_med"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.025, na.rm=TRUE), .names = "{col}_025"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.25, na.rm=TRUE), .names = "{col}_25"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.75, na.rm=TRUE), .names = "{col}_75"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.975, na.rm=TRUE), .names = "{col}_975"),
          .groups = "drop"
        )
    }
  }
  totals <- map(seq_len(replicates), function(i, iso3cs, type, indv_country_func, combine_func){
    message(i)
    df <- iso3cs %>%
      transmute(
        group = group,
        deaths_averted = future_map2(id, replicates, indv_country_func,
                                     type = type,
                                     .options = furrr_options(seed = TRUE))
      ) %>%
      split(~group, drop = TRUE) %>%
      map(
        combine_func
      )
  }, iso3cs = iso3cs, type = type, indv_country_func = indv_country_func, combine_func = combine_func)

  out <- future_map(unique(iso3cs$group), summarise_func, totals)
  if(length(out) == 1){
    out[[1]]
  } else {
    names(out) <- unique(iso3cs$group)
    out
  }
}

# deaths over time
dir.create("analysis/data")
out1 <- collate_outputs(orderly_ids, NULL, 100, "deaths", TRUE)
saveRDS(out1, "analysis/data_out/run_deaths.rds")

# deaths over time by income group
out2 <- collate_outputs(iso3cs, squire.page::get_income_group, 100, "deaths", TRUE)
saveRDS(out2, "analysis/data_out/run_deaths_wb.rds")

# deaths avertedby country for individual country plots
# TODO: Still need to run this with 100 but taking forever
iso3c_grouping <- function(x){x}
out3 <- collate_outputs(orderly_ids, iso3c_grouping, 10, "deaths", TRUE)
saveRDS(out3, "analysis/data_out/run_deaths_iso3c.rds")

# deaths averted total by scenario
out4 <- collate_outputs(orderly_ids, grouping = NULL, 100, "deaths", FALSE)
saveRDS(out4, "analysis/data_out/run_deaths_total.rds")

# deaths averted by age and iso3c for VSL
# TODO: Greg - could you adapt the combine function to generate a deaths averted by age and country
# iso3c_grouping <- function(x){x}
# out5 <- collate_outputs(orderly_ids, grouping = NULL, 100, "deaths", FALSE)
# saveRDS(out5, "analysis/data_out/run_deaths_total.rds")

# Totals by country
iso3c_grouping <- function(x){x}
out6 <- collate_outputs(orderly_ids, iso3c_grouping, 100, "deaths", FALSE)
saveRDS(out6, "analysis/data_out/run_deaths_iso3c_totals.rds")

# and save the relevant reports
pdftools::pdf_combine(
  file.path(
    "archive/run_simulations/",
    orderly_ids$id,
    "scenario_plot.pdf"
  ),
  "analysis/plots/scenario_plots_with_runs.pdf")

# ------------------------------------------------------------------------- #
# 4. Quick plots ------------------------------------------------------------
# ------------------------------------------------------------------------- #

save_figs <- function(name,
                      fig,
                      width = 6,
                      height = 6,
                      root = file.path(here::here(), "analysis/plots")) {

  dir.create(root, showWarnings = FALSE)
  fig_path <- function(name) {paste0(root, "/", name)}

  cowplot::save_plot(filename = fig_path(paste0(name,".png")),
                     plot = fig,
                     base_height = height,
                     base_width = width)

  pdf(file = fig_path(paste0(name,".pdf")), width = width, height = height)
  print(fig)
  dev.off()

}

# ------------------------------------------------------------------------- #
# 4a. Global ------------------------------------------------------------
# ------------------------------------------------------------------------- #
out1 <- readRDS("analysis/data_out/run_deaths.rds")
colors <- c("#ff5f58", "#00244f")
cepi_date <- as.Date("2020-04-20")
ymax <- max(out1$deaths_baseline_med, na.rm = TRUE)
xmax <- as.integer(as.Date("2022-01-01") - cepi_start_date) + 110

vline1 <- data.frame(x = 100,
                     xend = 100,
                     y = 0,
                     yend = ymax)

vline2 <- data.frame(x = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     xend = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     y = 0,
                     yend = ymax)


gg <- out1 %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  ggplot(aes(as.integer(date-cepi_date)+100)) +
  #geom_vline(xintercept = as.integer(as.Date("2020-12-08") - cepi_date) + 100, color = colors[1], lwd = 0.8) +
  geom_segment(data = vline1, aes(x = x, xend = xend, y = y, yend = yend), color = colors[2], lwd = 0.8) +
  geom_segment(data = vline2, aes(x = x, xend = xend, y = y, yend = yend), color = colors[1], lwd = 0.8) +
  #geom_vline(xintercept = 100, color = colors[2], lwd = 0.8) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med), fill="#fff3b5", alpha=1) +
  geom_line(lwd = 1.2, aes(y = deaths_baseline_med, linetype = "dashed"), show.legend = FALSE, color = colors[1]) +
  geom_line(lwd = 1.2, aes(y = deaths_med, linetype = "solid"), show.legend = FALSE, color = colors[2]) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed", size = 0.25)) +
  scale_x_continuous(breaks = c(100, 465), limits = c(0, xmax)) +
  scale_y_continuous(breaks = seq(0,50000,10000), limits = c(0, ymax+10000), expand = c(0, 0)) +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Global Daily COVID-19 Deaths") +
  expand_limits(x = 0, y = 0)

# now make our arrows
arrows <- data.frame(
  "label" = c("Start of \"100 Days Mission\" \nVaccination Campaign",
              "Start of Real World \nVaccination Campaign"),
  "x1" = c(100-30, 332-30),
  "x2" = c(95, 327),
  "y1" = rep(max(out1$deaths_baseline_med, na.rm = TRUE)) + 3000,
  "y2" = rep(max(out1$deaths_baseline_med, na.rm = TRUE)),
  "color" = colors
)

rwdeaths <- data.frame(
  "label" = "Real World \nCOVID-19 Deaths",
  "x1" = c(550),
  "x2" = c(500),
  "y1" = ymax - 3000,
  "y2" = ymax,
  "color" = colors[1]
)

mission_deaths <- data.frame(
  "label" = "\"100 Days Mission\" \nCOVID-19 Deaths",
  "x1" = c(260),
  "x2" = c(285),
  "y1" =4000,
  "y2" = 9000,
  "color" = colors[2]
)

saves_deaths <- data.frame(
  "label" = "Additional Lives Saved By \n\"100 Days Mission\"",
  "x1" = c(625),
  "x2" = c(575),
  "y1" =4000,
  "y2" = 9000,
  "color" = colors[2]
)

gg_global <- gg +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = rev(colors), curvature = 0.4, ncp = 10) +
  geom_curve(
    data = rwdeaths, aes(x = x1, y = y1+1500, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[1], curvature = 0.4, ncp = 10) +
  geom_curve(
    data = mission_deaths, aes(x = x1, y = y1+1500, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[2], curvature = 0.4, ncp = 10) +
  geom_curve(
    data = saves_deaths, aes(x = x1-30, y = y1+300, xend = x2-30, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = "black", curvature = -0.4, ncp = 10) +
  geom_text(data = arrows, aes(label = label, x = x1-5, y = y1+3900),color = rev(colors), hjust = "left", size = 6) +
  geom_text(data = rwdeaths, aes(label = label, x = x1+50, y = y1+1000),color = colors[1],  size = 6) +
  geom_text(data = mission_deaths, aes(label = label, x = x1-70, y = y1+1000),color = colors[2],  size = 6) +
  geom_text(data = saves_deaths, aes(label = label, x = x1+55, y = y1+1000),color = "black",  size = 6) +
  labs(title = ~ underline("World: Modelled Impact of \"100 Days Mission\" by the end of 2021                                                                                 "),
       subtitle = "") +
  theme(plot.title = element_text(color = "grey30"))

# save figs to plots directory
save_figs("deaths_global", gg_global, width = 14, height = 7, root = "analysis/plots")

# ------------------------------------------------------------------------- #
# 4b. Global by income group ------------------------------------------------------------
# ------------------------------------------------------------------------- #

out1 <- readRDS("analysis/data_out/run_deaths.rds")
out2 <- readRDS("analysis/data_out/run_deaths_wb.rds")
colors <- c("#ff5f58", "#00244f")
wbcols <- rev(viridis::viridis(4))
names(wbcols) <- c("Low-Income Countries","Low-Middle Income Countries","Upper-Middle Income Countries","High-Income Countries")

cepi_date <- as.Date("2020-04-20")
ymax <- max(out1$deaths_baseline_med, na.rm = TRUE)
xmax <- as.integer(as.Date("2022-01-01") - cepi_start_date) + 110

vline1 <- data.frame(x = 100,
                     xend = 100,
                     y = 0,
                     yend = ymax)

vline2 <- data.frame(x = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     xend = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     y = 0,
                     yend = ymax)



lic <- out1 %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  mutate(deaths_baseline_med = deaths_med + out2$LIC %>% filter(date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med),
         income = factor(names(wbcols[1]), levels = rev(names(wbcols))))
lmic <- lic %>%
  mutate(deaths_baseline_med = deaths_baseline_med + out2$LMIC %>% filter(date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med)) %>%
  mutate(income = factor(names(wbcols[2]), levels = rev(names(wbcols))))
umic <- lmic %>%
  mutate(deaths_baseline_med = deaths_baseline_med + out2$UMIC %>% filter(date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med)) %>%
  mutate(income = factor(names(wbcols[3]), levels = rev(names(wbcols))))
hic <- umic %>%
  mutate(deaths_baseline_med = deaths_baseline_med + out2$HIC %>% filter(date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med)) %>%
  mutate(income = factor(names(wbcols[4]), levels = rev(names(wbcols))))


gg <- out1 %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  ggplot(aes(as.integer(date-cepi_date)+100)) +
  geom_segment(data = vline1, aes(x = x, xend = xend, y = y, yend = yend), color = colors[2], lwd = 0.8) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med, fill = income), alpha=1, data = hic) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med, fill = income), alpha=1, data = umic) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med, fill = income), alpha=1, data = lmic) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med, fill = income), alpha=1, data = lic) +
  geom_line(aes(y=deaths_baseline_med), color=wbcols[3], alpha=1, data = umic) +
  geom_line(aes(y=deaths_baseline_med), color=wbcols[4], alpha=1, data = hic) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed", size = 0.25)) +
  scale_x_continuous(breaks = c(100, 465), limits = c(0, xmax)) +
  scale_y_continuous(breaks = seq(0,50000,10000), limits = c(0, ymax+10000), expand = c(0, 0)) +
  scale_fill_manual(values = wbcols, name = "    World Bank Income Group:") +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Global Daily COVID-19 Deaths") +
  expand_limits(x = 0, y = 0)


gg_global <- gg +
  geom_curve(
    data = arrows[1,], aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = rev(colors)[1], curvature = 0.4, ncp = 10) +
  geom_text(data = arrows[1,], aes(label = label, x = x1-5, y = y1+3900),color = rev(colors)[1], hjust = "left", size = 6) +
  labs(title = ~ underline("World: Modelled Impact of \"100 Days Mission\" by the end of 2021 by Income Group                                                       "),
       subtitle = "") +
  theme(plot.title = element_text(color = "grey30"))

gg_global <- gg_global +
  theme(legend.position = c(0.875, 0.92), legend.title.align = 1) +
  guides(fill = guide_legend(nrow=4,byrow=TRUE,label.position = "left",title.hjust = 0))

negative_excess <- data.frame(
  "label" = "Negative Deaths Prevented in \nUpper-Middle Income Countries",
  "x1" = c(575),
  "x2" = c(525),
  "y1" =44000,
  "y2" = 34000,
  "color" = colors[2]
)

gg_global2 <- gg_global +
  geom_curve(
    data = negative_excess, aes(x = x1-22, y = y1, xend = x2+80, yend = y2-3000),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.5,
    color = "black", curvature = 0.9, ncp = 10) +
  geom_text(data = negative_excess, aes(label = label, x = x1-15, y = y1+1000),color = "black", hjust = "left", size = 3.5)

save_figs("deaths_global_wb", gg_global2, width = 14, height = 7, root = "analysis/plots")

# ------------------------------------------------------------------------- #
# 4a. Global but different scenario ------------------------------------------------------------
# ------------------------------------------------------------------------- #

out1 <- readRDS("analysis/data_out/run_deaths.rds")
colors <- c("#ff5f58", "#00244f")
cepi_date <- as.Date("2020-04-20")
ymax <- max(out1$deaths_baseline_med, na.rm = TRUE)
xmax <- as.integer(as.Date("2022-01-01") - cepi_start_date) + 110

vline1 <- data.frame(x = 100,
                     xend = 100,
                     y = 0,
                     yend = ymax)

vline2 <- data.frame(x = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     xend = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     y = 0,
                     yend = ymax)


gg <- out1 %>%
  filter(scenario == 3) %>%
  filter(date < as.Date("2022-01-01")) %>%
  ggplot(aes(as.integer(date-cepi_date)+100)) +
  #geom_vline(xintercept = as.integer(as.Date("2020-12-08") - cepi_date) + 100, color = colors[1], lwd = 0.8) +
  geom_segment(data = vline1, aes(x = x, xend = xend, y = y, yend = yend), color = colors[2], lwd = 0.8) +
  geom_segment(data = vline2, aes(x = x, xend = xend, y = y, yend = yend), color = colors[1], lwd = 0.8) +
  #geom_vline(xintercept = 100, color = colors[2], lwd = 0.8) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med), fill="#fff3b5", alpha=1) +
  geom_line(lwd = 1.2, aes(y = deaths_baseline_med, linetype = "dashed"), show.legend = FALSE, color = colors[1]) +
  geom_line(lwd = 1.2, aes(y = deaths_med, linetype = "solid"), show.legend = FALSE, color = colors[2]) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed", size = 0.25)) +
  scale_x_continuous(breaks = c(100, 465), limits = c(0, xmax)) +
  scale_y_continuous(breaks = seq(0,50000,10000), limits = c(0, ymax+10000), expand = c(0, 0)) +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Global Daily COVID-19 Deaths") +
  expand_limits(x = 0, y = 0)

# now make our arrows
arrows <- data.frame(
  "label" = c("Start of \"100 Days Mission\" \nVaccination Campaign",
              "Start of Real World \nVaccination Campaign"),
  "x1" = c(100-30, 332-30),
  "x2" = c(95, 327),
  "y1" = rep(max(out1$deaths_baseline_med, na.rm = TRUE)) + 3000,
  "y2" = rep(max(out1$deaths_baseline_med, na.rm = TRUE)),
  "color" = colors
)

rwdeaths <- data.frame(
  "label" = "Real World \nCOVID-19 Deaths",
  "x1" = c(550),
  "x2" = c(500),
  "y1" = ymax - 3000,
  "y2" = ymax,
  "color" = colors[1]
)

mission_deaths <- data.frame(
  "label" = "\"100 Days Mission\" \nCOVID-19 Deaths",
  "x1" = c(260),
  "x2" = c(285),
  "y1" =4000,
  "y2" = 9000,
  "color" = colors[2]
)

saves_deaths <- data.frame(
  "label" = "Additional Lives Saved By \n\"100 Days Mission\"",
  "x1" = c(625-40),
  "x2" = c(575-40),
  "y1" =4000+9000,
  "y2" = 9000+9000,
  "color" = colors[2]
)

gg_global <- gg +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = rev(colors), curvature = 0.4, ncp = 10) +
  geom_curve(
    data = rwdeaths, aes(x = x1, y = y1+1500, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[1], curvature = 0.4, ncp = 10) +
  geom_curve(
    data = mission_deaths, aes(x = x1, y = y1+1500, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[2], curvature = 0.4, ncp = 10) +
  geom_curve(
    data = saves_deaths, aes(x = x1-30, y = y1+300, xend = x2-30, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = "black", curvature = -0.4, ncp = 10) +
  geom_text(data = arrows, aes(label = label, x = x1-5, y = y1+3900),color = rev(colors), hjust = "left", size = 6) +
  geom_text(data = rwdeaths, aes(label = label, x = x1+50, y = y1+1000),color = colors[1],  size = 6) +
  geom_text(data = mission_deaths, aes(label = label, x = x1-70, y = y1+1000),color = colors[2],  size = 6) +
  geom_text(data = saves_deaths, aes(label = label, x = x1+55, y = y1+1000),color = "black",  size = 6) +
  labs(title = ~ underline("World: Modelled Impact of \"100 Days Mission\" by the end of 2021                                                                                 "),
       subtitle = "") +
  theme(plot.title = element_text(color = "grey30"))

save_figs("deaths_global_economic", gg_global, width = 14, height = 7, root = "analysis/plots")

# ------------------------------------------------------------------------- #
# 4d. Total By Scenarios ------------------------------------------------------------
# ------------------------------------------------------------------------- #

source("src/run_simulations/funcs.R")
scenarios <- read.csv("src/run_simulations/scenarios.csv") %>%
  mutate(scenario = as.integer(rownames(.))) %>%
  describe_scenarios()

out4 <- readRDS("analysis/data_out/run_deaths_total.rds")
deaths_averted <- out4 %>%
  left_join(scenarios, by = "scenario") %>%
  mutate(Rt = factor(gsub(" ", "\n", Rt), gsub(" ", "\n", levels(Rt)))) %>%
  ggplot(aes(x = Rt, y = deaths_averted_med,
             ymin = deaths_averted_025, ymax = deaths_averted_975,
             color = Vaccine, group = interaction(Vaccine, Rt))) +
  geom_hline(yintercept = 0, color = "black") +
  geom_linerange(position = position_dodge(width = 0.5), lwd = 2, alpha = 0.3) +
  geom_linerange(aes(ymin = deaths_averted_25, ymax = deaths_averted_75),
                 position = position_dodge(width = 0.5), lwd = 2, alpha = 0.6) +
  geom_point(shape = 21, size =2, fill = "white", position = position_dodge(width = 0.5)) +
  ggpubr::theme_pubr(base_size = 14) +
  theme(panel.grid.major = element_line()) +
  scale_color_manual(name = "", values = c(pals::stepped3()[c(1,5,9,13)])) +
  labs(x = "", y = "\nCumulative Deaths Averted (median, IQR, 95% quantile)") +
  scale_y_continuous(n.breaks = 6) +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  theme(legend.text = element_text(size = 14), plot.margin = margin(0, 1, 0, 0, "cm")) +
  coord_flip() +
  scale_y_continuous(labels = scales::unit_format(unit = "Million", scale = 1e-6))

save_figs("deaths_scenario_averted", deaths_averted, width = 10, height = 7, root = "analysis/plots")

# ------------------------------------------------------------------------- #
# 4e. Example for Japan ------------------------------------------------------------
# ------------------------------------------------------------------------- #

# TODO
