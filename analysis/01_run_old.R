devtools::install_github("mrc-ide/squire.page")

# ISO3CS being looked at
iso3cs <- readLines("analysis/iso3cs")

# ---------------------------------------------- #
# 1. Demo Runs ------------------------------------------------------------
# ---------------------------------------------- #

library(foreach)
library(doParallel)
library(orderly)
library(tidyverse)
library(dplyr)
library(readxl)
library(tidyr)

# set up cluster
n.cores <- 14
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()

orderly_ids <- foreach(
  i = seq_along(iso3cs),
  .combine = 'c'
) %dopar% {

  suppressWarnings(suppressMessages(
    tryCatch(
      orderly::orderly_run(
        "run_simulations", parameters = list(iso3c = iso3cs[i], excess_mortality = TRUE, force_opening = TRUE, simulate_counterfactuals = FALSE,
                                             commit = "2e5ecf5b1b15698ff4d3b1adcb07d95aa81bc4ed"),
        echo = FALSE
      ),
      error = function(e){NA}
    )
  ))

}

parallel::stopCluster(my.cluster)
committed <- lapply(orderly_ids, orderly::orderly_commit)

# ---------------------------------------------- #
# 2. Real Runs ------------------------------------------------------------
# ---------------------------------------------- #

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
        "run_simulations", parameters = list(iso3c = iso3c_to_run[i], excess_mortality = TRUE, simulate_counterfactuals = TRUE, force_opening = TRUE,
                                             commit = "2e5ecf5b1b15698ff4d3b1adcb07d95aa81bc4ed"),
        echo = FALSE
      ),
      error = function(e){NA}
    )
  ))

}

parallel::stopCluster(my.cluster)
orderly_ids$orderly_id <- finished_orderly_ids

committed <- lapply(na.omit(orderly_ids$orderly_id), orderly_commit)
saveRDS(orderly_ids, "analysis/data_out_old/orderly_ids_old.rds")

# and save the relevant reports
pdftools::pdf_combine(
  file.path(
    "archive/run_simulations/",
    orderly_ids$orderly_id,
    "scenario_plot.pdf"
  ),
  "analysis/plots/scenario_plots_with_runs_old.pdf")


# ------------------------------------------------------------------------- #
# 3. Gather health outcomes   -----------------------------------------------
# ------------------------------------------------------------------------- #

orderly_ids <- readRDS("analysis/data_out_old/orderly_ids_old.rds")
names(orderly_ids) <- c("iso3c", "id")
library(furrr)

collate_outputs <- function(orderly_ids, grouping = NULL, replicates = 100, type = "deaths", over_time = TRUE, by_age = FALSE, max_date = as.Date("2022-01-01")){

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

    total_grouping <- if(by_age) {
      c("scenario", "age_group")
    } else {
      "scenario"
    }

    indv_country_func <- function(id, replicates, type){
      rep <- sample(replicates, 1)
      df <- readRDS(file.path("archive", "run_simulations", id, "data", "age_scenarios.Rds")) %>%
        filter(replicate == rep) %>%
        group_by(across(all_of(total_grouping))) %>%
        summarise(
          across(all_of(type), sum), .groups = "drop"
        )

      if(by_age){
        baseline_df <- readRDS(file.path("archive", "run_simulations", id, "data", "age_baseline.Rds")) %>%
          filter(replicate == rep) %>%
          select(age_group, all_of(type)) %>%
          setNames(c("age_group", paste0(type, "_baseline")))
        df <- left_join(df, baseline_df, by = "age_group")
      } else {
        df[[paste0(type, "_baseline")]] <- readRDS(file.path("archive", "run_simulations", id, "data", "age_baseline.Rds")) %>%
          filter(replicate == rep) %>%
          pull(all_of(type)) %>%
          sum()
      }

      df[[paste0(type, "_averted")]] <- df[[paste0(type, "_baseline")]] - df[[type]]

      df
    }
    combine_func <- function(x){
      map_dfr(x$deaths_averted, function(y){y}) %>%
        group_by(across(all_of(total_grouping))) %>%
        summarise(
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), sum),
          .groups = "drop"
        )
    }
    summarise_func <- function(g, totals){
      map_dfr(totals, function(df, g){
        df[[which(names(df) == g)]]
      }, g = g) %>%
        group_by(across(all_of(total_grouping))) %>%
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


# get our scenario match table
source("src/run_simulations/funcs.R")
scenarios <- read.csv("src/run_simulations/scenarios.csv") %>%
  mutate(scenario = as.integer(rownames(.))) %>%
  describe_scenarios() %>%
  select(scenario, Rt, Vaccine)

dir.create("analysis/data_out_old")


# ------------------------------------------------------------------------- #
# 3a. Gather health outcomes over time   ------------------------------------
# ------------------------------------------------------------------------- #

flatten_name <- function(x, name){
  nms <- names(x)
  for(i in seq_along(x)) {
    x[[i]][[name]] <- nms[i]
  }
  do.call(rbind, x)
}

# function to also save to equivalent csv
save_outputs <- function(x, path){

  to_round <- c("deaths", "infections", "hospitalisation", "days", "year", "economic", "openness")
  x <- x %>% mutate(across(matches(to_round), round))

  if(grepl("\\.rds$", path)){
    path <- gsub("\\.rds", "", path)
  }
  saveRDS(x, paste0(path, ".rds"))
  write.csv(x, paste0(path, ".csv"), row.names = FALSE)

}

# deaths over time globally
out_deaths_global <- collate_outputs(orderly_ids, NULL, 100, "deaths", TRUE)
out_deaths_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/run_deaths_global.rds")

# deaths over time by income group
out_deaths_income <- collate_outputs(orderly_ids, squire.page::get_income_group, 100, "deaths", TRUE)
out_deaths_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/run_deaths_income.rds")

# infections over time globally
out_infections_global <- collate_outputs(orderly_ids, NULL, 100, "infections", TRUE)
out_infections_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/run_infections_global.rds")

# infections over time by income group
out_infections_income <- collate_outputs(orderly_ids, squire.page::get_income_group, 100, "infections", TRUE)
out_infections_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/run_infections_income.rds")

# hospitalisations over time globally
out_hospitalisations_global <- collate_outputs(orderly_ids, NULL, 100, "hospitalisations", TRUE)
out_hospitalisations_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/run_hospitalisations_global.rds")

# hospitalisations over time by income group
out_hospitalisations_income <- collate_outputs(orderly_ids, squire.page::get_income_group, 100, "hospitalisations", TRUE)
out_hospitalisations_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/run_hospitalisations_income.rds")

# ------------------------------------------------------------------------- #
# 3b. Gather health outcomes total   ------------------------------------
# ------------------------------------------------------------------------- #

# deaths total globally
total_deaths_global <- collate_outputs(orderly_ids, NULL, 100, "deaths", FALSE)
total_deaths_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_deaths_global.rds")

# deaths total by income group
total_deaths_income <- collate_outputs(orderly_ids, squire.page::get_income_group, 100, "deaths", FALSE)
total_deaths_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_deaths_income.rds")

# infections total globally
total_infections_global <- collate_outputs(orderly_ids, NULL, 100, "infections", FALSE)
total_infections_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_infections_global.rds")

# infections total by income group
total_infections_income <- collate_outputs(orderly_ids, squire.page::get_income_group, 100, "infections", FALSE)
total_infections_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_infections_income.rds")

# hospitalisations total time globally
total_hospitalisations_global <- collate_outputs(orderly_ids, NULL, 100, "hospitalisations", FALSE)
total_hospitalisations_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_hospitalisations_global.rds")

# hospitalisations total time by income group
total_hospitalisations_income <- collate_outputs(orderly_ids, squire.page::get_income_group, 100, "hospitalisations", FALSE)
total_hospitalisations_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_hospitalisations_income.rds")

# ------------------------------------------------------------------------- #
# 4a. Gather life year economic outputs --------------------------------------
# ------------------------------------------------------------------------- #

life_years_saved <- function(orderly_ids, grouping = NULL, replicates = 10, max_date = as.Date("2022-01-01"), vsly){

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

  type <- "deaths"

  indv_country_func <- function(id, replicates, type){
    rep <- sample(replicates, 1)
    df <- readRDS(file.path("archive", "run_simulations", id, "data", "age_scenarios.Rds")) %>%
      filter(replicate == rep) %>%
      group_by(across(all_of(c("scenario", "age_group")))) %>%
      summarise(
        across(all_of(type), sum), .groups = "drop"
      )

    baseline_df <- readRDS(file.path("archive", "run_simulations", id, "data", "age_baseline.Rds")) %>%
      filter(replicate == rep) %>%
      select(age_group, all_of(type)) %>%
      setNames(c("age_group", paste0(type, "_baseline")))
    df <- left_join(df, baseline_df, by = "age_group")


    df[[paste0(type, "_averted")]] <- df[[paste0(type, "_baseline")]] - df[[type]]

    # add the iso3c in
    or <- readRDS(file.path("archive", "run_simulations", id, "orderly_run.rds"))
    df$iso3c <- or$meta$parameters$iso3c

    # merge the vsly in
    df <- df %>% left_join(
      vsly %>%
        select(iso3c, age_group, lg, lghat, gni, vsl, vsly, vsly_ud) %>%
        mutate(age_group = as.integer(age_group)) %>%
        mutate(productive = 1) %>%
        mutate(productive = replace(productive, age_group %in% c(1:3, 14:17), 0)),
      by = c("age_group", "iso3c")
    )

    # calculate our outputs
    df %>%
      mutate(life_years_saved = lghat*deaths_averted) %>%
      mutate(life_years_saved_undiscounted = lg*deaths_averted) %>%
      mutate(productive_life_years_saved = lghat*deaths_averted*productive) %>%
      mutate(productive_life_years_saved_undiscounted = lg*deaths_averted*productive) %>%
      mutate(economic_productive_loss = productive_life_years_saved * gni) %>%
      mutate(economic_productive_loss_undiscounted = productive_life_years_saved_undiscounted * gni) %>%
      mutate(economic_lives_saved = deaths_averted * vsl) %>%
      mutate(economic_life_years_saved = life_years_saved * vsly) %>%
      mutate(economic_life_years_saved_undiscounted = life_years_saved_undiscounted * vsly_ud) %>%
      select(scenario, life_years_saved:economic_life_years_saved_undiscounted) %>%
      group_by(scenario) %>%
      summarise(across(life_years_saved:economic_life_years_saved_undiscounted, sum))

  }
  combine_func <- function(x){
    map_dfr(x$deaths_averted, function(y){y}) %>%
      group_by(across(all_of("scenario"))) %>%
      summarise(
        across(-any_of("scenario"), sum),
        .groups = "drop"
      )
  }
  summarise_func <- function(g, totals){
    vars <- all_of(names(totals[[1]][[1]])[-1])
    map_dfr(totals, function(df, g){
      df[[which(names(df) == g)]]
    }, g = g) %>%
      group_by(across(all_of("scenario"))) %>%
      summarise(
        across(all_of(vars), ~median(.x, na.rm=TRUE), .names = "{col}_med"),
        across(all_of(vars), ~quantile(.x, 0.025, na.rm=TRUE), .names = "{col}_025"),
        across(all_of(vars), ~quantile(.x, 0.25, na.rm=TRUE), .names = "{col}_25"),
        across(all_of(vars), ~quantile(.x, 0.75, na.rm=TRUE), .names = "{col}_75"),
        across(all_of(vars), ~quantile(.x, 0.975, na.rm=TRUE), .names = "{col}_975"),
        .groups = "drop"
      )
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

vsly <- readRDS("analysis/data_raw/vsly.rds")

# life years globally
ly_out_global <- life_years_saved(orderly_ids, NULL, 100, vsly = vsly)
ly_out_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_lifeyears_global.rds")

# life years by income
ly_out_income <- life_years_saved(orderly_ids, squire.page::get_income_group, 100, vsly = vsly)
ly_out_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_lifeyears_income.rds")

# ------------------------------------------------------------------------- #
# 4b. Gather Hospitalisation costs --------------------------------------
# ------------------------------------------------------------------------- #

hosp_costs_saved <- function(orderly_ids, grouping = NULL, replicates = 10, max_date = as.Date("2022-01-01"), choice){

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

  type <- "hospitalisations"

  indv_country_func <- function(id, replicates, type){

    rep <- sample(replicates, 1)
    df <- readRDS(file.path("archive", "run_simulations", id, "data", "age_scenarios.Rds")) %>%
      filter(replicate == rep) %>%
      group_by(across(all_of(c("scenario")))) %>%
      summarise(
        across(all_of(type), sum), .groups = "drop"
      )

    df[[paste0(type, "_baseline")]] <- readRDS(file.path("archive", "run_simulations", id, "data", "age_baseline.Rds")) %>%
      filter(replicate == rep) %>%
      select(all_of(type)) %>%
      pull(all_of(type)) %>%
      sum()


    df[[paste0(type, "_averted")]] <- df[[paste0(type, "_baseline")]] - df[[type]]

    # add the iso3c in
    or <- readRDS(file.path("archive", "run_simulations", id, "orderly_run.rds"))
    df$iso3c <- or$meta$parameters$iso3c

    # merge the choice in
    df <- df %>% left_join(choice, by = "iso3c")

    # calculate our outputs
    df %>%
      mutate(hospitalisation_costs_averted = hospitalisations_averted*choice*7.5) %>%
      select(scenario, hospitalisation_costs_averted)

  }
  combine_func <- function(x){
    map_dfr(x$deaths_averted, function(y){y}) %>%
      group_by(across(all_of("scenario"))) %>%
      summarise(
        across(-any_of("scenario"), sum),
        .groups = "drop"
      )
  }
  summarise_func <- function(g, totals){
    vars <- all_of(names(totals[[1]][[1]])[-1])
    map_dfr(totals, function(df, g){
      df[[which(names(df) == g)]]
    }, g = g) %>%
      group_by(across(all_of("scenario"))) %>%
      summarise(
        across(all_of(vars), ~median(.x, na.rm=TRUE), .names = "{col}_med"),
        across(all_of(vars), ~quantile(.x, 0.025, na.rm=TRUE), .names = "{col}_025"),
        across(all_of(vars), ~quantile(.x, 0.25, na.rm=TRUE), .names = "{col}_25"),
        across(all_of(vars), ~quantile(.x, 0.75, na.rm=TRUE), .names = "{col}_75"),
        across(all_of(vars), ~quantile(.x, 0.975, na.rm=TRUE), .names = "{col}_975"),
        .groups = "drop"
      )
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

choice <- readRDS("analysis/data_raw/who_choice.rds")

# hospitalisation costs globally
hosp_costs_global <- hosp_costs_saved(orderly_ids, NULL, 100, choice = choice)
hosp_costs_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_hospcosts_global.rds")

# hospitalisation costs by income
hosp_costs_income <- hosp_costs_saved(orderly_ids, squire.page::get_income_group, 100, choice = choice)
hosp_costs_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_hospcosts_income.rds")


# ------------------------------------------------------------------------- #
# 5a. Gather hospital capacities   -----------------------------------
# ------------------------------------------------------------------------- #

hospital_capacities <- function(orderly_ids, grouping = NULL, replicates = 10, max_date = as.Date("2022-01-01")){

  if(is.null(grouping)){
    grouping <- function(x){"temp"}
  }

  iso3cs <- mutate(orderly_ids, group = grouping(iso3c))
  #get the replicates for each country using the age df (smaller)
  iso3cs <- mutate(iso3cs, replicates = map(id, function(id){
    readRDS(file.path("archive", "run_simulations", id, "data", "capacity_baseline.Rds")) %>%
      pull(replicate) %>%
      unique()
  }))

  type <- "days"

  indv_country_func <- function(id, replicates, type){
    rep <- sample(replicates, 1)
    df <- readRDS(file.path("archive", "run_simulations", id, "data", "capacity_scenarios.Rds")) %>%
      filter(replicate == rep) %>%
      group_by(across(all_of(c("scenario")))) %>%
      summarise(
        across(days, sum), .groups = "drop"
      )

    baseline_df <- readRDS(file.path("archive", "run_simulations", id, "data", "capacity_baseline.Rds")) %>%
      filter(replicate == rep) %>%
      select(days) %>%
      setNames(c(paste0(type, "_baseline")))
    df[[paste0(type, "_baseline")]] <- baseline_df[[paste0(type, "_baseline")]]
    df[[paste0(type, "_extra")]] <- df[[type]] - df[[paste0(type, "_baseline")]]

    df
  }

  combine_func <- function(x){
    map_dfr(x$deaths_averted, function(y){y}) %>%
      group_by(across(all_of("scenario"))) %>%
      summarise(
        across(-any_of("scenario"), sum),
        .groups = "drop"
      )
  }

  summarise_func <- function(g, totals){
    vars <- all_of(names(totals[[1]][[1]])[-1])
    map_dfr(totals, function(df, g){
      df[[which(names(df) == g)]]
    }, g = g) %>%
      group_by(across(all_of("scenario"))) %>%
      summarise(
        across(all_of(vars), ~median(.x, na.rm=TRUE), .names = "{col}_med"),
        across(all_of(vars), ~quantile(.x, 0.025, na.rm=TRUE), .names = "{col}_025"),
        across(all_of(vars), ~quantile(.x, 0.25, na.rm=TRUE), .names = "{col}_25"),
        across(all_of(vars), ~quantile(.x, 0.75, na.rm=TRUE), .names = "{col}_75"),
        across(all_of(vars), ~quantile(.x, 0.975, na.rm=TRUE), .names = "{col}_975"),
        .groups = "drop"
      )
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

# days past hospital capacity globally
capacity_out_global <- hospital_capacities(orderly_ids, NULL, 100)
capacity_out_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_breachedhospcapcity_global.rds")

# days past hospital capacity by income
capacity_out_income <- hospital_capacities(orderly_ids, squire.page::get_income_group, 100)
capacity_out_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_breachedhospcapcity_income.rds")

# ------------------------------------------------------------------------- #
# 5b. Gather gains in openness   -----------------------------------
# ------------------------------------------------------------------------- #

npi_gains <- function(orderly_ids, grouping = NULL, replicates = 10, max_date = as.Date("2022-01-01")){

  if(is.null(grouping)){
    grouping <- function(x){"temp"}
  }

  iso3cs <- mutate(orderly_ids, group = grouping(iso3c))
  #get the replicates for each country using the age df (smaller)
  iso3cs <- mutate(iso3cs, replicates = map(id, function(id){
    readRDS(file.path("archive", "run_simulations", id, "gain_in_openness.Rds"))[[1]] %>%
      pull(rep) %>%
      unique()
  }))

  type <- "gain_in_openness"

  indv_country_func <- function(id, replicates, type){
    rep <- sample(replicates, 1)
    df <- readRDS(file.path("archive", "run_simulations", id, "gain_in_openness.Rds")) %>%
      setNames(seq_along(.)) %>%
      flatten_name("scenario") %>%
      mutate(scenario = as.integer(scenario)) %>%
      rename(replicate = rep) %>%
      filter(replicate == rep) %>%
      group_by(across(all_of(c("scenario")))) %>%
      summarise(
        across(all_of(type), sum), .groups = "drop"
      )

    df
  }

  combine_func <- function(x){
    map_dfr(x$deaths_averted, function(y){y}) %>%
      group_by(across(all_of("scenario"))) %>%
      summarise(
        across(-any_of("scenario"), sum),
        .groups = "drop"
      )
  }

  summarise_func <- function(g, totals){
    vars <- all_of(names(totals[[1]][[1]])[-1])
    map_dfr(totals, function(df, g){
      df[[which(names(df) == g)]]
    }, g = g) %>%
      group_by(across(all_of("scenario"))) %>%
      summarise(
        across(all_of(vars), ~median(.x, na.rm=TRUE), .names = "{col}_med"),
        across(all_of(vars), ~quantile(.x, 0.025, na.rm=TRUE), .names = "{col}_025"),
        across(all_of(vars), ~quantile(.x, 0.25, na.rm=TRUE), .names = "{col}_25"),
        across(all_of(vars), ~quantile(.x, 0.75, na.rm=TRUE), .names = "{col}_75"),
        across(all_of(vars), ~quantile(.x, 0.975, na.rm=TRUE), .names = "{col}_975"),
        .groups = "drop"
      )
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

# days fewer NPIs globally
npi_gains_global <- npi_gains(orderly_ids, NULL, 100)
npi_gains_global %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_npigains_global.rds")

# days fewer NPIs by income
npi_gains_income <- npi_gains(orderly_ids, squire.page::get_income_group, 100)
npi_gains_income %>% flatten_name("income") %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_npigains_income.rds")

# ------------------------------------------------------------------------- #
# 5c. Gather school openings   -----------------------------------
# ------------------------------------------------------------------------- #

open_dates <- map(orderly_ids$orderly_id, function(x) {
  readRDS(file.path("archive", "run_simulations", x, "open_dates.Rds"))
}) %>% setNames(orderly_ids$iso3c) %>% flatten_name("iso3c")
save_outputs(open_dates, "analysis/data_out_old/open_dates.rds")

# now calculate closures
source("analysis/extra_functions.R")
closure_weeks <- closure_weeks(open_dates)

closure_weeks <- closure_weeks %>%
  rename(extra_full_school_weeks = extra_full) %>%
  rename(extra_partial_school_weeks = extra_partial)

vars <- c("extra_full_school_weeks", "extra_partial_school_weeks")
closure_weeks %>%
  group_by(scenario) %>%
  summarise(
    across(all_of(vars), ~sum(.x, na.rm=TRUE), .names = "{col}_total"),
    across(all_of(vars), ~median(.x, na.rm=TRUE), .names = "{col}_med"),
    across(all_of(vars), ~quantile(.x, 0.025, na.rm=TRUE), .names = "{col}_025"),
    across(all_of(vars), ~quantile(.x, 0.25, na.rm=TRUE), .names = "{col}_25"),
    across(all_of(vars), ~quantile(.x, 0.75, na.rm=TRUE), .names = "{col}_75"),
    across(all_of(vars), ~quantile(.x, 0.975, na.rm=TRUE), .names = "{col}_975")
  ) %>% left_join(scenarios) %>% relocate(scenario, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_schoolweeks_global.rds")

closure_weeks %>% mutate(income = squire.page::get_income_group(iso3c)) %>%
  group_by(scenario, income) %>%
  summarise(
    across(all_of(vars), ~sum(.x, na.rm=TRUE), .names = "{col}_total"),
    across(all_of(vars), ~median(.x, na.rm=TRUE), .names = "{col}_med"),
    across(all_of(vars), ~quantile(.x, 0.025, na.rm=TRUE), .names = "{col}_025"),
    across(all_of(vars), ~quantile(.x, 0.25, na.rm=TRUE), .names = "{col}_25"),
    across(all_of(vars), ~quantile(.x, 0.75, na.rm=TRUE), .names = "{col}_75"),
    across(all_of(vars), ~quantile(.x, 0.975, na.rm=TRUE), .names = "{col}_975")
  ) %>% left_join(scenarios) %>% relocate(scenario, income, Rt, Vaccine) %>%
  save_outputs("analysis/data_out_old/total_schoolweeks_income.rds")

