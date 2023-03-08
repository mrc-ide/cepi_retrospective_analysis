library(orderly)
library(tidyverse)
orderly_commit(orderly_run("generate_vaccine_profiles", echo = FALSE))
#we'll do some quick runs with reduced scenarios
old_scenarios <- read_csv(file.path("src", "run_simulations", "scenarios.csv"))
filter(old_scenarios, Rt == "baseline") %>%
  write_csv(file.path("src", "run_simulations", "scenarios.csv"))
#select some countries
iso3cs <- c("AFG", "ARM", "GBR",
            "BRA"
)

library(future)
library(furrr)
future::plan(future::multicore(workers = 10))

walk(iso3cs, function(iso3c){
  params <- list(
    iso3c = iso3c,
    excess_mortality = TRUE
  )
  orderly_commit(orderly_run("run_simulations", params, echo = FALSE))
})

gather_fit_ids <- function(iso3cs){
  search_terms <- paste0('latest(parameter:iso3c == "', iso3cs, '")')
  ids <- map_chr(search_terms, ~orderly_search(.x, name = "run_simulations"))
  ids
}
collate_outputs <- function(iso3cs, grouping = NULL, replicates = 1000, type = "deaths", over_time = TRUE){
  iso3cs <- tibble(iso3c = iso3cs)
  if(is.null(grouping)){
    grouping <- function(x){"temp"}
  }
  iso3cs <- mutate(iso3cs, group = grouping(iso3c), id = gather_fit_ids(iso3c))
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
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), median, .names = "{col}_med"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.025), .names = "{col}_025"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.975), .names = "{col}_975"),
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
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), median, .names = "{col}_med"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.025), .names = "{col}_025"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.975), .names = "{col}_975"),
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

collate_outputs(iso3cs, NULL, 10, "deaths", TRUE)
collate_outputs(iso3cs, NULL, 10, "deaths", FALSE)
collate_outputs(iso3cs, squire.page::get_income_group, 10, "deaths", FALSE)
collate_outputs(iso3cs, squire.page::get_income_group, 10, "infections", FALSE)
