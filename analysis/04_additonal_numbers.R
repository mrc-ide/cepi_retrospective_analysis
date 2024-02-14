library(tidyverse)

collate_scen_comparisons <- function(orderly_ids, grouping = NULL, replicates = 100, type = "deaths", max_date = as.Date("2022-01-01"),
                            scens_as_baseline, scens_to_compare){

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

    total_grouping <- "scenario"
    all_scens <- c(scens_as_baseline, scens_to_compare)

    indv_country_func <- function(id, replicates, type){
      rep <- sample(replicates, 1)
      df <- readRDS(file.path("archive", "run_simulations", id, "data", "age_scenarios.Rds")) %>%
        filter(replicate == rep) %>%
        filter(scenario %in% all_scens) %>%
        group_by(across(matches(total_grouping))) %>%
        summarise(
          across(all_of(type), sum), .groups = "drop"
        )

      df %>% pivot_wider(names_from = scenario, values_from = all_of(type), names_prefix = "scen") %>%
        rename_with(.fn = ~paste0(type, "_baseline"), .cols = matches(paste0("scen", scens_as_baseline)) ) %>%
        mutate(across(all_of(paste0("scen", scens_to_compare)), function(x){.[[paste0(type, "_baseline")]] - x}, .names = "{.col}_averted")) %>%
        pivot_longer(cols = matches("scen")) %>%
        mutate(scenario = substr(gsub("scen", "", name), 1, 1)) %>%
        mutate(name = replace(name, grepl("averted", name), paste0(type, "_averted"))) %>%
        mutate(name = replace(name, grepl("scen", name), paste0(type))) %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(baseline_scenario = scens_as_baseline)

    }

    combine_func <- function(x){
      map_dfr(x$deaths_averted, function(y){y}) %>%
        group_by(across(matches(total_grouping))) %>%
        summarise(
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), sum),
          .groups = "drop"
        )
    }
    summarise_func <- function(g, totals){
      map_dfr(totals, function(df, g){
        df[[which(names(df) == g)]]
      }, g = g) %>%
        group_by(across(matches(total_grouping))) %>%
        summarise(
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~median(.x, na.rm=TRUE), .names = "{col}_med"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.025, na.rm=TRUE), .names = "{col}_025"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.25, na.rm=TRUE), .names = "{col}_25"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.75, na.rm=TRUE), .names = "{col}_75"),
          across(all_of(paste0(type, c("", "_baseline", "_averted"))), ~quantile(.x, 0.975, na.rm=TRUE), .names = "{col}_975"),
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


# deaths over time globally
out_deaths_global_2_3 <- collate_scen_comparisons(orderly_ids, NULL, 100, "deaths", TRUE, scens_as_baseline = 2, scens_to_compare = 3)
out_deaths_global_11_12 <- collate_scen_comparisons(orderly_ids, NULL, 100, "deaths", TRUE, scens_as_baseline = 11, scens_to_compare = 12)
