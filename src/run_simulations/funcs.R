# gathers fit from github
grab_fit <- function(iso3c, excess_mortality, booster = FALSE){

  if (excess_mortality) {
    path <- paste0("https://github.com/mrc-ide/covid-vaccine-impact-orderly/raw/main/data/excess_mortality/model_fits/", iso3c, ".Rds")
  } else {
    path <- paste0("https://github.com/mrc-ide/covid-vaccine-impact-orderly/raw/main/data/reported_deaths/model_fits/", iso3c, ".Rds")
  }
  if (booster) {
    if (excess_mortality) {
      path <- paste0("https://github.com/mrc-ide/nimue_global_fits/raw/main/excess_mortality/", iso3c, ".Rds")
    } else {
      path <- paste0("https://github.com/mrc-ide/nimue_global_fits/raw/main/reported_deaths/", iso3c, ".Rds")
    }
  }

  download.file(path, "temp.Rds", mode = "wb")
  fit <- readRDS("temp.Rds")
  unlink("temp.Rds")
  fit
}

# utility for converting scenario codes to a description
describe_scenarios <- function(scenarios){
  mutate(scenarios,
         Rt = case_when(
           Rt == "baseline" ~ "Public Health Optimum",
           Rt == "target" ~ "Target based",
           Rt == "economic" ~ "Economic based"
         ),
         Vaccine = case_when(
           Vaccine == "early" ~ "Science",
           Vaccine == "manufacturing" ~ "Science + Manufacturing",
           Vaccine == "equity" ~ "Science + Infrastructure/Equity"
         ),
         Variant = case_when(
           Variant == "baseline" ~ "As it occurred",
           Variant == "reduced" ~ "Reduced"
         )
  )
}

#convert scenario codes and fit object into a list of simulations ready to run
implement_scenarios <- function(fit, scenarios, iso3c){
  pmap(scenarios, function(Rt, Vaccine, Variant, fit){
    #order matters since Rt can depend on vaccine coverage
    fit <- implement_vaccine(fit, Vaccine, iso3c)
    fit <- implement_rt(fit, Rt)
    fit <- implement_variant(fit, Variant)
    fit
  }, fit)
}

# implements rt trends for fits
implement_rt <- function(fit, Rt){
  if (Rt == "baseline") {
    fit
  } else if (Rt == "target") {
    fit
  } else if (Rt == "economic") {
    fit
  }
}

calculate_true_uptake <- function(doses, eligible_pop, t_extension){
  if(sum(doses) > eligible_pop){
    i_exceeds_eligible <- min(which(cumsum(doses) >= eligible_pop)) - 1
    actual_doses <- doses
    actual_doses[(i_exceeds_eligible + 1):(t_extension + 2)] <- 0
    actual_doses[i_exceeds_eligible] <- eligible_pop - sum(doses[1:i_exceeds_eligible])
    actual_doses
  } else {
    doses
  }

}

generate_vaccination_curve <- function(coverage, dose_ratio, t_coverage, t_staging, t_extension, max_vaccinatable_pop, dur_V, date_start) {
  #calculate first doses to meet coverage
  daily_doses <- coverage/(t_coverage - t_staging + sum(seq(t_staging))/t_staging)
  #setup first doses
  first_doses <- rep(0, t_extension + 2)
  first_doses[1:(t_staging + 1)] <- seq(0, t_staging) * daily_doses / t_staging
  first_doses[(t_staging + 1):(t_extension + 2)] <- daily_doses

  #how many does will actually be taken up
  actual_doses <- calculate_true_uptake(first_doses, max_vaccinatable_pop, t_extension)
  #could account for delay here

  #adjust for waning
  waning <- lag(actual_doses, dur_V, default = 0) #could make this align with exponential dist
  waned <- sum(waning[1:(t_coverage + 1)])

  #derive dose ratio, assuming second doses occur at the same rate
  #second_dose_delay <- t_coverage - t_staging + (sum(seq(t_staging))/t_staging) - dose_ratio * coverage / daily_doses
  second_dose_delay <- round(t_coverage - t_staging + (sum(seq(t_staging))/t_staging) - ((dose_ratio * (coverage - waned) + waned) / daily_doses))

  second_doses <- lag(first_doses, second_dose_delay, default = 0)
  #how many does will actually be taken up
  actual_doses_2 <- calculate_true_uptake(second_doses, max_vaccinatable_pop, t_extension)

  dose_ratio <- (cumsum(actual_doses_2) - cumsum(waning))/(cumsum(actual_doses) - cumsum(waning))

  tt <- seq(0, t_extension) + date_start

  list(
    tt = tt,
    max_vaccine = first_doses,
    dose_ratio = dose_ratio[-1]
  )
}


# Function to convert doses for Manufacture Scenario
max_grow <- function(x, roll = 7, tot_mult = 1) {

  # final dose
  fin <- tail(x, 1)

  # what is the total coverage
  tot <- sum(x) * tot_mult

  # use a weekly mean to decide on daily maximums
  x2 <- zoo::rollmean(x, roll, na.pad = TRUE)
  x2[is.na(x2)] <- 0

  # convert roll out to never decrease below current max daily dose
  for(i in seq_len(length(x2)-1)) {
    if(x2[i] > x2[i+1]) {
      x2[i+1] <- x2[i]
    }
  }

  # Create new doses over time to  stop at max coverage
  x3 <- cumsum(x2)
  end <- which(x3 > tot)[1]
  x2[end:length(x2)] <- 0
  x2[end] <- tot - sum(x2)

  # if it didn't finish on the last day
  # then allocate at the continued last rate to be fair against comparisons to early
  if(end < length(x2)) {
    x2[(end+1):length(x2)] <- fin
  }

  return(x2)

}

# Function to convert doses for equity/systems scenario
fast_grow <- function(x, roll = 7, tot_mult = 2, speed = 2, eoy_x) {

  # final dose
  fin <- tail(x, 1)

  # what is the total coverage
  tot <- sum(x) * tot_mult

  # use a weekly mean to decide on daily maximums
  x2 <- zoo::rollmean(x, roll, na.pad = TRUE)
  x2[is.na(x2)] <- 0

  # Increase roll out speed
  x2 <- x2 * speed

  # Now check that it reaches desired total coverage by end of year (eoy_x)
  x3 <- cumsum(x2)
  end <- which(x3[seq_len(eoy_x)] > tot)[1]

  # if nothing then we didn't hit desired coverage with the increase in speed
  # so scale it so it hits it the day before
  if(is.na(end)){
    x2 <- x2 * (tot/x3[eoy_x-1])
    x3 <- cumsum(x2)
    end <- which(x3[seq_len(eoy_x+1)] > tot)[1]
  }

  # and correct to stop increasing coverage there
  x2[end:length(x2)] <- 0
  x2[end] <- tot - sum(x2)

  # if it didn't finish on the last day
  # then allocate at the continued last rate to be fair against comparisons to early
  if(end < length(x2)) {
    x2[(end+1):length(x2)] <- fin
  }

  return(x2)

}

# Update vaccinations in a country model fit with new values
update_fit_vaccinations <- function(fit, new_values) {
  UseMethod("update_fit_vaccinations")
}

# Update vaccinations in a country model fit with new values for nimue model
update_fit_vaccinations.vacc_durR_nimue_simulation <- function(fit, new_values){
  fit$interventions$max_vaccine <-
    fit$pmcmc_results$inputs$interventions$max_vaccine <-
    new_values$max_vaccine
  fit$interventions$dose_ratio <-
    fit$pmcmc_results$inputs$interventions$dose_ratio <-
    new_values$dose_ratio
  fit$interventions$date_vaccine_change <-
    fit$pmcmc_results$inputs$interventions$date_vaccine_change  <-
    new_values$tt
  fit$interventions$date_vaccine_efficacy <-
    fit$pmcmc_results$inputs$interventions$date_vaccine_efficacy <-
    new_values$tt
  fit
}

# Update vaccinations in a country model fit with new values for booster model
update_fit_vaccinations.rt_optimised <- function(fit, new_values){
  fit$parameters$primary_doses <- new_values$primary_doses
  fit$parameters$booster_doses <- new_values$booster_doses
  fit$parameters$tt_primary_doses <- new_values$tt
  fit$parameters$tt_booster_doses <- new_values$tt
  fit
}


# Implement a vaccine strategy for a country fit
implement_vaccine <- function(fit, Vaccine, iso3c) {
  UseMethod("implement_vaccine")
}

# Implement a vaccine strategy for a country fit created using booster model
implement_vaccine.rt_optimised <- function(fit, Vaccine, iso3c){

  cepi_start_date <- as.Date("2020-04-20")
  end_of_cepi_year_one <- as.Date("2021-04-20")
  real_start_date <- as.Date("2020-12-08")

  #get vaccine details
  baseline_date <- as.Date("2021-12-08")
  baseline_coverage <- sum(fit$parameters$primary_doses)
  baseline_date_start <- fit$inputs$start_date[1] + fit$parameters$tt_primary_doses[2]

  # difference in start of vaccination
  difference <- as.numeric(real_start_date - cepi_start_date)
  if (Vaccine %in% c("manufacturing", "both")) {
    difference <- as.numeric(baseline_date_start - cepi_start_date)
  }

  # Extend our dose series as used in all scenarios:

  # Extend our doses
  primary <- c(
    fit$parameters$primary_doses,
    rep(mean(tail(fit$parameters$primary_doses,30)), difference)
  )

  # Extend our booster
  booster <- c(
    fit$parameters$booster_doses,
    rep(mean(tail(fit$parameters$booster_doses, 30)), difference)
  )


  # All scenarios coded up EXCEPT Vaccine efficacy adjustments
  if (Vaccine == "early") {

    # bring vaccination earlier
    start_vacc <- fit$parameters$tt_booster_doses[2] - difference
    end_vacc <- tail(fit$parameters$tt_booster_doses, 1)
    tt_doses <- c(0, seq(start_vacc, end_vacc, 1))

    new_values <- list(
      primary_doses = primary,
      booster_doses = booster,
      tt = tt_doses
    )
    return(update_fit_vaccinations(fit, new_values))

  } else if (Vaccine == "equity") {

    # forty percent of vaccinatable population
    forty_vaccinatable_pop <- (sum(squire::get_population(fit$parameters$country)$n[-(1:3)])*0.4)

    # how much increase is this for coverage
    increased_cov <- forty_vaccinatable_pop / sum(fit$parameters$primary_doses)

    # by what date/tt is this to be achieved by
    new_vacc_dates <- c(0, fit$parameters$tt_primary_doses[-1] - difference) + fit$inputs$start_date
    eoy <- which(new_vacc_dates == end_of_cepi_year_one)

    # adjust our primary vaccines
    primary <- fast_grow(primary, tot_mult = increased_cov, eoy_x = eoy, speed = 2)

    # adjust boosters as well?
    booster <- fast_grow(booster, tot_mult = increased_cov, eoy_x = eoy, speed = 2)

    # bring vaccination earlier
    start_vacc <- fit$parameters$tt_booster_doses[2] - difference
    end_vacc <- tail(fit$parameters$tt_booster_doses, 1)
    tt_doses <- c(0, seq(start_vacc, end_vacc, 1))

    new_values <- list(
      primary_doses = primary,
      booster_doses = booster,
      tt = tt_doses
    )
    return(update_fit_vaccinations(fit, new_values))

  } else if (Vaccine == "manufacturing") {

    #update to AZ or mRNA efficacy
    mrna <- readRDS("dominant_vaccines.Rds") %>%
      mutate(
        Moderna = if_else(is.na(Moderna), 0L, Moderna),
        `Pfizer.BioNTech` = if_else(is.na(`Pfizer.BioNTech`), 0L, `Pfizer.BioNTech`)
      ) %>%
      transmute(
        iso3 = countrycode::countrycode(country, "country.name", "iso3c"),
        mrna = if_else(
          dominant %in% c("Pfizer/BioNTech", "Moderna") |
            Moderna == 1 | `Pfizer.BioNTech` == 1,
          TRUE,
          FALSE
        )
      ) %>%
      filter(iso3 == iso3c) %>%
      pull(mrna)
    if (mrna) {
      ve <- list(
        ve_i_low = 0.63,
        ve_i_high = 0.86,
        ve_d_low = 0.83,
        ve_d_high = 0.95,
        ve_i_low_d = 0.36,
        ve_i_high_d = 0.88,
        ve_d_low_d = 0.83,
        ve_d_high_d = 0.93
      )
    } else {
      ve <- list(
        ve_i_low = 0.64,
        ve_i_high = 0.77,
        ve_d_low = 0.79,
        ve_d_high = 0.94,
        ve_i_low_d = 0.3,
        ve_i_high_d = 0.67,
        ve_d_low_d = 0.71,
        ve_d_high_d = 0.92
      )
    }
    ve <- map(ve, ~c(.x, 0.8 * .x, min(1, 1.1 * .x)))
    fit$interventions$vaccine_efficacies <- ve



    # adjust our primary vaccines
    primary <- max_grow(primary)

    # adjust boosters as well?
    booster <- max_grow(booster)

    # bring vaccination earlier
    start_vacc <- fit$parameters$tt_booster_doses[2] - difference
    end_vacc <- tail(fit$parameters$tt_booster_doses, 1)
    tt_doses <- c(0, seq(start_vacc, end_vacc, 1))

    new_values <- list(
      primary_doses = primary,
      booster_doses = booster,
      tt = tt_doses
    )
    return(update_fit_vaccinations(fit, new_values))

  } else if (Vaccine == "both") {


    # adjust our primary vaccines
    primary <- max_grow(primary)

    # adjust boosters as well?
    booster <- max_grow(booster)

    # bring vaccination earlier
    start_vacc <- fit$parameters$tt_booster_doses[2] - difference
    end_vacc <- tail(fit$parameters$tt_booster_doses, 1)
    tt_doses <- c(0, seq(start_vacc, end_vacc, 1))

    # forty percent of vaccinatable population
    forty_vaccinatable_pop <- (sum(squire::get_population(fit$parameters$country)$n[-(1:3)])*0.4)

    # how much increase is this for coverage
    increased_cov <- forty_vaccinatable_pop / sum(fit$parameters$primary_doses)

    # by what date/tt is this to be achieved by
    new_vacc_dates <- c(0, fit$parameters$tt_primary_doses[-1] - difference) + fit$inputs$start_date
    eoy <- which(new_vacc_dates == end_of_cepi_year_one)

    # adjust our primary vaccines
    primary <- fast_grow(primary, tot_mult = increased_cov, eoy_x = eoy, speed = 2)

    # adjust boosters as well?
    booster <- fast_grow(booster, tot_mult = increased_cov, eoy_x = eoy, speed = 2)


    new_values <- list(
      primary_doses = primary,
      booster_doses = booster,
      tt = tt_doses
    )
    return(update_fit_vaccinations(fit, new_values))

  }

  #generate a plot to show the differences between these
  #also need to make adjustments where doses will occur before the model begins (need to recheck how the initial states work)
}

# Implement a vaccine strategy for a country fit created using nimue
implement_vaccine.vacc_durR_nimue_simulation <- function(fit, Vaccine, iso3c){
  cepi_start_date <- as.Date("2020-04-20")
  real_start_date <- as.Date("2020-12-08")
  difference <- as.numeric(real_start_date - cepi_start_date)

  #get vaccine details
  baseline_date <- as.Date("2021-12-08")
  baseline_coverage <- sum(fit$interventions$max_vaccine)
  baseline_dose_ratio <- tail(fit$interventions$dose_ratio, 1)
  max_vaccinatable_pop <- sum(fit$parameters$population * fit$odin_parameters$vaccine_coverage_mat[fit$odin_parameters$N_prioritisation_steps, ])
  dur_V <- fit$parameters$dur_V
  baseline_date_start <- head(fit$interventions$date_vaccine_change, 1)
  t_staging <- 60

  if (Vaccine == "early_v1") {
    #staggered (but earlier) rollout
    vaccination_start <- baseline_date_start - difference
    t_coverage <- as.numeric(baseline_date - baseline_date_start)
    t_extension <- as.numeric(baseline_date - vaccination_start)
    return(update_fit_vaccinations(fit, generate_vaccination_curve(
      baseline_coverage, baseline_dose_ratio, t_coverage, t_staging, t_extension, max_vaccinatable_pop, dur_V, vaccination_start
    )))
  } else if (Vaccine == "early") {

    vaccination_start <- baseline_date_start - difference
    t_extension <- as.numeric(baseline_date - vaccination_start)

    max_vaccine <- c(
      fit$interventions$max_vaccine,
      rep(mean(tail(fit$interventions$max_vaccine,30)),difference)
    )

    # bring vaccination earlier
    new_values <- list(
      max_vaccine = max_vaccine,
      dose_ratio = fit$interventions$dose_ratio,
      tt = seq(0, t_extension) + vaccination_start
    )

    return(update_fit_vaccinations(fit, new_values))

  } else if (Vaccine == "equity") {
    #staggered (but earlier) rollout
    vaccination_start <- baseline_date_start - difference
    t_extension <- as.numeric(baseline_date - vaccination_start)
    #update final coverage to 40% by the end of the first year of vaccinations
    t_coverage <- 365
    pop <- sum(fit$parameters$population)
    if (baseline_dose_ratio * baseline_coverage / pop < 0.4) {
      dose_ratio <- pmin(1, 0.4 * pop / baseline_coverage)
      if (baseline_dose_ratio * baseline_coverage / pop < 0.4) {
        coverage <- pop * 0.4 / dose_ratio
      } else {
        coverage <- baseline_coverage
      }
    } else {
      coverage <- baseline_coverage
      dose_ratio <- baseline_dose_ratio
    }
    #update rollout speed to be much quicker
    t_staging_q <- t_staging/2
    return(update_fit_vaccinations(fit, generate_vaccination_curve(
      coverage, dose_ratio, t_coverage, t_staging_q, t_extension, max_vaccinatable_pop, dur_V, vaccination_start
    )))
  } else if (Vaccine == "manufacturing") {
    #update to AZ or mRNA efficacy
    mrna <- readRDS("dominant_vaccines.Rds") %>%
      mutate(
        Moderna = if_else(is.na(Moderna), 0L, Moderna),
        `Pfizer.BioNTech` = if_else(is.na(`Pfizer.BioNTech`), 0L, `Pfizer.BioNTech`)
      ) %>%
      transmute(
        iso3 = countrycode::countrycode(country, "country.name", "iso3c"),
        mrna = if_else(
          dominant %in% c("Pfizer/BioNTech", "Moderna") |
            Moderna == 1 | `Pfizer.BioNTech` == 1,
          TRUE,
          FALSE
        )
      ) %>%
      filter(iso3 == iso3c) %>%
      pull(mrna)
    if (mrna) {
      ve <- list(
        ve_i_low = 0.63,
        ve_i_high = 0.86,
        ve_d_low = 0.83,
        ve_d_high = 0.95,
        ve_i_low_d = 0.36,
        ve_i_high_d = 0.88,
        ve_d_low_d = 0.83,
        ve_d_high_d = 0.93
      )
    } else {
      ve <- list(
        ve_i_low = 0.64,
        ve_i_high = 0.77,
        ve_d_low = 0.79,
        ve_d_high = 0.94,
        ve_i_low_d = 0.3,
        ve_i_high_d = 0.67,
        ve_d_low_d = 0.71,
        ve_d_high_d = 0.92
      )
    }
    ve <- map(ve, ~c(.x, 0.8 * .x, min(1, 1.1 * .x)))
    fit$interventions$vaccine_efficacies <- ve
    #vaccination campaigns begin at the same time
    vaccination_start <- cepi_start_date
    t_coverage <- as.numeric(baseline_date - baseline_date_start)
    t_extension <- as.numeric(baseline_date - vaccination_start)
    return(update_fit_vaccinations(fit, generate_vaccination_curve(
      baseline_coverage, baseline_dose_ratio, t_coverage, t_staging, t_extension, max_vaccinatable_pop, dur_V, vaccination_start
    )))
  }

  #generate a plot to show the differences between these
  #also need to make adjustments where doses will occur before the model begins (need to recheck how the initial states work)
}


# Implement a variant strategy for a country fit
implement_variant <- function(fit, Vaccine) {
  UseMethod("implement_vaccine")
}


implement_variant.rt_optimised <- function(fit, Variant){
  if (Variant == "baseline") {
    fit
  } else if (Variant == "reduced") {

    # TODO

  }
}


implement_variant.vacc_durR_nimue_simulation <- function(fit, Variant){
  if (Variant == "baseline") {
    fit
  } else if (Variant == "reduced") {
    #just set to delta shift to occur very far into the future
    #instead of removing entirely we could just delay this?
    fit$interventions$delta_adjustments$start_date <-
      fit$pmcmc_results$inputs$pars_obs$delta_start_date <-
      "3050-01-01"
  }
}
