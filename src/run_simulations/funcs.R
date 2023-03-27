# -------------------------------------------------------------------------- ###
#  Functions for set up
# -------------------------------------------------------------------------- ###

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

  download.file(path, "temp.Rds", mode = "wb", quiet = TRUE)
  fit <- readRDS("temp.Rds")
  unlink("temp.Rds")
  fit
}

# utility for converting scenario codes to a description
describe_scenarios <- function(scenarios, variant = FALSE){

  if("Rt" %in% names(scenarios)) {
    scenarios <- mutate(scenarios,
                        Rt = case_when(
                          Rt == "baseline" ~ "Public Health Optimum",
                          Rt == "target" ~ "Target based",
                          Rt == "economic" ~ "Economic based"
                        ))

    scenarios$Rt <- factor(
      scenarios$Rt,
      c("Public Health Optimum",
        "Target based",
        "Economic based"))
  }

  if("Vaccine" %in% names(scenarios)) {
    scenarios <- mutate(scenarios,
                        Vaccine = case_when(
                          Vaccine == "early" ~ "Science",
                          Vaccine == "manufacturing" ~ "Science & Manufacturing",
                          Vaccine == "equity" ~ "Science & Infrastructure",
                          Vaccine == "both" ~ "Science Total (Manu. & Infr.)"))

    scenarios$Vaccine <- factor(
      scenarios$Vaccine,
      c("Science",
        "Science & Infrastructure",
        "Science & Manufacturing",
        "Science Total (Manu. & Infr.)"))
  }

  if("Variant" %in% names(scenarios)) {
    scenarios <- mutate(scenarios,
                        Variant = case_when(
                          Variant == "baseline" ~ "As it occurred",
                          Variant == "reduced" ~ "Reduced"))
  }
  scenarios
}

#convert scenario codes and fit object into a list of simulations ready to run
implement_scenarios <- function(fit, scenarios, iso3c, force_opening){
  pmap(scenarios, function(Rt, Vaccine, Variant, fit, force_opening){
    #order matters since Rt can depend on vaccine coverage
    fit <- implement_vaccine(fit, Vaccine, iso3c)
    fit <- implement_Rt(fit, Rt, iso3c, force_opening)
    fit <- implement_variant(fit, Variant)
    fit
  }, fit = fit, force_opening = force_opening)
}

# -------------------------------------------------------------------------- ###
#  Functions for Rt
# -------------------------------------------------------------------------- ###

# get simple Rt by date list for all sample draws
simple_Rt <- function (model_out) {
  date_0 <- model_out$inputs$start_date
  iso3c <- squire::get_population(model_out$parameters$country)$iso3c[1]
  return(
    lapply(seq_along(model_out$samples),
           function(y) {
             Rt <- model_out$samples[[y]]$R0
             tt <- list(change = seq_along(Rt), dates = date_0 +
                          model_out$samples[[y]]$tt_R0)
             df <- data.frame(
               Rt = Rt,
               date = date_0 + model_out$samples[[y]]$tt_R0
             ) %>%
               dplyr::mutate(t = as.numeric(.data$date - min(.data$date))) %>%
               dplyr::mutate(iso3c = iso3c, rep = y)
             return(df)
           }))
}

# implements rt trends for fits
implement_Rt <- function(fit, Rt, iso3c, force_opening){
  if (Rt == "baseline") {
    fit
  } else if (Rt == "target") {

    implement_target_Rt(fit, iso3c, force_opening)

  } else if (Rt == "economic") {

    implement_economic_Rt(fit, iso3c, force_opening)

  }
}

# implements rt trends for target scenario
implement_target_Rt <- function(fit, iso3c, force_opening){
  UseMethod("implement_target_Rt")
}

# implements rt trends for target scenario for booster model
implement_target_Rt.rt_optimised <- function(fit, iso3c, force_opening) {

  # get previous rt trend
  rt <- simple_Rt(fit)

  # get vaccine allocation
  vacc <- data.frame(
    "date" = fit$parameters$tt_booster_doses + fit$inputs$start_date,
    "primary" = cumsum(fit$parameters$primary_doses)
  ) %>%
    mutate(secondary = lag(primary, fit$parameters$second_dose_delay))

  # population size for the fit
  pop <- squire::get_population(fit$parameters$country)$n

  # coverage needed for 80% coverage of over 60s
  cov_needed <- sum(tail(pop,5) * 0.8)
  cov_needed_adults <- sum(head(pop, -3) * 0.8)

  # date of opening dependent on income group
  if(squire.page:::get_income_group(iso3c) == "HIC") {
    open_date <- vacc$date[which(vacc$secondary > cov_needed_adults)[1]]
  } else {
    open_date <- vacc$date[which(vacc$secondary > cov_needed)[1]]
  }


  if(is.na(open_date) & force_opening){
    #if they don't open & we are forcing
    open_date <- readRDS("average_time_to_opening.Rds")
    #apply for either the start of vaccinations or the first death which ever is later
    start_vaccinations_deaths <- max(fit$parameters$tt_booster_doses[2], min(fit$inputs$data$t_start)) + fit$inputs$start_date
    open_date <- start_vaccinations_deaths + open_date$time_to_open[open_date$income_group == squire.page:::get_income_group(iso3c)]
  }

  # if they only open after the end of 2021 then set to NA as
  # we only care about openings before 2022
  if(!is.na(open_date)) {
  if(open_date > as.Date("2021-12-31")) {
    open_date <- NA
  }
  }

  # if they don't ever hit the vaccine target, then we just use their default Rt
  if(!is.na(open_date)){

    # Rt associated with "opening" and update the Rt data frames for each sample
    rt_new <- lapply(rt, function(x){

      # filter to after July 2020
      x2 <- x %>% filter(date < as.Date("2022-01-01") & date > as.Date("2020-07-01"))
      rt_open <- quantile(x2$Rt, prob = c(0.95), na.rm=TRUE)
      new_open_date <- adjust_open_date_for_rt_peaks(x, open_date, rt_open)
      rt_preopen <- (x2$Rt[x2$date > new_open_date])[1]

      # set the new rt
      x$Rt[x$date >= new_open_date] <- rt_open
      x$Rt[x$date >= new_open_date][1:4] <- seq(rt_preopen, rt_open, length.out = 4)
      return(x)

    })

    # Assign the new Rt for each sample
    for(i in seq_along(rt_new)) {
      fit$samples[[i]]$R0 <- rt_new[[i]]$Rt
    }

  }
  return(fit)

}

adjust_open_date_for_rt_peaks <- function(x, open_date, rt_open){
  # check if open_date occurs during a peak in Rt (higher than Rt open)
  # if so we delay the open date until Rt falls back down
  # this simulates
  past_open <- x$date > open_date
  n <- x$date[past_open][min(which(x$Rt[past_open] < rt_open))]
}

# implements rt trends for economic scenario
implement_economic_Rt <- function(fit, iso3c, force_opening){
  UseMethod("implement_economic_Rt")
}

# implements rt trends for economic scenario for booster model
implement_economic_Rt.rt_optimised <- function(fit, iso3c, force_opening) {

  # get previous rt trend
  rt <- simple_Rt(fit)

  # get vaccine allocation
  vacc <- data.frame(
    "date" = fit$parameters$tt_booster_doses + fit$inputs$start_date,
    "primary" = cumsum(fit$parameters$primary_doses)
  ) %>%
    mutate(secondary = lag(primary, fit$parameters$second_dose_delay))

  # population size for the fit
  pop <- squire::get_population(fit$parameters$country)$n

  # coverage needed for 80% coverage of over 60s
  cov_needed <- sum(tail(pop,5) * 0.8)

  # date of opening
  open_date <- vacc$date[which(vacc$secondary > cov_needed)[1]]

  if(is.na(open_date) & force_opening){
    #if they don't open & we are forcing
    open_date <- readRDS("average_time_to_opening.Rds")
    #apply for either the start of vaccinations or the first death which ever is later
    start_vaccinations_deaths <- max(fit$parameters$tt_booster_doses[2], min(fit$inputs$data$t_start)) + fit$inputs$start_date
    open_date <- start_vaccinations_deaths + open_date$time_to_open[open_date$income_group == squire.page:::get_income_group(iso3c)]
  }

  # if they only open after the end of 2021 then set to NA as
  # we only care about openings before 2022
  if(!is.na(open_date)) {
  if(open_date > as.Date("2021-12-31")) {
    open_date <- NA
  }
  }

  # if they don't ever hit the vaccine target, then we just use their default Rt
  if(!is.na(open_date)){

  # income group and related school effect size from separate analysis
  income <- squire.page:::get_income_group(iso3c)
  school_eff <- c(0.2, 0.1, 0.05, 0.02)[as.integer(income)]

  # Rt associated with "opening" and update the Rt data frames for each sample
  rt_new <- lapply(rt, function(x){

    # filter to after July 2020
    x2 <- x %>% filter(date < as.Date("2022-01-01") & date > as.Date("2020-07-01"))
    rt_open <- quantile(x2$Rt, prob = c(0.95), na.rm=TRUE)
    new_open_date <- adjust_open_date_for_rt_peaks(x, open_date, rt_open)
    rt_preopen <- (x2$Rt[x2$date > new_open_date])[1]

    # set rt to be fully open
    x$Rt[x$date > new_open_date] <- rt_open

    # set the new rt for school opening for the first month
    x$Rt[x$date > new_open_date][1:2] <- rt_preopen * (school_eff+1)

    # Rt for stepped opening over the next 6 months
    x$Rt[x$date > new_open_date][3:14] <- seq((rt_preopen * (school_eff+1)), rt_open, length.out = 12)

    return(x)

  })

  # Assign the new Rt for each sample
  for(i in seq_along(rt_new)) {
    fit$samples[[i]]$R0 <- rt_new[[i]]$Rt
  }

  }

  return(fit)

}

# -------------------------------------------------------------------------- ###
#  Functions for Vaccines
# -------------------------------------------------------------------------- ###

# weekly average for dose rollout
roll_doses <- function(x, roll = 7) {

  # final dose
  fin <- tail(x, 1)

  # use a weekly mean to decide on daily maximums
  x2 <- zoo::rollmean(x, roll, na.pad = TRUE)

  # fill the end NAs with fin
  x2_end_na <- (which(diff(which(is.na(x2)))>1)+1)
  x2[which(is.na(x2))][x2_end_na:length(which(is.na(x2)))] <- fin

  # and 0 for the starters
  x2[is.na(x2)] <- 0

  return(x2)

}

# Function to convert doses for Manufacture Scenario
max_grow <- function(x, roll = 7, max_cov) {

  # final dose
  fin <- tail(x, 1)

  # what is the total coverage
  tot <- max_cov

  # weekly mean of doses to decide on daily maximums
  x2 <- roll_doses(x, roll)

  # convert roll out to never decrease below current max daily dose
  for(i in seq_len(length(x2)-1)) {
    if(x2[i] > x2[i+1]) {
      x2[i+1] <- x2[i]
    }
  }

  # Create new doses over time to  stop at max coverage
  x3 <- cumsum(x2)
  end <- which(x3 >= tot)[1]
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
fast_grow <- function(x, roll = 7, tot_mult, speed = 2, eoy_x, max_cov) {

  # final dose
  fin <- tail(x, 1)

  # In fast grow no need to roll
  x2 <- x

  # set total to be reached before plateau
  tot <- max_cov

  # Increase roll out speed
  # If tot_mult is greater than speed that means we won't
  # hit our first year target on time.
  if(tot_mult > speed){
    x2 <- x2 * tot_mult
  } else {
    x2 <- x2 * speed
  }

  # now find the point at which we go past our total
  x3 <- cumsum(x2)
  end <- which(x3 > floor(tot))[1]

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

#function to setup a booster dose curve for countries without boosting in real world
add_boosters <- function(booster, income_boosts, iso3c, start_vacc, end_vacc, tt_doses, pop_size){

  # when would a country with this income group start boosting
  t_boosting <- income_boosts$boost_start[income_boosts$wb == squire.page::get_income_group(iso3c)]
  t_boosting <- start_vacc + t_boosting

  # what annual per capita rate would a country with this income group boost
  annual_boost <- income_boosts$annual_boosts[income_boosts$wb == squire.page::get_income_group(iso3c)]/100

  # how long will they boost for
  days_boosting <- end_vacc - t_boosting

  # total_boosting amount
  total_boosters <- ((days_boosting)/365 * annual_boost) * sum(pop_size)
  daily_boosters <- round(total_boosters/days_boosting)

  # assign the boosters
  booster[(which(tt_doses == t_boosting)):length(booster)] <- daily_boosters
  booster
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
  if (Vaccine %in% c("manufacturing")) { # we handle the both case inside implement_vaccine_both
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

  # bring vaccination earlier
  start_vacc <- fit$parameters$tt_booster_doses[2] - difference
  end_vacc <- tail(fit$parameters$tt_booster_doses, 1)
  tt_doses <- seq(start_vacc, end_vacc, 1)
  if(start_vacc > 0){
    tt_doses <- c(0, tt_doses)
  } else {
    tt_doses <- c(tt_doses[1] - 1, tt_doses)
  }

  # All scenarios coded up
  if (Vaccine == "early") {

    new_values <- list(
      primary_doses = primary,
      booster_doses = booster,
      tt = tt_doses
    )
    return(update_fit_vaccinations(fit, new_values))

  } else if (Vaccine == "equity") {

    # use equity scenario
    new_values <- implement_vaccine_equity(
      fit, difference, end_of_cepi_year_one, primary, booster, start_vacc, end_vacc, tt_doses
    )
    return(update_fit_vaccinations(fit, new_values))

  } else if (Vaccine == "manufacturing") {

    # update vaccine efficacies
    fit <- update_vaccine_profile(fit)

    # use manufacture scenario
    new_values <- implement_vaccine_manufacturing(
      fit, difference, primary, booster, tt_doses
    )
    return(update_fit_vaccinations(fit, new_values))

  } else if (Vaccine == "both") {

    # update vaccine efficacies
    fit <- update_vaccine_profile(fit)

    # use both scenario
    new_values <- implement_vaccine_both(
      fit, difference, end_of_cepi_year_one, primary, booster, start_vacc, end_vacc, tt_doses, baseline_date_start, cepi_start_date
    )
    return(update_fit_vaccinations(fit, new_values))

  }

}

# Equity scenario
implement_vaccine_equity <- function(fit, difference, end_of_cepi_year_one, primary, booster, start_vacc, end_vacc, tt_doses, increased_cov = NULL){

  # --------------------------------------------- #
  # 1. change vaccine rollout speed
  # --------------------------------------------- #

  # forty percent of vaccinatable population
  pop_size <- squire::get_population(fit$parameters$country)$n
  forty_vac <- (sum(pop_size[-(1:3)])*0.4)

  # by what date/tt is this to be achieved by
  new_vacc_dates <- c(0, fit$parameters$tt_primary_doses[-1] - difference) + fit$inputs$start_date
  new_vacc_dates <- c(
    new_vacc_dates,
    seq.Date(tail(new_vacc_dates,1)+1,
             tail(fit$parameters$tt_primary_doses + fit$inputs$start_date,1),
             1)
  )
  eoy <- which(new_vacc_dates == end_of_cepi_year_one) - fit$parameters$second_dose_delay

  # how much increase is required to reach 40% coverage by this date
  if(is.null(increased_cov)) {
  increased_cov <- forty_vac / cumsum(primary)[eoy]
  }

  # maximum before plateau on primary
  max_primary_cov <- max(forty_vac, sum(fit$parameters$primary_doses))
  max_booster_cov <- sum(fit$parameters$booster_doses)

  # adjust our primary vaccines
  primary <- fast_grow(x = primary, tot_mult = increased_cov, eoy_x = eoy, speed = equity_speed, max_cov = max_primary_cov)

  # adjust boosters as well
  if(sum(booster) > 0){
    # just double the booster rate at this moment in time
    booster <- fast_grow(booster, tot_mult = equity_speed, eoy_x = eoy, speed = equity_speed, max_cov = max_booster_cov)
  } else {
    #can't scale like that if there are no booster doses to begin with
    booster <- add_boosters(booster, income_boosts, iso3c, start_vacc, end_vacc, tt_doses, pop_size)
  }

  # --------------------------------------------- #
  # 2. create and assign our new values
  # --------------------------------------------- #

  new_values <- list(
    primary_doses = primary,
    booster_doses = booster,
    tt = tt_doses,
    increased_cov = increased_cov,
    eoy = eoy
  )

  return(new_values)

}

# Manufacture scenario
implement_vaccine_manufacturing <- function(fit, difference, primary, booster, tt_doses, max_cov = NULL) {

  # --------------------------------------------- #
  # 1. change vaccine rollout speed
  # --------------------------------------------- #

  # if we have not been provided the max_coverage just use the maximum observed in reality
  if(is.null(max_cov)) {
    max_cov <- sum(fit$parameters$primary_doses)
  }

  # adjust our primary vaccines
  primary <- max_grow(primary, max_cov = sum(fit$parameters$primary_doses))

  # adjust boosters as well if the actually boosted in real world
  if(sum(booster) > 0){
    booster <- max_grow(booster, max_cov = sum(fit$parameters$booster_doses))
  }

  # --------------------------------------------- #
  # 2. create and assign our new values
  # --------------------------------------------- #

  new_values <- list(
    primary_doses = primary,
    booster_doses = booster,
    tt = tt_doses
  )

  return(new_values)

}

# Both scenario
implement_vaccine_both <- function(fit, difference, end_of_cepi_year_one, primary,
                                   booster, start_vacc, end_vacc, tt_doses,
                                   baseline_date_start, cepi_start_date){

  # --------------------------------------------- #
  # 1. change vaccine rollout speed
  # --------------------------------------------- #

  # ----------------------------------------------------------------- #
  # a) we need to know the increase that would have been provided in the equity
  # ----------------------------------------------------------------- #

  # forty percent of vaccinatable population
  pop_size <- squire::get_population(fit$parameters$country)$n
  forty_vac <- (sum(pop_size[-(1:3)])*0.4)

  # by what date/tt is this to be achieved by
  new_vacc_dates <- c(fit$parameters$tt_primary_doses[-1] - difference) + fit$inputs$start_date
  if(!fit$inputs$start_date %in% new_vacc_dates){
    new_vacc_dates <- c(0, new_vacc_dates)
  } else {
    new_vacc_dates <- c(new_vacc_dates[1] - 1, new_vacc_dates)
  }
  new_vacc_dates <- c(
    new_vacc_dates,
    seq.Date(tail(new_vacc_dates,1)+1,
             tail(fit$parameters$tt_primary_doses + fit$inputs$start_date,1),
             1)
  )

  # and what would that increased_cov be
  eoy <- which(new_vacc_dates == end_of_cepi_year_one) - fit$parameters$second_dose_delay
  increased_cov <- forty_vac / cumsum(primary)[eoy]

  # ----------------------------------------------------------------- #
  # b) now we can correctly update primary/booster/tt_doses
  # ----------------------------------------------------------------- #

  difference <- as.numeric(baseline_date_start - cepi_start_date)

  # Extend our dose series as used in all scenarios
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

  # bring vaccination earlier
  start_vacc <- fit$parameters$tt_booster_doses[2] - difference
  end_vacc <- tail(fit$parameters$tt_booster_doses, 1)
  tt_doses <- seq(start_vacc, end_vacc, 1)
  if(start_vacc > 0){
    tt_doses <- c(0, tt_doses)
  } else {
    tt_doses <- c(tt_doses[1] - 1, tt_doses)
  }

  # ----------------------------------------------------------------- #
  # c) now we can correctly make the equit changes
  # ----------------------------------------------------------------- #

  # make the equity changes
  new_values <- implement_vaccine_equity(
    fit, difference, end_of_cepi_year_one, primary, booster, start_vacc, end_vacc, tt_doses, increased_cov
  )

  # --------------------------------------------- #
  # 2. make manufacturer rollout speed changes
  # --------------------------------------------- #

  # forty percent of vaccinatable population
  pop_size <- squire::get_population(fit$parameters$country)$n
  forty_vac <- (sum(pop_size[-(1:3)])*0.4)

  # max coverage before going to final rate is either forty vacc if it wasn't reached
  # normally
  max_primary_cov <- max(c(forty_vac, sum(fit$parameters$primary_doses)))

  # adjust our primary vaccines
  primary <- max_grow(new_values$primary, max_cov = max_primary_cov)

  # adjust boosters as well only if they actually boosted in real world
  if(sum(new_values$booster) > 0) {
    booster <- max_grow(new_values$booster, max_cov = sum(new_values$booster))
  }

  # --------------------------------------------- #
  # 2. create and assign our new values
  # --------------------------------------------- #

  new_values <- list(
    primary_doses = primary,
    booster_doses = booster,
    tt = tt_doses
  )

  return(new_values)

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

# Update vaccine profile for different classes of vaccines
update_vaccine_profile <- function(fit){
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
    new_profile <- readRDS("vaccine_profiles.Rds")$mRNA
  } else {
    new_profile <- readRDS("vaccine_profiles.Rds")$Adenovirus
  }
  #derive variant timings
  new_ve_values <- reduce(c("Wild", "Delta", "Omicron"), function(params, variant){
    new_params <- compute_VoC_fixing_changes(fit, variant, new_profile)
    if(is.null(params)){
      new_params
    } else {
      out <- map(names(new_params), ~append(params[[.x]], new_params[[.x]]))
      names(out) <- names(new_params)
      out
    }
  }, .init = NULL)
  #one profile for all samples
  fit$parameters$vaccine_efficacy_disease <- new_ve_values$vaccine_efficacy_disease
  fit$parameters$vaccine_efficacy_infection <- new_ve_values$vaccine_efficacy_infection
  fit$parameters$dur_V <- new_ve_values$dur_V
  fit$parameters$tt_vaccine_efficacy_infection <-
    fit$parameters$tt_vaccine_efficacy_disease <-
    fit$parameters$tt_dur_V <- new_ve_values$tt
  #remove ve's from samples
  fit$samples <- map(fit$samples, function(sample){
    sample$vaccine_efficacy_disease <-
      sample$vaccine_efficacy_infection <-
      sample$dur_V <-
      sample$tt_vaccine_efficacy_infection <-
      sample$tt_vaccine_efficacy_disease <-
      sample$tt_dur_V <- NULL
    sample
  })

  fit
}

# Update timings of vaccine efficacies for
compute_VoC_fixing_changes <- function(fit, variant, new_profile){
  profile <- new_profile[new_profile$variant == variant, ] %>%
    arrange(parameter) %>%
    pull(value, parameter)
  if(variant == "Wild") {
    params <- extract_profile(profile) %>%
      map(~list(.x))
    params$tt <- 0
  } else {
    dur_R_index <- list(Delta = 1:2, Omicron = 3:4)[[variant]]
    t_timings <- fit$samples[[1]]$tt_dur_R[-1][dur_R_index]
    previous_variant <- list(Delta = "Wild", Omicron = "Delta")[[variant]]
    old_profile <- new_profile[new_profile$variant == previous_variant, ] %>%
      arrange(parameter) %>%
      pull(value, parameter)
    #just linear change
    interp_values <- map(seq_along(old_profile), function(i){
      seq(old_profile[i], profile[i], length.out = diff(t_timings) + 2)[-1]
    })
    tt <- seq(t_timings[1], t_timings[2])
    params <- map(seq_along(tt), function(t){
      out <- map_dbl(interp_values, ~.x[t])
      names(out) <- names(old_profile)
      extract_profile(out)
    }) %>%
      transpose()
    params$tt <- tt
  }
  params
}

# Extract vaccine profile
extract_profile <- function(profile){
  out <- list(
    vaccine_efficacy_disease = profile[c("pV_1_d", "fV_1_d", "fV_2_d", "bV_1_d", "bV_2_d", "bV_3_d")],
    vaccine_efficacy_infection = profile[c("pV_1_i", "fV_1_i", "fV_2_i", "bV_1_i", "bV_2_i", "bV_3_i")],
    dur_V = 1/profile[c("w_p", "w_1", "w_2")]
  )
  map(out, function(x){
    names(x) <- NULL
    x
  })
}

# -------------------------------------------------------------------------- ###
#  Functions for Variants
# -------------------------------------------------------------------------- ###

# Implement a variant strategy for a country fit
implement_variant <- function(fit, Vaccine) {
  UseMethod("implement_variant")
}

# Implement a variant strategy for a country fit using booster model
implement_variant.rt_optimised <- function(fit, Variant){
  if (Variant == "baseline") {
    fit
  } else if (Variant == "reduced") {

    # just set all future shift to occur very far into the future
    fit$parameters$tt_dur_get_ox_survive <- fit$parameters$tt_dur_get_ox_survive + 1e6
    fit$parameters$tt_dur_get_ox_die <- fit$parameters$tt_dur_get_ox_die + 1e6
    fit$parameters$tt_dur_get_mv_survive <- fit$parameters$tt_dur_get_mv_survive + 1e6
    fit$parameters$tt_dur_get_mv_die <- fit$parameters$tt_dur_get_mv_die + 1e6

    for(i in seq_along(fit$samples)) {
      fit$samples[[i]]$tt_dur_R <- fit$samples[[i]]$tt_dur_R + 1e6
      fit$samples[[i]]$tt_dur_V <- fit$samples[[i]]$tt_dur_R + 1e6
    }

  }
}

# Implement a variant strategy for a country fit using nimue model
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

# -------------------------------------------------------------------------- ###
#  Accessory Functions (could go in plotting)
# -------------------------------------------------------------------------- ###

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

get_deaths_infections_time <- function(out){
  value <- quick_format(out, c("D", "infections_cumu"), out$inputs$start_date)
  value$date <- as.Date(rownames(value))
  value <- value %>%
    group_by(replicate, compartment) %>%
    arrange(date) %>%
    filter(y > 0) %>%
    transmute(
      y = c(0, diff(y)),
      date = date,
      replicate = replicate
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = compartment, values_from = y) %>%
    rename(deaths = D, infections = infections_cumu)
  value
}

get_deaths_infections_age <- function(out){

  indexes <- nimue:::odin_index(out$model)[c("D", "infections_cumu")]

  map_dfr(
    indexes, function(index){
      map_dfr(1:17, function(age){
        map_dfr(seq_len(dim(out$output)[3]), function(i){
          df <- data.frame(y = rowSums(out$output[, index[age, ], i]), age_group = age)
          df$replicate <- i
          df$date <- seq_len(nrow(df)) + out$inputs$start_date
          return(df)
        })
      })
    }, .id = "compartment"
  ) %>%
    group_by(replicate, age_group, compartment) %>%
    summarise(
      y = max(y),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = compartment, values_from = y) %>%
    rename(deaths = D, infections = infections_cumu)
}

save_scenario <- function(out, name){
  df_time <- get_deaths_infections_time(out)
  df_age <- get_deaths_infections_age(out)
  saveRDS(df_time, paste0("data/time_", name, ".Rds"))
  saveRDS(df_age, paste0("data/age_", name, ".Rds"))
}

plot_deaths <- function(scenario_df, facet = FALSE){
  baseline <- readRDS("data/time_baseline.Rds") %>%
    group_by(replicate) %>%
    arrange(date) %>%
    mutate(
      across(
        c(deaths, infections), ~cumsum(.x)
      )
    ) %>%
    group_by(date) %>%
    summarise(
      across(
        c(deaths, infections), ~median(.x, na.rm=TRUE),
        .names = "{col}_med"
      ),
      across(
        c(deaths, infections), ~quantile(.x, 0.025, na.rm=TRUE),
        .names = "{col}_025"
      ),
      across(
        c(deaths, infections), ~quantile(.x, 0.975, na.rm=TRUE),
        .names = "{col}_975"
      ),
      .groups = "drop"
    )
  scenarios_x <- readRDS("data/time_scenarios.Rds") %>%
    group_by(scenario, replicate) %>%
    arrange(date) %>%
    mutate(
      across(
        c(deaths, infections), ~cumsum(.x)
      )
    ) %>%
    group_by(scenario, date) %>%
    summarise(
      across(
        c(deaths, infections), ~median(.x, na.rm=TRUE),
        .names = "{col}_med"
      ),
      across(
        c(deaths, infections), ~quantile(.x, 0.025, na.rm=TRUE),
        .names = "{col}_025"
      ),
      across(
        c(deaths, infections), ~quantile(.x, 0.975, na.rm=TRUE),
        .names = "{col}_975"
      ),
      .groups = "drop"
    )

  factor_label <- scenario_df %>%
    mutate(
      label = if_else(
        Variant == "baseline",
        paste0(Rt, " & ", Vaccine),
        paste0(Rt, ", ", Vaccine, " & ", Variant)
      )
    ) %>%
    pull(label)

  scenarios_x$scenario <- factor(
    factor_label[scenarios_x$scenario],
    levels = sort(factor_label)
  )
  scenarios_x$Rt <- gsub("(.*)( & .*)","\\1", scenarios_x$scenario)
  scenarios_x$Vaccine <- gsub("(.* & )(.*)","\\2", scenarios_x$scenario)
  scenarios_x <- describe_scenarios(scenarios_x)

  if(facet) {
    ggplot(baseline, aes(x = date)) +
      geom_ribbon(aes(ymin = deaths_025, ymax = deaths_975),
                  alpha = 0.1) +
      geom_line(aes(y = deaths_med)) +
      geom_ribbon(data = scenarios_x, aes(x = date, ymin = deaths_025, ymax = deaths_975, fill = scenario, group = scenario),
                  inherit.aes = FALSE,
                  alpha = 0.1) +
      geom_line(
        data = scenarios_x, aes(x = date, y = deaths_med, colour = scenario, group = scenario),
        inherit.aes = FALSE
      ) +
      facet_wrap(vars(scenario)) +
      ggpubr::theme_pubr(legend = "none") +
      labs(x = "", y = "Cumulative Deaths (95% quantile and median)")
  } else {
    ggplot(baseline, aes(x = date)) +
      geomtextpath::geom_textvline(label = "100-Day Target", xintercept = as.Date(cepi_start_date), hjust = 0.59) +
      geom_ribbon(aes(ymin = deaths_025, ymax = deaths_975, color = "Baseline"),
                  alpha = 0.1, show.legend = FALSE) +
      geom_line(aes(y = deaths_med, color = "Baseline"), lwd = 0.75) +
      geom_line(
        data = scenarios_x, aes(x = date, y = deaths_med, colour = Vaccine, group = scenario),
        inherit.aes = FALSE,
        lwd = 0.75
      ) +
      ggpubr::theme_pubr(base_size = 14) +
      facet_wrap(vars(Rt), ncol = 1) +
      scale_color_manual(name = "", values = c("Black", pals::stepped3()[c(1,5,9,13)])) +
      labs(x = "Date", y = "Cumulative Deaths (95% quantile and median)") +
      guides(color=guide_legend(nrow=2, byrow=TRUE)) +
      theme(panel.grid.major = element_line(),
            legend.key = element_rect(fill = "white", colour = "white"),
            legend.text = element_text(size = 14),
            axis.line = element_line(),
            panel.border = element_rect(color = "black", fill = NA))

  }
}

plot_deaths_averted <- function(scenario_df, facet = FALSE){
  baseline <- readRDS("data/time_baseline.Rds") %>%
    group_by(replicate) %>%
    arrange(date) %>%
    mutate(
      across(
        c(deaths, infections), ~cumsum(.x)
      )
    ) %>%
    rename(baseline_deaths = deaths,
           baseline_infections = infections)

  scenarios_x <- readRDS("data/time_scenarios.Rds") %>%
    group_by(scenario, replicate) %>%
    arrange(date) %>%
    mutate(
      across(
        c(deaths, infections), ~cumsum(.x)
      )
    )

  factor_label <- scenario_df %>%
    mutate(
      label = if_else(
        Variant == "baseline",
        paste0(Rt, " & ", Vaccine),
        paste0(Rt, ", ", Vaccine, " & ", Variant)
      )
    ) %>%
    pull(label)

  # now calculate deaths averted per scenario against baseline
  scenarios_x <- left_join(scenarios_x, baseline) %>%
    mutate(deaths_averted = baseline_deaths - deaths) %>%
    mutate(infections_averted = baseline_infections - infections) %>%
    group_by(scenario, date) %>%
    summarise(
      across(
        c(deaths_averted, infections_averted), ~median(.x, na.rm=TRUE),
        .names = "{col}_med"
      ),
      across(
        c(deaths_averted, infections_averted), ~quantile(.x, 0.025, na.rm=TRUE),
        .names = "{col}_025"
      ),
      across(
        c(deaths_averted, infections_averted), ~quantile(.x, 0.25, na.rm=TRUE),
        .names = "{col}_25"
      ),
      across(
        c(deaths_averted, infections_averted), ~quantile(.x, 0.75, na.rm=TRUE),
        .names = "{col}_75"
      ),
      across(
        c(deaths_averted, infections_averted), ~quantile(.x, 0.975, na.rm=TRUE),
        .names = "{col}_975"
      ),
      .groups = "drop"
    )

  # assign descriptions for the scenarios
  scenarios_x$scenario <- factor(
    factor_label[scenarios_x$scenario],
    levels = sort(factor_label)
  )
  scenarios_x$Rt <- gsub("(.*)( & .*)","\\1", scenarios_x$scenario)
  scenarios_x$Vaccine <- gsub("(.* & )(.*)","\\2", scenarios_x$scenario)

  deaths_averted <- scenarios_x %>%
    group_by(scenario, Rt, Vaccine) %>%
    filter(date == max(.$date)) %>%
    describe_scenarios() %>%
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
    labs(x = "", y = "Cumulative Deaths Averted (median, IQR, 95% quantile)") +
    scale_y_continuous(n.breaks = 6) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14), plot.margin = margin(0, 1, 0, 0, "cm")) +
    coord_flip()

  infections_averted <- scenarios_x %>%
    group_by(scenario, Rt, Vaccine) %>%
    summarise(across(deaths_averted_med:infections_averted_975, sum)) %>%
    describe_scenarios() %>%
    mutate(Rt = factor(gsub(" ", "\n", Rt), gsub(" ", "\n", levels(Rt)))) %>%
    ggplot(aes(x = Rt, y = infections_averted_med,
               ymin = infections_averted_025, ymax = infections_averted_975,
               color = Vaccine, group = interaction(Vaccine, Rt))) +
    geom_hline(yintercept = 0, color = "black") +
    geom_linerange(position = position_dodge(width = 0.5), lwd = 2, alpha = 0.3) +
    geom_linerange(aes(ymin = infections_averted_25, ymax = infections_averted_75),
                   position = position_dodge(width = 0.5), lwd = 2, alpha = 0.6) +
    geom_point(shape = 21, size =2, fill = "white", position = position_dodge(width = 0.5)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(panel.grid.major = element_line()) +
    scale_color_manual(name = "", values = c(pals::stepped3()[c(1,5,9,13)])) +
    labs(x = "", y = "Cumulative Infections Averted (median, IQR, 95% quantile)") +
    scale_y_continuous(n.breaks = 6) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14), plot.margin = margin(0, 1, 0, 0, "cm")) +
    coord_flip()

  cowplot::plot_grid(deaths_averted, infections_averted + theme(legend.position = "none"), ncol = 1, rel_heights = c(1,0.9))

}

simulate_early_vaccinations <- function(model_object){
    t_offset <- model_object$parameters$tt_primary_doses[1]

    #update times
    tt_vars <- grep("tt_", names(model_object$parameters), value = TRUE)
    model_object$parameters[tt_vars] <- map(model_object$parameters[tt_vars], function(tt){
      tt <- tt - t_offset
      tt[1] <- 0
      tt
    })

    old_initial_infections <- map(model_object$samples, ~.x$initial_infections)

    model_object$samples <- map(model_object$samples, function(x){
      x$initial_infections <- 0 #ensure there is no epidemic

      tt_vars <- grep("tt_", names(x), value = TRUE)
      x[tt_vars] <- map(x[tt_vars], function(tt){
        tt <- tt - t_offset
        tt[1] <- 0
        tt
      })

      x
    })

    #limit run to the just the pre-epidemic period
    new_data <- model_object$inputs$data %>%
      mutate(t_end = t_end - t_offset,
             t_start = t_start - t_offset)
    model_object$inputs$data <- model_object$inputs$data %>%
      head(1) %>%
      mutate(t_end= - t_offset)

    model_object$inputs$start_date <- model_object$inputs$start_date + t_offset

    model_object <- generate_draws(model_object)

    model_object$samples <- map(seq_along(model_object$samples), function(x){
      model_object$samples[[x]]$initial_infections <- old_initial_infections[[x]]
      model_object$samples[[x]]
    })

    model_object$inputs$data <- new_data

    #add backin the initial infections (need to do this)
    squire.page:::assign_infections
    #keep props the same across ages and vaccine status
    ages <- 4:14
    vaccines <- 1:7
    ages_vaccines <- map(ages, ~map_chr(vaccines, function(x){paste0(.x, ",", x)})) %>%
      unlist()

    S_vars <- paste0("S[", ages_vaccines, "]")
    E1_vars <- paste0("E1[", ages_vaccines, "]")

    prop <- unlist(old_initial_infections)/colSums(model_object$output[1, S_vars, ])

    last <- dim(model_object$output)[1]

    model_object$output[last, E1_vars, ] <-
      sweep(model_object$output[last, S_vars, ], 2, prop, "*")

    model_object$output[last, S_vars, ] <-
      sweep(model_object$output[last, S_vars, ], 2, 1 - prop, "*")

    model_object
}

