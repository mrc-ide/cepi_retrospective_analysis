
calculate_true_uptake <- function(doses, eligible_pop, t_extension) {
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

