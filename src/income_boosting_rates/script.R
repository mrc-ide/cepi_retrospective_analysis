# booster_analysis

# get owid boost data
vaccs <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
vaccs$date <- as_date(vaccs$date)
report_date <- as_date(report_date)
vaccs <- vaccs %>% filter(date <= report_date)

# boost rates per country
boost_rates <- vaccs %>%
  group_by(iso_code) %>%
  group_modify( ~ {

    x <- .x
    vacc_start <- x$date[which(x$daily_vaccinations > 0)[1]]
    boosts <- x$total_boosters_per_hundred[x$date == vacc_start + 365]
    t_boost_start <- x$date[which(x$total_boosters_per_hundred > 0)[1]]
    tb <- as.integer(t_boost_start - vacc_start)
    time <- 365

    if(length(boosts) == 0) boosts <- NA

    # if not one year mark then take last value
    if(is.na(boosts)){
      if(any(!is.na(x$total_boosters_per_hundred))){
        t <- tail(which(x$total_boosters_per_hundred > 0),1)
        date <- x$date[t]
        time <- as.integer(date - vacc_start)
        boosts <- x$total_boosters_per_hundred[t]
      }
    }

    # if there was no boosting set to NA
    if(length(t) == 0) t <- NA
    if(length(boosts) == 0) boosts <- NA
    if(length(tb) == 0) tb <- NA

    #return data frame with annual rate
    if(!is.na(boosts)) {
      return(data.frame(
        "iso3c" = x$iso_code[1],
        "wb" = squire.page:::get_income_group(x$iso_code[1]),
        "annual_boosts"  = (boosts/time) * 365,
        "boost_start" = tb
      ))
    } else {
      return(data.frame(
        "iso3c" = x$iso_code[1],
        "wb" = squire.page:::get_income_group(x$iso_code[1]),
        "annual_boosts"  = NA,
        "boost_start" = NA
      ))
    }

  }, .keep = TRUE)

# get median boosting rates by income and save the output
income_boosts <- boost_rates %>%
  na.omit %>%
  group_by(wb) %>%
  summarise(annual_boosts = median(annual_boosts, na.rm = TRUE),
            boost_start = median(boost_start, na.rm = TRUE))

saveRDS(income_boosts, "income_boosts.Rds")
