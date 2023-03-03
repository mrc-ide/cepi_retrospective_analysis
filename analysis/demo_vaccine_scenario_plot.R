## Demonstration of vaccine roll out scenarios for primary doses
library(tidyverse)

# Function to get Manufacture Scenario
max_grow <- function(x, roll = 7, tot_mult = 1) {

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

  return(x2)

}

# Function to get Systems Scenerio
fast_grow <- function(x, roll = 7, tot_mult = 2, speed = 2, eoy_x) {

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

  return(x2)

}

# Now get the fit for Kenya
iso3c <- "KEN"
excess_mortality <- FALSE
booster <- TRUE

## Get fit from github
source(here::here("src/run_simulations/funcs.R"))
fit <- grab_fit(iso3c, excess_mortality, booster)


# 1. Create the baseline
df <- data.frame("doses" = (fit$parameters$primary_doses) ,
           "date" = fit$parameters$tt_primary_doses + fit$inputs$start_date,
           "scen" = "Baseline") %>%
  complete(date = seq.Date(min(date), max(date), 1)) %>%
  mutate(doses = replace_na(doses, 0)) %>%
  mutate(scen = replace_na(scen, "Baseline"))

# 2. Create the Science scenario
df2 <- data.frame("doses" = (fit$parameters$primary_doses) ,
                  "date" = c(0, fit$parameters$tt_primary_doses[-1] - 232) + fit$inputs$start_date ,
                  "scen" = "Science") %>%
  complete(date = seq.Date(min(date), max(date), 1)) %>%
  mutate(doses = replace_na(doses, 0)) %>%
  mutate(scen = replace_na(scen, "Science"))

# 3. Create the Manufacturing scenario
df3 <- data.frame("doses" = max_grow(fit$parameters$primary_doses),
                  "date" = c(0, fit$parameters$tt_primary_doses[-1] - (fit$parameters$tt_primary_doses[2] - 100)) + as.Date("2020-01-08") ,
                  "scen" = "Science + Manufacturing")  %>%
  complete(date = seq.Date(min(date), max(date), 1)) %>%
  mutate(doses = replace_na(doses, 0)) %>%
  mutate(scen = replace_na(scen, "Science + Manufacturing"))

# 3. Create the Systems scenario
df4 <- data.frame("doses" = (fit$parameters$primary_doses) ,
                  "date" = c(0, fit$parameters$tt_primary_doses[-1] - 232) + fit$inputs$start_date,
                  "scen" = "Science + Systems") %>%
  complete(date = seq.Date(min(date), max(date), 1)) %>%
  mutate(doses = replace_na(doses, 0)) %>%
  mutate(scen = replace_na(scen, "Science + Systems"))

increased_cov <- (sum(squire::get_population(fit$parameters$country)$n[-(1:3)])*0.4) / sum(fit$parameters$primary_doses)
eoy <- which(df4$date == (as.integer((as.Date("2020-04-20") + 365))))
df4$doses <- fast_grow(df4$doses, tot_mult = increased_cov, eoy_x = eoy, speed = 2)

# 4. Create the Systems + Manufacturing scenario
df5 <- data.frame("doses" = max_grow(fit$parameters$primary_doses),
                  "date" = c(0, fit$parameters$tt_primary_doses[-1] - (fit$parameters$tt_primary_doses[2] - 100)) + as.Date("2020-01-08") ,
                  "scen" = "Science + Manufacturing + Systems")  %>%
  complete(date = seq.Date(min(date), max(date), 1)) %>%
  mutate(doses = replace_na(doses, 0)) %>%
  mutate(scen = replace_na(scen, "Science + Manufacturing + Systems"))

increased_cov <- (sum(squire::get_population(fit$parameters$country)$n[-(1:3)])*0.4) / sum(fit$parameters$primary_doses)
eoy <- which(df5$date == (as.integer((as.Date("2020-04-20") + 365))))
df5$doses <- fast_grow(df5$doses, tot_mult = increased_cov, eoy_x = eoy, speed = 2)

# Demonstration Plot of it all together
gg2 <- rbind(df, df2, df3, df4, df5) %>%
  group_by(scen) %>%
  mutate(doses = cumsum(doses)) %>%
  complete(date = seq.Date(min(date), max(df$date), 1)) %>%
  fill(doses, .direction = "down") %>%
  filter(date > as.Date("2020-01-08")) %>%
  ggplot(aes(x = as.integer(date - as.Date("2020-04-20"))+100, doses, color = scen)) +
  geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
  geom_line(lwd = 1) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(), legend.position = "right",
        legend.key = element_rect(fill = "white")) +
  scale_color_discrete(name = "Scenario:") +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Cumulative Primary Vaccine Doses")

gg1 <- rbind(df, df2, df3, df4, df5) %>%
  group_by(scen) %>%
  mutate(doses = (doses)) %>%
  complete(date = seq.Date(min(date), max(df$date), 1)) %>%
  fill(doses, .direction = "down") %>%
  filter(date > as.Date("2020-01-08")) %>%
  ggplot(aes(x = as.integer(date - as.Date("2020-04-20"))+100, doses, color = scen)) +
  geomtextpath::geom_textvline(label = "100-Day Mission Target", xintercept = 100, hjust = 0.59) +
  geom_line(lwd = 1) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(), legend.position = "right",
        legend.key = element_rect(fill = "white")) +
  scale_color_discrete(name = "Scenario:") +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Daily Primary Vaccine Doses")

cowplot::plot_grid(gg1, gg2, ncol = 1, align = "v")
