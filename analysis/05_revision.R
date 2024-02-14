#1.  Example BRA

source("src/run_simulations/funcs.R")
library(tidyverse)
iso3c <- "BRA"
end_date <- as.Date("2022-01-01")
orderly_ids <- readRDS("analysis/data_out/orderly_ids_new.rds")


log <- readRDS(paste0("archive/run_simulations/",orderly_ids$orderly_id[orderly_ids == iso3c],"/orderly_run.rds"))

## Get fit from github
fit <- grab_fit(iso3c, TRUE, TRUE)

## Update model fit objects
fit$inputs$data <- fit$inputs$data %>% filter((log$meta$parameters$cepi_start_date <= end_date))
fit$squire_model <- squire.page:::nimue_booster_min_model(use_dde = TRUE, use_difference = FALSE)
fit$model <- fit$squire_model$odin_model(
  user = squire.page:::assign_infections(
    squire.page:::setup_parameters(fit$squire_model, fit$parameters),
    mean(fit$inputs$initial_infections_interval)
  ),
  unused_user_action = "ignore"
)

original_out <- squire.page::generate_draws(fit)


tb <- readRDS("archive/run_simulations/20230430-193834-8f4b09dd/data/time_baseline.Rds")

head(tb)

#2. Compare 95%

npi <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/stringency_index_avg.csv")
npi_dates <- npi %>% pivot_longer(X01Jan2020:X20Feb2023) %>%
  rename(date = name) %>%
  mutate(date = strptime(gsub("X","",date), format = "%d%b%Y")) %>%
  filter(date > as.Date("2020-07-01") & date < as.Date("2021-12-31")) %>%
  group_by(country_code) %>%
  mutate(min_dates = value < min(value, na.rm = TRUE)+60) # plausible delay for NPI to have an effect and then relate to Rt as measured by deaths

# get all our base country fits
iso3cs <- orderly_ids$iso3cs

## Get fit from github
fit_list <- vector("list", length = length(iso3cs))
names(fit_list) <- iso3cs
for(i in (iso3cs)) {
  fit_list[[i]] <- grab_fit(i, TRUE, TRUE)
  }

## Get Rt from fits
rt_list <- vector("list", length = length(iso3cs))
rt_out_list <- vector("list", length = length(iso3cs))
names(rt_list) <- names(rt_out_list) <- iso3cs
for(i in (iso3cs)) {
  rt_out_list[[i]] <- simple_Rt(fit_list[[i]])
  # Rt associated with "opening" and updating the Rt data frames for each sample
  rt_prev <- lapply(rt_out_list[[i]], function(x){
    # filter to after July 2020
    x2 <- x %>% filter(date < as.Date("2022-01-01") & date > as.Date("2020-07-01"))
    rt_open <- quantile(x2$Rt, prob = c(0.95), na.rm=TRUE)
    rt_open
  })

  rt_list[[i]] <- data.frame(rep = seq_along(unlist(rt_prev)), rt_prev = unlist(rt_prev), iso3c = i)

}

for(i in (iso3cs)) {

  # add Rt during the dates with lowest NPIs
  if( i %in% iso3cs) {
    # Rt associated with "opening" and update the Rt data frames for each sample
    rt_new <- lapply(rt_out_list[[i]], function(x){
      # filter to after July 2020
      x2 <- x %>%
        complete(date = seq.Date(min(date), max(date), 1)) %>%
        fill(Rt:rep, .direction = "down") %>%
      filter(date %in% as.Date((npi_dates$date[which(npi_dates$min_dates[npi_dates$country_code == i])])))
      rt_open <- quantile(x2$Rt, prob = c(0.95), na.rm=TRUE)
      rt_open
    })

  rt_list[[i]]$rt_new <- unlist(rt_new)
  } else {
    rt_list[[i]]$rt_date <- NA
  }
}


rt_df <- do.call(rbind, rt_list)
rt_df %>%
  pivot_longer(cols = c(rt_prev, rt_new)) %>%
  group_by(iso3c, name) %>%
  summarise(
    low = quantile(value, 0.025, na.rm = TRUE),
    med = median(value, na.rm = TRUE),
    high = quantile(value, 0.975, na.rm = TRUE)
  ) %>%
  pivot_wider(values_from = low:high) %>%
  ggplot(aes(med_rt_new, med_rt_prev)) +
  geom_point() +
  geom_abline(slope = 1) +
  xlab("Rt based on minimum OxGRT Stringency") +
  ylab("Rt based on 95% quantile (current approach)") +
  xlim(c(1,9)) + ylim(c(1,9)) +
  theme_bw()

