ves_by_type <- read_csv("vaccine_efficacy_groups.csv") %>%
  mutate(dose = if_else(dose == "Partial", "First", "Second"))
#assumed numbers, booster restores efficacy NOTE Ideally we'd find better numbers, might be too many categories
master_ves <- tribble(
  ~Variant, ~Dose, ~Endpoint, ~Efficacy,
  "Wild", "First", "Infection", 0.6,
  "Wild", "Second", "Infection", 0.8,
  "Wild", "Booster", "Infection", 0.85,
  "Wild", "First", "Hospitalisation", 0.8,
  "Wild", "Second", "Hospitalisation", 0.98,
  "Wild", "Booster", "Hospitalisation", 0.98,
  "Delta", "First", "Infection", 0.224,
  "Delta", "Second", "Infection", 0.646,
  "Delta", "Booster", "Infection", 0.85,
  "Delta", "First", "Hospitalisation", 0.75,
  "Delta", "Second", "Hospitalisation", 0.94,
  "Delta", "Booster", "Hospitalisation", 0.98,
  "Omicron", "First", "Infection", 0,
  "Omicron", "Second", "Infection", 0.1,
  "Omicron", "Booster", "Infection", 0.55,
  "Omicron", "First", "Hospitalisation", 0.1557433,
  "Omicron", "Second", "Hospitalisation", 0.449152542372881,
  "Omicron", "Booster", "Hospitalisation", 0.90
)

#calculate omicron changes based on the changes from delta in the old generalised ves
logit <- function(x){
  log(x/(1-x))
}
inv_logit <- function(x){
  1/(1+exp(-x))
}
#assume that boosters are either mRNA or whol virus
changes_booster <- master_ves %>%
  pivot_wider(names_from = Dose, values_from = Efficacy) %>%
  transmute(
    variant = Variant,
    endpoint = Endpoint,
    p_change = logit(Booster) - logit(Second)
  )
booster_efficacies <- ves_by_type %>%
  filter(dose == "Second" & vaccine_type %in% c("mRNA", "Adenovirus")) %>%
  left_join(
    changes_booster,
    by = c("variant", "endpoint")
  ) %>%
  mutate(
    dose = "Booster",
    efficacy = inv_logit(logit(efficacy) + p_change)
  ) %>%
  select(!p_change)
ves_by_type <- ves_by_type %>%
  rbind(
    booster_efficacies
  )
changes_omicron <- master_ves %>%
  filter(Variant != "Wild") %>%
  pivot_wider(names_from = Variant, values_from = Efficacy) %>%
  transmute(
    dose = Dose,
    endpoint = Endpoint,
    p_change = Omicron/Delta,
  )
omicron_efficacies <- ves_by_type %>%
  filter(variant == "Delta") %>%
  left_join(
    changes_omicron,
    by = c("dose", "endpoint")
  ) %>%
  mutate(
    variant = "Omicron",
    efficacy = efficacy * p_change
  ) %>%
  select(!p_change)

ves_by_type <- ves_by_type %>%
  rbind(
    omicron_efficacies
  ) %>%
  arrange(vaccine_type, variant, endpoint, dose)

#overwrite with omicron data for boosters where possible
ves_by_type <- ves_by_type %>%
  mutate(
    efficacy = case_when(
      vaccine_type == "mRNA" & dose == "booster" & endpoint == "Infection" & variant == "Omicron" ~
        0.65, #https://www.nejm.org/doi/full/10.1056/NEJMoa2119451
      vaccine_type == "mRNA" & dose == "booster" & endpoint == "Hospitalisation" & variant == "Omicron" ~
        0.82, #https://www.sciencedirect.com/science/article/pii/S0264410X22005230
      vaccine_type == "Adenovirus" & dose == "booster" & endpoint == "Infection" & variant == "Delta" ~
        93, #seems way too high https://www.researchsquare.com/article/rs-1792139/v1
      vaccine_type == "Adenovirus" & dose == "booster" & endpoint == "Infection" & variant == "Omicron" ~
        27, #not neccesarily all AZ doses https://www.researchsquare.com/article/rs-1792139/v1 + preprint
      vaccine_type == "Adenovirus" & dose == "booster" & endpoint == "Hospitalisation" & variant == "Omicron" ~
        84, #seems too high https://www.researchsquare.com/article/rs-2015733/v1
      TRUE ~ efficacy
    )
  )


#limit to just mRNA + AZ (Adenovirus)
ves_by_type <- ves_by_type %>%
  filter(vaccine_type %in% c("mRNA", "Adenovirus"))

##Fit Waning Curves
simulate_time <- 2*365
calc_eff_primary_gen <- odin({
  initial(C1) <- 1
  initial(C2) <- 0
  deriv(C1) <- -w_p*C1
  deriv(C2) <- w_p*C1
  output(ve_d) <- C1 * fV_1_d + C2 * fV_2_d
  output(ve_i) <- C1 * fV_1_i + C2 * fV_2_i
  w_p <- user()
  fV_1_d <- user()
  fV_2_d <- user()
  fV_1_i <- user()
  fV_2_i <- user()
})
calc_eff_booster_gen <- odin({
  initial(C1) <- 1
  initial(C2) <- 0
  initial(C3) <- 0
  deriv(C1) <- -w_1*C1
  deriv(C2) <- w_1*C1 - w_2*C2
  deriv(C3) <- w_2*C2
  output(ve_d) <- C1 * bV_1_d + C2 * bV_2_d + C3 * bV_3_d
  output(ve_i) <- C1 * bV_1_i + C2 * bV_2_i + C3 * bV_3_i
  w_1 <- user()
  w_2 <- user()
  bV_1_d <- user()
  bV_2_d <- user()
  bV_3_d <- user()
  bV_1_i <- user()
  bV_2_i <- user()
  bV_3_i <- user()
})
fit_curve <- function(df) {
  parameter_infection <- df %>% filter(endpoint == "Infection") %>% pull(efficacy)
  parameter_hospitalisation <- df %>% filter(endpoint == "Hospitalisation") %>% pull(efficacy)
  dose <- unique(df$dose)
  variant <- unique(df$variant)
  platform <- unique(df$vaccine_type)

  message(paste0(dose, "; ", variant, "; ", platform))

  if(dose == "First"){
    out <- tibble(
      parameter = c("pV_1_i", "pV_1_d"),
      value = c(parameter_infection, parameter_hospitalisation),
      dose = dose,
      variant = variant,
      platform = platform
    )
    p <- NULL
  } else {
    #median values for now
    k <- 2.9
    h_s <- 69
    h_l <- 431
    t_s <- 95
    t_l <- 365
    #get scaling
    n_50_d <- 0.027
    n_50_i <- 0.113
    #calculate initial AB value
    ab_d <- c(10^((-log((1/parameter_hospitalisation) - 1)/k) + log10(n_50_d)),
              rep(NA, simulate_time))
    ab_i <- c(10^((-log((1/parameter_infection) - 1)/k) + log10(n_50_i)),
              rep(NA, simulate_time))
    #compute AB curve
    for(t in seq_len(simulate_time)){
      #get deacy rate
      if(t < t_s){
        decay <- 1/h_s
      } else if(t < t_l){
        along <- ((t - t_s)/(t_l - t_s))
        decay <- (1/h_s)*(1-along) + (1/h_l) * along
      } else {
        decay <- 1/h_l
      }
      #update ab
      ab_d[t + 1] <- ab_d[t] * exp(-decay)
      ab_i[t + 1] <- ab_i[t] * exp(-decay)
    }
    #convert to efficacy curve
    ve_d <- 1/(1 + exp(-k*(log10(ab_d) - log10(n_50_d))))
    ve_i <- 1/(1 + exp(-k*(log10(ab_i) - log10(n_50_i))))

    if(dose == "Second") {
      calc_eff_primary <- calc_eff_primary_gen$new(user = list(
        fV_1_d = parameter_hospitalisation,
        fV_2_d = 0,
        fV_1_i = parameter_infection,
        fV_2_i = 0,
        w_p = 0
      ))
      err_func <- function(pars) {
        #detect if ve is 0
        if(length(pars) < 3){
          pars <- c(pars, 0)
        }

        calc_eff_primary$set_user(
          user = list(
            fV_1_d =  parameter_hospitalisation,
            fV_1_i = parameter_infection,
            w_p = pars[1],
            fV_2_d = pars[2],
            fV_2_i = pars[3]
          )
        )
        mod_value <- calc_eff_primary$run(t = c(0, seq_len(simulate_time)))
        (sum((ve_d - mod_value[, "ve_d"])^2) + sum((ve_i - mod_value[, "ve_i"])^2)) %>% sqrt %>% log
      }
      lower = list(w_p = 1/(3*365), fV_2_d = 0.001, fV_2_i = 0)
      upper = list(w_p = 1/30, fV_2_d = parameter_hospitalisation, fV_2_i = parameter_infection)
      par = list(w_p = 1/365, fV_2_d = parameter_hospitalisation/2, fV_2_i = parameter_infection/2)
      if(parameter_infection == 0){
        lower$fV_2_i <- NULL
        upper$fV_2_i <- NULL
        par$fV_2_i <- NULL
      }
      if(
        (variant == "Wild" & platform %in% c("Adenovirus")) |
        (variant == "Omicron" & platform == "Adenovirus") |
        (variant == "Delta" & platform %in% c("mRNA"))){
        lower$fV_2_i <- 0.0001
      }
    } else {
      calc_eff_booster <- calc_eff_booster_gen$new(user = list(
        bV_1_d = parameter_hospitalisation,
        bV_2_d = 0,
        bV_3_d = 0,
        bV_1_i = parameter_infection,
        bV_2_i = 0,
        bV_3_i = 0,
        w_1 = 0,
        w_2 = 0
      ))
      err_func <- function(pars) {
        #detect if ve is 0
        if(length(pars) < 6){
          pars <- c(pars, rep(0, 2))
        }
        calc_eff_booster$set_user(
          user = list(
            w_1 = pars[1],
            w_2 = pars[2],
            bV_1_d = parameter_hospitalisation,
            bV_1_i = parameter_infection,
            bV_2_d = pars[3],
            bV_3_d = pars[4],
            bV_2_i = pars[5],
            bV_3_i = pars[6]
          )
        )
        mod_value <- calc_eff_booster$run(t = c(0, seq_len(simulate_time)))
        (sum((ve_d - mod_value[, "ve_d"])^2) + sum((ve_i - mod_value[, "ve_i"])^2)) %>%
          sqrt %>% log

      }
      lower = list(
        w_1 = 1/(3*365),
        w_2 = 1/(3*365),
        bV_2_d = 0,
        bV_3_d = 0,
        bV_2_i = 0,
        bV_3_i = 0
      )
      upper = list(
        w_1 = 1/30,
        w_2 = 1/30,
        bV_2_d = parameter_hospitalisation,
        bV_3_d = parameter_hospitalisation,
        bV_2_i = parameter_infection,
        bV_3_i = parameter_infection
      )
      par =  list(
        w_1 = 1/365,
        w_2 = 1/365,
        bV_2_d = parameter_hospitalisation/2,
        bV_3_d = parameter_hospitalisation/2,
        bV_2_i = parameter_infection/2,
        bV_3_i = parameter_infection/2
      )
      if(parameter_infection == 0){
        lower$bV_2_i <- NULL
        upper$bV_2_i <- NULL
        par$bV_2_i <- NULL
        lower$bV_3_i <- NULL
        upper$bV_3_i <- NULL
        par$bV_3_i <- NULL
      }
      if((variant %in% c("Wild") & platform == "mRNA") |
         (variant == "Wild" & platform == "Adenovirus")){
        lower$bV_3_i <- 0.00001
      }
    }
    res <- optim(unlist(par), fn = err_func, method = "L-BFGS-B", lower = lower, upper = upper)
    if(res$convergence != 0){
      stop(res$message)
    }
    if(parameter_infection == 0){
      if(dose == "Second"){
        res$par[3] <- 0
      } else {
        res$par[6] <- 0
      }
    }
    if(dose == "Booster"){
      #ensure is decreasing
      if(res$par[4] > res$par[3]){
        res$par[4] <- res$par[3]
      }
      if(res$par[6] > res$par[5]){
        res$par[6] <- res$par[5]
      }
    }
    #make plot
    if(dose == "Second"){
      calc_eff_primary$set_user(
        user = list(
          w_p = res$par[1],
          fV_1_d =  parameter_hospitalisation,
          fV_1_i = parameter_infection,
          fV_2_d = res$par[2],
          fV_2_i = res$par[3]
        )
      )
      mod_value <- calc_eff_primary$run(t = c(0, seq_len(simulate_time)))
      ve_f_d <- mod_value[, "ve_d"]
      ve_f_i <- mod_value[, "ve_i"]
    } else {
      calc_eff_booster$set_user(
        user = list(
          w_1 = res$par[1],
          w_2 = res$par[2],
          bV_1_d = parameter_hospitalisation,
          bV_1_i = parameter_infection,
          bV_2_d = res$par[3],
          bV_3_d = res$par[4],
          bV_2_i = res$par[5],
          bV_3_i = res$par[6]
        )
      )
      mod_value <- calc_eff_booster$run(t = c(0, seq_len(simulate_time)))
      ve_f_d <- mod_value[, "ve_d"]
      ve_f_i <- mod_value[, "ve_i"]
    }
    p <- ggplot(
      tibble(
        t = rep(c(0, seq_len(simulate_time)), 4),
        `Protection:` = c(rep("Disease", (simulate_time + 1)*2), rep("Infection", (simulate_time + 1)*2)),
        `Version:` = rep(c(rep("AB Process", simulate_time + 1), rep("Booster Model", simulate_time + 1)), 2),
        `Vaccine Efficacy` = c(ve_d, ve_f_d, ve_i, ve_f_i)
      )
      , aes(x = t, y = `Vaccine Efficacy`, colour = `Protection:`, linetype = `Version:`)
    ) +
      geom_line() +
      labs(y = "Vaccine Efficacy", x = "Days Since Dose", title = paste0("Type: ", df$vaccine_type[1])) +
      ggpubr::theme_pubclean() +
      coord_cartesian(ylim = c(0, 1))

    out <- as.data.frame(res$par)
    out <- mutate(out, parameter = rownames(out),
                  parameter = if_else(parameter == "", "fV_2_i", parameter)) %>%
      rename(value = `res$par`) %>%
      rbind(tibble(
        value = c(parameter_hospitalisation, parameter_infection),
        parameter = if_else(rep(dose == "Booster", 2), c("bV_1_d", "bV_1_i"), c("fV_1_d", "fV_1_i"))
      ))
    rownames(out) <- NULL
    out$dose <- dose
    out$variant <- variant
    out$platform <- platform
  }
  #return parameters
  return(list(out, p))
}
set.seed(1000100001)
values <-
  ves_by_type %>%
  group_by(vaccine_type, dose, variant) %>%
  group_split() %>%
  map(fit_curve)

#split into plots and data
plots <- map(values, ~.x[[2]])
efficacies <- map(values, ~.x[[1]])

#calibration plot
types <- map_chr(efficacies, ~if_else(.x$parameter[1] == "pV_1_i", "", paste0("Dose: ", .x$dose[1], ", Variant: ", .x$variant[1])))
pdf("calibration_plot.pdf")
map(unique(types), function(type){
  if (type != ""){
    ggpubr::ggarrange(plotlist = plots[types == type], common.legend = TRUE) %>%
      ggpubr::annotate_figure(top = ggpubr::text_grob(type))
  }
})
dev.off()

#save profiles
map_dfr(efficacies, ~.x) %>%
  select(!dose) %>%
  split(~platform) %>%
  map(~select(.x, !platform)) %>%
  saveRDS("vaccine_profiles.Rds")
