set.seed(1000101)

#setup colours
library(scales)
colours <- hue_pal()(3)
names(colours) <- c("Hospitalisation", "Hospitalisation\n(Scaled for Breakthrough)", "Infection")
##Set-up VEs for Vaccine Types
ves_by_type <- read_csv("vaccine_efficacy_groups.csv") %>%
  mutate(dose = if_else(dose == "Partial", "First", "Second")) %>%
  #ensure ve does not increase from Wild to Delta
  pivot_wider(names_from = variant, values_from = efficacy) %>%
  mutate(
    Delta = pmin(Delta, Wild)
  ) %>%
  pivot_longer(
    cols = c("Wild", "Delta"),
    names_to = "variant",
    values_to = "efficacy"
  )


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
  "Delta", "Booster", "Hospitalisation", 0.98
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
  ) %>% #average across variants for simplicities sake
  group_by(endpoint)  %>%
  summarise(p_change = mean(p_change))

booster_efficacies <- ves_by_type %>%
  filter(dose == "Second" & vaccine_type %in% c("mRNA", "Adenovirus")) %>%
  left_join(
    changes_booster,
    by = c("endpoint")
  ) %>%
  mutate(
    dose = "Booster",
    efficacy = inv_logit(logit(efficacy) + p_change)
  ) %>%
  select(!p_change)
ves_by_type <- ves_by_type %>%
  rbind(
    booster_efficacies
  ) %>%
  arrange(vaccine_type, variant, endpoint, dose)


#limit to just mRNA + AZ (Adenovirus)
ves_by_type <- ves_by_type %>%
  filter(vaccine_type %in% c("mRNA", "Adenovirus"))

#ves_by_type %>%
#  pivot_wider(names_from = variant, values_from = efficacy) %>%
#  select(vaccine_type, endpoint, dose, Wild, Delta, Omicron) %>%
#  filter(vaccine_type == "mRNA")
#load parameter chains
ab_params <- list(
  ni50 = -1.040957,
  ns50 = -1.675559,
  k = 3.182298,
  hl_s = 34.53602,
  hl_l = 573.5029,
  period_s = 75.1927
)

##Fit Waning Curves
simulate_time <- 2*365
calc_eff_gen <- odin({
  initial(C1) <- 1
  initial(C2) <- 0
  initial(C3) <- 0
  deriv(C1) <- -w_1*C1
  deriv(C2) <- w_1*C1 - w_2*C2
  deriv(C3) <- w_2*C2
  output(ve_d) <- (C1 * ved + C2 * ved_2 + C3 * ved_3)
  output(ve_i) <- (C1 * vei + C2 * vei_2 + C3 * vei_3)
  w_1 <- user()
  w_2 <- user()
  ved <- user()
  ved_2 <- user()
  ved_3 <- user()
  vei <- user()
  vei_2 <- user()
  vei_3 <- user()
})

simulate_ab <- function(t, initial_ab, h_s, h_l, t_s) {
  pi1 <- -log(2)/h_s
  pi2 <- -log(2)/h_l
  initial_ab * (
    (exp(pi1 * t + pi2 * t_s) + exp(pi1 * t_s + pi2 * t))/
    (exp(pi1 * t_s) + exp(pi2 * t_s))
  )
}

ab_to_ve <- function(ab, n50, k){
  1/(1 + exp(-k * (log10(ab) - n50)))
}

err_lines <- function(l1, l2){
  #sum((l1 - l2)^2/l1)
  sum(((l1 - l2)/l1)^2)
  #scale it so the lower values have more weight
}

calculate_ve <- function(p1, p2, p3) {
  ved <- p1
  ved_2 <- ved * p2
  ved_3 <- ved_2 * p3
  c(ved, ved_2, ved_3)
}

first_doses <- ves_by_type %>%
  filter(dose == "First") %>%
  group_by(vaccine_type, dose, variant) %>%
  group_split() %>%
  map(function(df){
    parameter_infection <- df %>% filter(endpoint == "Infection") %>% pull(efficacy)
    parameter_hospitalisation <- df %>% filter(endpoint == "Hospitalisation") %>% pull(efficacy)
    dose <- unique(df$dose)
    variant <- unique(df$variant)
    platform <- unique(df$vaccine_type)
    tibble(
      parameter = c("pV_i", "pV_d"),
      value = c(parameter_infection, (parameter_hospitalisation - parameter_infection)/(1 - parameter_infection)),
      dose = dose,
      variant = variant,
      platform = platform
    )
  })

other_doses <- ves_by_type %>%
  filter(dose != "First") %>%
  group_by(vaccine_type, dose, variant) %>%
  group_split() %>%
  map(function(df){
    parameter_infection <- df %>% filter(endpoint == "Infection") %>% pull(efficacy)
    parameter_hospitalisation <- df %>% filter(endpoint == "Hospitalisation") %>% pull(efficacy)

    dose <- unique(df$dose)
    variant <- unique(df$variant)
    platform <- unique(df$vaccine_type)

    message(paste0(dose, "; ", variant, "; ", platform))

    t_plot <- seq(0, simulate_time, length.out = 100)

    calc_eff <- calc_eff_gen$new(user = list(
      ved = parameter_hospitalisation,
      vei = parameter_infection,
      ved_2 = 1,
      ved_3 = 1,
      vei_2 = 1,
      vei_3 = 1,
      w_1 = 0,
      w_2 = 0
    ))

    #need to calculate initial dose level
    #assume ve is 30 days after dose
    t_measure <- 30
    #just assume the initia AB is
    err_func <- function(initial_ab) {
      ab_t_measure <- simulate_ab(t_measure, initial_ab, ab_params$hl_s, ab_params$hl_l, ab_params$period_s)
      ve_i <- ab_to_ve(ab_t_measure, ab_params$ni50, ab_params$k)
      ve_d <- ab_to_ve(ab_t_measure, ab_params$ns50, ab_params$k)
      err_lines(parameter_infection, ve_i) +
        err_lines(parameter_hospitalisation, ve_d)
    }
    res <- optimize(err_func, interval = c(0, 10), maximum = FALSE)
    initial_ab <- res$minimum

    if(platform == "mRNA" & variant == "Wild" & dose == "Second"){
      example_ab <<- initial_ab
    }

    abs <- simulate_ab(0:simulate_time, initial_ab,  ab_params$hl_s,  ab_params$hl_l,  ab_params$period_s)
    ve_i <- ab_to_ve(abs, ab_params$ni50, ab_params$k)
    ve_d <- ab_to_ve(abs, ab_params$ns50, ab_params$k)
    #plot for diagnostics
    initial_ab_plot <- tibble(
      t = 0:(2*t_measure),
      Hospitalisation = ve_d[1:(2*t_measure + 1)],
      Infection = ve_i[1:(2*t_measure + 1)]
    ) %>% 
      pivot_longer(cols = -t, names_to = "Endpoint", values_to = "Efficacy") %>%
      mutate(
        Endpoint = factor(Endpoint, levels = c("Infection", "Hospitalisation", "Hospitalisation\n(Scaled for Breakthrough)"))
      ) %>%
      ggplot(aes(x = t, y = Efficacy, colour = Endpoint)) + 
      geom_line(show.legend = FALSE) +
      geom_point(data = tibble(
        t = t_measure,
        Efficacy = c(parameter_infection, parameter_hospitalisation),
        Endpoint = c("Infection", "Hospitalisation")
      ) %>%
        mutate(
          Endpoint = factor(Endpoint, levels = c("Infection", "Hospitalisation", "Hospitalisation\n(Scaled for Breakthrough)"))
        ), shape = "x", size = 5, show.legend = FALSE) +
      scale_colour_manual(values = colours) +
      ggpubr::theme_pubclean() +
      labs(
        title = "Fit of initial Immune Response level, X shows the input efficacies",
        y = "Vaccine Efficacy", x = "Days Since Dose"
      )

    #scale for break through infection
    ve_d <- (ve_d - ve_i)/(1 - ve_i)

    err_func <- function(pars) {
      veds <- calculate_ve(pars[3], pars[4], pars[5])
      veis <- calculate_ve(pars[6], pars[7], pars[8])
      calc_eff$set_user(
        user = list(
          w_1 = pars[1],
          w_2 = pars[2],
          ved = veds[1],
          ved_2 = veds[2],
          ved_3 = veds[3],
          vei = veis[1],
          vei_2 = veis[2],
          vei_3 = veis[3]
        )
      )
      mod_value <- calc_eff$run(t = seq(0, simulate_time))
      log(sqrt(err_lines(ve_d, mod_value[, "ve_d"]) + err_lines(ve_i, mod_value[, "ve_i"])))
    }
    lower = list(
      w_1 = 1/(3*365),
      w_2 = 1/(3*365),
      ved_1 = 0,
      ved_2 = 0,
      ved_3 = 0,
      vei_1 = 0,
      vei_2 = 0,
      vei_3 = 0
    )
    upper = list(
      w_1 = 1/30,
      w_2 = 1/30,
      ved_1 = 1,
      ved_2 = 1,
      ved_3 = 1,
      vei_1 = 1,
      vei_2 = 1,
      vei_3 = 1
    )
    par =  list(
      w_1 = 1/365,
      w_2 = 1/365,
      ved_1 = 0.5,
      ved_2 = 0.5,
      ved_3 = 0.5,
      vei_1 = 0.5,
      vei_2 = 0.5,
      vei_3 = 0.5
    )
    res <- dfoptim::nmkb(unlist(par), fn = err_func, lower = unlist(lower), upper = unlist(upper), control = list(maxfeval = 5000))
    if(res$convergence != 0){
      stop(res$message)
    }

    #add randomness (should do it in fitting really)
    out <- map(1, function(i){
      pars <- res$par
      veds <- calculate_ve(pars[3], pars[4], pars[5])
      veis <- calculate_ve(pars[6], pars[7], pars[8])
      tibble(
        value = c(pars[1:2], veds, veis),
        parameter = names(par)
      )
    })[[1]]

    #plot
    t_plot <- 0:simulate_time
    calc_eff$set_user(
        user = list(
          w_1 = out$value[1],
          w_2 = out$value[2],
          ved = out$value[3],
          ved_2 = out$value[4],
          ved_3 = out$value[5],
          vei = out$value[6],
          vei_2 = out$value[7],
          vei_3 = out$value[8]
        )
      )
    mod_value <- calc_eff$run(t = t_plot)
    p <- tibble(
        t = rep(t_plot, 2),
        value = c(mod_value[, "ve_d"], mod_value[, "ve_i"]),
        Endpoint = c(rep("Hospitalisation\n(Scaled for Breakthrough)", length(t_plot)), rep("Infection", length(t_plot)))
    ) %>%
      mutate(
        model = "Booster Model"
      ) %>%
      rbind(
        tibble(
          t = t_plot,
          value = ve_d,
          Endpoint = "Hospitalisation\n(Scaled for Breakthrough)",
          model = "AB Process"
        )
      ) %>%
      rbind(
        tibble(
          t = t_plot,
          value = ve_i,
          Endpoint = "Infection",
          model = "AB Process"
        )
      ) %>%
      mutate(
        Endpoint = factor(Endpoint, levels = c("Infection", "Hospitalisation", "Hospitalisation\n(Scaled for Breakthrough)"))
      ) %>%
      ggplot(aes(x = t, y = value, color = Endpoint, linetype = model)) +
        geom_line() +
      labs(y = "Vaccine Efficacy", x = "Days Since Dose", title = paste0("Dose: ", dose, ", Variant: ", variant, ", Type: ", df$vaccine_type[1]), linetype = "Model", colour = "Endpoint") +
      ggpubr::theme_pubclean() + scale_alpha(guide = 'none') +
      scale_colour_manual(values = colours, drop = FALSE) +
      ylim(c(0, 1))

    out$dose <- dose
    out$variant <- variant
    out$platform <- platform

    list(
      out = out,
      plot = p,
      ab_plot = initial_ab_plot
    )
})
#split into plots and data
plots <- map(other_doses, ~list(plot = .x$plot, ab_plot = .x$ab_plot))
other_doses <- map(other_doses, ~.x$out)

platform <- map_chr(other_doses, ~.x$platform[1])
platforms <- unique(platform)
variant <- map_chr(other_doses, ~.x$variant[1])
variants <- c("Wild", "Delta")
dose <- map_chr(other_doses, ~.x$dose[1])
doses <- unique(dose)

#prep for example plot

example_plots <- plots[platform == "mRNA" & variant == "Wild" & dose == "Second"][[1]]

##Calibration plots
plots <- map(plots, ~ggarrange(.x$plot, .x$ab_plot, ncol = 1, heights = c(0.6, 0.2)))
p <- map(platforms, function(plat){
  dose_list <- map(doses, function(dos){
    var_list <- map(variants, function(vari){
      index <- detect_index(other_doses, ~.x$platform[1] == plat & .x$variant[1] == vari & .x$dose[1] == dos)
      if(index > 0){
        plots[[index]]
      }
    })
    if(every(var_list, is.null)){
      return(NULL)
    } else {
      return(ggarrange(plotlist = var_list, nrow = 1, common.legend = TRUE))
    }
  }) %>%
    compact()
  ggarrange(plotlist = dose_list, ncol = 1, common.legend = TRUE)
})

dir.create("plots", showWarnings = FALSE)
iwalk(p, \(x, idx) ggsave(paste0("plots/", idx, ".png"), x, width = 20, height = 20))

rm(p, plots)

first_doses <- map_dfr(first_doses, ~.x)
other_doses <- map_dfr(other_doses, ~.x)

efficacies <- other_doses %>%
  mutate(
    parameter = case_when(
      parameter == "w_1" & dose == "Second" ~ "fw_1",
      parameter == "w_2" & dose == "Second" ~ "fw_2",
      parameter == "ved_1" & dose == "Second" ~ "fV_d_1",
      parameter == "ved_2" & dose == "Second" ~ "fV_d_2",
      parameter == "ved_3" & dose == "Second" ~ "fV_d_3",
      parameter == "vei_1" & dose == "Second" ~ "fV_i_1",
      parameter == "vei_2" & dose == "Second" ~ "fV_i_2",
      parameter == "vei_3" & dose == "Second" ~ "fV_i_3",
      parameter == "w_1" & dose == "Booster" ~ "bw_1",
      parameter == "w_2" & dose == "Booster" ~ "bw_2",
      parameter == "ved_1" & dose == "Booster" ~ "bV_d_1",
      parameter == "ved_2" & dose == "Booster" ~ "bV_d_2",
      parameter == "ved_3" & dose == "Booster" ~ "bV_d_3",
      parameter == "vei_1" & dose == "Booster" ~ "bV_i_1",
      parameter == "vei_2" & dose == "Booster" ~ "bV_i_2",
      parameter == "vei_3" & dose == "Booster" ~ "bV_i_3",
      TRUE ~ parameter
    )
  ) %>% 
  rbind(
    first_doses
  )

#save profiles
efficacies %>%
  select(!dose) %>%
  split(~platform) %>%
  map(~select(.x, !platform)) %>%
  saveRDS("vaccine_profiles.Rds")

#example plot
load("/home/gregbarnsley/Documents/imperial/cepi/cepi_retrospective_analysis/analysis/data_raw/mcmc_chain.Rdata", verbose = TRUE)
n_samples <- 1000
mcmc <- mcmc$output[mcmc$output$phase == "sampling",]
n_its <- nrow(mcmc)
mcmc <- mcmc[round(seq(1, n_its, length.out = n_samples)),]
mcmc <- mcmc %>%
  select(ni50, ns50, nd50, k, hl_s, hl_l, period_s, period_l)

ab_with_stochastic <- pmap_dfr(
  mcmc,
  function(ni50, ns50, nd50, k, hl_s, hl_l, period_s, period_l) {
    ab_t_measure <- simulate_ab(0:simulate_time, example_ab, hl_s, hl_l, period_s)
    ve_i <- ab_to_ve(ab_t_measure, ni50, k)
    ve_d <- ab_to_ve(ab_t_measure, ns50, k)
    ve_d <- (ve_d - ve_i)/(1 - ve_i)
    tibble(
      t = rep(0:simulate_time, 2),
      ve = c(ve_i, ve_d),
      Endpoint = c(rep("Infection", simulate_time + 1), rep("Hospitalisation\n(Scaled for Breakthrough)", simulate_time + 1))
    )
  }, .id = "iteration"
) %>%
  mutate(
    Endpoint = factor(Endpoint, levels = c("Infection", "Hospitalisation", "Hospitalisation\n(Scaled for Breakthrough)"))
  ) %>%
  group_by(t, Endpoint) %>%
  summarise(
    median = median(ve),
    lower = quantile(ve, 0.025),
    upper = quantile(ve, 0.975),
    .groups = "drop"
  )

#simulate the noise from the fitting VE fits
add_noise <- function(x) {
  N_samples <- 50
  main_pars <- x
  map_dfr(seq_len(N_samples), function(i){
    pars <- main_pars
      pars[1:2] <- 1/pars[1:2]
      #convert pars to props
      pars[5] <- pars[5]/pars[4]
      pars[4] <- pars[4]/pars[3]
      pars[8] <- pars[8]/pars[7]
      pars[7] <- pars[7]/pars[6]
      #add noise
      pars <- pars + runif(length(pars), -0.05, 0.05)
      pars[1:2] <- 1/pars[1:2]
      pars[pars < 0] <- 0
      pars[pars > 1] <- 1
    
      veds <- calculate_ve(pars[3], pars[4], pars[5])
      veis <- calculate_ve(pars[6], pars[7], pars[8])
      tibble(
        value = c(pars[1:2], veds, veis),
        parameter = names(pars),
        iteration = i
      )
    })
}

random_efficacies <- efficacies  %>% 
  filter(dose == "Second" & variant == "Wild" & platform == "mRNA") %>%
  pull(value, parameter) %>%
  add_noise() %>% 
  group_by(iteration) %>% 
  group_split() %>% 
  map(~pull(.x, value, parameter)) %>% 
  map_dfr(
    function(x) {
      t_plot <- 0:simulate_time
      calc_eff <- calc_eff_gen$new(user = list(
        w_1 = x["fw_1"],
        w_2 = x["fw_2"],
        ved = x["fV_d_1"],
        ved_2 = x["fV_d_2"],
        ved_3 = x["fV_d_3"],
        vei = x["fV_i_1"],
        vei_2 = x["fV_i_2"],
        vei_3 = x["fV_i_3"]
      ))
      mod_value <- calc_eff$run(t = t_plot)
      tibble(
        t = mod_value[, "t"],
        `Hospitalisation\n(Scaled for Breakthrough)` = mod_value[, "ve_d"],
        Infection = mod_value[, "ve_i"]
      )
    }, .id = "sample"
  ) %>% 
  pivot_longer(
    c(`Hospitalisation\n(Scaled for Breakthrough)`, Infection), names_to = "Endpoint", values_to = "ve"
  ) %>%
  mutate(
    Endpoint = factor(Endpoint, levels = c("Infection", "Hospitalisation", "Hospitalisation\n(Scaled for Breakthrough)"))
  ) %>%
  mutate(
    sample = paste0(sample, ":", Endpoint)
  )

stoch_plot <- ggplot(ab_with_stochastic, aes(x = t, y = median, colour = Endpoint, fill = Endpoint)) +
  geom_line(show.legend = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA, show.legend = FALSE) +
  labs(x = "Days Since Dose", y = "Vaccine Efficacy", title = "", colour = "Endpoint", fill = "Endpoint") +
  theme_pubclean() +
  scale_colour_manual(values = colours) +
  scale_fill_manual(values = colours) +
  geom_line(
    data = random_efficacies,
    aes(x = t, y = ve, colour = Endpoint, fill = Endpoint, group = sample),
    alpha = 0.2, linetype = "dashed", show.legend = FALSE
  ) +
  scale_y_continuous(labels = scales::percent)

#VE data
ves <- tribble(
  ~Endpoint, ~Efficacy, ~lower, ~upper,
  "Infection", 80.0,	56.0,	91.0,
  "Infection", 90.0,	83.0,	94.0,
  "Infection", 88.7,	68.4,	97.1,
  "Infection", 92.8,	92.6,	93.0,
  "Infection", 86.0,	72.0,	94.0,
  "Infection", 91.2,	88.8,	93.1,
  "Hospitalisation", 87.0,	55.0, 100.0
) %>%
  arrange(Endpoint, Efficacy) %>%
  mutate(
    Endpoint = factor(Endpoint, levels = c("Infection", "Hospitalisation", "Hospitalisation\n(Scaled for Breakthrough)")),
    Efficacy = Efficacy/100,
    lower = lower/100,
    upper = upper/100,
    t = 30 + seq(-2.5, 2.5, length.out = 8)[-1]#assume 30 days again?
  )

example_plot <- ggarrange(
  ggarrange(
    example_plots$ab_plot + labs(title = "") +
      scale_y_continuous(labels = scales::percent) +
      geom_point(data = ves, aes(x = t, y = Efficacy, colour = Endpoint), show.legend = FALSE) +
      geom_errorbar(data = ves, aes(x = t, ymin = lower, ymax = upper, colour = Endpoint), show.legend = FALSE),
    example_plots$plot + labs(title = "") +
      scale_y_continuous(labels = scales::percent),
    nrow = 1, common.legend = TRUE, labels = c("A", "B")
  ),
  stoch_plot, ncol = 1, labels = c("", "C")
)
ggsave("plots/example_plot.png", example_plot, width = 10, height = 10, bg = "white")

zip("plots.zip", file.path("plots", list.files("plots")))

unlink("plots", recursive = TRUE)
