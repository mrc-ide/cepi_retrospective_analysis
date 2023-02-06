# gathers fit from github
grab_fit <- function(iso3c, excess_mortality){
    if (excess_mortality) {
        path <- paste0("https://github.com/mrc-ide/covid-vaccine-impact-orderly/raw/main/data/excess_mortality/model_fits/", iso3c, ".Rds")
    } else {
        path <- paste0("https://github.com/mrc-ide/covid-vaccine-impact-orderly/raw/main/data/reported_deaths/model_fits/", iso3c, ".Rds")
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
implement_scenarios <- function(fit, scenarios){
    pmap(scenarios, function(Rt, Vaccine, Variant, fit){
        #order matters since Rt can depend on vaccine coverage
        fit <- implement_vaccine(fit, Vaccine)
        fit <- implement_rt(fit, Rt)
        fit <- implement_variant(fit, Variant)
        fit
    }, fit)
}
implement_rt <- function(fit, Rt){
    if (Rt == "baseline") {
        fit
    } else if (Rt == "target") {

    } else if (Rt == "economic")
}
implement_vaccine <- function(fit, Vaccine){
    cepi_start_date <- as.Date("2020-04-20")
    real_start_date <- as.Date("2020-12-08")
    difference <- as.numeric(real_start_date - cepi_start_date)

    if (Vaccine %in% c("early", "equity")) {
        #staggered (but earlier) rollout
        fit$interventions$date_vaccine_change <-
            fit$interventions$date_vaccine_change - difference
        fit$interventions$date_vaccine_efficacy <-
            fit$interventions$date_vaccine_efficacy - difference
        fit$pmcmc_results$inputs$interventions$date_vaccine_change <-
            fit$pmcmc_results$inputs$interventions$date_vaccine_change -
                difference
        fit$pmcmc_results$inputs$interventions$date_vaccine_efficacy <-
            fit$pmcmc_results$inputs$interventions$date_vaccine_efficacy -
                difference
        if (vaccine == "equity") {
            #update final coverage to 40% by the end of the first year of vaccinations
            #update rollout speed to be much quicker
        }
    } else if (vaccine == "manufacturing") {
        #update to AZ or mRNA efficacy
        #vaccination campaigns begin at the same time
        #final coverage remains the same as before
    }

    #generate a plot to show the differences between these



    #currently no forwards adjustments to dose ratio or rates (we can take this from the real data?)
    #also need to make adjustments where doses will occur before the model begins (need to recheck how the initial states work)
}
implement_variant <- function(fit, Variant){
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