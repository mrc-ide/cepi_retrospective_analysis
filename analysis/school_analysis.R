## ------------------------------------------------------------------
## Approach 1: Temporal association with our daily reported data fits
## ------------------------------------------------------------------

schools <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c1m_school_closing.csv")

pos1 <- which(names(schools) == "X01Jan2020")
pos2 <- ncol(schools)-1
schdf <- schools[,-1] %>%
  filter(is.na(region_name)) %>%
  pivot_longer(pos1:pos2) %>%
  mutate(date = as.Date(gsub("^X", "", name), format = "%d%b%Y")) %>%
  mutate(school = value) %>%
  mutate(iso3c = country_code) %>%
  select(iso3c, country_name,date, school)

open_dates <- schdf %>% group_by(country_name, iso3c) %>%
  filter(date > as.Date("2020-04-13")) %>% #date at which schools were most closed worldwide
  summarise(sch_date = date[school <= 1][1]) %>%
  as.data.frame() %>%
  mutate(Rt_before = NA,
         Rt_after = NA,
         Rt_max = NA,
         sch_eff = NA)

iso3 <- read.csv("https://github.com/mrc-ide/covid-vaccine-impact-orderly/releases/download/v1.0.1/dominant_vaccines.csv")$iso3
open_dates  <- open_dates %>% filter(iso3c %in% na.omit(iso3))

for(i in seq_along(open_dates$iso3c)) {

  # use the original model fits for these
  if(!is.na(open_dates$sch_date[i])) {

    fit <- tryCatch(
    grab_fit(open_dates$iso3c[i], FALSE, FALSE),
     error = function(e){NA}
    )

    if(!is.na(fit[1])) {
    rt <- rt <- squire.page::get_Rt(fit)
    rt <- rt %>% group_by(date) %>% summarise(Rt = median(Rt))


    Rt_after <- (rt %>% filter(date > open_dates$sch_date[i]) %>% pull(Rt))[1:60]
    Rt_before <- tail((rt %>% filter(date < open_dates$sch_date[i]) %>% pull(Rt)),30)
    Rt_max <- quantile(rt %>% filter(date < as.Date("2022-01-01") & date > as.Date("2020-07-01")) %>%
                 pull(Rt), prob = 0.95)

    open_dates$sch_eff[i] <- (mean(Rt_after,na.rm = TRUE) - mean(Rt_before, na.rm = TRUE))
    open_dates$Rt_before[i] <- list(Rt_before)
    open_dates$Rt_after[i] <- list(Rt_after)
    open_dates$Rt_max[i] <- Rt_max
    }
  }

}

open_dates$wb <- squire.page:::get_income_group(open_dates$iso3c)
open_dates <- na.omit(open_dates)

open_dates$rel_scheff <- unlist(lapply(seq_along(open_dates$country_name), function(x) {
 mean(open_dates$Rt_after[[x]][1:14]) / mean(tail(open_dates$Rt_before[[x]],14))
}))

open_dates %>% ggplot(aes(x = rel_scheff, fill = wb)) +
  geom_density(alpha = 0.3)

open_dates %>% group_by(wb) %>%
  summarise(sch_eff = median(rel_scheff, na.rm = TRUE))

## ------------------------------------------------------------------
## Approach 2: Contact matrix enrollment approach
## ------------------------------------------------------------------


# get schooling enrollment by income group
sch_enrollment <- read.csv(file.path(here::here(), "analysis/school_enrollment.csv"))
sch_enrollment$wb <- squire.page::get_income_group(sch_enrollment$iso3c)
wb_sch <- sch_enrollment %>% filter(grepl("(\\, primary)|(\\, secondary)", indicator))
wb_sch$type <- substr(wb_sch$indicator_id, 4, 6)

# calculate impact for an  iso3c
mm_sch_eff <- function(iso3c) {

  wb <- squire.page::get_income_group(iso3c)

  # default school closures by income group
  defaults <- wb_sch %>% group_by(type, wb) %>% summarise(value = median(value), .groups = 'drop')
  def_prm <- defaults$value[defaults$wb == wb & defaults$type == "PRM"]
  def_sec <- defaults$value[defaults$wb == wb & defaults$type == "SEC"]

  # get country specific valus
  prim <- wb_sch$value[wb_sch$iso3c == iso3c & wb_sch$type == "PRM"]
  seco <- wb_sch$value[wb_sch$iso3c == iso3c & wb_sch$type == "SEC"]

  # scale if missing
  if(length(prim) == 0 && length(seco) == 1) {
    prim <- seco * (def_prm/def_sec)
  }
  if(length(seco) == 0 && length(prim) == 1) {
    seco <- prim * (def_sec/def_prm)
  }
  # if still missing
  if(length(prim) == 0) {
    prim <- def_prm
    seco <- def_sec
  }

# mixing matrix
mm <- squire:::process_contact_matrix_scaled_age(
  squire::get_mixing_matrix(iso3c = iso3c),
  squire::get_population(iso3c = iso3c)$n
)

# eigen associated with country
eigen_sch_open <- squire:::adjusted_eigen(
  squire::default_durations()$dur_IMild,
  squire::default_durations()$dur_ICase,
  squire::default_probs()$prob_hosp,
  mm)

# reduce all contacts that these kids make based on enrollment
mm[2:3,2] <- mm[2:3,2] * (1-(prim/100))
mm[2:3,3] <- mm[2:3,3] * (1-(seco/100))

# eigen with schools closed
eigen_sch_closed <- squire:::adjusted_eigen(
  squire::default_durations()$dur_IMild,
  squire::default_durations()$dur_ICase,
  squire::default_probs()$prob_hosp,
  mm)

# relative difference is thus impact
school_closure_impact <- eigen_sch_open/eigen_sch_closed
school_closure_impact
}

open_dates$mm_sch_eff <- vapply(open_dates$iso3c, mm_sch_eff, numeric(1))

open_dates %>% group_by(wb) %>%
  summarise(mm_sch_eff = median(mm_sch_eff, na.rm = TRUE))
