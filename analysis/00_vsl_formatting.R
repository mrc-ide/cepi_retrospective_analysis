# ----------------------------------#
# 1. VSL Formatting
# ----------------------------------#

# Read in our VSL table from Damian
vsl <- readxl::read_excel("analysis/data_raw/GNIPC 2021_VSL estimates 03-13-2023 income groups VSL estimates 04-05-2023-1_OJ.xls", skip = 5) %>%
  select(c(9,10,12,18)) %>%
  setNames(c("country", "gni", "iso3c", "vsl")) %>%
  na.omit()

# Wat are the missing countries that we need
iso3cs <- xml2::read_html("https://github.com/mrc-ide/nimue_global_fits/tree/main/excess_mortality")
iso3cs <- gsub("\\.Rds","",rvest::html_text(rvest::html_nodes(iso3cs,".js-navigation-open.Link--primary")))

# assign all the income and gnis for them based on their income
extra <- data.frame("country" = countrycode::countrycode(iso3cs[!(iso3cs %in% vsl$iso3c)], "iso3c", "country.name.en"),
                    "iso3c" = iso3cs[!(iso3cs %in% vsl$iso3c)])
extra$income <- squire.page::get_income_group(extra$iso3c)
extra$gni <- vsl$gni[match(extra$income, vsl$iso3c)]
extra$vsl <- vsl$vsl[match(extra$income, vsl$iso3c)]

# bind together
vsl <- rbind(vsl, extra %>% select(country, gni, iso3c, vsl))
vsl$income <- squire.page::get_income_group(vsl$iso3c)
vsl$income[vsl$iso3c == "XKX"] <- "LMIC"
vsl$income[which(vsl$iso3c %in% c("LMIC", "LIC", "UMIC", "HIC"))] <-
  vsl$iso3c[which(vsl$iso3c %in% c("LMIC", "LIC", "UMIC", "HIC"))]

vsl <- rbind(vsl[!grepl("income", vsl$country),], vsl[grep("income", vsl$country),])

saveRDS(vsl, "analysis/data_raw/vsl.rds")
write.csv(vsl, "analysis/data_raw/vsl.csv")

# ----------------------------------#
# 2. VSLY Formatting
# ----------------------------------#

# WHO 2019 life expectancy
lg <- read.csv("analysis/data_raw/life_expectancy.csv")

# Sort out the age groups
lg <- lg %>%
  mutate(Dim2 = replace(Dim2, Dim2 == "15-19  years", "15-19 years")) %>%
  mutate(Dim2 = factor(Dim2, levels = Dim2[1:17])) %>%
  mutate(age_group = factor(age_group, levels = age_group[1:17])) %>%
  arrange(iso3c, Dim2)

# Need to create life expectancies for countries without WHO coverage
needed_lg <- iso3cs[which(!iso3cs %in% lg$iso3c)]

income_lg <- lg %>% mutate(income = squire.page::get_income_group(iso3c)) %>%
  group_by(income, Dim2, age_group) %>%
  summarise(lg = median(lg))

lg_new <- data.frame(iso3c = needed_lg) %>%
  mutate(income = squire.page::get_income_group(iso3c)) %>%
  split(.$iso3c) %>%
  map_dfr(function(x) {
    income_lg %>%
      filter(income == x$income) %>%
      mutate(income = x$iso3c) %>%
      rename(iso3c = income)}
    ) %>%
  rbind(lg) %>%
  arrange(iso3c, Dim2, age_group)


# now to add in Ng
sqpop <- squire::population %>% select(iso3c, age_group, n) %>% rename(Ng = n)
sqpop$age_group <- lg_new$age_group[1:17][as.integer(sqpop$age_group)]
lg_new <- left_join(lg_new %>% ungroup, sqpop %>% ungroup, by = c("age_group", "iso3c"))

# calculate lghat
lg_new <- lg_new %>%
  group_by(iso3c, Dim2, age_group) %>% mutate(lghat = sum(((1)/((1+0.03)^(1:round(lg))))))
lg_new <- left_join(lg_new, vsl)

## calculate low and high vsl
lg_new <- lg_new %>%
  group_by(iso3c) %>%
  mutate(w_nglghat = sum(Ng * lghat) / sum(Ng)) %>%
  mutate(w_ng = sum(Ng * lg) / sum(Ng)) %>%
  mutate(vsly = vsl / w_nglghat) %>%
  mutate(vsly_ud = vsl / w_ng)

saveRDS(lg_new, "analysis/data_raw/vsly.rds")
write.csv(lg_new, "analysis/data_raw/vsly.csv")
