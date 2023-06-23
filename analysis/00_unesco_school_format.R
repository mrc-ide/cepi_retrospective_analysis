library(dplyr)
library(readxl)
library(tidyr)


df <- read_xlsx("analysis/data_raw/UNESCO_school_closures_database.xlsx") %>% rename(iso3c = "Country ID")
df$Date <- as.Date(df$Date)

open_dates <- readRDS("./open_dates.rds")

open_dates_wide <- open_dates %>% pivot_wider(names_prefix = "scenario",names_from="scenario", values_from = "open_date")

df <-left_join(df, open_dates_wide, by="iso3c")



full <- df %>%  filter(Status == "Closed due to COVID-19") %>% group_by(iso3c,Country)  %>%
  summarise(full_total= sum(Date<=as.Date("2021-12-31")),
            full_2 = sum(Date<= scenario2),
            full_3 = sum(Date<= scenario3),
            full_5 = sum(Date<= scenario5),
            full_6 = sum(Date<= scenario6),
            full_8 = sum(Date<= scenario8),
            full_9 = sum(Date<= scenario9),
            full_11 = sum(Date<= scenario11),
            full_12 = sum(Date<= scenario12)) %>%
  mutate(full_1= full_total,
         full_4=full_total,
         full_7 = full_total,
         full_10= full_total)

partial <- df %>%  filter(Status == "Partially open") %>% group_by(iso3c,Country) %>%
  summarise(partial_total= sum(Date<=as.Date("2021-12-31")),
            partial_2 = sum(Date<= scenario2),
            partial_3 = sum(Date<= scenario3),
            partial_5 = sum(Date<= scenario5),
            partial_6 = sum(Date<= scenario6),
            partial_8 = sum(Date<= scenario8),
            partial_9 = sum(Date<= scenario9),
            partial_11 = sum(Date<= scenario11),
            partial_12 = sum(Date<= scenario12)) %>%
  mutate(partial_1= partial_total,
         partial_4=partial_total,
         partial_7 = partial_total,
         partial_10= partial_total)


F1 <- full %>% select(c(iso3c, full_total))
F3 <- full %>% select (-c(full_total, Country)) %>% pivot_longer (cols= full_2:full_10, names_to ="scenario", names_prefix="full_", values_to= "full_toDate")
F3$scenario <- as.numeric(F3$scenario)

P1 <- partial  %>% select(c(iso3c, partial_total))
P2 <- partial %>% select (-c(partial_total, Country)) %>% pivot_longer (cols= partial_2:partial_10, names_to ="scenario", names_prefix="partial_", values_to= "partial_toDate")
P2$scenario <- as.numeric(P2$scenario)




out <- left_join(open_dates,F1, by= "iso3c")  %>%  left_join(.,P1, by="iso3c")

out <- left_join (out ,F3, by=c("iso3c","scenario")) %>%  left_join(.,P2,  by=c("iso3c","scenario")) %>%
  mutate (extra_full = full_total - full_toDate,
          extra_partial = partial_total- partial_toDate)

out <- out %>%  mutate_at(vars(4:9), ~ceiling(. / 7))


saveRDS(out, file="analysis/data_out/closure_weeks.RDS")


