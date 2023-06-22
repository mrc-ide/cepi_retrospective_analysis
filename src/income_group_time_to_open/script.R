owid <- read_csv("https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/vaccinations.csv") %>%
  rename(iso3c = iso_code) %>%
  group_by(iso3c) %>%
  summarise(vacc_start = date[min(which(total_vaccinations > 0))])
grt <- read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_nat_latest.csv")%>%
  transmute(
    iso3c = CountryCode,
    date = ymd(Date),
    grt_si = StringencyIndex_Average
  ) %>%
  filter(!is.na(grt_si))

opening_vaccine <- grt %>%
  filter(iso3c %in% squire::population$iso3c) %>%
  left_join(
    owid, by = "iso3c"
  ) %>%
  mutate(
    post_vaccine_campaign = date > vacc_start
  ) %>%
  group_by(iso3c) %>%
  mutate(
    #consider opening up to occur once si is less than it's overall median
    open = grt_si < median(grt_si)
  )

time_to_opening <- opening_vaccine %>%
  filter(post_vaccine_campaign) %>%
  filter(open) %>%
  summarise(
    time_to_open = as.numeric(min(date) - vacc_start[1])
  ) %>%
  mutate(
    income_group = squire.page::get_income_group(iso3c)
  )

#ignore countries that are already open (for now)
time_to_opening <- time_to_opening %>%
  mutate(exclude = time_to_open <= 10)

#get average over each income group
income_group_avg <- time_to_opening %>%
  filter(!exclude) %>%
  group_by(
    income_group
  ) %>%
  summarise(
    time_to_open = round(mean(time_to_open))
  )

saveRDS(
  income_group_avg,
  "average_time_to_opening.Rds"
)

plot_df <- time_to_opening %>%
  arrange(income_group, !exclude, time_to_open) %>%
  mutate(ordering = rev(seq_along(iso3c)),
         iso3c = if_else(
           exclude,
           paste0(iso3c, " (excluded)"),
           iso3c
         ))

income_group_df <- plot_df %>%
  filter(!exclude) %>%
  group_by(income_group) %>%
  summarise(mid_iso3c = iso3c[which.min(abs((mean(ordering) - ordering)))]) %>%
  left_join(
    income_group_avg, by = "income_group"
  ) %>%
  mutate(
    label = paste0("Mean: ", time_to_open)
  )

labs_df <- plot_df %>%
  transmute(
    iso3c = if_else(
      exclude,
      paste0(iso3c, " (Excluded)"),
      iso3c
    ),
    ordering = ordering,
    y = if_else(
      exclude,
      -13,
      -5
    )
  )

ic_labs_df <- plot_df %>%
  group_by(income_group) %>%
  summarise(x = iso3c[which.max(ordering)], y = max(time_to_open))



ggsave("diagnostic_plot.png",
       ggplot(
         plot_df,
         aes(x = fct_reorder(iso3c, ordering), y = time_to_open, fill = income_group)
       ) +
         geom_col(alpha = 0.95) +
         geom_point(data = income_group_df, aes(x = mid_iso3c, y = time_to_open)) +
         geom_text(data = income_group_df, aes(x = mid_iso3c, label = label), vjust = "bottom", hjust = "bottom") +
         geom_text(data = ic_labs_df, aes(label = income_group, x = x, y = y, color = income_group), inherit.aes = FALSE,  vjust = "top", hjust = "bottom")+
         labs(x = "", y = "Time from start of vaccinations to when GRT-SI is less than its median",
              colour = "Income Group:", fill = "Income Group:") +
         coord_flip() +
         theme_pubclean(flip = TRUE),
       height = 22, width = 20
)

