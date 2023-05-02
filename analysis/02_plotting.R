# ------------------------------------------------------------------------- #
# Quick plots ------------------------------------------------------------
# ------------------------------------------------------------------------- #

save_figs <- function(name,
                      fig,
                      width = 6,
                      height = 6,
                      root = file.path(here::here(), "analysis/plots")) {

  dir.create(root, showWarnings = FALSE)
  fig_path <- function(name) {paste0(root, "/", name)}

  cowplot::save_plot(filename = fig_path(paste0(name,".png")),
                     plot = fig,
                     base_height = height,
                     base_width = width)

  pdf(file = fig_path(paste0(name,".pdf")), width = width, height = height)
  print(fig)
  dev.off()

}

# ------------------------------------------------------------------------- #
# Global ------------------------------------------------------------
# ------------------------------------------------------------------------- #

out1 <- readRDS("analysis/data_out/run_deaths_global.rds")
colors <- c("#ff5f58", "#00244f")
cepi_date <- cepi_start_date <- as.Date("2020-04-20")
ymax <- max(out1$deaths_baseline_med, na.rm = TRUE)
xmax <- as.integer(as.Date("2022-01-01") - cepi_start_date) + 110

vline1 <- data.frame(x = 100,
                     xend = 100,
                     y = 0,
                     yend = ymax)

vline2 <- data.frame(x = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     xend = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     y = 0,
                     yend = ymax)


gg <- out1 %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  ggplot(aes(as.integer(as.Date(date)-cepi_date)+100)) +
  #geom_vline(xintercept = as.integer(as.Date("2020-12-08") - cepi_date) + 100, color = colors[1], lwd = 0.8) +
  geom_segment(data = vline1, aes(x = x, xend = xend, y = y, yend = yend), color = colors[2], lwd = 0.8) +
  geom_segment(data = vline2, aes(x = x, xend = xend, y = y, yend = yend), color = colors[1], lwd = 0.8) +
  #geom_vline(xintercept = 100, color = colors[2], lwd = 0.8) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med), fill="#fff3b5", alpha=1) +
  geom_line(lwd = 1.2, aes(y = deaths_baseline_med, linetype = "dashed"), show.legend = FALSE, color = colors[1]) +
  geom_line(lwd = 1.2, aes(y = deaths_med, linetype = "solid"), show.legend = FALSE, color = colors[2]) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed", size = 0.25)) +
  scale_x_continuous(breaks = c(100, 465), limits = c(0, xmax)) +
  scale_y_continuous(breaks = seq(0,50000,10000), limits = c(0, ymax+10000), expand = c(0, 0)) +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Global Daily COVID-19 Deaths") +
  expand_limits(x = 0, y = 0)

# now make our arrows
arrows <- data.frame(
  "label" = c("Start of \"100 Days Mission\" \nVaccination Campaign",
              "Start of Real World \nVaccination Campaign"),
  "x1" = c(100-30, 332-30),
  "x2" = c(95, 327),
  "y1" = rep(max(out1$deaths_baseline_med, na.rm = TRUE)) + 3000,
  "y2" = rep(max(out1$deaths_baseline_med, na.rm = TRUE)),
  "color" = colors
)

rwdeaths <- data.frame(
  "label" = "Real World \nCOVID-19 Deaths",
  "x1" = c(550),
  "x2" = c(500),
  "y1" = ymax - 3000,
  "y2" = ymax,
  "color" = colors[1]
)

mission_deaths <- data.frame(
  "label" = "\"100 Days Mission\" \nCOVID-19 Deaths",
  "x1" = c(260),
  "x2" = c(285),
  "y1" =4000,
  "y2" = 9000,
  "color" = colors[2]
)

saves_deaths <- data.frame(
  "label" = "Additional Lives Saved By \n\"100 Days Mission\"",
  "x1" = c(625),
  "x2" = c(575),
  "y1" =4000,
  "y2" = 9000,
  "color" = colors[2]
)

gg_global <- gg +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = rev(colors), curvature = 0.4, ncp = 10) +
  geom_curve(
    data = rwdeaths, aes(x = x1, y = y1+1500, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[1], curvature = 0.4, ncp = 10) +
  geom_curve(
    data = mission_deaths, aes(x = x1, y = y1+1500, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[2], curvature = 0.4, ncp = 10) +
  # geom_curve(
  #   data = saves_deaths, aes(x = x1-30, y = y1+300, xend = x2-30, yend = y2),
  #   arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
  #   color = "black", curvature = -0.4, ncp = 10) +
  geom_text(data = arrows, aes(label = label, x = x1-5, y = y1+3900),color = rev(colors), hjust = "left", size = 6) +
  geom_text(data = rwdeaths, aes(label = label, x = x1+50, y = y1+1000),color = colors[1],  size = 6) +
  geom_text(data = mission_deaths, aes(label = label, x = x1-70, y = y1+1000),color = colors[2],  size = 6) +
  # geom_text(data = saves_deaths, aes(label = label, x = x1+55, y = y1+1000),color = "black",  size = 6) +
  # geom_segment(data = saves_deaths, aes(x = x1-88, xend = x1-88, y = 27500, yend = 4000), color = "black", lwd = 0.8,
  #              arrow = grid::arrow(ends = "both", length = ggplot2::unit(0.75, "lines"))) +
  geom_label(data = saves_deaths, aes(label = label, x = x1-88, y = y1+13000),
             fill = "#fff3b5",  size = 5.5, label.size = 0, label.padding = ggplot2::unit(0.5, "lines")) +
  labs(title = ~ underline("World: Modelled Impact of \"100 Days Mission\" by the end of 2021                                                                                 "),
       subtitle = "") +
  theme(plot.title = element_text(color = "grey30"))

# save figs to plots directory
save_figs("deaths_global", gg_global, width = 14, height = 7)

# ------------------------------------------------------------------------- #
# Global Hosps ------------------------------------------------------------
# ------------------------------------------------------------------------- #

out_hosps <- readRDS("analysis/data_out/run_hospitalisations_global.rds")
colors <- c("#ff5f58", "#00244f")
cepi_date <- cepi_start_date <- as.Date("2020-04-20")
ymax <- sum(out_hosps$hospitalisations_baseline_med[out_hosps$scenario==1], na.rm = TRUE)
xmax <- as.integer(as.Date("2022-01-01") - cepi_start_date) + 110

vline1 <- data.frame(x = 100,
                     xend = 100,
                     y = 0,
                     yend = ymax)

vline2 <- data.frame(x = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     xend = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     y = 0,
                     yend = ymax)


gg <- out_hosps %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  ggplot(aes(as.integer(as.Date(date)-cepi_date)+100)) +
  #geom_vline(xintercept = as.integer(as.Date("2020-12-08") - cepi_date) + 100, color = colors[1], lwd = 0.8) +
  geom_segment(data = vline1, aes(x = x, xend = xend, y = y, yend = yend), color = colors[2], lwd = 0.8) +
  geom_segment(data = vline2, aes(x = x, xend = xend, y = y, yend = yend), color = colors[1], lwd = 0.8) +
  #geom_vline(xintercept = 100, color = colors[2], lwd = 0.8) +
  geom_ribbon(aes(ymin=cumsum(hospitalisations_med),ymax=cumsum(hospitalisations_baseline_med)), fill="#fff3b5", alpha=1) +
  geom_line(lwd = 1.2, aes(y = cumsum(hospitalisations_baseline_med), linetype = "dashed"), show.legend = FALSE, color = colors[1]) +
  geom_line(lwd = 1.2, aes(y = cumsum(hospitalisations_med), linetype = "solid"), show.legend = FALSE, color = colors[2]) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed", size = 0.25)) +
  scale_x_continuous(breaks = c(100, 465), limits = c(0, xmax)) +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Global Cumulative COVID-19 Hospitalisations") +
  expand_limits(x = 0, y = 0)

# now make our arrows
arrows <- data.frame(
  "label" = c("Start of \"100 Days Mission\" \nVaccination Campaign",
              "Start of Real World \nVaccination Campaign"),
  "x1" = c(100-30, 332-30),
  "x2" = c(95, 327),
  "y1" = rep(ymax + 3000),
  "y2" = rep(ymax),
  "color" = colors
)

rwdeaths <- data.frame(
  "label" = "Real World \nCOVID-19 \nHospitalisations",
  "x1" = c(550),
  "x2" = c(500),
  "y1" = ymax - 3000,
  "y2" = ymax,
  "color" = colors[1]
)

mission_deaths <- data.frame(
  "label" = "\"100 Days Mission\" \nCOVID-19 \nHospitalisations",
  "x1" = c(260),
  "x2" = c(285),
  "y1" =4000,
  "y2" = 9000,
  "color" = colors[2]
)

saves_deaths <- data.frame(
  "label" = "Additional Hospitalisations \nPrevented By\n\"100 Days Mission\"",
  "x1" = c(625),
  "x2" = c(575),
  "y1" =4000,
  "y2" = 9000,
  "color" = colors[2]
)

gg_global_hosps_cumulative <- gg +
  geom_curve(
    data = arrows, aes(x = x1, y = y1+1e6, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = rev(colors), curvature = 0.4, ncp = 10) +
  geom_curve(
    data = rwdeaths, aes(x = x1+100, y = y1+1500, xend = x2+200, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[1], curvature = -0.4, ncp = 10) +
  geom_curve(
    data = mission_deaths, aes(x = x1-20, y = y1+2e7, xend = x2, yend = y2+1.3e7),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[2], curvature = -0.4, ncp = 10) +
  # geom_curve(
  #   data = saves_deaths, aes(x = x1-30, y = y1+300, xend = x2-30, yend = y2),
  #   arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
  #   color = "black", curvature = -0.4, ncp = 10) +
  geom_text(data = arrows, aes(label = label, x = x1-5, y = y1+6e6),color = rev(colors), hjust = "left", size = 6) +
  geom_text(data = rwdeaths, aes(label = label, x = x1+50, y = y1+1e6),color = colors[1],  size = 6) +
  geom_text(data = mission_deaths, aes(label = label, x = x1-70, y = y1+2e7),color = colors[2],  size = 6) +
  # geom_text(data = saves_deaths, aes(label = label, x = x1+55, y = y1+1000),color = "black",  size = 6) +
  # geom_segment(data = saves_deaths, aes(x = x1-88, xend = x1-88, y = 27500, yend = 4000), color = "black", lwd = 0.8,
  #              arrow = grid::arrow(ends = "both", length = ggplot2::unit(0.75, "lines"))) +
  geom_label(data = saves_deaths, aes(label = label, x = x1-20, y = y1+3.2e7),
             fill = "#fff3b5",  size = 5.5, label.size = 0, label.padding = ggplot2::unit(0.5, "lines")) +
  labs(title = ~ underline("World: Modelled Impact of \"100 Days Mission\" by the end of 2021                                                                                 "),
       subtitle = "") +
  theme(plot.title = element_text(color = "grey30"))

# save figs to plots directory
save_figs("hosps_global_cumulative", gg_global_hosps_cumulative, width = 14, height = 7)

# ------------------------------------------------------------------------- #
# Global by income group ------------------------------------------------------------
# ------------------------------------------------------------------------- #

out1 <- readRDS("analysis/data_out/run_deaths_global.rds")
out2 <- readRDS("analysis/data_out/run_deaths_income.rds")
colors <- c("#ff5f58", "#00244f")
wbcols <- rev(viridis::viridis(4))
names(wbcols) <- c("Low-Income Countries","Low-Middle Income Countries","Upper-Middle Income Countries","High-Income Countries")

cepi_date <- as.Date("2020-04-20")
ymax <- max(out1$deaths_baseline_med, na.rm = TRUE)
xmax <- as.integer(as.Date("2022-01-01") - cepi_start_date) + 110

vline1 <- data.frame(x = 100,
                     xend = 100,
                     y = 0,
                     yend = ymax)

vline2 <- data.frame(x = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     xend = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     y = 0,
                     yend = ymax)



lic <- out1 %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  mutate(deaths_baseline_med = deaths_med + out2 %>% filter(income == "LIC" & date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med),
         income = factor(names(wbcols[1]), levels = rev(names(wbcols))))
lmic <- lic %>%
  mutate(deaths_baseline_med = deaths_baseline_med + out2 %>% filter(income == "LMIC" & date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med)) %>%
  mutate(income = factor(names(wbcols[2]), levels = rev(names(wbcols))))
umic <- lmic %>%
  mutate(deaths_baseline_med = deaths_baseline_med + out2 %>% filter(income == "UMIC" & date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med)) %>%
  mutate(income = factor(names(wbcols[3]), levels = rev(names(wbcols))))
hic <- umic %>%
  mutate(deaths_baseline_med = deaths_baseline_med + out2 %>% filter(income == "HIC" & date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med)) %>%
  mutate(income = factor(names(wbcols[4]), levels = rev(names(wbcols))))


gg <- out1 %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  ggplot(aes(as.integer(as.Date(date)-cepi_date)+100)) +
  geom_segment(data = vline1, aes(x = x, xend = xend, y = y, yend = yend), color = colors[2], lwd = 0.8) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med, fill = income), alpha=1, data = hic) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med, fill = income), alpha=1, data = umic) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med, fill = income), alpha=1, data = lmic) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med, fill = income), alpha=1, data = lic) +
  geom_line(aes(y=deaths_baseline_med), color=wbcols[3], alpha=1, data = umic) +
  geom_line(aes(y=deaths_baseline_med), color=wbcols[4], alpha=1, data = hic) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed", size = 0.25)) +
  scale_x_continuous(breaks = c(100, 465), limits = c(0, xmax)) +
  scale_y_continuous(breaks = seq(0,50000,10000), limits = c(0, ymax+10000), expand = c(0, 0)) +
  scale_fill_manual(values = wbcols, name = "    World Bank Income Group:") +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Global Daily COVID-19 Deaths") +
  expand_limits(x = 0, y = 0)


gg_global <- gg +
  geom_curve(
    data = arrows[1,], aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = rev(colors)[1], curvature = 0.4, ncp = 10) +
  geom_text(data = arrows[1,], aes(label = label, x = x1-5, y = y1+3900),color = rev(colors)[1], hjust = "left", size = 6) +
  labs(title = ~ underline("World: Modelled Impact of \"100 Days Mission\" by the end of 2021 by Income Group                                                       "),
       subtitle = "") +
  theme(plot.title = element_text(color = "grey30"))

gg_global <- gg_global +
  theme(legend.position = c(0.875, 0.92), legend.title.align = 1) +
  guides(fill = guide_legend(nrow=4,byrow=TRUE,label.position = "left",title.hjust = 0))

negative_excess <- data.frame(
  "label" = "Negative Deaths Prevented in \nUpper-Middle Income Countries",
  "x1" = c(575),
  "x2" = c(525),
  "y1" =44000,
  "y2" = 34000,
  "color" = colors[2]
)

gg_global2 <- gg_global +
  geom_curve(
    data = negative_excess, aes(x = x1-22, y = y1, xend = x2+80, yend = y2-3000),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.5,
    color = "black", curvature = 0.9, ncp = 10) +
  geom_text(data = negative_excess, aes(label = label, x = x1-15, y = y1+1000),color = "black", hjust = "left", size = 3.5)

save_figs("deaths_global_income", gg_global2, width = 14, height = 7)

# ------------------------------------------------------------------------- #
# Global but different scenario ------------------------------------------------------------
# ------------------------------------------------------------------------- #

out1 <- readRDS("analysis/data_out/run_deaths_global.rds")
colors <- c("#ff5f58", "#00244f")
cepi_date <- as.Date("2020-04-20")
ymax <- max(out1$deaths_baseline_med, na.rm = TRUE)
xmax <- as.integer(as.Date("2022-01-01") - cepi_start_date) + 110

vline1 <- data.frame(x = 100,
                     xend = 100,
                     y = 0,
                     yend = ymax)

vline2 <- data.frame(x = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     xend = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     y = 0,
                     yend = ymax)


gg <- out1 %>%
  filter(scenario == 3) %>%
  filter(date < as.Date("2022-01-01")) %>%
  ggplot(aes(as.integer(as.Date(date)-cepi_date)+100)) +
  #geom_vline(xintercept = as.integer(as.Date("2020-12-08") - cepi_date) + 100, color = colors[1], lwd = 0.8) +
  geom_segment(data = vline1, aes(x = x, xend = xend, y = y, yend = yend), color = colors[2], lwd = 0.8) +
  geom_segment(data = vline2, aes(x = x, xend = xend, y = y, yend = yend), color = colors[1], lwd = 0.8) +
  #geom_vline(xintercept = 100, color = colors[2], lwd = 0.8) +
  geom_ribbon(aes(ymin=deaths_med,ymax=deaths_baseline_med), fill="#fff3b5", alpha=1) +
  geom_line(lwd = 1.2, aes(y = deaths_baseline_med, linetype = "dashed"), show.legend = FALSE, color = colors[1]) +
  geom_line(lwd = 1.2, aes(y = deaths_med, linetype = "solid"), show.legend = FALSE, color = colors[2]) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed", size = 0.25)) +
  scale_x_continuous(breaks = c(100, 465), limits = c(0, xmax)) +
  scale_y_continuous(breaks = seq(0,50000,10000), limits = c(0, ymax+10000), expand = c(0, 0)) +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Global Daily COVID-19 Deaths") +
  expand_limits(x = 0, y = 0)

# now make our arrows
arrows <- data.frame(
  "label" = c("Start of \"100 Days Mission\" \nVaccination Campaign",
              "Start of Real World \nVaccination Campaign"),
  "x1" = c(100-30, 332-30),
  "x2" = c(95, 327),
  "y1" = rep(max(out1$deaths_baseline_med, na.rm = TRUE)) + 3000,
  "y2" = rep(max(out1$deaths_baseline_med, na.rm = TRUE)),
  "color" = colors
)

rwdeaths <- data.frame(
  "label" = "Real World \nCOVID-19 Deaths",
  "x1" = c(550),
  "x2" = c(500),
  "y1" = ymax - 3000,
  "y2" = ymax,
  "color" = colors[1]
)

mission_deaths <- data.frame(
  "label" = "\"100 Days Mission\" \nCOVID-19 Deaths",
  "x1" = c(260),
  "x2" = c(285),
  "y1" =4000,
  "y2" = 9000,
  "color" = colors[2]
)

saves_deaths <- data.frame(
  "label" = "Additional Lives Saved By \n\"100 Days Mission\"",
  "x1" = c(625-40),
  "x2" = c(575-40),
  "y1" =4000+9000,
  "y2" = 9000+9000,
  "color" = colors[2]
)

gg_global_economic <- gg +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = rev(colors), curvature = 0.4, ncp = 10) +
  geom_curve(
    data = rwdeaths, aes(x = x1, y = y1+1500, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[1], curvature = 0.4, ncp = 10) +
  geom_curve(
    data = mission_deaths, aes(x = x1, y = y1+1500, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[2], curvature = 0.4, ncp = 10) +
  geom_curve(
    data = saves_deaths, aes(x = x1-50, y = y1-8000, xend = x2-30, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = "black", curvature = -0.4, ncp = 10) +
  geom_text(data = arrows, aes(label = label, x = x1-5, y = y1+3900),color = rev(colors), hjust = "left", size = 6) +
  geom_text(data = rwdeaths, aes(label = label, x = x1+50, y = y1+1000),color = colors[1],  size = 6) +
  geom_text(data = mission_deaths, aes(label = label, x = x1-70, y = y1+1000),color = colors[2],  size = 6) +
  geom_text(data = saves_deaths, aes(label = label, x = x1+55, y = y1-8000),color = "black",  size = 6) +
  labs(title = ~ underline("World: Modelled Impact of \"100 Days Mission\" & Economic Based Opening by the end of 2021                                     "),
       subtitle = "") +
  theme(plot.title = element_text(color = "grey30"))

save_figs("deaths_global_economic", gg_global_economic, width = 14, height = 7)

# ------------------------------------------------------------------------- #
# Global cumulative ------------------------------------------------------------
# ------------------------------------------------------------------------- #
out1 <- readRDS("analysis/data_out/run_deaths_global.rds")
colors <- c("#ff5f58", "#00244f")
cepi_date <- cepi_start_date <- as.Date("2020-04-20")
ymax <- sum(out1$deaths_baseline_med[out1$scenario == 1], na.rm = TRUE)
xmax <- as.integer(as.Date("2022-01-01") - cepi_start_date) + 110

vline1 <- data.frame(x = 100,
                     xend = 100,
                     y = 0,
                     yend = ymax)

vline2 <- data.frame(x = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     xend = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     y = 0,
                     yend = ymax)


gg <- out1 %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  ggplot(aes(as.integer(as.Date(date)-cepi_date)+100)) +
  #geom_vline(xintercept = as.integer(as.Date("2020-12-08") - cepi_date) + 100, color = colors[1], lwd = 0.8) +
  geom_segment(data = vline1, aes(x = x, xend = xend, y = y, yend = yend), color = colors[2], lwd = 0.8) +
  geom_segment(data = vline2, aes(x = x, xend = xend, y = y, yend = yend), color = colors[1], lwd = 0.8) +
  #geom_vline(xintercept = 100, color = colors[2], lwd = 0.8) +
  geom_ribbon(aes(ymin=cumsum(deaths_med),ymax=cumsum(deaths_baseline_med)), fill="#fff3b5", alpha=1) +
  geom_line(lwd = 1.2, aes(y = cumsum(deaths_baseline_med), linetype = "dashed"), show.legend = FALSE, color = colors[1]) +
  geom_line(lwd = 1.2, aes(y = cumsum(deaths_med), linetype = "solid"), show.legend = FALSE, color = colors[2]) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed", size = 0.25)) +
  scale_x_continuous(breaks = c(100, 465), limits = c(0, xmax)) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6), limits = c(0, 19e6), expand = c(0, 0)) +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Global Cumulative COVID-19 Deaths") +
  expand_limits(x = 0, y = 0)

# now make our arrows
arrows <- data.frame(
  "label" = c("Start of \"100 Days Mission\" \nVaccination Campaign",
              "Start of Real World \nVaccination Campaign"),
  "x1" = c(100-30, 332-30),
  "x2" = c(95, 327),
  "y1" = ymax + 300000,
  "y2" = ymax,
  "color" = colors
)

rwdeaths <- data.frame(
  "label" = "Real World \nCOVID-19 Deaths",
  "x1" = c(375),
  "x2" = c(425),
  "y1" = ymax - 5000000,
  "y2" = ymax - 9000000,
  "color" = colors[1]
)

mission_deaths <- data.frame(
  "label" = "\"100 Days Mission\" \nCOVID-19 Deaths",
  "x1" = c(260),
  "x2" = c(285),
  "y1" = 6500000,
  "y2" = 3500000,
  "color" = colors[2]
)

saves_deaths <- data.frame(
  "label" = "Additional Lives Saved By \n\"100 Days Mission\"",
  "x1" = c(625),
  "x2" = c(575),
  "y1" =8500000,
  "y2" = 12500000,
  "color" = colors[2]
)

ytlift <- 900000
gg_global_cumulative <- gg +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = rev(colors), curvature = 0.4, ncp = 10) +
  geom_curve(
    data = rwdeaths, aes(x = x1+90, y = y1+1500000, xend = x2+90, yend = y2+3500000),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[1], curvature = -0.4, ncp = 10) +
  geom_curve(
    data = mission_deaths, aes(x = x1, y = y1+1e6, xend = x2+40, yend = y2+4e5),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = colors[2], curvature = -0.4, ncp = 10) +
  # geom_curve(
  #   data = saves_deaths, aes(x = x1-30, y = y1+300, xend = x2-30, yend = y2),
  #   arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
  #   color = "black", curvature = -0.4, ncp = 10) +
  geom_text(data = arrows, aes(label = label, x = x1-5, y = y1+ytlift+300000),color = rev(colors), hjust = "left", size = 6) +
  geom_text(data = rwdeaths, aes(label = label, x = x1+50, y = y1+ytlift+2e5),color = colors[1],  size = 6) +
  geom_text(data = mission_deaths, aes(label = label, x = x1-60, y = y1+ytlift),color = colors[2],  size = 6) +
  geom_segment(data = saves_deaths, aes(x = x1-25, xend = x1-25, y = 13e6, yend = 5e6), color = "black", lwd = 0.8,
               arrow = grid::arrow(ends = "both", length = ggplot2::unit(0.75, "lines"))) +
  geom_label(data = saves_deaths, aes(label = label, x = x1-14, y = y1+ytlift-4e5),
             fill = "#fff3b5",  size = 6, label.size = 0, label.padding = ggplot2::unit(0.5, "lines")) +
  labs(title = ~ underline("World: Modelled Impact of \"100 Days Mission\" by the end of 2021                                                                                 "),
       subtitle = "") +
  theme(plot.title = element_text(color = "grey30"))

# save figs to plots directory
save_figs("deaths_global_cumulative", gg_global_cumulative, width = 14, height = 7)

# ------------------------------------------------------------------------- #
# Global cumulative by income group ------------------------------------------------------------
# ------------------------------------------------------------------------- #

out1 <- readRDS("analysis/data_out/run_deaths_global.rds")
out2 <- readRDS("analysis/data_out/run_deaths_income.rds")
colors <- c("#ff5f58", "#00244f")
wbcols <- rev(viridis::viridis(4))
names(wbcols) <- c("Low-Income Countries","Low-Middle Income Countries","Upper-Middle Income Countries","High-Income Countries")

cepi_date <- cepi_start_date <- as.Date("2020-04-20")
ymax <- sum(out1$deaths_baseline_med[out1$scenario == 1], na.rm = TRUE)
xmax <- as.integer(as.Date("2022-01-01") - cepi_start_date) + 110

vline1 <- data.frame(x = 100,
                     xend = 100,
                     y = 0,
                     yend = ymax)

vline2 <- data.frame(x = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     xend = as.integer(as.Date("2020-12-08") - cepi_date) + 100,
                     y = 0,
                     yend = ymax)


lic <- out1 %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  mutate(deaths_baseline_med = deaths_med + out2 %>% filter(income == "LIC" & date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med),
         income = factor(names(wbcols[1]), levels = rev(names(wbcols))))
lmic <- lic %>%
  mutate(deaths_baseline_med = deaths_baseline_med + out2 %>% filter(income == "LMIC" & date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med)) %>%
  mutate(income = factor(names(wbcols[2]), levels = rev(names(wbcols))))
umic <- lmic %>%
  mutate(deaths_baseline_med = deaths_baseline_med + out2 %>% filter(income == "UMIC" & date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med)) %>%
  mutate(income = factor(names(wbcols[3]), levels = rev(names(wbcols))))
hic <- umic %>%
  mutate(deaths_baseline_med = deaths_baseline_med + out2 %>% filter(income == "HIC" & date < as.Date("2022-01-01") & scenario == 1) %>% pull(deaths_averted_med)) %>%
  mutate(income = factor(names(wbcols[4]), levels = rev(names(wbcols))))

gg <- out1 %>%
  filter(scenario == 1) %>%
  filter(date < as.Date("2022-01-01")) %>%
  ggplot(aes(as.integer(as.Date(date)-cepi_date)+100)) +
  geom_segment(data = vline1, aes(x = x, xend = xend, y = y, yend = yend), color = colors[2], lwd = 0.8) +
  #geom_segment(data = vline2, aes(x = x, xend = xend, y = y, yend = yend), color = colors[1], lwd = 0.8) +
  geom_line(aes(y=cumsum(deaths_baseline_med)), color=wbcols[4], alpha=1, data = hic) +
  geom_ribbon(aes(ymin=cumsum(deaths_med),ymax=cumsum(deaths_baseline_med), fill = income), alpha=1, data = hic) +
  geom_ribbon(aes(ymin=cumsum(deaths_med),ymax=cumsum(deaths_baseline_med), fill = income), alpha=1, data = umic) +
  geom_ribbon(aes(ymin=cumsum(deaths_med),ymax=cumsum(deaths_baseline_med), fill = income), alpha=1, data = lmic) +
  geom_ribbon(aes(ymin=cumsum(deaths_med),ymax=cumsum(deaths_baseline_med), fill = income), alpha=1, data = lic) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.x = element_line(color = "grey"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed", size = 0.25)) +
  scale_x_continuous(breaks = c(100, 465), limits = c(0, xmax)) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6), limits = c(0, 20e6), expand = c(0, 0)) +
  scale_fill_manual(values = wbcols, name = "    World Bank Income Group") +
  xlab("Days Since Recognition of COVID-19") +
  ylab("Global Cumulative COVID-19 Deaths") +
  expand_limits(x = 0, y = 0)


gg_global_income_cumulative <- gg +
  geom_curve(
    data = arrows[1,], aes(x = x1, y = y1+0.4e6, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch"), type = "closed", angle = 45), size = 0.75,
    color = rev(colors)[1], curvature = 0.4, ncp = 10) +
  geom_text(data = arrows[1,], aes(label = label, x = x1-5, y = y1+ytlift+0.7e6),color = rev(colors)[1], hjust = "left", size = 6) +
  labs(title = ~ underline("World: Modelled Impact of \"100 Days Mission\" by the end of 2021 by Income Group                                                          "),
       subtitle = "") +
  theme(plot.title = element_text(color = "grey30"))

gg_global_income_cumulative <- gg_global_income_cumulative +
  theme(legend.position = c(0.75, 0.96), legend.title.align = 0.5) +
  guides(fill = guide_legend(nrow=2,byrow=TRUE,label.position = "left",title.hjust = 1))

save_figs("deaths_global_cumulative_income", gg_global_income_cumulative, width = 14, height = 7)

# ------------------------------------------------------------------------- #
# Totals Functions -------------------------------------------------------------
# ------------------------------------------------------------------------- #

# Get our scenario descriptors
source("src/run_simulations/funcs.R")
scenarios <- read.csv("src/run_simulations/scenarios.csv") %>%
  mutate(scenario = as.integer(rownames(.))) %>%
  describe_scenarios()

# functions for creating total scenario comparisons grouped by rt
plot_totals_by_rt <- function(res_full, var, ylab, unit = "Trillion", scale = 1e-12) {

  ylab <- paste0("\n", ylab, " (median, IQR, 95% quantile)")
  res_sub <- res_full %>% select(scenario, matches(var))
  names(res_sub) <- gsub(var, "var", names(res_sub))

  res_sub %>%
    left_join(
      scenarios %>%
        mutate(Rt = factor(as.character(scenarios$Rt), levels = c("History Based", "Economic Based", "Target Based"))),
      by = "scenario") %>%
    mutate(Rt = factor(gsub(" ", "\n", Rt), gsub(" ", "\n", levels(Rt)))) %>%
    ggplot(aes(x = Rt, y = var_med,
               ymin = var_025, ymax = var_975,
               color = Vaccine, group = interaction(Vaccine, Rt))) +
    geom_hline(yintercept = 0, color = "black") +
    geom_linerange(position = position_dodge(width = 0.5), lwd = 2, alpha = 0.3) +
    geom_linerange(aes(ymin = var_25, ymax = var_75),
                   position = position_dodge(width = 0.5), lwd = 2, alpha = 0.6) +
    geom_point(shape = 21, size =2, fill = "white", position = position_dodge(width = 0.5)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(panel.grid.major = element_line()) +
    scale_color_manual(name = "Vaccine Production and Equity:", values = c(pals::stepped3()[c(1,9,5,13)])) +
    labs(x = "\nSpeed of Lifting NPI Restrictions\n", y = ylab) +
    scale_y_continuous(n.breaks = 6) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14), plot.margin = margin(0, 1, 0, 0, "cm")) +
    coord_flip() +
    scale_y_continuous(labels = scales::unit_format(prefix = prefix, unit = unit, scale = scale))

}

# functions for creating total scenario comparisons grouped by Vaccine
plot_totals_by_vaccine <- function(res_full, var, ylab, prefix = "$", unit = "Trillion", scale = 1e-12, n.breaks = 5) {

  ylab <- paste0("\n", ylab, " (median, IQR, 95% quantile)")
  res_sub <- res_full %>% select(scenario, matches(var))
  names(res_sub) <- gsub(var, "var", names(res_sub))

  res_sub %>%
    left_join(
      scenarios %>%
        mutate(Rt = factor(as.character(scenarios$Rt), levels = c("History Based", "Economic Based", "Target Based"))),
      by = "scenario") %>%
    mutate(Rt = factor(gsub(" ", "\n", Rt), gsub(" ", "\n", levels(Rt)))) %>%
    ggplot(aes(x = Vaccine, y = var_med,
               ymin = var_025, ymax = var_975,
               color = Rt, group = interaction(Vaccine, Rt))) +
    # geom_hline(yintercept = 0, color = "black") +
    geom_linerange(position = position_dodge(width = 0.5), lwd = 2, alpha = 0.3) +
    geom_linerange(aes(ymin = var_25, ymax = var_75),
                   position = position_dodge(width = 0.5), lwd = 2, alpha = 0.6) +
    geom_point(shape = 21, size =2, fill = "white", position = position_dodge(width = 0.5)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(panel.grid.major = element_line()) +
    scale_color_manual(name = "Speed of Lifting NPI Restrictions:", values = c(pals::stepped3()[c(1,9,5,13)])) +
    labs(x = "\nVaccine Production and Equity\n", y = ylab) +
    guides(color=guide_legend(nrow=1, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14), plot.margin = margin(0, 1, 0, 0, "cm")) +
    coord_flip() +
    scale_y_continuous(n.breaks = n.breaks, labels = scales::unit_format(prefix = prefix, unit = unit, scale = scale))

}

# ------------------------------------------------------------------------- #
# Health Economic Plotting --------------------------------------------------
# ------------------------------------------------------------------------- #

# Our table of impacts
res_full <- readRDS("analysis/data_out/total_lifeyears_global.rds")

# VSL
var <- "economic_lives_saved"
ylab <- "Global Value of Statistical Life"
vsl_scenario_averted_by_vaccine <- plot_totals_by_vaccine(res_full,  var, ylab)
save_figs("vsl_scenario_averted_by_vaccine", vsl_scenario_averted_by_vaccine, width = 10, height = 7)

# VSLY
var <- "economic_life_years_saved"
ylab <- "Global Value of Statistical Life Years"
vsly_scenario_averted_by_vaccine <- plot_totals_by_vaccine(res_full,  var, ylab)
save_figs("vsly_scenario_averted_by_vaccine", vsly_scenario_averted_by_vaccine, width = 10, height = 7)

# Productive economic
var <- "economic_productive_loss"
ylab <- "Global Productivity Losses Averted"
productive_years_averted_by_vaccine <- plot_totals_by_vaccine(res_full,  var, ylab)
save_figs("productive_years_averted_by_vaccine", productive_years_averted_by_vaccine, width = 10, height = 7)

# Our table of hosp impacts
hosp_full <- readRDS("analysis/data_out/total_hospcosts_global.rds")

# hosp costs
var <- "hospitalisation_costs_averted"
ylab <- "Global Hospitalization Costs Averted"
hospitalisation_costs_averted_by_vaccine <- plot_totals_by_vaccine(hosp_full,  var, ylab, unit = "Billion", scale = 1e-9, n.breaks = 3)
save_figs("hospitalisation_costs_averted_by_vaccine", hospitalisation_costs_averted_by_vaccine, width = 10, height = 7)

# ------------------------------------------------------------------------- #
# Total Deaths By Scenarios ------------------------------------------------------------
# ------------------------------------------------------------------------- #

# total deaths by scenarios
death_totals_global <- readRDS("analysis/data_out/total_deaths_global.rds")

var <- "deaths_averted"
ylab <- "Global Deaths Averted"
total_deaths_averted_by_vaccine <- plot_totals_by_vaccine(death_totals_global, var, ylab, prefix = "", unit = "Million", scale = 1e-6, n.breaks = 5)
save_figs("total_deaths_averted_by_vaccine", total_deaths_averted_by_vaccine, width = 10, height = 7)

# ------------------------------------------------------------------------- #
# School and NPI Plotting ---------------------------------------------------
# ------------------------------------------------------------------------- #

plot_medians_by_vaccine <- function(res_full, var, ylab, prefix = "$", unit = "Trillion", scale = 1e-12, flip = TRUE) {

ylab <- paste0("\n", ylab, "\n")
res_sub <- res_full %>% filter(Rt != "History Based") %>% select(scenario, matches(var))
names(res_sub) <- gsub(var, "var", names(res_sub))

res_sub <- res_sub %>%
  left_join(
    scenarios %>%
      mutate(Rt = factor(as.character(scenarios$Rt), levels = c("Economic Based", "Target Based"))),
    by = "scenario") %>%
  mutate(Rt = factor(gsub(" ", "\n", Rt), gsub(" ", "\n", levels(Rt))))

if(!flip){
  res_sub <- res_sub %>%
    mutate(Vaccine = gsub("n &", "n \n&", Vaccine)) %>%
    mutate(Vaccine = gsub("l ", "l \n", Vaccine)) %>%
    mutate(Vaccine = factor(Vaccine))
}


gg <- res_sub %>%
  ggplot(aes(x = Vaccine, y = var_med,
             color = Rt, fill = Rt, group = interaction(Vaccine, Rt))) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_errorbar(position = position_dodge(width = 0.75), width = 0.5, color = "black") +
  ggpubr::theme_pubr(base_size = 14) +
  theme(panel.grid.major = element_line()) +
  scale_color_manual(name = "Speed of Lifting NPI Restrictions:", values = c(pals::stepped3()[c(9,5)])) +
  scale_fill_manual(name = "Speed of Lifting NPI Restrictions:", values = c(pals::stepped3()[c(9,5)])) +
  labs(x = "\nVaccine Production and Equity\n", y = ylab) +
  scale_y_continuous(n.breaks = 6) +
  guides(color=guide_legend(nrow=1, byrow=TRUE)) +
  theme(legend.text = element_text(size = 14), plot.margin = margin(0, 1, 0, 0, "cm")) +
  scale_y_continuous(labels = scales::unit_format(prefix = prefix, unit = unit, scale = scale, big.mark = ""))

if(flip){
  gg +
  coord_flip()
} else {
  gg
}

}

# npi gains by scenarios
npi_gains_global <- readRDS("analysis/data_out/total_npigains_global.rds")

var <- "gain_in_openness"
ylab <- "Global Additional Days Without NPIs"
total_npis_averted_by_vaccine <- plot_medians_by_vaccine(npi_gains_global, var, ylab, prefix = "", unit = "Thousand", scale = 1e-3)
save_figs("total_npis_averted_by_vaccine", total_npis_averted_by_vaccine, width = 10, height = 7)

# school weeks gained
school_gains_global <- readRDS("analysis/data_out/total_schoolweeks_global.rds")
school_gains_global <- school_gains_global %>%
  rename(extra_full_school_weeks_total_med = extra_full_school_weeks_total) %>%
  rename(extra_partial_school_weeks_total_med = extra_partial_school_weeks_total)

var <- "extra_full_school_weeks_total"
ylab <- "Global Extra Weeks of Schools Being Fully Open"
total_full_school_weeks_by_vaccine <- plot_medians_by_vaccine(school_gains_global, var, ylab, prefix = "", unit = "", scale = 1)
save_figs("total_full_school_weeks_by_vaccine", total_full_school_weeks_by_vaccine, width = 10, height = 7)

var <- "extra_partial_school_weeks_total"
ylab <- "Global Extra Weeks of Schools Being Partially Open"
total_partial_school_weeks_by_vaccine <- plot_medians_by_vaccine(school_gains_global, var, ylab, prefix = "", unit = "", scale = 1)
save_figs("total_partial_school_weeks_by_vaccine", total_partial_school_weeks_by_vaccine, width = 10, height = 7)

# ------------------------------------------------------------------------- #
# Combination Plotting ---------------------------------------------------
# ------------------------------------------------------------------------- #

# ------------------------------------------------------------------------- #
# Combined NPI and VSL ---------------------------------------------------
# ------------------------------------------------------------------------- #

# combined_npi effects
combined_npi_impacts <- cowplot::plot_grid(
  cowplot::get_legend(total_npis_averted_by_vaccine),
  cowplot::plot_grid(
    total_npis_averted_by_vaccine + theme(legend.position = "none"),
    total_full_school_weeks_by_vaccine + theme(legend.position = "none"),
    total_partial_school_weeks_by_vaccine + theme(legend.position = "none"),
    ncol = 1, labels = c(LETTERS[5:7]), scale = 0.9),
  ncol = 1, rel_heights = c(0.1,1)
)

# combine with scenario VSL
combined_vsl_impacts <- cowplot::plot_grid(
  cowplot::get_legend(vsl_scenario_averted_by_vaccine),
  cowplot::plot_grid(
    vsl_scenario_averted_by_vaccine + theme(legend.position = "none"),
    vsly_scenario_averted_by_vaccine + theme(legend.position = "none"),
    productive_years_averted_by_vaccine + theme(legend.position = "none"),
    hospitalisation_costs_averted_by_vaccine + theme(legend.position = "none"),
    ncol = 1, labels = c(LETTERS[1:4]), scale = 0.9),
  ncol = 1, rel_heights = c(0.1,1)
)

combined_impacts <- cowplot::plot_grid(combined_vsl_impacts, combined_npi_impacts, ncol = 2) +
  theme(plot.background = element_rect(fill = "white", color = "white"))
save_figs("combined_impacts_by_scenario", combined_impacts, width = 20, height = 13)

# ------------------------------------------------------------------------- #
# Combined Deaths, VSL and NPIs ---------------------------------------------
# ------------------------------------------------------------------------- #

# combine with scenario VSL
var <- "gain_in_openness"
ylab <- "Global Additional Days Without NPIs"
total_npis_averted_by_vaccine <- plot_medians_by_vaccine(npi_gains_global, var, ylab, prefix = "", unit = "Thousand", scale = 1e-3, flip = FALSE)

# VSL
var <- "economic_lives_saved"
ylab <- "Global Value of Statistical Life"
vsl_scenario_averted_by_vaccine <- plot_totals_by_vaccine(res_full,  var, ylab, n.breaks = 3)
save_figs("vsl_scenario_averted_by_vaccine", vsl_scenario_averted_by_vaccine, width = 10, height = 7)

var <- "deaths_averted"
ylab <- "Global Deaths Averted"
total_deaths_averted_by_vaccine <- plot_totals_by_vaccine(death_totals_global, var, ylab, prefix = "", unit = "Million", scale = 1e-6, n.breaks = 3)
save_figs("total_deaths_averted_by_vaccine", total_deaths_averted_by_vaccine, width = 10, height = 7)

combined_death_vsl_npi_impacts <-  cowplot::plot_grid(
  cowplot::plot_grid(
    total_deaths_averted_by_vaccine,
    vsl_scenario_averted_by_vaccine,
    ncol = 1, labels = c(LETTERS[1:2]), scale = 0.9
    ),
  cowplot::plot_grid(total_npis_averted_by_vaccine + theme(axis.text.x = element_text(size = 10)), scale = 0.95),
  labels = c("", "C"), rel_widths = c(1,0.9), scale = 0.975) + theme(plot.background = element_rect(fill = "white"))
save_figs("combined_death_vsl_npi_impacts", combined_death_vsl_npi_impacts, width = 20, height = 10)

# ------------------------------------------------------------------------- #
# Combined Income and VSLs ---------------------------------------------
# ------------------------------------------------------------------------- #

# Our table of impacts income wide
res_income <- readRDS("analysis/data_out/total_lifeyears_income.rds")
hospcosts_income <- readRDS("analysis/data_out/total_hospcosts_income.rds")

hecon_l <- res_income %>% filter(scenario == 1) %>%
  select(income, economic_lives_saved_med, economic_life_years_saved_med, economic_productive_loss_med) %>%
  left_join(hospcosts_income %>% filter(scenario == 1) %>%
              select(income, hospitalisation_costs_averted_med)) %>%
  pivot_longer(economic_lives_saved_med:hospitalisation_costs_averted_med) %>%
  mutate(income = factor(income, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  mutate(name = replace(name, name == "economic_lives_saved_med", "Value of Statistical Life")) %>%
  mutate(name = replace(name, name == "economic_life_years_saved_med", "Value of Statistical Life Years")) %>%
  mutate(name = replace(name, name == "economic_productive_loss_med", "Productivity Losses Averted")) %>%
  mutate(name = replace(name, name == "hospitalisation_costs_averted_med", "Hospitalization Costs Averted")) %>% split(~name) %>%
  lapply(function(x) {
    if(x$name[1] == "Hospitalization Costs Averted"){
      unit <- "Billion"
      scale <- 1e-9
    } else {
      unit <- "Trillion"
      scale <- 1e-12
    }
    x %>%
      ggplot(aes(x = income, y = value,
                 color = income, fill = income, group = interaction( income))) +
      geom_bar(stat = "identity", position = "dodge") +
      # geom_errorbar(position = position_dodge(width = 0.75), width = 0.5, color = "black") +
      ggpubr::theme_pubr(base_size = 14) +
      theme(panel.grid.major = element_line()) +
      scale_color_manual(values = rev(as.character(wbcols)), name = "    World Bank Income Group") +
      scale_fill_manual(values = rev(as.character(wbcols)), name = "    World Bank Income Group") +
      labs(x = "", y = x$name) +
      scale_y_continuous(n.breaks = 6) +
      theme(legend.text = element_text(size = 14), plot.margin = margin(0, 1, 0, 0, "cm"), panel.border = element_rect(fill = NA)) +
      scale_y_continuous(labels = scales::unit_format(prefix = "$", unit = unit, scale = scale, big.mark = "")) +
      theme(legend.position = "none")
  })

value_income <- cowplot::plot_grid(plotlist = hecon_l[c(3,4,2,1)], ncol = 4, labels = LETTERS[2:5])


income_combined <- cowplot::plot_grid(
  gg_global_income_cumulative + ggtitle(""),
  NA,
  value_income,
  ncol = 1, labels = c("A", "", ""), rel_heights = c(1,0.1,0.75)
) + theme(plot.background = element_rect(fill = "white", color = "white"))

save_figs("combined_impacts_by_income", income_combined, width = 18, height = 12)




