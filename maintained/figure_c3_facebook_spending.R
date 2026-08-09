# aggarwal_etal_2023/maintained/figure_c3_facebook_spending.R
# Output: output/figure_c3_facebook_spending.{pdf,png,csv}
# Depends on: original/ad_library_2020.csv, helpers.R
# Description: Supplementary Figure 3. Weekly lower bound of Facebook spending
#   by all advertisers on advertisements naming Biden or Trump in the programme's
#   states. The deposited extract is 1.1 million advertisements and 433 MB, so
#   this is the slowest script in the pipeline.

source(here::here("maintained", "helpers.R"))

ad_library <-
  here::here("original", "ad_library_2020.csv") |>
  read_csv(show_col_types = FALSE) |>
  mutate(
    week = as.Date(floor_date(ad_delivery_start_time, unit = "week")),
    spend_ub = if_else(spend_ub == -1, 1000000, spend_ub)
  )

weekly <-
  ad_library |>
  filter(week >= as.Date("2020-02-01")) |>
  group_by(week) |>
  summarize(n_ads = n(), spend_lb = sum(spend_lb), spend_ub = sum(spend_ub),
            .groups = "drop")

g <-
  ggplot(weekly, aes(x = week, y = spend_lb)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.Date("2020-11-03"), color = "red",
             linetype = "dashed") +
  geom_text(x = as.Date("2020-11-10"), y = 15e6, label = "Election Day",
            colour = "red", angle = -90) +
  scale_y_continuous(labels = label_dollar()) +
  scale_x_date(labels = label_date(format = "%B %Y")) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Facebook Spending, Lower Bound (weekly)", x = "Date")

ggsave(here::here("maintained", "output", "figure_c3_facebook_spending.pdf"),
       plot = g, height = 4, width = 6)
ggsave(here::here("maintained", "output", "figure_c3_facebook_spending.png"),
       plot = g, height = 4, width = 6, dpi = 300)

write_csv(weekly, here::here("maintained", "output", "figure_c3_facebook_spending.csv"))

# The appendix quotes the lower bound over the whole extract rather than over the
# weeks the figure draws, so it is written here beside the figure it belongs to.
tibble(
  quantity = c("ad_library_total_ads", "ad_library_total_spend_lb",
               "ad_library_acronym_spend_lb"),
  value = c(nrow(ad_library), sum(ad_library$spend_lb),
            sum(ad_library$spend_lb[ad_library$is_acronym]))
) |>
  write_csv(here::here("maintained", "output", "text_ad_library_totals.csv"))
