# aggarwal_etal_2023/maintained/figure_b2_acronym_spending.R
# Output: output/figure_b2_acronym_spending.{pdf,png,csv}
# Depends on: original/ad_library_2020_acronym.csv, helpers.R
# Description: Supplementary Figure 2. Lower bound of Acronym's Facebook
#   spending on advertisements naming Biden or Trump, by month, split by
#   advertisement format and by keyword. Turnout advertisements are excluded.

source(here::here("maintained", "helpers.R"))

acronym_ads <- load_acronym_ads() |> filter(!is_turnout)

by_format <-
  acronym_ads |>
  group_by(ad_format, month) |>
  summarize(spend_lb = sum(spend_lb), n_ads = n(), .groups = "drop")

by_keyword <-
  acronym_ads |>
  group_by(ad_type, month) |>
  summarize(spend_lb = sum(spend_lb), n_ads = n(), .groups = "drop")

g_format <-
  ggplot(by_format, aes(x = month, y = spend_lb, color = ad_format,
                        shape = ad_format)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(name = "Ad Format", palette = "Set2") +
  scale_shape_discrete(name = "Ad Format") +
  scale_y_continuous(labels = label_dollar(), limits = c(0, 6e5)) +
  scale_x_date(labels = label_date(format = "%B %Y")) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  ) +
  labs(y = "ACRONYM Facebook Spending, Lower Bound")

g_keyword <-
  ggplot(by_keyword, aes(x = month, y = spend_lb, group = ad_type,
                         color = ad_type, shape = ad_type)) +
  geom_line() +
  geom_point() +
  scale_color_brewer(name = "Keyword", palette = "Set1") +
  scale_shape_discrete(name = "Keyword") +
  scale_y_continuous(labels = label_dollar(), limits = c(0, 6e5)) +
  scale_x_date(labels = label_date(format = "%B %Y")) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  ) +
  labs(y = "ACRONYM Facebook Spending, Lower Bound")

g <- g_format + g_keyword

ggsave(here::here("maintained", "output", "figure_b2_acronym_spending.pdf"),
       plot = g, height = 4, width = 8)
ggsave(here::here("maintained", "output", "figure_b2_acronym_spending.png"),
       plot = g, height = 4, width = 8, dpi = 300)

bind_rows(
  by_format |> rename(group = ad_format) |> mutate(panel = "Ad Format"),
  by_keyword |> rename(group = ad_type) |> mutate(panel = "Keyword")
) |>
  mutate(group = str_replace_all(group, "\\n", " ")) |>
  select(panel, group, month, spend_lb, n_ads) |>
  arrange(panel, group, month, .locale = "en") |>
  write_csv(here::here("maintained", "output", "figure_b2_acronym_spending.csv"))
