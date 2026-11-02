library(dplyr)
library(ggplot2)

analysis_data <- read.csv(
  "./data/processed/data.csv",
  na.strings = "",
  check.names = FALSE
)

conflict_matmordiff <- analysis_data|>
  dplyr::select(iso, year, matmor) |>
  dplyr::filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(diff_matmor = matmor - matmor[1L]) |>
  # keep only countries whose matmor was higher in 2017 than in 2000
  filter(any(year == 2017 & diff_matmor > 0, na.rm = TRUE)) |>
  ungroup()

# maternal mortality trend over time, one coloured line per country
fig_matmor <- ggplot(conflict_matmordiff,
                     aes(x = year, y = matmor, colour = iso, group = iso)) +
  geom_line(alpha = 0.6) +
  labs(x = "Year",
       y = "Maternal mortality (per 100,000 live births)",
       colour = "Country") +
  theme_bw()

fig_matmor
ggsave("./figure/fig_matmor.png")
