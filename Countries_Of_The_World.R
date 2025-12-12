
library(dplyr)
library(ggplot2)

data_raw <- read.csv("countries of the world.csv", stringsAsFactors = FALSE)


data_clean <- data_raw %>%
  rename(
    GDP = GDP....per.capita.,
    InfantMortality_raw = Infant.mortality..per.1000.births.,
    Region = Region
  ) %>%
  mutate(
    GDP = as.numeric(GDP),
    InfantMortality = as.numeric(gsub(",", ".", InfantMortality_raw)),
    Region = trimws(Region)   # tidy region labels
  ) %>%
  filter(!is.na(GDP), !is.na(InfantMortality))

p_hist <- ggplot(data_clean, aes(x = InfantMortality)) +
  geom_histogram(bins = 30, colour = "black", fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Infant Mortality Across Countries",
    x = "Infant mortality (per 1000 births)",
    y = "Number of countries"
  ) +
  theme_minimal(base_size = 13)

print(p_hist)

p_scatter <- ggplot(data_clean, aes(x = GDP, y = InfantMortality, colour = Region)) +
  geom_point(size = 2, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, colour = "black") +
  labs(
    title = "Relationship Between GDP per Capita and Infant Mortality",
    x = "GDP per capita (US dollars)",
    y = "Infant mortality (per 1000 births)",
    colour = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

print(p_scatter)

p_scatter_log <- ggplot(data_clean, aes(x = log10(GDP), y = InfantMortality, colour = Region)) +
  geom_point(size = 2, alpha = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, colour = "black") +
  labs(
    title = "GDP per Capita (log10) vs Infant Mortality",
    x = "log10(GDP per capita)",
    y = "Infant mortality (per 1000 births)",
    colour = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

print(p_scatter_log)

cor_result <- cor.test(data_clean$GDP, data_clean$InfantMortality, method = "pearson")
print(cor_result)

