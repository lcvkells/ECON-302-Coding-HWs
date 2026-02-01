# ECON 302 GDP HW1Q1.1

# install.packages("tidyverse")
# install.packages(c("cansim", "statcanR")) # used after q1.1

#LOAD PACKAGES
library(tidyverse)

# READING CSV & removing unnecessary headers
GDP_NOMINAL_1961to2025 <- read_csv("3610010401-noSymbol-current_prices.csv",
                                   skip = 11) 
# Nominal GDP, period: quarterly ; headers removed
head(GDP_NOMINAL_1961to2025)
colnames(GDP_NOMINAL_1961to2025)

GDP_REAL_1961to2025 <- read_csv("3610010401-noSymbol-chained2017dollars.csv",
                                skip = 11) 
# Real GDP, period: quarterly ; headers removed
head(GDP_REAL_1961to2025)
colnames(GDP_NOMINAL_1961to2025)


# Cleaning & Querying the dataset
GDP_NOMINAL_long <- GDP_NOMINAL_1961to2025 |>
  filter(Estimates == "Gross domestic product at market prices") |>
  mutate(across(-Estimates, as.numeric)) |>
  pivot_longer(cols = -Estimates,
               names_to = "quarter",
               values_to = "gdp_nominal") |>
  mutate(time = row_number()) |>
  drop_na(gdp_nominal)

nrow(GDP_NOMINAL_long)
sum(is.na(GDP_NOMINAL_long$gdp_nominal))
head(GDP_NOMINAL_long[, c("quarter","gdp_nominal","time")])

GDP_REAL_long <- GDP_NOMINAL_1961to2025 |>
  filter(Estimates == "Gross domestic product at market prices") |>
  mutate(across(-Estimates, as.numeric)) |>
  pivot_longer(cols = -Estimates,
               names_to = "quarter",
               values_to = "gdp_real") |>
  mutate(time = row_number()) |>
  drop_na(gdp_real)

sum(is.na(GDP_REAL_long$gdp_real))

# Plotting graphs
ggplot(GDP_NOMINAL_long, aes(x = time, y = gdp_nominal)) +
  geom_line(color ="lightgreen", linewidth=1.3) +
  scale_x_continuous(breaks = seq(1, max(CIG_plot$time), 
                                  by = 40), # every 10 yrs
                     labels = seq(1961, 2025, 
                                  by = 10)) +
  labs(title = "Nominal GDP in Canada (Quarterly)",
       x = "Year",
       y = "GDP (Current Prices, Millions of Dollars)") +
  theme_minimal()

ggplot(GDP_REAL_long, aes(x = time, y = gdp_real)) +
  geom_line(color ="pink", linewidth=1.3) +
  scale_x_continuous(breaks = seq(1, max(CIG_plot$time), 
                                  by = 40),
                     labels = seq(1961, 2025, 
                                  by = 10)) +
  labs(title = "Real GDP in Canada (Quarterly)",
       x = "Year",
       y = "GDP (Current Prices, Millions of Dollars)") +
  theme_minimal()


# ECON 302 GDP HW1Q1.2

# Consumption using REAL GDP

C_real <- GDP_REAL_1961to2025 |> 
  filter(Estimates == "Household final consumption expenditure") |>
  mutate(across(-Estimates, as.numeric)) |>
  pivot_longer(cols = -Estimates,
               names_to = "quarter",
               values_to = "consumption") |>
  drop_na(consumption) |>
  mutate(time = row_number())

# Investments using REAL GDP

I_real <- GDP_REAL_1961to2025 |>
  filter(Estimates == "Gross fixed capital formation") |>
  mutate(across(-Estimates, as.numeric)) |>
  pivot_longer(cols = -Estimates,
               names_to = "quarter",
               values_to = "investment") |>
  drop_na(investment) |>
  mutate(time = row_number())

# Government Spending using REAL GDP

G_real <- GDP_REAL_1961to2025 |>
  filter(Estimates == "General governments final consumption expenditure") |>
  mutate(across(-Estimates, as.numeric)) |>
  pivot_longer(cols = -Estimates, 
               names_to = "quarter",
               values_to = "government") |>
  drop_na(government) |>
  mutate(time = row_number())

summary(G_real$government)


# Double Checking Results

GDP_REAL_1961to2025 |>
  filter(Estimates %in% c("Household final consumption expenditure",
                          "Gross fixed capital formation",
                          "General governments final consumption expenditure")) |>
  select(Estimates, `Q1 1961`, `Q2 1961`, `Q3 1961`, `Q4 1961`)

# Visualization
C_plot <- C_real |>
  select(time, value = consumption) |>
  mutate(series = "Consumption")
I_plot <- I_real |>
  select(time, value = investment) |>
  mutate(series = "Investment")
G_plot <- G_real |>
  select(time, value = government) |>
  mutate(series = "Government")

CIG_plot <- bind_rows(C_plot, I_plot, G_plot)

ggplot(CIG_plot, aes(x = time, y = value, color = series)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    breaks = seq(1, max(CIG_plot$time), 
                 by = 20),  # every 10 years
    labels = seq(1961, 2025, 
                 by = 5)) +
  labs(title = "Real Consumption, Investment, and Government Spending in Canada",
       x = "Year",
       y = "Chained 2017 dollars (millions)",
       color = "Series") +
  theme_minimal()

# testing on second computers




# ECON 302 GDP HW1Q1.3




