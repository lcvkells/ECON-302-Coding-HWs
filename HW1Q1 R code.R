

# install.packages("tidyverse")
# install.packages("stargazer")
# NOTE TO SELF: TO commit git: git pull, make changes, save, git commit -m "Clear message describing what you changed", git push

#LOAD PACKAGES
library(tidyverse)
library(stargazer)


# ECON 302 GDP HW1Q1
# PART 1
# NOMINAL AND REAL GDP

# Reading CSV & removing unnecessary headers
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

GDP_REAL_long <- GDP_NOMINAL_1961to2025 |>
  filter(Estimates == "Gross domestic product at market prices") |>
  mutate(across(-Estimates, as.numeric)) |>
  pivot_longer(cols = -Estimates,
               names_to = "quarter",
               values_to = "gdp_real") |>
  mutate(time = row_number()) |>
  drop_na(gdp_real)

# Sanity Checks  
nrow(GDP_NOMINAL_long)
sum(is.na(GDP_NOMINAL_long$gdp_nominal))
head(GDP_NOMINAL_long[, c("quarter","gdp_nominal","time")])
sum(is.na(GDP_REAL_long$gdp_real))

# Plotting graphs
ggplot(GDP_NOMINAL_long, aes(x = time, y = gdp_nominal)) +
  geom_line(color ="green", linewidth=1) +
  scale_x_continuous(breaks = seq(1, max(CIG_plot$time), 
                                  by = 8), # every 10 yrs
                     labels = seq(1961, 2025, 
                                  by = 2)) +
  labs(title = "Nominal GDP in Canada (Quarterly)",
       x = "Year",
       y = "GDP (Current Prices, Millions of Dollars)") +
  theme_minimal()

ggplot(GDP_REAL_long, aes(x = time, y = gdp_real)) +
  geom_line(color ="pink", linewidth=1) +
  scale_x_continuous(breaks = seq(1, max(CIG_plot$time), 
                                  by = 40),
                     labels = seq(1961, 2025, 
                                  by = 10)) +
  labs(title = "Real GDP in Canada (Quarterly)",
       x = "Year",
       y = "GDP (Current Prices, Millions of Dollars)") +
  theme_minimal()


# PART 2
# FINDING CONSUMPTION, INVESTMENTS, AND GOVERNMENT SPENDING
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


# PART 3 
# GMI RATIO POST 1997

GDP_IncomeBased_1997to2025 <- read.csv("GDP_Income1997-3610010301-noSymbol.csv",
                                       skip = 10)
head(GDP_IncomeBased_1997to2025)
colnames(GDP_IncomeBased_1997to2025)

# Filtering
gross_mixed_income <- GDP_IncomeBased_1997to2025 |>
  filter(Estimates=="Gross mixed income") |>
  pivot_longer(cols = -Estimates,
               names_to = "quarter",
               values_to = "GMI") |>
  mutate(GMI = as.numeric(gsub(",", "", GMI))) |>
  drop_na(GMI)

gross_domestic_income <- GDP_IncomeBased_1997to2025 |>
  filter(Estimates=="Gross domestic product at market prices") |>
  pivot_longer(cols = -Estimates,
               names_to = "quarter",
               values_to = "GDI") |>
  mutate(GDI = as.numeric(gsub(",", "", GDI))) |>
  drop_na(GDI)

GMI_ratio_1997 <- tibble(quarter = gross_mixed_income$quarter,
                         ratio = gross_mixed_income$GMI/gross_domestic_income$GDI)|>
  mutate(year = as.integer(substr(quarter, nchar(quarter)-3, nchar(quarter))),
         qtr  = as.integer(substr(quarter, 2, 2)),
         time = (year - 1997) * 4 + qtr) |>
  arrange(time)
GMI_ratio_1997
colnames(GMI_ratio_1997)


ggplot(GMI_ratio_1997, aes(x = time, y = ratio)) +
  geom_line(color = "purple", linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0.5, max(GMI_ratio_1997$time), by = 4),
    labels = seq(1997, 2025, by = 1)) +
  labs( title = "Gross Mixed Income as a Share of Total Income in Canada",
        x = "Year",
        y = "Gross Mixed Income / GDP (Income-based)") +
  theme_minimal()

# PART 4
# REAL GDP 2019-2025

GDP_industry_2019to2025 <- read.csv("GDP_Real_2019-2025-3610044901-noSymbol.csv",
                                    skip = 11) 
names(GDP_industry_2019to2025)

GDP_industry <- GDP_industry_2019to2025 |>
  rename(Industry = "North.American.Industry.Classification.System..NAICS..3")
colnames(GDP_industry)

GDP_industry_long <- GDP_industry |>
  pivot_longer(cols = -Industry,
               names_to = "period",
               values_to = "GDP") |>
  mutate(GDP = as.numeric(gsub(",", "", GDP))) |>
  drop_na(GDP)

head(GDP_industry_long)

GDP_industry_annual_wages <- GDP_industry_long |>
  mutate(year = as.integer(substr(.data$period, 4, 7))) |>
  group_by(Industry, year) |>
  summarise(GDP = mean(GDP), .groups = "drop")
head(GDP_industry_annual_wages)

balanced_GDP_industry <- GDP_industry_annual_wages |>
  group_by(Industry) |>
  filter(n_distinct(year) == 7) |>
  ungroup() #to filter for industries that have data for all years


#computing!
industry_growth <- balanced_GDP_industry |>
  filter(year %in% c(2019, 2025)) |>
  pivot_wider(names_from = year,
              values_from = GDP) |>
  mutate(growth = (`2025` - `2019`) / `2019`)


#industry_growth <- industry_growth |>
#  mutate(classification = case_when(
#      growth > mean(industry_growth$growth) + sd(industry_growth$growth) ~ "More than 1 SD above mean",
#      growth < mean(industry_growth$growth) - sd(industry_growth$growth) ~ "More than 1 SD below mean",
#      TRUE ~ "Within 1 SD of mean"))

top3 <- industry_growth |>
  arrange(desc(growth)) |>
  slice(1:3)

bottom3 <- industry_growth |>
  arrange(growth) |>
  slice(1:3)

final_table <- bind_rows(top3, bottom3) |>
  select(Industry, `2019`, `2025`, growth)


stargazer(final_table,
          type = "html",
          summary = FALSE,
          digits = 4,
          title = "Top 3 and Bottom 3 Industries by Real GDP Growth (2019–2025)",
          out = "industry_growth.html")

browseURL("industry_growth.html")

# FINAL VERSION -- COMMIT TO GIT





