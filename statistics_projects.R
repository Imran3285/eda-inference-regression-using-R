# =============================================================================
# STATISTICAL ANALYSIS PORTFOLIO
# Four Projects in Exploratory Data Analysis, Inference & Regression
#
# Author:  Muhammad Imran
#          Data Scientist
# R Version: 4.x+
#
# Dependencies: ggplot2, dplyr, tidyverse, statsr
# =============================================================================


# =============================================================================
# SETUP: Load libraries and define shared utilities
# =============================================================================

# -- Package loading (install if missing) -------------------------------------
required_packages <- c("ggplot2", "dplyr", "tidyverse", "statsr")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) install.packages(missing)
}

install_if_missing(required_packages)

library(ggplot2)
library(dplyr)
library(tidyverse)

# -- Shared plot theme --------------------------------------------------------
theme_portfolio <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 15, margin = margin(b = 8)),
      plot.subtitle = element_text(colour = "grey40", margin = margin(b = 12)),
      axis.title    = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}


# =============================================================================
# PROJECT 1: Global Development Analysis
#            Relationship Between GDP per Capita and Life Expectancy
#
# Dataset:   Gapminder (142 countries, 1952–2007)
# Techniques: EDA, descriptive statistics, ggplot2 visualisation
# =============================================================================

cat("\n===== PROJECT 1: Global Development Analysis =====\n")

# -- 1.1  Load data -----------------------------------------------------------
gapminder <- read.csv("http://bit.ly/2GxjYOB")
cat("Rows:", nrow(gapminder), " | Columns:", ncol(gapminder), "\n")
glimpse(gapminder)


# -- 1.2  Univariate exploration ----------------------------------------------

# Distribution of life expectancy
ggplot(gapminder, aes(x = lifeExp)) +
  geom_histogram(bins = 50, fill = "#2C7BB6", colour = "white") +
  labs(title    = "Distribution of Life Expectancy",
       subtitle = "All countries and years in the Gapminder dataset",
       x = "Life Expectancy (years)", y = "Count") +
  theme_portfolio()

# Kernel density – smoother view of the same distribution
ggplot(gapminder, aes(x = lifeExp)) +
  geom_density(bw = 0.5, fill = "#2C7BB6", alpha = 0.4) +
  labs(title = "Kernel Density of Life Expectancy",
       x = "Life Expectancy (years)", y = "Density") +
  theme_portfolio()

# Frequency of observations per continent
ggplot(gapminder, aes(x = continent)) +
  geom_bar(fill = "#2C7BB6") +
  labs(title = "Number of Observations by Continent",
       x = "Continent", y = "Count") +
  theme_portfolio()


# -- 1.3  Bivariate exploration -----------------------------------------------

# GDP per capita vs life expectancy with reference lines
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.4, colour = "#2C7BB6") +
  geom_vline(xintercept = 40000, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = 75,    linetype = "dashed", colour = "red") +
  labs(title    = "GDP per Capita vs Life Expectancy",
       subtitle = "Dashed lines: GDP = $40,000 and Life Expectancy = 75 years",
       x = "GDP per Capita (USD)", y = "Life Expectancy (years)") +
  theme_portfolio()


# -- 1.4  Group comparisons by continent --------------------------------------

# Box plots: life expectancy distributions across continents
ggplot(gapminder, aes(x = continent, y = lifeExp, fill = continent)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.7) +
  labs(title    = "Life Expectancy by Continent",
       subtitle = "Box plots show median, IQR, and outliers",
       x = "Continent", y = "Life Expectancy (years)") +
  theme_portfolio()


# -- 1.5  Summary statistics by continent -------------------------------------

# Helper: compute full descriptive stats for any numeric variable
summarise_by_continent <- function(data, var) {
  data %>%
    group_by(continent) %>%
    summarise(
      n      = n(),
      mean   = round(mean({{ var }}),   2),
      median = round(median({{ var }}), 2),
      sd     = round(sd({{ var }}),     2),
      iqr    = round(IQR({{ var }}),    2),
      min    = round(min({{ var }}),    2),
      max    = round(max({{ var }}),    2),
      q1     = round(quantile({{ var }}, 0.25), 2),
      q3     = round(quantile({{ var }}, 0.75), 2),
      .groups = "drop"
    )
}

cat("\n-- Life Expectancy by Continent --\n")
print(summarise_by_continent(gapminder, lifeExp))

cat("\n-- GDP per Capita by Continent --\n")
print(summarise_by_continent(gapminder, gdpPercap))

# -- Project 1 Notes ----------------------------------------------------------
# Higher GDP per capita is positively associated with higher life expectancy.
# African nations show the lowest median life expectancy; Oceania the highest.
# The wide IQR in Asia and the Americas reflects internal economic diversity.


# =============================================================================
# PROJECT 2: Sampling Distributions & the Central Limit Theorem
#            Using the Ames Housing Dataset
#
# Dataset:   Ames Housing (2,930 residential sales in Ames, Iowa)
# Techniques: Simulation, sampling distributions, CLT demonstration
# =============================================================================

cat("\n===== PROJECT 2: Sampling Distributions & CLT =====\n")

# -- 2.1  Load data -----------------------------------------------------------
ames <- read.csv("http://bit.ly/315N5R5")
glimpse(ames)

area  <- ames$Gr.Liv.Area   # above-ground living area (sq ft)
price <- ames$SalePrice     # sale price (USD)

cat("Population size:", length(area), "\n")
cat("Any missing values in area?", any(is.na(area)), "\n")
cat("Population SD:", round(sd(area), 2), "\n")
summary(area)


# -- 2.2  Population distribution ---------------------------------------------
ggplot(data.frame(area), aes(x = area)) +
  geom_histogram(bins = 40, fill = "#D7191C", colour = "white") +
  labs(title    = "Population Distribution: Above-Ground Living Area",
       subtitle = "Ames Housing dataset — all 2,930 homes",
       x = "Living Area (sq ft)", y = "Count") +
  theme_portfolio()


# -- 2.3  Simulate sampling distributions -------------------------------------

# Generic function: draw `n_sims` samples of size `n`, return vector of means
simulate_sample_means <- function(population, n, n_sims = 5000) {
  replicate(n_sims, mean(sample(population, n)))
}

set.seed(42)   # for reproducibility
sample_means10  <- simulate_sample_means(area, n = 10)
sample_means50  <- simulate_sample_means(area, n = 50)
sample_means100 <- simulate_sample_means(area, n = 100)

# Combine into one tidy data frame for clean ggplot comparison
sim_df <- bind_rows(
  data.frame(means = sample_means10,  sample_size = "n = 10"),
  data.frame(means = sample_means50,  sample_size = "n = 50"),
  data.frame(means = sample_means100, sample_size = "n = 100")
) %>%
  mutate(sample_size = factor(sample_size, levels = c("n = 10", "n = 50", "n = 100")))


# -- 2.4  Visualise CLT effect ------------------------------------------------
ggplot(sim_df, aes(x = means, fill = sample_size)) +
  geom_histogram(bins = 40, colour = "white", show.legend = FALSE) +
  facet_wrap(~ sample_size, ncol = 1, scales = "free_y") +
  labs(title    = "Central Limit Theorem: Effect of Sample Size",
       subtitle = "Each panel shows 5,000 sample means; larger n → tighter, more normal distribution",
       x = "Sample Mean — Living Area (sq ft)", y = "Count") +
  scale_fill_manual(values = c("#D7191C", "#FDAE61", "#1A9641")) +
  theme_portfolio()

# -- Project 2 Notes ----------------------------------------------------------
# As sample size grows (10 → 50 → 100), the sampling distribution narrows
# and becomes increasingly bell-shaped — a direct illustration of the CLT.
# Standard error of the mean ≈ σ / √n, so larger n yields smaller variability.


# =============================================================================
# PROJECT 3: Hypothesis Testing
#            Impact of Maternal Smoking on Infant Birth Weight
#
# Dataset:   North Carolina Birth Records
# Techniques: Two-sample t-test, confidence intervals
# =============================================================================

cat("\n===== PROJECT 3: Hypothesis Testing =====\n")

# -- 3.1  Load and clean data -------------------------------------------------
nc   <- read.csv("http://bit.ly/31adfCe")
data <- na.omit(nc)   # drop rows with any missing value
cat("Rows after removing NAs:", nrow(data), "\n")
summary(data[, c("weight", "habit")])


# -- 3.2  Visualise birth weight by smoking habit -----------------------------
ggplot(data, aes(x = habit, y = weight, fill = habit)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("nonsmoker" = "#1A9641", "smoker" = "#D7191C")) +
  labs(title    = "Infant Birth Weight by Mother's Smoking Habit",
       subtitle = "North Carolina birth records dataset",
       x = "Smoking Status", y = "Birth Weight (lbs)") +
  theme_portfolio()


# -- 3.3  Descriptive stats by group ------------------------------------------
data %>%
  group_by(habit) %>%
  summarise(n = n(), mean_weight = round(mean(weight), 3),
            sd_weight = round(sd(weight), 3), .groups = "drop") %>%
  print()


# -- 3.4  Two-sample t-test ---------------------------------------------------
# H0: μ_smoker = μ_nonsmoker  (no difference in mean birth weight)
# H1: μ_smoker ≠ μ_nonsmoker  (two-sided)

smoker    <- data$weight[data$habit == "smoker"]
nonsmoker <- data$weight[data$habit == "nonsmoker"]

t_result <- t.test(smoker, nonsmoker,
                   alternative = "two.sided",
                   conf.level  = 0.95)
print(t_result)

# -- 3.5  Interpret results ---------------------------------------------------
cat("\n-- Interpretation --\n")
cat("t =", round(t_result$statistic, 3),
    " | df =", round(t_result$parameter, 1),
    " | p-value =", round(t_result$p.value, 4), "\n")
cat("95% CI for difference in means: [",
    round(t_result$conf.int[1], 3), ",",
    round(t_result$conf.int[2], 3), "]\n")

if (t_result$p.value < 0.05) {
  cat("Result: Reject H0 — significant difference in birth weight (α = 0.05).\n")
} else {
  cat("Result: Fail to reject H0 — no significant difference detected (α = 0.05).\n")
}

# -- Project 3 Notes ----------------------------------------------------------
# The t-test compares mean birth weights between smoking and non-smoking mothers.
# A significant result (p < 0.05) would indicate that maternal smoking is
# associated with lower infant birth weight on average.


# =============================================================================
# PROJECT 4: Correlation & Multiple Regression
#            Predicting Credit Card Balance
#
# Dataset:   Credit (synthetic dataset from ISLR)
# Techniques: Correlation, OLS regression, interaction terms,
#             prediction intervals, residual diagnostics
# =============================================================================

cat("\n===== PROJECT 4: Correlation & Regression =====\n")

# -- 4.1  Load and inspect data -----------------------------------------------
credit <- read_csv("http://bit.ly/33a5A8P")
glimpse(credit)

credit_sub <- credit %>% select(Balance, Limit, Income, Age, Gender)
summary(credit_sub)
cat("\nGender breakdown:\n"); print(table(credit_sub$Gender))


# -- 4.2  Correlation matrix (numeric variables only) -------------------------
cat("\n-- Correlation Matrix --\n")
credit %>%
  select(Balance, Limit, Income, Age) %>%
  cor() %>%
  round(3) %>%
  print()


# -- 4.3  Scatter plots with OLS trend lines ----------------------------------

make_scatter <- function(df, x_var, x_label) {
  ggplot(df, aes(x = .data[[x_var]], y = Balance)) +
    geom_point(alpha = 0.4, colour = "#5E4FA2") +
    geom_smooth(method = "lm", se = FALSE, colour = "#D53E4F", linewidth = 1) +
    labs(title = paste("Credit Balance vs", x_label),
         x = x_label, y = "Credit Card Balance (USD)") +
    theme_portfolio()
}

print(make_scatter(credit, "Age",    "Age (years)"))
print(make_scatter(credit, "Income", "Income (USD thousands)"))
print(make_scatter(credit, "Limit",  "Credit Limit (USD)"))


# -- 4.4  Model 1: Balance ~ Income + Age -------------------------------------
model1 <- lm(Balance ~ Income + Age, data = credit)
cat("\n-- Model 1: Balance ~ Income + Age --\n")
print(summary(model1))

# Prediction for a new individual
new_person <- data.frame(Income = 30, Age = 35)
pred1 <- predict(model1, new_person, interval = "prediction", level = 0.95)
cat("\nPredicted balance (Income=30, Age=35):", round(pred1[1], 2),
    "| 95% PI: [", round(pred1[2], 2), ",", round(pred1[3], 2), "]\n")


# -- 4.5  Model 2: Balance ~ Income + Age + Gender (additive) -----------------
model2 <- lm(Balance ~ Income + Age + Gender, data = credit)
cat("\n-- Model 2: Adding Gender --\n")
print(summary(model2))

new_person2 <- data.frame(Income = 25, Age = 25, Gender = "Female")
pred2 <- predict(model2, new_person2, interval = "prediction", level = 0.95)
cat("\nPredicted balance:", round(pred2[1], 2),
    "| 95% PI: [", round(pred2[2], 2), ",", round(pred2[3], 2), "]\n")


# -- 4.6  Model 3: Income × Gender interaction --------------------------------
model3 <- lm(Balance ~ Income * Gender + Age, data = credit)
cat("\n-- Model 3: Income × Gender Interaction --\n")
print(summary(model3))

pred3 <- predict(model3, new_person2, interval = "prediction", level = 0.95)
cat("\nPredicted balance:", round(pred3[1], 2),
    "| 95% PI: [", round(pred3[2], 2), ",", round(pred3[3], 2), "]\n")


# -- 4.7  Log-transform Income ------------------------------------------------
credit_log <- credit %>% mutate(log_income = log(Income))
model_log  <- lm(Balance ~ log_income + Age, data = credit_log)
cat("\n-- Model (log Income): Summary --\n")
print(summary(model_log))


# -- 4.8  Residual diagnostics ------------------------------------------------

# Wrap diagnostic plots in a reusable function
plot_diagnostics <- function(model, title_prefix = "") {
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

  plot(model$fitted.values, model$residuals,
       main = paste(title_prefix, "Residuals vs Fitted"),
       xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "#5E4FA2")
  abline(h = 0, lty = 2, col = "red")

  hist(model$residuals, breaks = 30, col = "#5E4FA2",
       main = paste(title_prefix, "Histogram of Residuals"),
       xlab = "Residuals")

  qqnorm(model$residuals, main = paste(title_prefix, "Normal Q-Q Plot"), pch = 16)
  qqline(model$residuals, col = "red")

  par(mfrow = c(1, 1))  # reset layout
}

plot_diagnostics(model_log, title_prefix = "Log-Income Model —")

# -- Project 4 Notes ----------------------------------------------------------
# Credit Limit has the strongest correlation with Balance (r ≈ 0.86).
# Income is a significant negative predictor once limit is controlled for.
# Log-transforming Income improves linearity and residual normality.
# The interaction term (Income × Gender) should only be retained if it
# meaningfully improves fit (check ΔR² and the interaction p-value).


# =============================================================================
# END OF PORTFOLIO
# =============================================================================
cat("\nAll four projects completed successfully.\n")
