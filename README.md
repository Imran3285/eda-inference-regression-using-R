# 📊 Statistical Analysis Portfolio — R

A portfolio of four applied statistics projects built in R, progressing from
exploratory data analysis through to multiple regression modelling across
real-world datasets.

---

## 🎯 The Goal

Statistics is only useful if the analysis is honest — right test for the right
data, models built in the right order, and results interpreted in plain terms.

Each project in this portfolio was designed with that in mind: clear problem
statements, deliberate methodological choices, and findings explained beyond
just reporting a p-value.

---

## 📊 Datasets

| Project | Dataset | Records | Source |
|---------|---------|---------|--------|
| 1 | Gapminder | 1,704 obs × 6 vars | Jennifer Bryan / Gapminder Foundation |
| 2 | Ames Housing | 2,930 obs × 82 vars | De Cock (2011), Iowa State |
| 3 | NC Birth Records | 1,000 obs (800 complete) | OpenIntro Statistics |
| 4 | ISLR Credit | 400 obs × 13 vars | James et al. — Introduction to Statistical Learning |

---

## What I Actually Did

### Reusable Code Over Copy-Paste

Every repeated operation across projects was wrapped into a named function
rather than duplicated. This isn't just style — it reduces the chance of
making a change in one place and forgetting another.

```r
# One call handles descriptive stats for any variable — Project 1
summarise_by_continent <- function(data, var) {
  data %>% group_by(continent) %>%
    summarise(n = n(), mean = round(mean({{ var }}), 2),
              median = round(median({{ var }}), 2),
              sd = round(sd({{ var }}), 2), iqr = round(IQR({{ var }}), 2),
              .groups = "drop")
}

# Single function runs all CLT simulations — Project 2
simulate_sample_means <- function(population, n, n_sims = 5000) {
  replicate(n_sims, mean(sample(population, n)))
}

# One function produces all scatter plots — Project 4
make_scatter <- function(df, x_var, x_label) { ... }

# One function runs all residual diagnostics — Project 4
plot_diagnostics <- function(model, title_prefix = "") { ... }
```

### Appropriate Tests for the Data Type

A t-test on a binary outcome, or accuracy on a class-imbalanced dataset,
gives numbers that look fine and mean nothing. Each project uses the test
that matches the data:

- **Project 3** — Welch two-sample t-test (unequal variance not assumed,
  unlike Student's t-test which requires it)
- **Project 4** — OLS regression only after confirming numeric outcome,
  linearity, and approximate normality of residuals
- Hypothesis statements written before looking at results — not reverse-engineered from them

### Honest Model Building in Project 4

The original version of this analysis ran regressions on Income and Age
while ignoring Credit Limit entirely. The correlation matrix shows
Limit–Balance at r = 0.862 — the strongest relationship in the dataset.
Leaving it out produces a badly underspecified model and misleading Income
coefficients.

The corrected analysis builds models in deliberate sequence:

| Model | Formula | Adj. R² | What changed |
|-------|---------|---------|--------------|
| M1 (Baseline) | Balance ~ Income + Age | 0.218 | Intentionally incomplete — establishes the cost of omitting Limit |
| M2 (Best) | Balance ~ Limit + Income + Age | ~0.75 | Income **flips negative** — suppression effect revealed |
| M3 | + Gender | minimal Δ | Gender not significant once financials are controlled |
| M4 | + Income×Gender | minimal Δ | Interaction adds nothing — correctly dropped |

The Income sign flip is the analytically interesting result here. In M1,
Income appears to increase balance. Once Limit is added, Income becomes
negative — wealthier people pay down debt more readily. This is omitted
variable bias made visible, and it only appears when you let the correlation
matrix guide variable selection before modelling.

### set.seed() for Reproducibility

Project 2 runs 15,000 simulations (5,000 per sample size condition).
`set.seed(42)` is set once before all three calls so results are identical
on every run.

---

## Key Findings

### Project 1 — Global Development

```
continent     n    mean   median     sd    iqr    min    max
Africa      624   48.87    47.79   9.15  12.04  23.60  76.44
Americas    300   64.66    67.05   9.35  13.34  37.58  80.65
Asia        396   60.06    61.79  11.86  18.08  28.80  82.60
Europe      360   71.90    72.24   5.43   5.88  43.59  81.76
Oceania      24   74.33    73.66   3.80   6.35  69.12  81.24
```

The GDP–life expectancy relationship is strongly non-linear — large gains at
low GDP values, flattening sharply above ~$15,000. A log transformation of
GDP would linearise this. Asia's IQR of 18.1 years is the widest of any
continent, reflecting the economic gap between nations like Japan and
Afghanistan measured in the same dataset.

### Project 2 — Central Limit Theorem

| n | Empirical SD of means | Theoretical SE (σ/√n) |
|---|-----------------------|-----------------------|
| 10 | ~159.8 | 159.9 |
| 50 | ~71.5 | 71.5 |
| 100 | ~50.6 | 50.6 |

Empirical and theoretical values match to within rounding error across all
three conditions. The standard deviation of sample means halves when sample
size quadruples — exactly as σ/√n predicts.

### Project 3 — Hypothesis Testing

```
Welch Two Sample t-test

t = -2.3625,  df = 108.54,  p-value = 0.01994
95% CI for difference in means: [-0.663, -0.058]

nonsmoker: mean = 7.250 lbs  (n = 716)
smoker:    mean = 6.886 lbs  (n = 84)
```

**Decision: Reject H₀** at α = 0.05. Babies born to smokers weigh on average
0.36 lbs less. The confidence interval excludes zero, confirming the
difference is not attributable to chance. Worth noting: statistical
significance does not automatically imply clinical significance — 0.36 lbs
is meaningful in neonatal medicine but the wide prediction intervals suggest
individual variation is large.

### Project 4 — Regression

```
Correlation matrix (key entries):
  Limit–Balance:  r = 0.862   strongest predictor, must be included
  Income–Balance: r = 0.464   moderate, but collinear with Limit (r = 0.792)
  Age–Balance:    r = 0.002   effectively zero — Age is a weak predictor

Best model (M2) test-set prediction:
  Limit=5000, Income=30, Age=35
  Predicted balance: $XXX  |  95% PI: [$XXX, $XXX]
```

Residual diagnostics show a "swoosh" pattern in residuals vs fitted at low
fitted values — caused by the structural floor of zero-balance accounts in
the dataset, not a model failure. The Q-Q plot shows mild right-skew at the
upper tail consistent with the right-skewed Balance distribution.

---

## Plots

### Project 1

| Life Expectancy Distribution | Kernel Density |
|:---:|:---:|
| ![p1_hist](images/p1_lifeexp_histogram.png) | ![p1_density](images/p1_lifeexp_density.png) |

| GDP vs Life Expectancy | Life Expectancy by Continent |
|:---:|:---:|
| ![p1_scatter](images/p1_gdp_vs_lifeexp.png) | ![p1_box](images/p1_boxplots_by_continent.png) |

### Project 2

| Population Distribution | CLT: Effect of Sample Size |
|:---:|:---:|
| ![p2_pop](images/p2_population_distribution.png) | ![p2_clt](images/p2_clt_sample_sizes.png) |

### Project 3

![p3_box](images/p3_birthweight_by_habit.png)

### Project 4

| vs Credit Limit | vs Income | vs Age |
|:---:|:---:|:---:|
| ![p4_limit](images/p4_balance_vs_limit.png) | ![p4_income](images/p4_balance_vs_income.png) | ![p4_age](images/p4_balance_vs_age.png) |

![p4_diag](images/p4_residual_diagnostics.png)

---

## 📓 View the Full Analysis

Open **[statistics_portfolio.html](./statistics_portfolio.html)** to browse
all four projects with code, plots, and findings rendered inline — no R
installation required.

---

## Repository Structure

```
.
├── README.md
├── statistical_analysis_portfolio.R     # Full annotated script — all 4 projects
├── statistics_projects.Rmd              # R Markdown version with narrative
├── statistics_portfolio.html            # Interactive HTML — view without R
├── images/
│   ├── p1_lifeexp_histogram.png
│   ├── p1_lifeexp_density.png
│   ├── p1_observations_by_continent.png
│   ├── p1_gdp_vs_lifeexp.png
│   ├── p1_boxplots_by_continent.png
│   ├── p2_population_distribution.png
│   ├── p2_clt_sample_sizes.png
│   ├── p3_birthweight_by_habit.png
│   ├── p4_balance_vs_limit.png
│   ├── p4_balance_vs_income.png
│   ├── p4_balance_vs_age.png
│   └── p4_residual_diagnostics.png
└── .gitignore
```

---

## Reproducing the Results

```r
# Clone the repository
git clone https://github.com/Imran3285/r-statistics-portfolio.git
cd r-statistics-portfolio

# Run the full script — datasets load automatically from source URLs
source("statistical_analysis_portfolio.R")

# Or knit the R Markdown for formatted output with all plots inline
rmarkdown::render("statistics_projects.Rmd")
```

All datasets load automatically from their original URLs — no manual
downloads required. Random seed is fixed at 42 in Project 2 — results are
fully reproducible.

---

## Stack

R · ggplot2 · dplyr · tidyverse · statsr

---

## Author

**Muhammad Imran**  
Data Scientist
