# ============================================================
# Central Limit Theorem Project
# ============================================================

# install.packages("moments")

library(ggplot2)
library(moments)

# Reproducibility
set.seed(6512)

# Number of simulations
num_simulations <- 10000

# Sample sizes to investigate
sample_sizes <- c(5, 10, 20, 30, 50, 100, 200)

# ============================================================
# GENERAL SIMULATION FUNCTION
# ============================================================

simulate_means <- function(n, num_simulations, population_function) {
  sample_means <- numeric(num_simulations)
  for (i in 1:num_simulations) {
    sample <- population_function(n)
    sample_means[i] <- mean(sample)
  }
  return(sample_means)
}

# ============================================================
# 1. STANDARD NORMAL POPULATION
# ============================================================

# Population function
normal_population <- function(n) {
  
  rnorm(n, mean = 0, sd = 1)
}

# Theoretical population values
normal_mean <- 0
normal_sd <- 1

normal_mean
normal_sd

# Run simulations for every sample size
normal_results <- list()
for (n in sample_sizes) {
  normal_results[[as.character(n)]] <- simulate_means(
    n = n,
    num_simulations = num_simulations,
    population_function = normal_population
  )
}

# Create summary table
normal_summary <- data.frame(
  n = sample_sizes,
  simulated_mean = NA,
  simulated_sd = NA,
  theoretical_se = NA,
  skewness = NA,
  kurtosis = NA
)

# Fill in summary table
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  means <- normal_results[[as.character(n)]]
  normal_summary$simulated_mean[i] <- mean(means)
  normal_summary$simulated_sd[i] <- sd(means)
  normal_summary$theoretical_se[i] <- 1 / sqrt(n)
  normal_summary$skewness[i] <- skewness(means)
  normal_summary$kurtosis[i] <- kurtosis(means)
}

normal_summary

# Compare simulated SE to theoretical SE
normal_summary$SE_difference <-
  normal_summary$simulated_sd -
  normal_summary$theoretical_se

normal_summary$SE_percent_difference <-
  abs(
    normal_summary$simulated_sd -
      normal_summary$theoretical_se
  ) /
  normal_summary$theoretical_se * 100

normal_summary

# Normal Histograms
# ----------------------------

hist(
  normal_results[["5"]],
  main = "Standard Normal Sampling Distribution, n = 5",
  xlab = "Sample Mean"
)

hist(
  normal_results[["30"]],
  main = "Standard Normal Sampling Distribution, n = 30",
  xlab = "Sample Mean"
)

hist(
  normal_results[["100"]],
  main = "Standard Normal Sampling Distribution, n = 100",
  xlab = "Sample Mean"
)

# Normal Q-Q Plots
# ----------------------------

qqnorm(
  normal_results[["5"]],
  main = "Q-Q Plot: Standard Normal, n = 5"
)

qqline(normal_results[["5"]])

qqnorm(
  normal_results[["30"]],
  main = "Q-Q Plot: Standard Normal, n = 30"
)

qqline(normal_results[["30"]])

qqnorm(
  normal_results[["100"]],
  main = "Q-Q Plot: Standard Normal, n = 100"
)

qqline(normal_results[["100"]])

# Normal n = 30 Details
# ----------------------------

normal_30 <- normal_results[["30"]]
mean(normal_30)
sd(normal_30)
1 / sqrt(30)
skewness(normal_30)
kurtosis(normal_30)

# ============================================================
# 2. SIX-SIDED DIE POPULATION
# ============================================================

# Population function
die_population <- function(n) {
  
  sample(1:6, size = n, replace = TRUE)
}

# Theoretical population values
die_mean <- 3.5

die_sd <- sqrt(
  mean((1:6 - die_mean)^2)
)

die_mean
die_sd

# Run simulations for every sample size
die_results <- list()
for (n in sample_sizes) {
  die_results[[as.character(n)]] <- simulate_means(
    n = n,
    num_simulations = num_simulations,
    population_function = die_population
  )
}

# Create summary table
die_summary <- data.frame(
  n = sample_sizes,
  simulated_mean = NA,
  simulated_sd = NA,
  theoretical_se = NA,
  skewness = NA,
  kurtosis = NA
)

# Fill in summary table
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  means <- die_results[[as.character(n)]]
  die_summary$simulated_mean[i] <- mean(means)
  die_summary$simulated_sd[i] <- sd(means)
  die_summary$theoretical_se[i] <- die_sd / sqrt(n)
  die_summary$skewness[i] <- skewness(means)
  die_summary$kurtosis[i] <- kurtosis(means)
}

die_summary

# Compare simulated SE to theoretical SE
die_summary$SE_difference <-
  die_summary$simulated_sd -
  die_summary$theoretical_se

die_summary$SE_percent_difference <-
  abs(
    die_summary$simulated_sd -
      die_summary$theoretical_se
  ) /
  die_summary$theoretical_se * 100

die_summary


# Die Histograms
# ----------------------------

hist(
  die_results[["5"]],
  main = "Six-Sided Die Sampling Distribution, n = 5",
  xlab = "Sample Mean"
)

hist(
  die_results[["30"]],
  main = "Six-Sided Die Sampling Distribution, n = 30",
  xlab = "Sample Mean"
)

hist(
  die_results[["100"]],
  main = "Six-Sided Die Sampling Distribution, n = 100",
  xlab = "Sample Mean"
)


# Die Q-Q Plots
# ----------------------------

qqnorm(
  die_results[["5"]],
  main = "Q-Q Plot: Six-Sided Die, n = 5"
)

qqline(die_results[["5"]])

qqnorm(
  die_results[["30"]],
  main = "Q-Q Plot: Six-Sided Die, n = 30"
)

qqline(die_results[["30"]])

qqnorm(
  die_results[["100"]],
  main = "Q-Q Plot: Six-Sided Die, n = 100"
)

qqline(die_results[["100"]])


# Die n = 30 Details
# ----------------------------

die_30 <- die_results[["30"]]
mean(die_30)
sd(die_30)
die_sd / sqrt(30)
skewness(die_30)
kurtosis(die_30)

# ============================================================
# 3. EXPONENTIAL POPULATION
# ============================================================

# Population function
exponential_population <- function(n) {
  
  rexp(n, rate = 1)
}

# Theoretical population values
exponential_mean <- 1
exponential_sd <- 1

exponential_mean
exponential_sd

# Run simulations for every sample size
exponential_results <- list()
for (n in sample_sizes) {
  exponential_results[[as.character(n)]] <- simulate_means(
    n = n,
    num_simulations = num_simulations,
    population_function = exponential_population
  )
}

# Create summary table
exponential_summary <- data.frame(
  n = sample_sizes,
  simulated_mean = NA,
  simulated_sd = NA,
  theoretical_se = NA,
  skewness = NA,
  kurtosis = NA
)

# Fill in summary table
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  means <- exponential_results[[as.character(n)]]
  exponential_summary$simulated_mean[i] <- mean(means)
  exponential_summary$simulated_sd[i] <- sd(means)
  exponential_summary$theoretical_se[i] <-
    exponential_sd / sqrt(n)
  exponential_summary$skewness[i] <- skewness(means)
  exponential_summary$kurtosis[i] <- kurtosis(means)
}

exponential_summary

# Compare simulated SE to theoretical SE
exponential_summary$SE_difference <-
  exponential_summary$simulated_sd -
  exponential_summary$theoretical_se

exponential_summary$SE_percent_difference <-
  abs(
    exponential_summary$simulated_sd -
      exponential_summary$theoretical_se
  ) /
  exponential_summary$theoretical_se * 100

exponential_summary


# Exponential Histograms
# ----------------------------

hist(
  exponential_results[["5"]],
  main = "Exponential Sampling Distribution, n = 5",
  xlab = "Sample Mean"
)

hist(
  exponential_results[["30"]],
  main = "Exponential Sampling Distribution, n = 30",
  xlab = "Sample Mean"
)

hist(
  exponential_results[["100"]],
  main = "Exponential Sampling Distribution, n = 100",
  xlab = "Sample Mean"
)


# Exponential Q-Q Plots
# ----------------------------

qqnorm(
  exponential_results[["5"]],
  main = "Q-Q Plot: Exponential, n = 5"
)

qqline(exponential_results[["5"]])

qqnorm(
  exponential_results[["30"]],
  main = "Q-Q Plot: Exponential, n = 30"
)

qqline(exponential_results[["30"]])

qqnorm(
  exponential_results[["100"]],
  main = "Q-Q Plot: Exponential, n = 100"
)

qqline(exponential_results[["100"]])


# Exponential n = 30 Details
# ----------------------------

exponential_30 <- exponential_results[["30"]]
mean(exponential_30)
sd(exponential_30)
exponential_sd / sqrt(30)
skewness(exponential_30)
kurtosis(exponential_30)


# ============================================================
# 4. BINOMIAL SMALL POPULATION
# ============================================================

# Population function
binomial_small_population <- function(n) {
  
  rbinom(n, size = 10, prob = 0.2)
}

# Theoretical population values
binomial_small_mean <- 10 * 0.2

binomial_small_sd <- sqrt(
  10 * 0.2 * (1 - 0.2)
)

binomial_small_mean
binomial_small_sd

# Run simulations for every sample size
binomial_small_results <- list()
for (n in sample_sizes) {
  binomial_small_results[[as.character(n)]] <- simulate_means(
    n = n,
    num_simulations = num_simulations,
    population_function = binomial_small_population
  )
}

# Create summary table
binomial_small_summary <- data.frame(
  n = sample_sizes,
  simulated_mean = NA,
  simulated_sd = NA,
  theoretical_se = NA,
  skewness = NA,
  kurtosis = NA
)

# Fill in summary table
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  means <- binomial_small_results[[as.character(n)]]
  binomial_small_summary$simulated_mean[i] <- mean(means)
  binomial_small_summary$simulated_sd[i] <- sd(means)
  binomial_small_summary$theoretical_se[i] <-
    binomial_small_sd / sqrt(n)
  binomial_small_summary$skewness[i] <- skewness(means)
  binomial_small_summary$kurtosis[i] <- kurtosis(means)
}

binomial_small_summary

# Compare simulated SE to theoretical SE
binomial_small_summary$SE_difference <-
  binomial_small_summary$simulated_sd -
  binomial_small_summary$theoretical_se

binomial_small_summary$SE_percent_difference <-
  abs(
    binomial_small_summary$simulated_sd -
      binomial_small_summary$theoretical_se
  ) /
  binomial_small_summary$theoretical_se * 100

binomial_small_summary

# Binomial Histograms
# ----------------------------

hist(
  binomial_small_results[["5"]],
  main = "Binomial Small Sampling Distribution, n = 5",
  xlab = "Sample Mean"
)

hist(
  binomial_small_results[["30"]],
  main = "Binomial Small Sampling Distribution, n = 30",
  xlab = "Sample Mean"
)

hist(
  binomial_small_results[["100"]],
  main = "Binomial Small Sampling Distribution, n = 100",
  xlab = "Sample Mean"
)

# Binomial Q-Q Plots
# ----------------------------

qqnorm(
  binomial_small_results[["5"]],
  main = "Q-Q Plot: Binomial Small, n = 5"
)

qqline(binomial_small_results[["5"]])

qqnorm(
  binomial_small_results[["30"]],
  main = "Q-Q Plot: Binomial Small, n = 30"
)

qqline(binomial_small_results[["30"]])

qqnorm(
  binomial_small_results[["100"]],
  main = "Q-Q Plot: Binomial Small, n = 100"
)
qqline(binomial_small_results[["100"]])

# Binomial n = 30 Details
# ----------------------------

binomial_small_30 <- binomial_small_results[["30"]]
mean(binomial_small_30)
sd(binomial_small_30)
binomial_small_sd / sqrt(30)
skewness(binomial_small_30)
kurtosis(binomial_small_30)


# ============================================================
# 5. CAUCHY POPULATION
# ============================================================

# Population function
cauchy_population <- function(n) {
  
  rcauchy(n, location = 0, scale = 1)
}

# The Cauchy distribution does not have a
# finite theoretical mean or standard deviation.

cauchy_mean <- NA
cauchy_sd <- NA

cauchy_mean
cauchy_sd

# Run simulations for every sample size
cauchy_results <- list()

for (n in sample_sizes) {
  cauchy_results[[as.character(n)]] <- simulate_means(
    n = n,
    num_simulations = num_simulations,
    population_function = cauchy_population
  )
}

# Create summary table
cauchy_summary <- data.frame(
  n = sample_sizes,
  simulated_mean = NA,
  simulated_sd = NA,
  theoretical_se = NA,
  skewness = NA,
  kurtosis = NA
)

# Fill in summary table
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  means <- cauchy_results[[as.character(n)]]
  cauchy_summary$simulated_mean[i] <- mean(means)
  cauchy_summary$simulated_sd[i] <- sd(means)
  cauchy_summary$theoretical_se[i] <- NA
  cauchy_summary$skewness[i] <- skewness(means)
  cauchy_summary$kurtosis[i] <- kurtosis(means)
}

cauchy_summary

# Cauchy Histograms
# ----------------------------

hist(
  cauchy_results[["5"]],
  main = "Cauchy Sampling Distribution, n = 5",
  xlab = "Sample Mean"
)

hist(
  cauchy_results[["30"]],
  main = "Cauchy Sampling Distribution, n = 30",
  xlab = "Sample Mean"
)

hist(
  cauchy_results[["100"]],
  main = "Cauchy Sampling Distribution, n = 100",
  xlab = "Sample Mean"
)

# Cauchy Q-Q Plots
# ----------------------------

qqnorm(
  cauchy_results[["5"]],
  main = "Q-Q Plot: Cauchy, n = 5"
)

qqline(cauchy_results[["5"]])

qqnorm(
  cauchy_results[["30"]],
  main = "Q-Q Plot: Cauchy, n = 30"
)

qqline(cauchy_results[["30"]])

qqnorm(
  cauchy_results[["100"]],
  main = "Q-Q Plot: Cauchy, n = 100"
)

qqline(cauchy_results[["100"]])

# Cauchy n = 30 Details
# ----------------------------

cauchy_30 <- cauchy_results[["30"]]
mean(cauchy_30)
sd(cauchy_30)
skewness(cauchy_30)
kurtosis(cauchy_30)

# ============================================================
# ORIGINAL POPULATION DISTRIBUTIONS
# ============================================================

set.seed(6512)

# Standard Normal
hist(
  rnorm(10000, mean = 0, sd = 1),
  main = "Original Population: Standard Normal",
  xlab = "Value"
)

# Six-Sided Die
barplot(
  rep(1/6, 6),
  names.arg = 1:6,
  main = "Original Population: Six-Sided Die",
  xlab = "Outcome",
  ylab = "Probability"
)

# Exponential
hist(
  rexp(10000, rate = 1),
  main = "Original Population: Exponential",
  xlab = "Value"
)

# Binomial Small
barplot(
  dbinom(0:10, size = 10, prob = 0.2),
  names.arg = 0:10,
  main = "Original Population: Binomial Small",
  xlab = "Outcome",
  ylab = "Probability"
)

# Cauchy
hist(
  rcauchy(10000, location = 0, scale = 1),
  breaks = 100,
  xlim = c(-10, 10),
  main = "Original Population: Cauchy",
  xlab = "Value"
)

# ============================================================
# FINAL COMPARISON TABLE
# ============================================================

normal_final <- data.frame(
  population = "Standard Normal",
  theoretical_mean = 0,
  normal_summary
)

die_final <- data.frame(
  population = "Six-Sided Die",
  theoretical_mean = die_mean,
  die_summary
)

exponential_final <- data.frame(
  population = "Exponential",
  theoretical_mean = exponential_mean,
  exponential_summary
)

binomial_final <- data.frame(
  population = "Binomial Small",
  theoretical_mean = binomial_small_mean,
  binomial_small_summary
)

cauchy_final <- data.frame(
  population = "Cauchy",
  theoretical_mean = NA,
  cauchy_summary
)

# Combine all five populations
final_table <- rbind(
  normal_final,
  die_final,
  exponential_final,
  binomial_final,
  cauchy_final
)

# Display the full comparison table
final_table


# ============================================================
# COMPARE ALL POPULATIONS WHEN n = 30
# ============================================================

comparison_n30 <- final_table[
  final_table$n == 30,
  c(
    "population",
    "theoretical_mean",
    "simulated_mean",
    "simulated_sd",
    "theoretical_se",
    "skewness",
    "kurtosis"
  )
]

comparison_n30




