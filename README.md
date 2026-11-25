# Central Limit Theorem Simulation Project

## Project Overview
This project explores the Central Limit Theorem (CLT) through simulations in R. It examines how the distribution of sample means change as sample sizes change.
I examined five different population distributions.

The main objective is to understand how sample size influences the shape, center, and variability of sampling distributions. 
This also includes cases where the original population is not normally distributed.

## Research Question
How does increasing sample sizes affect the distribution of sample means across different population distributions, and are there noticeable exceptions to the CLT?

## Populations Investigated
The project examines five different populations: 
| Population | Description | Theoretical Mean |
|---|---|---:|
| Standard Normal | Normally distributed population | 0 |
| Six-Sided Die | Discrete uniform distribution (1–6) | 3.5 |
| Exponential | Positively skewed distribution (rate = 1) | 1 |
| Binomial | Binomial distribution (size = 10, p = 0.2) | 2 |
| Cauchy | Heavy-tailed distribution | Undefined |

## Simulation Methodology
Simulation Information:
- Programming Language: R
- Simulations: 10,000 per sample size and population
- Random Seed: 6512
- Sample Sizes: 5, 10, 20, 30, 50, 100, 200

The resulting sampling distributions were analyzed using:
- Simulated means
- Simulated standard deviations
- Theoretical standard errors
- Skewness
- Kurtosis
- Histograms
- Normal Q-Q plots

## Key Findings
1. Standard Normal: The sample distribution remains normal across all sample sizes
2. Six-Sided Die: The sample distribution becomes increasingly normal as the sample size increases.
3. Exponential: The sample distribution becomes increasingly normal as the sample size increases, but not at n=30.
4. Binomial Small: The sample distribution becomes increasingly normal as the sample size increases, where n=30 is sufficient.
5. Cauchy: The sample distribution becomes increasingly normal, but not at n=30, because it does not follow the usual CLT behavior.

## How to Run the Project
1. Clone/Download this repository.
2. Open the project in RStudio.
3. Install the required packages if not already installed.
   '''r
   install.packages("ggplot2")
   install.packages("moments")
   '''
4. Open Project1.R
5. Run the script to generate the same simulation, summary tables, histograms, and Q-Q plots.

## Conclusion
This project demonstrates that the sample distribution of the mean generally approaches a normal distribution as the sample size increases, even if the original population is not normally distributed. 

Through simulation and statistical visualization, this project provides a practical exploration of the CLT and its limitations.
