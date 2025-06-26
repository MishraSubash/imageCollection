# Time Series State-Space and Hidden Markov Models

### Part A
```
# Load packages
library(ggplot2)
library(gridExtra)
library(dlm)
library(tidyverse)
library(xts)
library(depmixS4)
library(gamlss.data)

# Load the polio dataset
data(polio)
polio_cases <- as.numeric(polio)
n_obs <- length(polio_cases)
time_dates <- seq(from = as.Date("1970-01-01"), to = as.Date("1983-12-01"), by = "month")

if (length(time_dates) != n_obs) {
  stop("Mismatch between number of observations and generated dates. Please check data.")
}

cat("Polio data loaded: ", n_obs, " observations from ",
    format(min(time_dates), "%Y-%m"), " to ", format(max(time_dates), "%Y-%m"), "\n\n")

# Fit the Poisson HMM Multiple Times
best_aic <- Inf
best_model_fit <- NULL
num_runs <- 50

cat("Fitting Poisson HMM with 2 states multiple times...\n")
for (i in 1:num_runs) {
  set.seed(i)
  mod <- depmix(response = polio_cases ~ 1, family = poisson(), nstates = 2,
                data = data.frame(polio_cases = polio_cases))
  fm_try <- try(fit(mod, verbose = FALSE), silent = TRUE)
  
  if (!inherits(fm_try, "try-error")) {
    current_aic <- AIC(fm_try)
    if (current_aic < best_aic) {
      best_aic <- current_aic
      best_model_fit <- fm_try
      cat(sprintf("Run %d: New best AIC = %.2f\n", i, best_aic))
    }
  } else {
    cat(sprintf("Run %d: Fit failed (could not converge).\n", i))
  }
}

if (is.null(best_model_fit)) {
  stop("No model converged successfully after multiple runs. Consider increasing num_runs or checking data/model specification.")
}

cat("\nBest HMM model selected with AIC:", AIC(best_model_fit), "\n")

# Extract parameters
all_pars <- getpars(best_model_fit)

p12_logit <- all_pars[1]
p21_logit <- all_pars[2]

p12 <- plogis(p12_logit)
p11 <- 1 - p12
p21 <- plogis(p21_logit)
p22 <- 1 - p21

Gamma_matrix <- matrix(c(p11, p12,
                         p21, p22), byrow = TRUE, nrow = 2)

print(Gamma_matrix)

lambda_state1_log <- all_pars[3]
lambda_state2_log <- all_pars[4]

lambda_state1 <- exp(lambda_state1_log)
lambda_state2 <- exp(lambda_state2_log)


cat("Lambda for State 1: ", lambda_state1, "\n")
cat("Lambda for State 2: ", lambda_state2, "\n")

if (lambda_state1 > lambda_state2) {
  plot_lambda_low <- lambda_state2
  plot_lambda_high <- lambda_state1
} else {
  plot_lambda_low <- lambda_state1
  plot_lambda_high <- lambda_state2
}

# Determine the Most Likely State at Each Month
post_probs <- posterior(best_model_fit)
most_likely_state_idx <- apply(post_probs[, c("S1", "S2")], 1, which.max)

all_pars <- getpars(best_model_fit)
lambda_state1_log <- all_pars[3]
lambda_state2_log <- all_pars[4]

lambda_val_S1 <- exp(lambda_state1_log)
lambda_val_S2 <- exp(lambda_state2_log)

if (lambda_val_S1 < lambda_val_S2) {
  most_likely_state_labels <- factor(most_likely_state_idx, levels = c(1, 2),
                                     labels = c("State 1 (Low Cases)", "State 2 (High Cases)"))
  plot_lambda_low <- lambda_val_S1
  plot_lambda_high <- lambda_val_S2
} else {
  most_likely_state_labels <- factor(most_likely_state_idx, levels = c(1, 2),
                                     labels = c("State 2 (High Cases)", "State 1 (Low Cases)"))
  plot_lambda_low <- lambda_val_S2
  plot_lambda_high <- lambda_val_S1
}

# Create Time Series Plot with Most Likely States
plot_df <- data.frame(
  Date = time_dates,
  PolioCases = polio_cases,
  MostLikelyState = most_likely_state_labels
)

ggplot(plot_df, aes(x = Date, y = PolioCases)) +
  geom_line(aes(color = MostLikelyState), size = 0.8) +
  geom_point(aes(color = MostLikelyState), size = 1.5, alpha = 0.7) +
  labs(
    title = "Monthly Polio Cases (1970-1983) with Hidden HMM States",
    subtitle = paste0("AIC: ", round(AIC(best_model_fit), 2),
                      " | State 1 (Low Cases) Lambda: ", round(plot_lambda_low, 2),
                      " | State 2 (High Cases) Lambda: ", round(plot_lambda_high, 2)),
    x = "Date",
    y = "Number of Polio Cases"
  ) +
  scale_color_manual(name = "Most Likely State",
                     values = c("State 1 (Low Cases)" = "blue", "State 2 (High Cases)" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

```
**Best AIC: 530.0655**

**State Transition Probability Matrix (Gamma)**

|  | toS1  | toS2  |
|-----------|-------|-------|
| from S1    | 0.5 | 0.5 |
| from S2    | 0.731 | 0.268 |

**Estimate of lambda (rate parameter) for each hidden state**
| State | Re1.(Intercept) |
|--------|----------------|
| State 1    | 1.95        |
| State 2    | 1.39         |

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/RplotOriginal_PeerGraded.png)
![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/Rplot_peerGraded_Fig_A.png)

## Part B
From the Part A, we got the λ values of 1.95 for state 1 and 1.39 for state 2. Higher λ value indicates to periods with a higher average number of polio cases. 

Likelihood of Future Observations: 
Given an observation of a least 4 polio cases at time t, we are likely in the state with higher mean, which is state 1 (λ = 1.95) in this updated scenario. 

Consistency with General Knowledge about Infectious Disease Dynamics:
This finding is consistent with general knowledge of infectious disease dynamics. Once a disease outbreak is in a higher activity phase (indicated by ≥4 cases and being in the higher λ state), it typically exhibits some persistence. Cases are more likely to remain present (e.g., at least two cases) in the immediate subsequent period rather than abruptly dropping to zero. The model's higher probability of remaining in the "active" state and emitting a moderate number of cases reflects this inertia in disease spread.


## Part C
```
library(ggplot2)
library(gridExtra)
library(dlm)
library(tidyverse)
library(xts)
library(depmixS4)
library(gamlss.data)

data(polio)
polio_cases <- as.numeric(polio)
n_obs <- length(polio_cases)
time_dates <- seq(from = as.Date("1970-01-01"), to = as.Date("1983-12-01"), by = "month")

if (length(time_dates) != n_obs) {
  stop("Mismatch between number of observations and generated dates. Please check data.")
}

cat("Polio data loaded: ", n_obs, " observations from ",
    format(min(time_dates), "%Y-%m"), " to ", format(max(time_dates), "%Y-%m"), "\n\n")

best_aic <- Inf
best_model_fit <- NULL
num_runs <- 100

cat("Fitting Poisson HMM with 3 states multiple times...\n")
for (i in 1:num_runs) {
  set.seed(i)
  mod <- depmix(response = polio_cases ~ 1, family = poisson(), nstates = 3,
                data = data.frame(polio_cases = polio_cases))
  fm_try <- try(fit(mod, verbose = FALSE), silent = TRUE)
  
  if (!inherits(fm_try, "try-error")) {
    current_aic <- AIC(fm_try)
    if (is.finite(current_aic) && current_aic < best_aic) {
      best_aic <- current_aic
      best_model_fit <- fm_try
      cat(sprintf("Run %d: New best AIC = %.2f\n", i, best_aic))
    }
  }
}

if (is.null(best_model_fit)) {
  stop("No model converged successfully after multiple runs or all resulted in high AIC. Consider increasing num_runs or checking data/model specification.")
}

cat("\nBest HMM model selected with AIC:", AIC(best_model_fit), "\n")

cat("\nModel Fitting Results (3 States)\n")
current_aic_report <- AIC(best_model_fit)
cat("AIC of the best fitted HMM: ", current_aic_report, "\n")

if (current_aic_report < 531) {
  cat("The AIC is less than 531, meeting the hint's criterion.\n")
} else {
  cat("The AIC is NOT less than 531. Consider increasing num_runs or refining the model.\n")
}

all_pars <- getpars(best_model_fit)

lambda_state1 <- exp(all_pars[7])
lambda_state2 <- exp(all_pars[8])
lambda_state3 <- exp(all_pars[9])
lambda_values <- c(State1 = lambda_state1, State2 = lambda_state2, State3 = lambda_state3)
sorted_lambdas <- sort(lambda_values)

cat("\nEstimate of lambda (rate parameter) for each hidden state:\n")
cat("  State corresponding to lowest lambda: ", names(sorted_lambdas[1]), " (λ = ", round(sorted_lambdas[1], 4), ")\n", sep="")
cat("  State corresponding to medium lambda: ", names(sorted_lambdas[2]), " (λ = ", round(sorted_lambdas[2], 4), ")\n", sep="")
cat("  State corresponding to highest lambda: ", names(sorted_lambdas[3]), " (λ = ", round(sorted_lambdas[3], 4), ")\n", sep="")

Gamma_matrix <- matrix(NA, nrow = 3, ncol = 3,
                       dimnames = list(c("From S1", "From S2", "From S3"),
                                       c("To S1", "To S2", "To S3")))

trans_params <- all_pars[1:6]

e_eta_12 <- exp(trans_params[1])
e_eta_13 <- exp(trans_params[2])
denom_1 <- 1 + e_eta_12 + e_eta_13
Gamma_matrix[1, 1] <- 1 / denom_1
Gamma_matrix[1, 2] <- e_eta_12 / denom_1
Gamma_matrix[1, 3] <- e_eta_13 / denom_1

e_eta_21 <- exp(trans_params[3])
e_eta_23 <- exp(trans_params[4])
denom_2 <- e_eta_21 + 1 + e_eta_23
Gamma_matrix[2, 1] <- e_eta_21 / denom_2
Gamma_matrix[2, 2] <- 1 / denom_2
Gamma_matrix[2, 3] <- e_eta_23 / denom_2

e_eta_31 <- exp(trans_params[5])
e_eta_32 <- exp(trans_params[6])
denom_3 <- e_eta_31 + e_eta_32 + 1
Gamma_matrix[3, 1] <- e_eta_31 / denom_3
Gamma_matrix[3, 2] <- e_eta_32 / denom_3
Gamma_matrix[3, 3] <- 1 / denom_3

cat("\nState Transition Probability Matrix (Gamma):\n")
print(round(Gamma_matrix, 4))

post_probs <- posterior(best_model_fit)
most_likely_state_idx <- apply(post_probs[, c("S1", "S2", "S3")], 1, which.max)

state_map <- match(c(names(sorted_lambdas[1]), names(sorted_lambdas[2]), names(sorted_lambdas[3])),
                   c("State1", "State2", "State3"))

state_labels_plot <- paste0("State ", c(1, 2, 3), " (",
                            ifelse(state_map[1] == 1, "Low", ifelse(state_map[1] == 2, "Medium", "High")), "/",
                            ifelse(state_map[2] == 1, "Low", ifelse(state_map[2] == 2, "Medium", "High")), "/",
                            ifelse(state_map[3] == 1, "Low", ifelse(state_map[3] == 2, "Medium", "High")), " Cases)")

mapped_state_labels <- character(length(most_likely_state_idx))
for (j in 1:n_obs) {
  original_state_index <- most_likely_state_idx[j]
  if (original_state_index == state_map[1]) {
    mapped_state_labels[j] <- "State 1 (Low Cases)"
  } else if (original_state_index == state_map[2]) {
    mapped_state_labels[j] <- "State 2 (Medium Cases)"
  } else {
    mapped_state_labels[j] <- "State 3 (High Cases)"
  }
}
most_likely_state_factor <- factor(mapped_state_labels, levels = c("State 1 (Low Cases)", "State 2 (Medium Cases)", "State 3 (High Cases)"))

plot_df <- data.frame(
  Date = time_dates,
  PolioCases = polio_cases,
  MostLikelyState = most_likely_state_factor
)

ggplot(plot_df, aes(x = Date, y = PolioCases)) +
  geom_line(aes(color = MostLikelyState), size = 0.8) +
  geom_point(aes(color = MostLikelyState), size = 1.5, alpha = 0.7) +
  labs(
    title = "Monthly Polio Cases (1970-1983) with Hidden HMM States",
    subtitle = paste0("AIC: ", round(AIC(best_model_fit), 2),
                      " | λ1(Low): ", round(sorted_lambdas[1], 2),
                      " | λ2(Med): ", round(sorted_lambdas[2], 2),
                      " | λ3(High): ", round(sorted_lambdas[3], 2)),
    x = "Date",
    y = "Number of Polio Cases"
  ) +
  scale_color_manual(name = "Most Likely State",
                     values = c("State 1 (Low Cases)" = "blue",
                                "State 2 (Medium Cases)" = "green",
                                "State 3 (High Cases)" = "red")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

```
**AIC of the best fitted HMM:  529.9555**

- State corresponding to medium lambda: State1 (λ = 1.2376)
- State corresponding to highest lambda: State2 (λ = 2.0048)
- State corresponding to lowest lambda: State3 (λ = 1.0956)


**State Transition Probability Matrix (Gamma)**
|  | To S1  | To S2  | To S3  |
|-----------|--------|--------|--------|
| S1   | 0.2119 | 0.5761 | 0.2119 |
| S2   | 0.2233 | 0.2233 | 0.5533 |
| S3   | 0.3542 | 0.3229 | 0.3229 |

![](https://raw.githubusercontent.com/MishraSubash/imageCollection/refs/heads/main/PeerGraded_Part_C.png)
