
#==================================NARORA==============================================
library(readxl)

NMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/NM VBGF.xlsx")

# Check your sample distribution
table(NMdata$Age)

#==================== NORMALITY CHECK ====================

# 1. Basic Summary Statistics
summary(NMdata$Length)
summary(NMdata$Age)

# 2. Shapiro-Wilk Normality Test
shapiro.test(NMdata$Length)
shapiro.test(log(NMdata$Length))  # also test log-transformed

# 3. Skewness & Kurtosis
library(e1071)
cat("Skewness:", skewness(NMdata$Length), "\n")
cat("Kurtosis:", kurtosis(NMdata$Length), "\n")

# 4. Visual Checks
par(mfrow = c(2, 2))

# Histogram
hist(NMdata$Length, 
     main = "Histogram of Length", 
     xlab = "Length", 
     col = "steelblue", 
     border = "white",
     probability = TRUE)
lines(density(NMdata$Length), col = "red", lwd = 2)

# Q-Q Plot (raw)
qqnorm(NMdata$Length, main = "Q-Q Plot (Raw Length)")
qqline(NMdata$Length, col = "red", lwd = 2)

# Q-Q Plot (log-transformed)
qqnorm(log(NMdata$Length), main = "Q-Q Plot (Log Length)")
qqline(log(NMdata$Length), col = "red", lwd = 2)

# Boxplot
boxplot(NMdata$Length ~ NMdata$Age,
        main = "Length by Age Group",
        xlab = "Age", ylab = "Length",
        col = "lightblue")

# Linf estimate from mean length
mean(NMdata$Length)  # Linf ≈ mean length / 0.95 * some factor

# Compute Lmax (maximum observed length in the data)
Lmax_observed <- max(NMdata$Length, na.rm = TRUE)

Lmax_observed / 0.95  # rough Linf estimate

#-------------------------------2P VBGF (NARORA)-------------------------------
# Fit VBGF model
NM_model_2p <- nls(
  Length ~ Linf * (1 - exp(-K * Age)),
  data = NMdata,
  start = list(Linf = Lmax_observed * 1.1, K = 0.3),  # Better start: Linf ~ 1.1 * Lmax
  control = nls.control(maxiter = 500, warnOnly = TRUE)
)

print(summary(NM_model_2p))

# Extract coefficients
Linf <- coef(NM_model_2p)["Linf"]
K    <- coef(NM_model_2p)["K"]

# Age sequence for smooth curve
age_seq <- seq(0, max(NMdata$Age) + 1, length.out = 200)
pred_2p <- Linf * (1 - exp(-K * age_seq))

# Plot
plot(NMdata$Age, NMdata$Length,
     pch = 16, col = "steelblue",
     xlab = "Age (years)",
     ylab = "Total Length (cm)",
     main = "Von Bertalanffy Growth Curve - Narora Mahseer",
     xlim = c(0, max(NMdata$Age) + 1),
     ylim = c(0, max(NMdata$Length) + 2))

# Fitted curve
lines(age_seq, pred_2p, col = "red", lwd = 2)

# Asymptote line
abline(h = Linf, col = "darkgray", lty = 2, lwd = 1.5)

# Legend
legend("bottomright",
       legend = c("Observed",
                  "VBGF fit",
                  paste0("Linf = ", round(Linf, 2), " cm"),
                  paste0("K = ",    round(K, 3),    " yr⁻¹")),
       col    = c("steelblue", "red", NA, NA),
       pch    = c(16, NA, NA, NA),
       lty    = c(NA, 1,  NA, NA),
       lwd    = c(NA, 2,  NA, NA),
       bty    = "n")


#t0 evaluation (Pauly's equation)
t0 <- -0.392 - 0.275*log10(Linf) - 1.038*log10(K)

t0

#------------------------3P VBGF (NARORA)---------------------------------------
library(readxl)
library(minpack.lm)

#data <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/NM VBGF.xlsx")

Lmax_observed <- max(NMdata$Length, na.rm = TRUE)

NM_model_3p <- nls(
  Length ~ Linf * (1 - exp(-K * (Age - t0))),
  data = NMdata,
  start = list(
    Linf = Lmax_observed * 1.1,
    K = 0.5,
    t0 = -0.5
  ),
  algorithm = "port",
  lower = c(Linf = Lmax_observed, K = 0.01, t0 = -5),
  upper = c(Linf = Lmax_observed * 1.15, K = 2, t0 = 0),
  control = nls.control(maxiter = 1000)
)

summary(NM_model_3p)

# Extract coefficients
Linf <- coef(NM_model_3p)["Linf"]
K    <- coef(NM_model_3p)["K"]
t0   <- coef(NM_model_3p)["t0"]

# Age sequence for smooth curve
age_seq <- seq(0, max(NMdata$Age) + 1, length.out = 200)
pred_3p <- Linf * (1 - exp(-K * (age_seq - t0)))

# Plot
plot(NMdata$Age, NMdata$Length,
     pch = 16, col = "steelblue",
     xlab = "Age (years)",
     ylab = "Total Length (cm)",
     main = "Von Bertalanffy Growth Curve - Narora Mahseer",
     xlim = c(0, max(NMdata$Age) + 1),
     ylim = c(0, max(NMdata$Length) + 2))

# Fitted curve
lines(age_seq, pred_3p, col = "red", lwd = 2)

# Asymptote line
abline(h = Linf, col = "darkgray", lty = 2, lwd = 1.5)

# Legend
legend("bottomright",
       legend = c("Observed",
                  paste0("VBGF fit"),
                  paste0("Linf = ", round(Linf, 2), " cm"),
                  paste0("K = ",    round(K, 3),    " yr⁻¹"),
                  paste0("t0 = ",   round(t0, 2),   " yr")),
       col    = c("steelblue", "red", NA, NA, NA),
       pch    = c(16, NA, NA, NA, NA),
       lty    = c(NA, 1,  NA, NA, NA),
       lwd    = c(NA, 2,  NA, NA, NA),
       bty    = "n")

#--------------------------2P VBGF NARORA (BAYESIANS)----------------------------
#==================== LOAD DATA ====================

library(readxl)
library(dplyr)
library(brms)
library(cmdstanr)
library(ggplot2)

NMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/NM VBGF.xlsx")

NMdata <- NMdata %>%
  mutate(
    Age = as.numeric(Age),
    Length = as.numeric(Length)
  ) %>%
  na.omit()

#==================== MAXIMUM OBSERVED LENGTH ====================
# Observed maximum length
Lmax_observed <- max(NMdata$Length)

# Starting Linf
Linf_start <- Lmax_observed * 1.1


#==================== DEFINE VBGF MODEL ====================

vb_formula_2p <- bf(
  Length ~ Linf * (1 - exp(-K * Age)),
  Linf ~ 1,
  K ~ 1,
  nl = TRUE
)

#==================== PRIORS ====================
# Replace 40 with your approximate Linf value if needed

#GAMMA
priors_2p <- c(
  prior(normal(18.348, 1),  nlpar = "Linf", lb = 15),
  prior(normal(0.35, 0.1),  nlpar = "K",    lb = 0),
  prior(gamma(0.01, 0.01),  class = "shape")   # ✅ correct for Gamma family
)

#GAMMA
NM_bayes_2p <- brm(
  formula = vb_formula_2p,
  data    = NMdata,
  family  = Gamma(link = "identity"),
  prior   = priors_2p,
  chains  = 4,
  iter    = 8000,
  warmup  = 3000,
  cores   = 4,
  backend = "cmdstanr",
  control = list(
    adapt_delta   = 0.999,
    max_treedepth = 15
  ),
  seed = 123
)

summary(NM_bayes_2p)

#==================== FIT MODEL GAUSSIAN ====================
#GAUSSIAN
#priors_2p <- c(
# prior(normal(18.348, 1), nlpar = "Linf", lb = 15),
#prior(normal(0.35, 0.1), nlpar = "K", lb = 0),
#prior(student_t(3, 0, 2), class = "sigma", lb = 0)
#)

#GAUSSIAN
#NM_bayes_2p <- brm(
 # formula = vb_formula_2p,
  #data = NMdata,
  #family = Gamma(link = "identity"),
  #prior = priors_2p,
  #chains = 4,
  #iter = 8000,
  #warmup = 3000,
  #cores = 4,
  #backend = "cmdstanr",
  #control = list(
   # adapt_delta = 0.999,
  #  max_treedepth = 15
  #),
  #seed = 123
#)
#summary(NM_bayes_2p)

#--------------------3P VBGF NARORA (BAYESIANS)---------------------------------
library(readxl)
library(dplyr)
library(brms)
library(cmdstanr)
library(ggplot2)

# Import data
NMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/NM VBGF.xlsx")

NMdata <- NMdata %>%
  mutate(
    Age = as.numeric(Age),
    Length = as.numeric(Length)
  ) %>%
  na.omit()


# 3-parameter VBGF formula
vb_formula_3p <- bf(
  Length ~ Linf * (1 - exp(-K * (Age - t0))),
  Linf ~ 1,
  K ~ 1,
  t0 ~ 1,
  nl = TRUE
)

# Priors
priors_3p <- c(
  prior(normal(25, 2),  nlpar = "Linf", lb = 15),
  prior(normal(0.4, 0.1),  nlpar = "K",    lb = 0),
  prior(normal(-0.5, 0.5),  nlpar = "t0"),            # no lb — t0 can be negative
  prior(gamma(0.01, 0.01),  class = "shape")
)

# Fit Bayesian 3P model
NM_bayes_3p <- brm(
  formula = vb_formula_3p,
  data    = NMdata,
  family  = Gamma(link = "identity"),
  prior   = priors_3p,
  chains  = 4,
  iter    = 8000,
  warmup  = 3000,
  cores   = 4,
  backend = "cmdstanr",
  control = list(
    adapt_delta   = 0.999,
    max_treedepth = 15
  ),
  seed = 123
)

# Model summary
summary(NM_bayes_3p)

#DIAGNOSTICS 3P bayes==========================================
# Check L∞ vs K correlation in posterior
draws <- as_draws_df(NM_bayes_3p)
cor(draws$b_Linf_Intercept, draws$b_K_Intercept)

library(bayesplot)

# Pairs plot of all three growth parameters
mcmc_pairs(draws, 
           pars = c("b_Linf_Intercept", "b_K_Intercept", "b_t0_Intercept"),
           off_diag_args = list(size = 0.5, alpha = 0.3))


library(bayesplot)

pp_check(NM_bayes_3p, ndraws = 100)

# Check for systematic bias across the range
pp_check(NM_bayes_3p, type = "scatter_avg")

# Check residual distribution
pp_check(NM_bayes_3p, type = "error_hist", ndraws = 11)

# Check if variance is consistent across fitted values (heteroscedasticity)
pp_check(NM_bayes_3p, type = "error_scatter_avg")

# 2. Growth curve plot
age_seq <- seq(0, 6, by = 0.1)
fitted_vals <- fitted(NM_bayes_3p,
                      newdata = data.frame(Age = age_seq),
                      probs = c(0.025, 0.975))

plot_data <- data.frame(
  Age    = age_seq,
  Length = fitted_vals[, "Estimate"],
  Lower  = fitted_vals[, "Q2.5"],
  Upper  = fitted_vals[, "Q97.5"]
)

ggplot() +
  geom_ribbon(data = plot_data,
              aes(x = Age, ymin = Lower, ymax = Upper),
              alpha = 0.3, fill = "steelblue") +
  geom_line(data = plot_data,
            aes(x = Age, y = Length),
            color = "steelblue", linewidth = 1.2) +
  geom_point(data = NMdata,
             aes(x = Age, y = Length),
             alpha = 0.5, color = "black") +
  labs(title = "Bayesian VBGF 3P - Growth Curve",
       x = "Age (years)", y = "Length (cm)") +
  theme_classic()

# AIC comparison
library(AICcmodavg)

AICcmodavg::AICc(NM_model_2p)
AICcmodavg::AICc(NM_model_3p)

# BIC comparison
BIC(NM_model_2p, NM_model_3p)

#LOOWAIC
loo(NM_bayes_3pIII)
loo(NM_bayes_3pII)
loo(NM_bayes_3p)


pred_nls2 <- predict(NM_model_2p)
rmse_nls2 <- sqrt(mean((NMdata$Length - pred_nls2)^2))
rmse_nls2

pred_nls3 <- predict(NM_model_3p)
rmse_nls3 <- sqrt(mean((NMdata$Length - pred_nls3)^2))
rmse_nls3

pred_bayes2 <- fitted(NM_bayes_2p)[,1]

rmse_bayes2 <- sqrt(mean((NMdata$Length - pred_bayes2)^2))
rmse_bayes2

pred_bayes3 <- fitted(NM_bayes_3p)[,1]

rmse_bayes3 <- sqrt(mean((NMdata$Length - pred_bayes3)^2))
rmse_bayes3

model_compare <- data.frame(
  Model = c("NLS_2P","NLS_3P","Bayes_2P","Bayes_3P"),
  RMSE = c(rmse_nls2, rmse_nls3, rmse_bayes2, rmse_bayes3)
)

model_compare

# Maximum observed length
Lmax <- max(NMdata$Length, na.rm = TRUE)

# Froese & Binohlan equation
log_Linf <- 0.044 + 0.9841 * log10(Lmax)

# Convert back from log
NMLinf_FB <- 10^log_Linf

NMLinf_FB

#===========================KANPUR=============================================

KMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/KM VBGF.xlsx")

# Check your sample distribution
table(KMdata$Age)

#==================== NORMALITY CHECK ====================

# 1. Basic Summary Statistics
summary(KMdata$Length)
summary(KMdata$Age)

# 2. Shapiro-Wilk Normality Test
shapiro.test(KMdata$Length)
shapiro.test(log(KMdata$Length))  # also test log-transformed

# 3. Skewness & Kurtosis
library(e1071)
cat("Skewness:", skewness(KMdata$Length), "\n")
cat("Kurtosis:", kurtosis(KMdata$Length), "\n")

# 4. Visual Checks
par(mfrow = c(2, 2))

# Histogram
hist(KMdata$Length, 
     main = "Histogram of Length", 
     xlab = "Length", 
     col = "steelblue", 
     border = "white",
     probability = TRUE)
lines(density(KMdata$Length), col = "red", lwd = 2)

# Q-Q Plot (raw)
qqnorm(KMdata$Length, main = "Q-Q Plot (Raw Length)")
qqline(KMdata$Length, col = "red", lwd = 2)

# Q-Q Plot (log-transformed)
qqnorm(log(KMdata$Length), main = "Q-Q Plot (Log Length)")
qqline(log(KMdata$Length), col = "red", lwd = 2)

# Boxplot
boxplot(KMdata$Length ~ KMdata$Age,
        main = "Length by Age Group",
        xlab = "Age", ylab = "Length",
        col = "lightblue")


# Linf estimate from mean length
mean(KMdata$Length)  # Linf ≈ mean length / 0.95 * some factor

# Compute Lmax (maximum observed length in the data)
Lmax_observed <- max(KMdata$Length, na.rm = TRUE)

Lmax_observed / 0.95  # rough Linf estimate

#-------------------------------2P VBGF (KANPUR)-------------------------------
# Fit VBGF model
KM_model_2p <- nls(
  Length ~ Linf * (1 - exp(-K * Age)),
  data = KMdata,
  start = list(Linf = Lmax_observed * 1.1, K = 0.3),  # Better start: Linf ~ 1.1 * Lmax
  control = nls.control(maxiter = 500, warnOnly = TRUE)
)

print(summary(KM_model_2p))

# Extract coefficients
Linf <- coef(KM_model_2p)["Linf"]
K    <- coef(KM_model_2p)["K"]

# Age sequence for smooth curve
age_seq <- seq(0, max(KMdata$Age) + 1, length.out = 200)
pred_2p <- Linf * (1 - exp(-K * age_seq))

# Plot
plot(KMdata$Age, KMdata$Length,
     pch = 16, col = "steelblue",
     xlab = "Age (years)",
     ylab = "Total Length (cm)",
     main = "Von Bertalanffy Growth Curve - Kanpur Mahseer",
     xlim = c(0, max(KMdata$Age) + 1),
     ylim = c(0, max(KMdata$Length) + 2))

# Fitted curve
lines(age_seq, pred_2p, col = "red", lwd = 2)

# Asymptote line
abline(h = Linf, col = "darkgray", lty = 2, lwd = 1.5)

# Legend
legend("bottomright",
       legend = c("Observed",
                  "VBGF fit",
                  paste0("Linf = ", round(Linf, 2), " cm"),
                  paste0("K = ",    round(K, 3),    " yr⁻¹")),
       col    = c("steelblue", "red", NA, NA),
       pch    = c(16, NA, NA, NA),
       lty    = c(NA, 1,  NA, NA),
       lwd    = c(NA, 2,  NA, NA),
       bty    = "n")


#t0 evaluation (Pauly's equation)
t0 <- -0.392 - 0.275*log10(Linf) - 1.038*log10(K)

t0

#------------------------3P VBGF (KANPUR)---------------------------------------
library(readxl)
library(minpack.lm)

#data <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/KM VBGF.xlsx")

Lmax_observed <- max(KMdata$Length, na.rm = TRUE)

KM_model_3p <- nls(
  Length ~ Linf * (1 - exp(-K * (Age - t0))),
  data = KMdata,
  start = list(
    Linf = Lmax_observed * 1.1,
    K = 0.5,
    t0 = -0.5
  ),
  algorithm = "port",
  lower = c(Linf = Lmax_observed, K = 0.01, t0 = -5),
  upper = c(Linf = Lmax_observed * 1.15, K = 2, t0 = 0),
  control = nls.control(maxiter = 1000)
)

summary(KM_model_3p)

# Extract coefficients
Linf <- coef(KM_model_3p)["Linf"]
K    <- coef(KM_model_3p)["K"]
t0   <- coef(KM_model_3p)["t0"]

# Age sequence for smooth curve
age_seq <- seq(0, max(KMdata$Age) + 1, length.out = 200)
pred_3p <- Linf * (1 - exp(-K * (age_seq - t0)))

# Plot
plot(KMdata$Age, KMdata$Length,
     pch = 16, col = "steelblue",
     xlab = "Age (years)",
     ylab = "Total Length (cm)",
     main = "Von Bertalanffy Growth Curve - KANPUR Mahseer",
     xlim = c(0, max(KMdata$Age) + 1),
     ylim = c(0, max(KMdata$Length) + 2))

# Fitted curve
lines(age_seq, pred_3p, col = "red", lwd = 2)

# Asymptote line
abline(h = Linf, col = "darkgray", lty = 2, lwd = 1.5)

# Legend
legend("bottomright",
       legend = c("Observed",
                  paste0("VBGF fit"),
                  paste0("Linf = ", round(Linf, 2), " cm"),
                  paste0("K = ",    round(K, 3),    " yr⁻¹"),
                  paste0("t0 = ",   round(t0, 2),   " yr")),
       col    = c("steelblue", "red", NA, NA, NA),
       pch    = c(16, NA, NA, NA, NA),
       lty    = c(NA, 1,  NA, NA, NA),
       lwd    = c(NA, 2,  NA, NA, NA),
       bty    = "n")

#--------------------------2P VBGF KANPUR (BAYESIANS)----------------------------
#==================== LOAD DATA ====================

library(readxl)
library(dplyr)
library(brms)
library(cmdstanr)
library(ggplot2)

KMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/KM VBGF.xlsx")

KMdata <- KMdata %>%
  mutate(
    Age = as.numeric(Age),
    Length = as.numeric(Length)
  ) %>%
  na.omit()

#==================== MAXIMUM OBSERVED LENGTH ====================
# Observed maximum length
Lmax_observed <- max(KMdata$Length)

# Starting Linf
Linf_start <- Lmax_observed * 1.1


#==================== DEFINE VBGF MODEL ====================

vb_formula_2p <- bf(
  Length ~ Linf * (1 - exp(-K * Age)),
  Linf ~ 1,
  K ~ 1,
  nl = TRUE
)

#==================== PRIORS ====================
# Replace 40 with your approximate Linf value if needed

#GAMMA
priors_2p <- c(
  prior(normal(25, 2),  nlpar = "Linf", lb = 15),
  prior(normal(0.4, 0.1),  nlpar = "K",    lb = 0),
  prior(gamma(0.01, 0.01),  class = "shape")   # ✅ correct for Gamma family
)

#GAMMA
KM_bayes_2p <- brm(
  formula = vb_formula_2p,
  data    = KMdata,
  family  = Gamma(link = "identity"),
  prior   = priors_2p,
  chains  = 4,
  iter    = 8000,
  warmup  = 3000,
  cores   = 4,
  backend = "cmdstanr",
  control = list(
    adapt_delta   = 0.999,
    max_treedepth = 15
  ),
  seed = 123
)

summary(KM_bayes_2p)

#--------------------3P VBGF KANPUR (BAYESIANS)---------------------------------
library(readxl)
library(dplyr)
library(brms)
library(cmdstanr)
library(ggplot2)

# Import data
KMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/KM VBGF.xlsx")

KMdata <- KMdata %>%
  mutate(
    Age = as.numeric(Age),
    Length = as.numeric(Length)
  ) %>%
  na.omit()


# 3-parameter VBGF formula
vb_formula_3p <- bf(
  Length ~ Linf * (1 - exp(-K * (Age - t0))),
  Linf ~ 1,
  K ~ 1,
  t0 ~ 1,
  nl = TRUE
)

# Priors
priors_3p <- c(
  prior(normal(25, 2),  nlpar = "Linf", lb = 15),
  prior(normal(0.4, 0.1),  nlpar = "K",    lb = 0),
  prior(normal(-0.5, 0.5),  nlpar = "t0"),            # no lb — t0 can be negative
  prior(gamma(0.01, 0.01),  class = "shape")
)

# Fit Bayesian 3P model
KM_bayes_3p <- brm(
  formula = vb_formula_3p,
  data    = KMdata,
  family  = Gamma(link = "identity"),
  prior   = priors_3p,
  chains  = 4,
  iter    = 8000,
  warmup  = 3000,
  cores   = 4,
  backend = "cmdstanr",
  control = list(
    adapt_delta   = 0.999,
    max_treedepth = 15
  ),
  seed = 123
)

# Model summary
summary(KM_bayes_3p)

#DIAGNOSTICS 3P bayes==========================================
# Check L∞ vs K correlation in posterior
draws <- as_draws_df(KM_bayes_3p)
cor(draws$b_Linf_Intercept, draws$b_K_Intercept)

library(bayesplot)

# Pairs plot of all three growth parameters
mcmc_pairs(draws, 
           pars = c("b_Linf_Intercept", "b_K_Intercept", "b_t0_Intercept"),
           off_diag_args = list(size = 0.5, alpha = 0.3))


library(bayesplot)

pp_check(KM_bayes_3p, ndraws = 100)

# Check for systematic bias across the range
pp_check(KM_bayes_3p, type = "scatter_avg")

# Check residual distribution
pp_check(KM_bayes_3p, type = "error_hist", ndraws = 11)

# Check if variance is consistent across fitted values (heteroscedasticity)
pp_check(KM_bayes_3p, type = "error_scatter_avg")

# AIC comparison
library(AICcmodavg)

AICcmodavg::AICc(KM_model_2p)
AICcmodavg::AICc(KM_model_3p)

# BIC comparison
BIC(KM_model_2p, KM_model_3p)

#LOOWAIC
loo(KM_bayes_2p)
loo(KM_bayes_3p)


pred_nls2 <- predict(KM_model_2p)
rmse_nls2 <- sqrt(mean((KMdata$Length - pred_nls2)^2))
rmse_nls2

pred_nls3 <- predict(KM_model_3p)
rmse_nls3 <- sqrt(mean((KMdata$Length - pred_nls3)^2))
rmse_nls3

pred_bayes2 <- fitted(KM_bayes_2p)[,1]

rmse_bayes2 <- sqrt(mean((KMdata$Length - pred_bayes2)^2))
rmse_bayes2

pred_bayes3 <- fitted(KM_bayes_3p)[,1]

rmse_bayes3 <- sqrt(mean((KMdata$Length - pred_bayes3)^2))
rmse_bayes3

model_compare <- data.frame(
  Model = c("NLS_2P","NLS_3P","Bayes_2P","Bayes_3P"),
  RMSE = c(rmse_nls2, rmse_nls3, rmse_bayes2, rmse_bayes3)
)

model_compare

# Maximum observed length
Lmax <- max(KMdata$Length, na.rm = TRUE)

# Froese & Binohlan equation
log_Linf <- 0.044 + 0.9841 * log10(Lmax)

# Convert back from log
KMLinf_FB <- 10^log_Linf

KMLinf_FB

#===========================VARANASI=============================================
library(readxl)
VMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/VM VBGF.xlsx")

# Check your sample distribution
table(VMdata$Age)

# Linf estimate from mean length
mean(VMdata$Length)  # Linf ≈ mean length / 0.95 * some factor

# Compute Lmax (maximum observed length in the data)
Lmax_observed <- max(VMdata$Length, na.rm = TRUE)

Lmax_observed / 0.95  # rough Linf estimate

#-------------------------------2P VBGF (VARANASI)-------------------------------
# Fit VBGF model
VM_model_2p <- nls(
  Length ~ Linf * (1 - exp(-K * Age)),
  data = VMdata,
  start = list(Linf = Lmax_observed * 1.1, K = 0.3),  # Better start: Linf ~ 1.1 * Lmax
  control = nls.control(maxiter = 500, warnOnly = TRUE)
)

print(summary(VM_model_2p))

# Extract coefficients
Linf <- coef(VM_model_2p)["Linf"]
K    <- coef(VM_model_2p)["K"]

# Age sequence for smooth curve
age_seq <- seq(0, max(VMdata$Age) + 1, length.out = 200)
pred_2p <- Linf * (1 - exp(-K * age_seq))

# Plot
plot(VMdata$Age, VMdata$Length,
     pch = 16, col = "steelblue",
     xlab = "Age (years)",
     ylab = "Total Length (cm)",
     main = "Von Bertalanffy Growth Curve - Varanasi Mahseer",
     xlim = c(0, max(VMdata$Age) + 1),
     ylim = c(0, max(VMdata$Length) + 2))

# Fitted curve
lines(age_seq, pred_2p, col = "red", lwd = 2)

# Asymptote line
abline(h = Linf, col = "darkgray", lty = 2, lwd = 1.5)

# Legend
legend("bottomright",
       legend = c("Observed",
                  "VBGF fit",
                  paste0("Linf = ", round(Linf, 2), " cm"),
                  paste0("K = ",    round(K, 3),    " yr⁻¹")),
       col    = c("steelblue", "red", NA, NA),
       pch    = c(16, NA, NA, NA),
       lty    = c(NA, 1,  NA, NA),
       lwd    = c(NA, 2,  NA, NA),
       bty    = "n")


#t0 evaluation (Pauly's equation)
VMt0 <- -0.392 - 0.275*log10(Linf) - 1.038*log10(K)

VMt0

#------------------------3P VBGF (VARANASI)---------------------------------------
library(minpack.lm)

#data <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/VM VBGF.xlsx")

Lmax_observed <- max(VMdata$Length, na.rm = TRUE)

VM_model_3p <- nls(
  Length ~ Linf * (1 - exp(-K * (Age - t0))),
  data = VMdata,
  start = list(
    Linf = Lmax_observed * 1.1,
    K = 0.5,
    t0 = -0.5
  ),
  algorithm = "port",
  lower = c(Linf = Lmax_observed, K = 0.01, t0 = -5),
  upper = c(Linf = Lmax_observed * 1.15, K = 2, t0 = 0),
  control = nls.control(maxiter = 1000)
)

summary(VM_model_3p)

# Extract coefficients
Linf <- coef(VM_model_3p)["Linf"]
K    <- coef(VM_model_3p)["K"]
t0   <- coef(VM_model_3p)["t0"]

# Age sequence for smooth curve
age_seq <- seq(0, max(VMdata$Age) + 1, length.out = 200)
pred_3p <- Linf * (1 - exp(-K * (age_seq - t0)))

# Plot
plot(VMdata$Age, VMdata$Length,
     pch = 16, col = "steelblue",
     xlab = "Age (years)",
     ylab = "Total Length (cm)",
     main = "Von Bertalanffy Growth Curve - Varanasi Mahseer",
     xlim = c(0, max(VMdata$Age) + 1),
     ylim = c(0, max(VMdata$Length) + 2))

# Fitted curve
lines(age_seq, pred_3p, col = "red", lwd = 2)

# Asymptote line
abline(h = Linf, col = "darkgray", lty = 2, lwd = 1.5)

# Legend
legend("bottomright",
       legend = c("Observed",
                  paste0("VBGF fit"),
                  paste0("Linf = ", round(Linf, 2), " cm"),
                  paste0("K = ",    round(K, 3),    " yr⁻¹"),
                  paste0("t0 = ",   round(t0, 2),   " yr")),
       col    = c("steelblue", "red", NA, NA, NA),
       pch    = c(16, NA, NA, NA, NA),
       lty    = c(NA, 1,  NA, NA, NA),
       lwd    = c(NA, 2,  NA, NA, NA),
       bty    = "n")

#--------------------------2P VBGF VARANASI (BAYESIANS)----------------------------
#==================== LOAD DATA ====================

library(readxl)
library(dplyr)
library(brms)
library(cmdstanr)
library(ggplot2)

VMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/VM VBGF.xlsx")

VMdata <- VMdata %>%
  mutate(
    Age = as.numeric(Age),
    Length = as.numeric(Length)
  ) %>%
  na.omit()

#==================== MAXIMUM OBSERVED LENGTH ====================
# Observed maximum length
Lmax_observed <- max(VMdata$Length)

# Starting Linf
Linf_start <- Lmax_observed * 1.1


#==================== DEFINE VBGF MODEL ====================

vb_formula_2p <- bf(
  Length ~ Linf * (1 - exp(-K * Age)),
  Linf ~ 1,
  K ~ 1,
  nl = TRUE
)

#==================== PRIORS ====================
# Replace 40 with your approximate Linf value if needed

priors_2p <- c(
  prior(normal(25, 2),  nlpar = "Linf", lb = 15),
  prior(normal(0.4, 0.1),  nlpar = "K",    lb = 0),
  prior(gamma(0.01, 0.01),  class = "shape")
)

#==================== FIT MODEL ====================

VM_bayes_2p <- brm(
  formula = vb_formula_2p,
  data = VMdata,
  family = Gamma(link = "identity"),
  prior = priors_2p,
  chains = 4,
  iter = 8000,
  warmup = 3000,
  cores = 4,
  backend = "cmdstanr",
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 15
  ),
  seed = 123
)
summary(VM_bayes_2p)

#--------------------3P VBGF KANPUR (BAYESIANS)---------------------------------
library(readxl)
library(dplyr)
library(brms)
library(cmdstanr)
library(ggplot2)

# Import data
VMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/VM VBGF.xlsx")

VMdata <- VMdata %>%
  mutate(
    Age = as.numeric(Age),
    Length = as.numeric(Length)
  ) %>%
  na.omit()


# 3-parameter VBGF formula
vb_formula_3p <- bf(
  Length ~ Linf * (1 - exp(-K * (Age - t0))),
  Linf ~ 1,
  K ~ 1,
  t0 ~ 1,
  nl = TRUE
)

# Priors
priors_3p <- c(
  prior(normal(25, 2),  nlpar = "Linf", lb = 15),
  prior(normal(0.4, 0.1),  nlpar = "K",    lb = 0),
  prior(normal(-0.5, 0.5),  nlpar = "t0"),            # no lb — t0 can be negative
  prior(gamma(0.01, 0.01),  class = "shape")
)


# Fit Bayesian 3P model
VM_bayes_3p <- brm(
  formula = vb_formula_3p,
  data = VMdata,
  family = Gamma(link = "identity"),               #student() gaussian()
  prior = priors_3p,
  chains = 4,
  iter = 8000,
  warmup = 3000,
  cores = 4,
  backend = "cmdstanr",
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 15
  ),
  seed = 123
)

# Model summary
summary(VM_bayes_3p)

#DIAGNOSTICS 3P bayes==========================================
# Check L∞ vs K correlation in posterior
draws <- as_draws_df(VM_bayes_3p)
cor(draws$b_Linf_Intercept, draws$b_K_Intercept)

library(bayesplot)

# Pairs plot of all three growth parameters
mcmc_pairs(draws, 
           pars = c("b_Linf_Intercept", "b_K_Intercept", "b_t0_Intercept"),
           off_diag_args = list(size = 0.5, alpha = 0.3))


library(bayesplot)

pp_check(VM_bayes_3p, ndraws = 100)

# Check for systematic bias across the range
pp_check(VM_bayes_3p, type = "scatter_avg")

# Check residual distribution
pp_check(KM_bayes_3p, type = "error_hist", ndraws = 11)

# Check if variance is consistent across fitted values (heteroscedasticity)
pp_check(VM_bayes_3p, type = "error_scatter_avg")

# AIC comparison
library(AICcmodavg)

AICcmodavg::AICc(VM_model_2p)
AICcmodavg::AICc(VM_model_3p)

# BIC comparison
BIC(VM_model_2p, VM_model_3p)

#LOOWAIC
loo(VM_bayes_2p)
loo(VM_bayes_3p)


pred_nls2 <- predict(VM_model_2p)
rmse_nls2 <- sqrt(mean((VMdata$Length - pred_nls2)^2))
rmse_nls2

pred_nls3 <- predict(VM_model_3p)
rmse_nls3 <- sqrt(mean((VMdata$Length - pred_nls3)^2))
rmse_nls3

pred_bayes2 <- fitted(VM_bayes_2p)[,1]

rmse_bayes2 <- sqrt(mean((VMdata$Length - pred_bayes2)^2))
rmse_bayes2

pred_bayes3 <- fitted(VM_bayes_3p)[,1]

rmse_bayes3 <- sqrt(mean((VMdata$Length - pred_bayes3)^2))
rmse_bayes3

model_compare <- data.frame(
  Model = c("NLS_2P","NLS_3P","Bayes_2P","Bayes_3P"),
  RMSE = c(rmse_nls2, rmse_nls3, rmse_bayes2, rmse_bayes3)
)

model_compare

# Maximum observed length
Lmax <- max(VMdata$Length, na.rm = TRUE)

# Froese & Binohlan equation
log_Linf <- 0.044 + 0.9841 * log10(Lmax)

# Convert back from log
VMLinf_FB <- 10^log_Linf

VMLinf_FB

#===========================BHAGALPUR=============================================

BMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/BM VBGF.xlsx")

# Check your sample distribution
table(BMdata$Age)

# Linf estimate from mean length
mean(BMdata$Length)  # Linf ≈ mean length / 0.95 * some factor

# Compute Lmax (maximum observed length in the data)
Lmax_observed <- max(BMdata$Length, na.rm = TRUE)

Lmax_observed / 0.95  # rough Linf estimate

#-------------------------------2P VBGF (BHAGALPUR)-------------------------------
# Fit VBGF model
BM_model_2p <- nls(
  Length ~ Linf * (1 - exp(-K * Age)),
  data = BMdata,
  start = list(Linf = Lmax_observed * 1.1, K = 0.3),  # Better start: Linf ~ 1.1 * Lmax
  control = nls.control(maxiter = 500, warnOnly = TRUE)
)

print(summary(BM_model_2p))

# Extract coefficients
Linf <- coef(BM_model_2p)["Linf"]
K    <- coef(BM_model_2p)["K"]

# Age sequence for smooth curve
age_seq <- seq(0, max(BMdata$Age) + 1, length.out = 200)
pred_2p <- Linf * (1 - exp(-K * age_seq))

# Plot
plot(BMdata$Age, BMdata$Length,
     pch = 16, col = "steelblue",
     xlab = "Age (years)",
     ylab = "Total Length (cm)",
     main = "Von Bertalanffy Growth Curve - Bhagalpur Mahseer",
     xlim = c(0, max(BMdata$Age) + 1),
     ylim = c(0, max(BMdata$Length) + 2))

# Fitted curve
lines(age_seq, pred_2p, col = "red", lwd = 2)

# Asymptote line
abline(h = Linf, col = "darkgray", lty = 2, lwd = 1.5)

# Legend
legend("bottomright",
       legend = c("Observed",
                  "VBGF fit",
                  paste0("Linf = ", round(Linf, 2), " cm"),
                  paste0("K = ",    round(K, 3),    " yr⁻¹")),
       col    = c("steelblue", "red", NA, NA),
       pch    = c(16, NA, NA, NA),
       lty    = c(NA, 1,  NA, NA),
       lwd    = c(NA, 2,  NA, NA),
       bty    = "n")


#t0 evaluation (Pauly's equation)
BMt0 <- -0.392 - 0.275*log10(Linf) - 1.038*log10(K)

BMt0

#------------------------3P VBGF (BHAGALPUR)---------------------------------------
library(readxl)
library(minpack.lm)

#data <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/BM VBGF.xlsx")

Lmax_observed <- max(BMdata$Length, na.rm = TRUE)

BM_model_3p <- nls(
  Length ~ Linf * (1 - exp(-K * (Age - t0))),
  data = BMdata,
  start = list(
    Linf = Lmax_observed * 1.1,
    K = 0.5,
    t0 = -0.5
  ),
  algorithm = "port",
  lower = c(Linf = Lmax_observed, K = 0.01, t0 = -5),
  upper = c(Linf = Lmax_observed * 1.15, K = 2, t0 = 0),
  control = nls.control(maxiter = 1000)
)

summary(BM_model_3p)

# Extract coefficients
Linf <- coef(BM_model_3p)["Linf"]
K    <- coef(BM_model_3p)["K"]
t0   <- coef(BM_model_3p)["t0"]

# Age sequence for smooth curve
age_seq <- seq(0, max(BMdata$Age) + 1, length.out = 200)
pred_3p <- Linf * (1 - exp(-K * (age_seq - t0)))

# Plot
plot(BMdata$Age, BMdata$Length,
     pch = 16, col = "steelblue",
     xlab = "Age (years)",
     ylab = "Total Length (cm)",
     main = "Von Bertalanffy Growth Curve - Bhagalpur Mahseer",
     xlim = c(0, max(BMdata$Age) + 1),
     ylim = c(0, max(BMdata$Length) + 2))

# Fitted curve
lines(age_seq, pred_3p, col = "red", lwd = 2)

# Asymptote line
abline(h = Linf, col = "darkgray", lty = 2, lwd = 1.5)

# Legend
legend("bottomright",
       legend = c("Observed",
                  paste0("VBGF fit"),
                  paste0("Linf = ", round(Linf, 2), " cm"),
                  paste0("K = ",    round(K, 3),    " yr⁻¹"),
                  paste0("t0 = ",   round(t0, 2),   " yr")),
       col    = c("steelblue", "red", NA, NA, NA),
       pch    = c(16, NA, NA, NA, NA),
       lty    = c(NA, 1,  NA, NA, NA),
       lwd    = c(NA, 2,  NA, NA, NA),
       bty    = "n")

#--------------------------2P VBGF BHAGALPUR (BAYESIANS)----------------------------
#==================== LOAD DATA ====================

library(readxl)
library(dplyr)
library(brms)
library(cmdstanr)
library(ggplot2)

BMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/BM VBGF.xlsx")

BMdata <- BMdata %>%
  mutate(
    Age = as.numeric(Age),
    Length = as.numeric(Length)
  ) %>%
  na.omit()

#==================== MAXIMUM OBSERVED LENGTH ====================
# Observed maximum length
Lmax_observed <- max(BMdata$Length)

# Starting Linf
Linf_start <- Lmax_observed * 1.1


#==================== DEFINE VBGF MODEL ====================

vb_formula_2p <- bf(
  Length ~ Linf * (1 - exp(-K * Age)),
  Linf ~ 1,
  K ~ 1,
  nl = TRUE
)

#==================== PRIORS ====================
# Replace 40 with your approximate Linf value if needed

priors_2p <- c(
  prior(normal(25, 2),  nlpar = "Linf", lb = 15),
  prior(normal(0.4, 0.1),  nlpar = "K",    lb = 0),
  prior(gamma(0.01, 0.01),  class = "shape")
)

#==================== FIT MODEL ====================

BM_bayes_2p <- brm(
  formula = vb_formula_2p,
  data = BMdata,
  family = Gamma(link = "identity"),
  prior = priors_2p,
  chains = 4,
  iter = 8000,
  warmup = 3000,
  cores = 4,
  backend = "cmdstanr",
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 15
  ),
  seed = 123
)
summary(BM_bayes_2p)

#--------------------3P VBGF BHAGALPUR (BAYESIANS)---------------------------------
library(readxl)
library(dplyr)
library(brms)
library(cmdstanr)
library(ggplot2)

# Import data
BMdata <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/VBGF (REVISION)/BM VBGF.xlsx")

BMdata <- BMdata %>%
  mutate(
    Age = as.numeric(Age),
    Length = as.numeric(Length)
  ) %>%
  na.omit()


# 3-parameter VBGF formula
vb_formula_3p <- bf(
  Length ~ Linf * (1 - exp(-K * (Age - t0))),
  Linf ~ 1,
  K ~ 1,
  t0 ~ 1,
  nl = TRUE
)

# Priors
priors_3p <- c(
  prior(normal(25, 2),  nlpar = "Linf", lb = 15),
  prior(normal(0.4, 0.1),  nlpar = "K",    lb = 0),
  prior(normal(-0.5, 0.5),  nlpar = "t0"),            # no lb — t0 can be negative
  prior(gamma(0.01, 0.01),  class = "shape")
)

# Fit Bayesian 3P model
BM_bayes_3p <- brm(
  formula = vb_formula_3p,
  data = BMdata,
  family = Gamma(link = "identity"),               #student() gaussian()
  prior = priors_3p,
  chains = 4,
  iter = 8000,
  warmup = 3000,
  cores = 4,
  backend = "cmdstanr",
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 15
  ),
  seed = 123
)

# Model summary
summary(BM_bayes_3p)

#DIAGNOSTICS 3P bayes==========================================
# Check L∞ vs K correlation in posterior
draws <- as_draws_df(BM_bayes_3p)
cor(draws$b_Linf_Intercept, draws$b_K_Intercept)

library(bayesplot)

# Pairs plot of all three growth parameters
mcmc_pairs(draws, 
           pars = c("b_Linf_Intercept", "b_K_Intercept", "b_t0_Intercept"),
           off_diag_args = list(size = 0.5, alpha = 0.3))


library(bayesplot)

pp_check(BM_bayes_3p, ndraws = 100)

# Check for systematic bias across the range
pp_check(BM_bayes_3p, type = "scatter_avg")

# Check residual distribution
pp_check(BM_bayes_3p, type = "error_hist", ndraws = 11)

# Check if variance is consistent across fitted values (heteroscedasticity)
pp_check(BM_bayes_3p, type = "error_scatter_avg")

# AIC comparison
library(AICcmodavg)

AICcmodavg::AICc(BM_model_2p)
AICcmodavg::AICc(BM_model_3p)

# BIC comparison
BIC(BM_model_2p, BM_model_3p)

#LOOWAIC
loo_BM2P <- loo(BM_bayes_2p)
looicBM2P <- loo_BM2P$estimates["looic", "Estimate"]
looicBM2P

loo_BM3P <- loo(BM_bayes_3p)
looicBM3P <- loo_BM3P$estimates["looic", "Estimate"]
looicBM3P

pred_nls2 <- predict(BM_model_2p)
rmse_nls2 <- sqrt(mean((BMdata$Length - pred_nls2)^2))
rmse_nls2

pred_nls3 <- predict(BM_model_3p)
rmse_nls3 <- sqrt(mean((BMdata$Length - pred_nls3)^2))
rmse_nls3

pred_bayes2 <- fitted(BM_bayes_2p)[,1]

rmse_bayes2 <- sqrt(mean((BMdata$Length - pred_bayes2)^2))
rmse_bayes2

pred_bayes3 <- fitted(BM_bayes_3p)[,1]

rmse_bayes3 <- sqrt(mean((BMdata$Length - pred_bayes3)^2))
rmse_bayes3

model_compare <- data.frame(
  Model = c("NLS_2P","NLS_3P","Bayes_2P","Bayes_3P"),
  RMSE = c(rmse_nls2, rmse_nls3, rmse_bayes2, rmse_bayes3)
)

model_compare

# Maximum observed length
Lmax <- max(BMdata$Length, na.rm = TRUE)

# Froese & Binohlan equation
log_Linf <- 0.044 + 0.9841 * log10(Lmax)

# Convert back from log
BMLinf_FB <- 10^log_Linf

BMLinf_FB

citation("cmdstanr")
