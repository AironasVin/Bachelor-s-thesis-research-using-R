# Load required packages
library(readxl)
library(plm)
library(lmtest)
library(nlme)
library(car)
library(sandwich)
library(glmmTMB)
library(performance)
library(xtsum)
library(xtable)
library(stargazer)

# Load the data
data <- read_excel("/Users/aironas.vinickas/Desktop/Bakalauras/data.xlsx")
attach(data)

# Convert data to panel data format
panel_data <- pdata.frame(data, index = c("Symbol", "Period"))

# Perform panel unit root tests for variables 
# (index = individual allows to separate firms; variables are stationary because they are relative or log-transformed)
purtest(panel_data$tobinsq, pmax = 1, index = "individual", exo = "intercept", test = "madwu") # variable is stationary (p-value very low)
purtest(panel_data$lnsize, pmax = 1, index = "individual", exo = "intercept", test = "madwu")
purtest(panel_data$prof, pmax = 1, index = "individual", exo = "intercept", test = "madwu")
purtest(panel_data$liq, pmax = 1, index = "individual", exo = "intercept", test = "madwu")
purtest(panel_data$lev, pmax = 1, index = "individual", exo = "intercept", test = "madwu")
purtest(panel_data$growth, pmax = 1, index = "individual", exo = "intercept", test = "madwu")

# Build random effects models
remodel1 <- plm(tobinsq ~ hedgdum + lnsize + prof + liq + lev + growth + div,
                data = panel_data, index = c("Symbol", "Period"), model = "random")
remodel2 <- plm(tobinsq ~ fxhedge + irhedge + cmhedge + lnsize + prof + liq + lev + growth + div,
                data = panel_data, index = c("Symbol", "Period"), model = "random")

# Build fixed effects models
femodel1 <- plm(tobinsq ~ hedgdum + lnsize + prof + liq + lev + growth + div,
                data = panel_data, index = c("Symbol", "Period"), model = "within", effect = "time")
femodel2 <- plm(tobinsq ~ fxhedge + irhedge + cmhedge + lnsize + prof + liq + lev + growth + div,
                data = panel_data, index = c("Symbol", "Period"), model = "within", effect = "time")

# Build pooling OLS models
pmodel1 <- plm(tobinsq ~ hedgdum + lnsize + prof + liq + lev + growth + div,
               data = panel_data, index = c("Symbol", "Period"), model = "pooling")
pmodel2 <- plm(tobinsq ~ fxhedge + irhedge + cmhedge + lnsize + prof + liq + lev + growth + div,
               data = panel_data, index = c("Symbol", "Period"), model = "pooling")

# Model diagnostics: choose the better model
ranef(remodel1)
fixef(femodel1)

# F-test: if p-value is very low, fixed effects are better than pooled OLS
pFtest(femodel1, pmodel1)
pFtest(femodel2, pmodel2)

# Hausman test: if p-value is very low, fixed effects are better than random effects
phtest(femodel1, remodel1)
phtest(femodel2, remodel2)
# Use fixed effects model

# Test for heteroskedasticity
plmtest(femodel1, type = "bp") # heteroskedasticity detected
plmtest(femodel2, type = "bp") # heteroskedasticity detected

# Test for autocorrelation
pbgtest(femodel1) # serial correlation detected
pbgtest(femodel2) # serial correlation detected

# Residual analysis and diagnostics
residuals(femodel1)
hist(residuals(femodel2), breaks = 20, main = "Residuals")
shapiro.test(resid(femodel2))

# Multicollinearity diagnostics
independent_vars <- femodel1$model[, -1]
vif(femodel1)
check_collinearity(femodel1)
check_collinearity(femodel2)

# Robust standard errors (Arellano method)
check_heteroscedasticity(coeftest(femodel1, vcovHC(femodel1, method = "arellano")))
cor1 <- cor(vcovHC(femodel1, method = "arellano"))
cor2 <- cor(vcovHC(femodel2, method = "arellano"))

# Covariance matrix
covariance <- vcovHC(femodel1, method = "arellano")
covariance

# Descriptive statistics
xtsum(panel_data)
stargazer(panel_data, type = "text", title = "Descriptive statistics", digits = 1, out = "descriptive_stats1.txt")
stargazer(subset(panel_data, panel_data$hedgdum == 0), type = "text", title = "Descriptive statistics (No Hedge)", digits = 1, out = "descriptive_stats2.txt")
stargazer(subset(panel_data, panel_data$hedgdum == 1), type = "text", title = "Descriptive statistics (Hedge)", digits = 1, out = "descriptive_stats3.txt")

# Model output with robust SE
robust1 <- coeftest(femodel1, vcovHC(femodel1, method = "arellano"))
robust2 <- coeftest(femodel2, vcovHC(femodel2, method = "arellano"))

# Save output
stargazer(femodel1, femodel2, robust1, robust2, type = "text", out = "model_output.txt")
stargazer(cor1, cor2, type = "text", out = "correlations.txt")

# Robust estimates details
coef1 <- coef(robust1)
coef2 <- coef(robust2)
ste1 <- robust1[, 2]
ste2 <- robust2[, 2]
t_values1 <- robust1[, 3]
t_values2 <- robust2[, 3]
p_values1 <- robust1[, 4]
p_values2 <- robust2[, 4]

results_df1 <- data.frame(
  'Coefficients' = coef1,
  'Robust SE' = ste1,
  't-value' = t_values1,
  'p-value' = p_values1
)
results_df1

# Summary
summary(femodel1)
summary(femodel2)
