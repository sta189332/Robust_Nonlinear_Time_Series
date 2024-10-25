# Install necessary packages if not already installed
#install.packages("ggplot2")

# Load the required package
library(ggplot2)

# Parameters for STAR model
alpha <- c(0.5, -0.3)  # Linear part coefficients
beta <- c(1.2, -0.5)   # Nonlinear part coefficients
gamma <- 5             # Smoothness parameter
c <- 0                 # Threshold parameter
n <- 500               # Number of data points

# Generate time series data
set.seed(42)
y <- numeric(n)
epsilon <- rnorm(n, mean = 0, sd = 1)  # White noise

for (t in 3:n) {
  G <- 1 / (1 + exp(-gamma * (y[t-1] - c)))  # Smooth transition function (logistic)
  y[t] <- alpha[1] + alpha[2] * y[t-1] + G * (beta[1] + beta[2] * y[t-1]) + epsilon[t]
}

# Plot the simulated time series
data <- data.frame(Time = 1:n, y = y)
ggplot(data, aes(x = Time, y = y)) +
  geom_line(color = "blue") +
  labs(title = "Simulated Nonlinear Time Series (STAR Model)",
       x = "Time", y = "y_t") +
  theme_minimal()

## Hypothesis for the RESET Test:
# Null Hypothesis (H₀): The model is correctly specified and linear. This means the relationship in the time series can be adequately explained by a linear model.

# Alternative Hypothesis (H₁): The model is misspecified and nonlinear. This suggests that higher-order terms (such as quadratic or cubic terms) should be included, indicating nonlinearity.

## Decision Rule:
# Significance Level (α): Choose a significance level (typically 0.05).

# Decision Criterion:
  
#  If p-value ≤ α (0.05): Reject the null hypothesis (H₀). This means there is evidence of nonlinearity in the time series.
# If p-value > α (0.05): Fail to reject the null hypothesis (H₀). This means the time series can be adequately modeled using a linear approach.

# Conducting the RESET Test in R
# Install the required package if not already installed
#install.packages("lmtest")

# Load the required package
library(lmtest)

# Fit a simple linear autoregressive model (AR(1)) for the time series "y"
linear_model <- lm(y ~ lag(y, -1))

# Perform the RESET test for linearity
reset_test <- resettest(linear_model, power = 2:3, type = "regressor")

# Print the result of the RESET test
print(reset_test)

# Conclusion Based on the Output:
#RESET = 0.30073, p-value = 0.7404
# Since the p-value (0.7404) is greater than the chosen significance level (0.05), we fail to reject the null hypothesis. This implies that there is no sufficient evidence to suggest nonlinearity in the time series data. Therefore, based on the RESET test, the time series can be adequately explained by a linear model.

# Final Conclusion:
# There is no evidence of nonlinearity in the time series based on the RESET test results.


  
  
  
  
  
  
  
