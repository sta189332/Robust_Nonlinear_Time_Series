# Install necessary packages if not already installed
install.packages("ggplot2")

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
