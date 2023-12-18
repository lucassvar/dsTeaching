library(dplyr)
library(ggplot2)
set.seed(123)

# generate data with student t distribution and calculate density
data_t <- rt(1000, df = 5)
density_t <- dt(data_t, df = 5)
plot_t <- data.frame(x = data_t, f = density_t) %>% ggplot(aes(x, f)) + geom_line()

# generate data with chi-squared distribution and calculate density
data_chi <- rchisq(1000, df = 3)
density_chi <- dchisq(data_chi, df = 3)
plot_chi <- data.frame(x = data_chi, f = density_chi) %>% ggplot(aes(x, f)) + geom_line()

# generate data with exponential distribution and calculate density
data_exp <- rexp(1000, rate = .2)
density_exp <- dexp(data_exp, rate = .2)
plot_exp <- data.frame(x = data_exp, f = density_exp) %>% ggplot(aes(x, f)) + geom_line()

# generate data with gamma distribution and calculate density
data_gamma <- rgamma(1000, shape = 2, rate = 1)
density_gamma <- dgamma(data_exp, shape = 2, rate = 1)
plot_gamma <- data.frame(x = data_gamma, f = density_gamma) %>% ggplot(aes(x, f)) + geom_line()

# generate data with exponential distribution and calculate density
data_beta <- rbeta(1000, shape1 = 2, shape2 = 5)
density_beta <- dbeta(data_exp, shape1 = 2, shape2 = 5)
plot_beta <- data.frame(x = data_beta, f = density_beta) %>% ggplot(aes(x, f)) + geom_line()
