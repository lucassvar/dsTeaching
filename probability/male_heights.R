library(tidyverse)
library(dslabs)
data(heights)

# define male heights
male_heights <- heights %>% filter(sex=="Male") %>% pull(height)

# define a function for computing CDF of "a"
func <- function(a){mean(male_heights <= a)}
1 - func(70) # probability of a male being taller than 70 inches

# it's possible to calculate it without the need of creating a function
1 - pnorm(70.5, mean(male_heights), sd(male_heights))

# plot distribution of exact heights in data
x <- male_heights
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


# using rnorm to generate results
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, mean(x), sd(x))    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 84)    # proportion of times that tallest person exceeded 7 feet (84 inches)
