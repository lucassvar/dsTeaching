# load the gtools library for permutation functions
library(gtools)

# set a seed for reproducibility
set.seed(1)

# define the names of the runners
runners <- c("J1", "J2", "J3", "1", "2", "3", "4", "5")

# generate all possible permutations of 3 runners from the given list
possible_results <- permutations(8, 3, v = runners)

# define the Jamaican runners
jamaica_runners <- c("J1", "J2", "J3")

# calculate the probability of all three Jamaican runners winning in a permutation
mean(possible_results[, 1] %in% jamaica_runners & possible_results[, 2] %in% jamaica_runners & possible_results[, 3] %in% jamaica_runners)


# define the countries of the runners
runners_countries <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

# simulate B iterations of medal results and calculate the probability of all three medals going to Jamaica
B <- 10000
results <- replicate(B, {
  medalists <- sample(runners_countries, 3)
  (medalists[1] == "Jamaica" & medalists[2] == "Jamaica" & medalists[3] == "Jamaica")
})
mean(results)
