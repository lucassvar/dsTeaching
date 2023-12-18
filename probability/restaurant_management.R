# create sides combinations (order doesn't matter)
sides_combinations <- combinations(6, 2)
sides_combinations <- paste(sides_combinations[,1], sides_combinations[,2])

# generate all possible combinations
menu_combinations <- expand.grid(Entree = 1:6, Sides = sides_combinations, Drink = 1:2)

# print the total number of meal combinations (divided by 2 cause order between Side1 and Side2 doesn't matter)
print(nrow(menu_combinations))



# for 3 drink options
menu_combinations <- expand.grid(Entree = 1:6, Sides = sides_combinations, Drink = 1:3) # increased the number of drink choices
print(nrow(menu_combinations))



# option to choose 3 sides
sides_combinations <- combinations(6, 3)
sides_combinations <- paste(sides_combinations[,1], sides_combinations[,2])

# generate all possible combinations
menu_combinations <- expand.grid(Entree = entree_options, Sides = sides_combinations, Drink = drink_options)

# print the total number of meal combinations (divided by 2 cause order between Side1 and Side2 doesn't matter)
print(nrow(menu_combinations))



# find the minimum number of entree options required to generate over 365 combinations
sides_combinations <- combinations(6, 2)
sides_combinations <- paste(sides_combinations[,1], sides_combinations[,2])

# function to find the lowest entree options to reach 365 combinations
num_entrees <- function(n) {
  menu_combinations <- expand.grid(Entree = 1:n, Sides = sides_combinations, Drink = 1:3)
  option_num <- paste(n, nrow(menu_combinations), sep = ": ")
}

sapply(seq(1, 12), num_entrees)



# find the minimum number of sides options required to generate over 365 combinations
num_sides <- function(n) {
  sides_combinations <- combinations(n, 2)
  sides_combinations <- paste(sides_combinations[,1], sides_combinations[,2])
  menu_combinations <- expand.grid(Entree = 1:6, Sides = sides_combinations, Drink = 1:3)
  option_num <- paste(n, nrow(menu_combinations), sep = ": ")
}

sapply(seq(2, 12), num_sides)
