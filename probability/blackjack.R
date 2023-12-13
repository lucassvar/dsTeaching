library(gtools)

# code to generate a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
paste("Prob. drawing a king:", round(mean(deck %in% kings), 3))

# probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
paste("Prob. drawing two kings first:", round(sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings), 3))

# probability of a natural blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52, 2, v=deck) # all possible hands
paste("Prob. natural blackjack:", round(mean(hands[,1] %in% aces & hands[,2] %in% facecard), 3))

# monte carlo simulation for natural blackjack prob.
hand <- sample(deck, 2)
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)