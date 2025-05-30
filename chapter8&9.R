
library(pryr)
parenvs(all = TRUE)

as.environment("package:stats")
globalenv()
baseenv()
emptyenv()
parent.env(globalenv())
ls(emptyenv())
ls(globalenv())

head(globalenv()$deck, 3)
assign("new", "Hello Global", envir = globalenv())
globalenv()$new
environment()
new
new <- "Hello Active"
new
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

show_env <- function(){
  list(ran.in = environment(), 
       parent = parent.env(environment()), 
       objects = ls.str(environment()))
}

show_env()
show_env()
environment(show_env)
environment(parenvs)
show_env <- function(){
  a <- 1
  b <- 2
  c <- 3
  list(ran.in = environment(), 
       parent = parent.env(environment()), 
       objects = ls.str(environment()))
}
show_env()
foo <- "take me to your runtime"

show_env <- function(x = foo){
  list(ran.in = environment(), 
       parent = parent.env(environment()), 
       objects = ls.str(environment()))
}

show_env()

deal <- function() {
  deck[1, ]
}
environment(deal)
deal()
deal()
deal()

DECK <- deck

deck <- deck[-1, ]

head(deck, 3)

deal <- function() {
  card <- deck[1, ]
  deck <- deck[-1, ]
  card
}

deal <- function() {
  card <- deck[1, ]
  assign("deck", deck[-1, ], envir = globalenv())
  card
}
deal()
deal()
deal()
shuffle <- function(cards) { 
  random <- sample(1:52, size = 52)
  cards[random, ]
}
head(deck, 3)
a <- shuffle(deck)

head(deck, 3)
head(a, 3)
shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}
shuffle()

deal()
deal()
setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
}

setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)

deal <- cards$deal
shuffle <- cards$shuffle

deal
shuffle
environment(deal)
environment(shuffle)
setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle

rm(deck)

shuffle()

deal()
deal()


play()
play()

get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

get_symbols()
get_symbols()
get_symbols()

score(c("DD", "DD", "DD"))
play <- function() {
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}

play <- function() {
  
  # step 1: generate symbols
  symbols <- get_symbols()
  
  # step 2: display the symbols
  print(symbols)
  
  # step 3: score the symbols
  score(symbols)
}

if (this) {
  that
}

if (num < 0) {
  num <- num * -1
}

num <- -2

if (num < 0) {
  num <- num * -1
}

num

num <- 4

if (num < 0) {
  num <- num * -1
}

num

num <- -1

if (num < 0) {
  print("num is negative.")
  print("Don't worry, I'll fix it.")
  num <- num * -1
  print("Now num is positive.")
}

num

x <- 1
if (3 == 3) {
  x <- 2
}
x

x <- 1
if (TRUE) {
  x <- 2
}
x

x <- 1
if (x == 1) {
  x <- 2
  if (x == 1) {
    x <- 3
  }
}
x

if (this) {
  Plan A
} else {
  Plan B
}

a <- 3.14
dec <- a - trunc(a)
dec

if (dec >= 0.5) {
  a <- trunc(a) + 1
} else {
  a <- trunc(a)
}

a


a <- 1
b <- 1

if (a > b) {
  print("A wins!")
} else if (a < b) {
  print("B wins!")
} else {
  print("Tie.")
}

if ( # Case 1: all the same <1>) {
  prize <- # look up the prize <3>
  } else if ( # Case 2: all bars <2> ) {
    prize <- # assign $5 <4>
    } else {
      # count cherries <5>
      prize <- # calculate a prize <7>
    }

# count diamonds <6>
# double the prize if necessary <8>

score <- function(symbols) {
  
  # calculate a prize
  
  prize
}

symbols <- c("7", "7", "7")

symbols <- c("B", "BB", "BBB")
symbols <- c("C", "DD", "0")

symbols
##  "7" "7" "7"

symbols[1] == symbols[2] & symbols[2] == symbols[3]
## TRUE

symbols[1] == symbols[2] & symbols[1] == symbols[3]
## TRUE

all(symbols == symbols[1])
## TRUE

length(unique(symbols) == 1)

same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]

if (same) {
  prize <- # look up the prize
} else if ( # Case 2: all bars ) {
  prize <- # assign $5
  } else {
    # count cherries
    prize <- # calculate a prize
  }

# count diamonds
# double the prize if necessary

symbols <- c("B", "BBB", "BB")

symbols[1] == "B" | symbols[1] == "BB" | symbols[1] == "BBB" &
  symbols[2] == "B" | symbols[2] == "BB" | symbols[2] == "BBB" &
  symbols[3] == "B" | symbols[3] == "BB" | symbols[3] == "BBB"

all(symbols %in% c("B", "BB", "BBB"))

same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B", "BB", "BBB")

if (same) {
  prize <- # look up the prize
} else if (all(bars)) {
  prize <- # assign $5
} else {
  # count cherries
  prize <- # calculate a prize
}

# count diamonds
# double the prize if necessary

symbols <- c("B", "B", "B")
all(symbols %in% c("B", "BB", "BBB"))

if (same) {
  symbol <- symbols[1]
  if (symbol == "DD") {
    prize <- 800
  } else if (symbol == "7") {
    prize <- 80
  } else if (symbol == "BBB") {
    prize <- 40
  } else if (symbol == "BB") {
    prize <- 5
  } else if (symbol == "B") {
    prize <- 10
  } else if (symbol == "C") {
    prize <- 10
  } else if (symbol == "0") {
    prize <- 0
  }
}

payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
             "B" = 10, "C" = 10, "0" = 0)
payouts

payouts["DD"]
##  DD 
## 100 

payouts["B"]
##  B
## 10

unname(payouts["DD"])

symbols <- c("7", "7", "7")
symbols[1]
## "7"

payouts[symbols[1]]
##  7 
## 80 

symbols <- c("C", "C", "C")
payouts[symbols[1]]
##  C 
## 10 

same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B", "BB", "BBB")

if (same) {
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
               "B" = 10, "C" = 10, "0" = 0)
  prize <- unname(payouts[symbols[1]])
} else if (all(bars)) {
  prize <- # assign $5
} else {
  # count cherries
  prize <- # calculate a prize
}

# count diamonds
# double the prize if necessary

same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B", "BB", "BBB")

if (same) {
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
               "B" = 10, "C" = 10, "0" = 0)
  prize <- unname(payouts[symbols[1]])
} else if (all(bars)) {
  prize <- 5
} else {
  # count cherries
  prize <- # calculate a prize
}

# count diamonds
# double the prize if necessary

symbols <- c("C", "DD", "C")
symbols == "C"

sum(symbols == "C")
sum(symbols == "DD")
same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B", "BB", "BBB")

if (same) {
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
               "B" = 10, "C" = 10, "0" = 0)
  prize <- unname(payouts[symbols[1]])
} else if (all(bars)) {
  prize <- 5
} else {
  cherries <- sum(symbols == "C")
  prize <- # calculate a prize
}

diamonds <- sum(symbols == "DD")
# double the prize if necessary

if (cherries == 2) {
  prize <- 5
} else if (cherries == 1) {
  prize <- 2
} else {}
prize <- 0
}

c(0, 2, 5)
cherries + 1
## 1

c(0, 2, 5)[cherries + 1]
## 0

cherries + 1
## 2

c(0, 2, 5)[cherries + 1]
## 2

cherries + 1
## 3

c(0, 2, 5)[cherries + 1]
## 5

same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B", "BB", "BBB")

if (same) {
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
               "B" = 10, "C" = 10, "0" = 0)
  prize <- unname(payouts[symbols[1]])
} else if (all(bars)) {
  prize <- 5
} else {
  cherries <- sum(symbols == "C")
  prize <- c(0, 2, 5)[cherries + 1]
}

diamonds <- sum(symbols == "DD")
# double the prize if necessary

prize * 1      # 1 = 2 ^ 0
prize * 2      # 2 = 2 ^ 1
prize * 4      # 4 = 2 ^ 2
prize * 8      # 8 = 2 ^ 3

prize * 2 ^ diamonds

same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B", "BB", "BBB")

if (same) {
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
               "B" = 10, "C" = 10, "0" = 0)
  prize <- unname(payouts[symbols[1]])
} else if (all(bars)) {
  prize <- 5
} else {
  cherries <- sum(symbols == "C")
  prize <- c(0, 2, 5)[cherries + 1]
}

diamonds <- sum(symbols == "DD")
prize * 2 ^ diamonds

# identify case
same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B", "BB", "BBB")

# get prize
if (same) {
  payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
               "B" = 10, "C" = 10, "0" = 0)
  prize <- unname(payouts[symbols[1]])
} else if (all(bars)) {
  prize <- 5
} else {
  cherries <- sum(symbols == "C")
  prize <- c(0, 2, 5)[cherries + 1]
}

# adjust for diamonds
diamonds <- sum(symbols == "DD")
prize * 2 ^ diamonds

score <- function (symbols) {
  # identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  
  # get prize
  if (same) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[symbols[1]])
  } else if (all(bars)) {
    prize <- 5
  } else {
    cherries <- sum(symbols == "C")
    prize <- c(0, 2, 5)[cherries + 1]
  }
  
  # adjust for diamonds
  diamonds <- sum(symbols == "DD")
  prize * 2 ^ diamonds
}

play <- function() {
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}

play()
play()
play()












