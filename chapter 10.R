num <- 1000000000
print(num)

class(num) <- c("POSIXct", "POSIXt")
print(num)

attributes(deck)

row.names(deck)

row.names(deck) <- 101:152

levels(deck) <- c("level 1", "level 2", "level 3")

attributes(deck)

one_play <- play()
one_play

attributes(one_play)

attr(one_play, "symbols") <- c("B", "0", "B")

attributes(one_play)

attr(one_play, "symbols")

one_play

one_play + 1

play <- function() {
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}

play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize
}

play()

two_play <- play()

two_play

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols)
}

three_play <- play()
three_play

slot_display <- function(prize){
  
  # extract symbols
  symbols <- attr(prize, "symbols")
  
  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")
  
  # combine symbol with prize as a character string
  # \n is special escape sequence for a new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")
  
  # display character string in console without quotes
  cat(string)
}

slot_display(one_play)

symbols <- attr(one_play, "symbols")

symbols

symbols <- paste(symbols, collapse = " ")

symbols

prize <- one_play
string <- paste(symbols, prize, sep = "\n$")

string

cat(string)

slot_display(play())
slot_display(play())

print(pi)
## 3.141593

pi
## 3.141593


print(head(deck))

head(deck)

print(play())

play()
num <- 1000000000
print(num)

class(num) <- c("POSIXct", "POSIXt")
print(num)

print

print.POSIXct

print.factor

methods(print)

class(one_play) <- "slots"

args(print)
print.slots <- function(x, ...) {
  cat("I'm using the print.slots method")
}

print(one_play)
one_play
rm(print.slots)

now <- Sys.time()
attributes(now)

print.slots <- function(x, ...) {
  slot_display(x)
}

one_play

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols)
}

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}

class(play())

play()
play()

methods(class = "factor")

play1 <- play()
play1

play2 <- play()
play2

c(play1, play2)

play1[1]

