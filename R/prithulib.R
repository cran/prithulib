#' @title  Rolling an unbiased die
#'
#' @description  Rolls a die for 'n' number of times
#' @usage roll(n = 1, size = 6)
#' @author Prithul Chaturvedi
#' @param n is the number of times to roll the die (default 1)
#' @param size is the number of faces on the die (default 6)
#' @return Outcome(s) of rolling of the die
#' @examples roll(2,10)
#' @export
#'
roll <- function(n=1, size = 6)
{
  d <- c(1:size)
  r <- sample(d, n, replace = T)
  return(r)
}



#' @title  Tossing an unbiased coin
#'
#' @description  Tosses a coin for 'n' number of times
#' @usage toss(n = 1)
#' @author Prithul Chaturvedi
#'
#' @param n is the number of times the coin is tossed (default 1)
#' @return Outcome(s) of tossing the coin
#' @examples toss(3)
#' @export
#'
toss <- function(n=1)
{
  d <- c("Heads", "Tails")
  r <- sample(d, n, replace = T)
  return(r)
}



#' @title  Rock, Paper, Scissors
#'
#' @description  Play Rock, Paper, Scissors
#' @usage rpc()
#' @author Prithul Chaturvedi
#' @return A move
#' @examples rpc()
#' @export
#'
rpc <- function()
{
  d <- c("Rock", "Paper", "Scissors")
  r <- sample(d, 1, replace = T)
  return(r)
}



#' @title  Choosing a card
#'
#' @description  An event of choosing a card from a pack of 52 cards (Joker optional)
#' @usage cards(n, joker = FALSE)
#' @author Prithul Chaturvedi
#' @param n is the number of cards to be chosen (default 1)
#' @param joker TRUE includes the Joker card in the deck (default FALSE)
#' @return The chosen card(s)
#' @examples cards(3, joker = TRUE)
#' @export
#'
cards <- function(n=1, joker = FALSE)
{
  if (joker == FALSE)
  {
    a <- c('Ace of clubs (\u2663)','King of clubs (\u2663)','Queen of clubs (\u2663)','Jack of clubs (\u2663)','10 of clubs (\u2663)','9 of clubs (\u2663)','6 of clubs (\u2663)','8 of clubs (\u2663)','7 of clubs (\u2663)','5 of clubs (\u2663)','4 of clubs (\u2663)','3 of clubs (\u2663)','2 of clubs (\u2663)',
           'Ace of diamonds (\u2666)','King of diamonds (\u2666)','Queen of diamonds (\u2666)','Jack of diamonds (\u2666)','10 of diamonds (\u2666)','9 of diamonds (\u2666)','8 of diamonds (\u2666)','7 of diamonds (\u2666)','6 of diamonds (\u2666)','5 of diamonds (\u2666)','4 of diamonds (\u2666)','3 of diamonds (\u2666)','2 of diamonds (\u2666)',
           '3 of hearts (\u2665)','4 of hearts (\u2665)','5 of hearts (\u2665)','6 of hearts (\u2665)','7 of hearts (\u2665)','8 of hearts (\u2665)','9 of hearts (\u2665)','10 of hearts (\u2665)','Jack of hearts (\u2665)','Queen of hearts (\u2665)','King of hearts (\u2665)','2 of hearts (\u2665)','Ace of hearts (\u2665)',
           'King of spades (\u2660)','Queen of spades (\u2660)','Jack of spades (\u2660)','10 of spades (\u2660)','9 of spades (\u2660)','8 of spades (\u2660)','7 of spades (\u2660)','6 of spades (\u2660)','5 of spades (\u2660)','4 of spades (\u2660)','3 of spades (\u2660)','2 of spades (\u2660)','Ace of spades (\u2660)')
  }
  else
  {
    a <- c("Joker", 'Ace of clubs (\u2663)','King of clubs (\u2663)','Queen of clubs (\u2663)','Jack of clubs (\u2663)','10 of clubs (\u2663)','9 of clubs (\u2663)','6 of clubs (\u2663)','8 of clubs (\u2663)','7 of clubs (\u2663)','5 of clubs (\u2663)','4 of clubs (\u2663)','3 of clubs (\u2663)','2 of clubs (\u2663)',
           'Ace of diamonds (\u2666)','King of diamonds (\u2666)','Queen of diamonds (\u2666)','Jack of diamonds (\u2666)','10 of diamonds (\u2666)','9 of diamonds (\u2666)','8 of diamonds (\u2666)','7 of diamonds (\u2666)','6 of diamonds (\u2666)','5 of diamonds (\u2666)','4 of diamonds (\u2666)','3 of diamonds (\u2666)','2 of diamonds (\u2666)',
           '3 of hearts (\u2665)','4 of hearts (\u2665)','5 of hearts (\u2665)','6 of hearts (\u2665)','7 of hearts (\u2665)','8 of hearts (\u2665)','9 of hearts (\u2665)','10 of hearts (\u2665)','Jack of hearts (\u2665)','Queen of hearts (\u2665)','King of hearts (\u2665)','2 of hearts (\u2665)','Ace of hearts (\u2665)',
           'King of spades (\u2660)','Queen of spades (\u2660)','Jack of spades (\u2660)','10 of spades (\u2660)','9 of spades (\u2660)','8 of spades (\u2660)','7 of spades (\u2660)','6 of spades (\u2660)','5 of spades (\u2660)','4 of spades (\u2660)','3 of spades (\u2660)','2 of spades (\u2660)','Ace of spades (\u2660)')

  }
  r <- sample(a, n)
  return(r)
}

