#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game ) #Returns 
} 



#' @title
#'   Select a door in the Monty Hall Problem game.
#'
#' @description
#'   `select_door()` returns a random door (1, 2, or 3).
#'
#' @details
#'   This simulates the contestant’s initial choice of one of the three
#'   available doors. This is independent of the car and goat locations.
#'
#' @param ... no arguments are used by the function.
#'
#' @return An integer 1, 2, or 3 indicating the chosen door.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}


#' @title
#'    Reveal a goat door.
#'
#' @description
#'   `open_goat_door()` returns a door that the host opens
#'    to reveal a goat.
#'
#' @details
#'    If the contestant initially selected the car, then the host chooses
#'    at random one of the two goat doors. If the contestant initially
#'    selected a goat, the host opens the other goat door.
#'
#' @param game A character vector created by `create_game()` representing
#'   the hidden prizes.
#' @param a.pick An integer (1, 2, or 3) indicating the contestant’s initial choice.
#'
#' @return An integer 1, 2, or 3 indicating the opened goat door.
#'
#' @examples
#'   gg <- create_game()
#'   choice <- select_door()
#'   open_goat_door(gg, choice)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'    Change or stay with the initial door.
#'
#' @description
#'   `change_door()` determines the contestant’s final choice:
#'    stay or switch.
#'
#' @details
#'    If `stay = TRUE`, the contestant keeps their original door.
#'    If `stay = FALSE`, the contestant switches to the other unopened
#'    door that is not the initial pick. The host has already revealed the 
#'    goat behind the door not already mentioned.
#'
#'
#' @param stay Logical, indicating whether to stay (`TRUE`) or switch (`FALSE`).
#' @param opened.door An integer (1, 2, or 3) indicating the door opened by the host.
#' @param a.pick An integer (1, 2, or 3) representing the contestant’s initial choice.
#'
#' @return An integer between 1 and 3 corresponding to the contestant’s
#'    final choice.
#'
#' @examples
#'   gg <- create_game()
#'   choice <- select_door()
#'   o <- open_goat_door(gg, choice)
#'   change_door(stay = FALSE, opened.door = o, a.pick = choice)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}


#' @title
#'    Determine whether the contestant won.
#'
#' @description
#'    `determine_winner()` checks whether the final pick corresponds
#'    to the car or a goat.
#'
#' @details
#'   The function compares the contestant’s final pick to the
#'   hidden prize vector. Returns `"WIN"` if the final door
#'   hides the car and `"LOSE"` otherwise.
#'
#' @param final.pick An integer (1, 2, or 3) representing the contestant’s final door.
#' @param game A character vector created by `create_game()`.
#'
#' @return A character string: `"WIN"` or `"LOSE"`.
#'
#' @examples
#'   gg <- create_game()
#'   choice <- select_door()
#'   o <- open_goat_door(g, p)
#'   final <- change_door(FALSE, o, p)
#'   determine_winner(final, gg)
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play one round of the Monty Hall game.
#'
#' @description
#'   `play_game()` simulates a full round of the Monty Hall game
#'   including the contestant’s initial choice, the host opening a goat door,
#'   and the outcome for both stay and switch strategies.
#'
#' @details
#'   Returns a data frame with two rows, one for the "stay" strategy and one
#'   for the "switch" strategy, and a column indicating whether that strategy
#'   resulted in a win or loss.
#'
#' @param ... no arguments are used by the function.
#'
#' @return A data frame with two columns: `strategy` and `outcome`.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title
#'   Simulate multiple Monty Hall games.
#'
#' @description
#'   `play_n_games()` runs n repetitions of the Monty Hall game
#'   and summarizes the win proportions for each strategy.
#'
#' @details
#'   The function calls `play_game()` `n` times, collects the results,
#'   and calculates the probability of winning when choosing to stay
#'   versus switch.
#'
#' @param n Integer, number of games to simulate (default = 100).
#'
#' @return A data frame with the raw results of all simulated games.
#'   A summary table of win rates is also printed to the console.
#'
#' @examples
#'   play_n_games(1000)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
