# implement stack of bavarian cards
library(R6)


bavarian_cardstack <- R6Class(
  classname = "bavarian_cardstack",
  public = list(
    draw = function(n = 1) {
      n_cards <- length(private$..cards)
      if (n > n_cards) {
        stop(paste("There are only", n_cards, "left!"))
      }
      cards_drawn <- private$..cards[1:n]
      if (n == n_cards) {
        private$..cards <- character()
        message("The card stack is empty!")
      } else {
        private$..cards <- private$..cards[(n + 1):n_cards]
      }
      cards_drawn
    },
    fill_up_and_mix = function() self$initialize(),
    pick_up = function() {
      n_cards <- length(private$..cards)
      if (n_cards < 2) {
        stop(paste(
          "Pick up makes no sense if you have only",
          n_cards, "cards."
        ))
      }
      pick_up_index <- sample(1:(n_cards - 1), 1)
      lower_stack <- private$..cards[1:pick_up_index]
      upper_stack <- private$..cards[(pick_up_index + 1):n_cards]
      private$..cards <- c(upper_stack, lower_stack)
    },
    initialize = function() {
      color <- c("G", "H", "E", "S")
      value <- c(6:10, "U", "O", "K", "A")
      cards <- paste0(rep(color, each = 9), rep(value, times = 4))
      private$..cards <- sample(cards)
    },
    print = function(...) {
      cat("Hello players!\n")
      cat("  There are ", length(private$..cards), " cards left in the stack.\n")
      cat("\nHere they are:\n")
      cat(private$..cards)
    }
  ),
  private = list(
    ..cards = as.character()
  )
)

my_stack <- bavarian_cardstack$new()

# Antwort:
# Methoden welche interne Objekte modifizieren, wie zb draw() in diesem beispiel
# welches ..cards modifiziert könnten nicht wie hier benutzerfreundlich mit
# my_stack$draw() aufgerufen werden sondern man müsste an eine Zuweisung
# denken damit die Modifikation greift (my_stack <- draw(my_stack, n))
# also so richtig pfundig
