# Habe Teilaufgabe iii) leider zu spät gelesen und schon von Anfang an balance
# als private behandelt. Da es ohnehin besser ist diese als private zu behandeln,
# damit der User die balance nicht manuell verändern kann.
# Dies grenzt die Schreibrechte sinnvoll ein.
# Hab die Funktion aus i) und ii) dann aus zeitgründen nicht mehr zurückverändert...
# Antwort iv: der default sollte von deep = FALSE auf deep = TRUE gesetzt
# werden damit auch der Transaktionslog mitgeklont wird (da nicht deepe
# clones nicht rekursiv sind)

#' Account class
#'
#' @description R6 class to represent a general bank account
#'
#' The bank account comes with the following basic functionality and has
#' the private fields `..balance` and `..account_holder` that store the most
#' important information.
Account <- R6::R6Class(
  classname = "Account",
  public = list(
    #' @description initialize bank account and check the inputs
    #'
    #' @param account_holder single character vector
    #' @param balance single numeric value
    initialize = function(account_holder, balance){
      balance <- try(as.numeric(balance), silent = TRUE)
      checkmate::assert_numeric(balance, all.missing = FALSE, len = 1,
                                finite = TRUE)
      checkmate::assert_character(account_holder, all.missing = FALSE, len = 1)
      private$..balance <- balance
      private$..account_holder <- account_holder
    },
    #' @description deposit money on the bank account
    #'
    #' @param amount single non negative value to deposit
    deposit = function(amount){
      amount <- try(as.numeric(amount), silent = TRUE)
      checkmate::assert_numeric(amount, lower = 0, any.missing = FALSE,
                                finite = TRUE, len = 1)
      private$..balance <- sum(amount, private$..balance)
    },
    #' @description withdraw money from the bank account
    #'
    #' @param amount single non negative value to withdraw
    withdraw = function(amount){
      amount <- try(as.numeric(amount), silent = TRUE)
      checkmate::assert_numeric(amount, lower = 0, finite = TRUE,
                                any.missing = FALSE, len = 1)
      private$..balance <- private$..balance - amount
    },
    #' @description print account holder and balance
    #'
    #' @return just prints in the console
    print = function() {
      cat(private$..account_holder, "has the balance:", private$..balance,
          "dollars.")
      invisible(self)
    }
  ),
  private = list(
    ..balance = 0,
    ..account_holder = "Max Mustermann"
  ),
  active = list(
    #' @field balance
    #' read only access to variable `balance`
    balance = function() private$..balance
  )
)
