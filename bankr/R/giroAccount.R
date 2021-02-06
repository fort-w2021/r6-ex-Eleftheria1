#' GiroAccount class
#'
#' @description R6 class to represent a Giro bank account
#'
#' Additional features to the general `Account` are a maximal overdraft
#' limit as well as a fee to punish negative account balances.
GiroAccount <- R6::R6Class(
  classname = "GiroAccount",
  inherit = Account,
  public = list(
    #' @description initialize bank account and check the inputs
    #'
    #' @param account_holder single character vector
    #' @param balance single numeric value
    #' @param overdraft_max single non negative numeric value
    #' @param overdraft_fee_rate single numeric value between 0 and 1
    initialize = function(account_holder, balance,
                          overdraft_max = Inf, overdraft_fee_rate = 0.09) {
      super$initialize(account_holder, balance)
      checkmate::assert_numeric(overdraft_max,
        lower = 0, any.missing = FALSE,
        len = 1
      )
      checkmate::assert_numeric(overdraft_fee_rate,
        lower = 0, upper = 1,
        len = 1, any.missing = FALSE
      )
      private$..overdraft_max <- overdraft_max
      private$..overdraft_fee_rate <- overdraft_fee_rate
    },
    #' @description withdraw money from the giro account.
    #' charges fee if giro account negative. throws error,
    #' if overdraft limit is reached
    #'
    #' @param amount single non negative value to withdraw
    withdraw = function(amount) {
      if (-private$..overdraft_max > (private$..balance - amount)) {
        possible_withdraw <- private$..overdraft_max + private$..balance
        stop(paste(
          "You've reached your overdraft limit. At most you can withdraw",
          possible_withdraw
        ))
      }
      super$withdraw(amount)
      if (private$..balance < 0) {
        cat(
          "Your account balance is negative :(\nWe have to charge you a overdraft fee of",
          private$..overdraft_fee_rate * -private$..balance
        )
        private$..balance <- private$..balance * (1 + private$..overdraft_fee_rate)
      }
    }
  ),
  private = list(
    ..overdraft_max = 0,
    ..overdraft_fee_rate = 0
  )
)
