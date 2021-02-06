test_that("init-Account", {
  expect_true(
    R6::is.R6(Account$new("Marvin", -10000))
  )
  expect_true(
    R6::is.R6(Account$new("Elisa", 1.4))
  )
  expect_true(
    R6::is.R6(Account$new("Günni", "1200"))
  )
  expect_error(Account$new(c("Marvin","Marvins"), -10000))
  expect_error(Account$new("FS", Inf))
  expect_error(Account$new("Marvin", NA))
  expect_warning(expect_error(Account$new("Marvin", "IOO")),regexp = "NA")
})

test_deposit_account <- Account$new("Deposit tester", 0)
test_that("deposit-Account", {
  test_deposit_account$deposit(10)
  expect_equal(test_deposit_account$balance, 10)
  expect_error(test_deposit_account$deposit(-10))
})

test_withdraw_account <- Account$new("withdraw tester", 10)
test_that("withdraw-Account", {
  test_withdraw_account$withdraw(10)
  expect_equal(test_withdraw_account$balance, 0)
  expect_error(test_withdraw_account$deposit(-10))
})
