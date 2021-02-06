test_that("init-GiroAccount", {
  expect_true(
    R6::is.R6(GiroAccount$new("Marvin", -10000))
  )
  expect_true(
    R6::is.R6(GiroAccount$new("Marvin", 0,
                              overdraft_max = 100,
                              overdraft_fee_rate = 0.2))
  )
  expect_error(
    R6::is.R6(GiroAccount$new("Marvin", 0,
                              overdraft_max = 100,
                              overdraft_fee_rate = 1.2))
  )
  expect_error(
    R6::is.R6(GiroAccount$new("Marvin", 0,
                              overdraft_max = -100,
                              overdraft_fee_rate = .1))
  )
})

test_withdraw_giroaccount <- GiroAccount$new("withdraw tester", 10, 10, 0.5)
test_that("withdraw-GiroAccount", {
  expect_error(test_withdraw_giroaccount$withdraw(50),
               regexp =  "You've reached your overdraft limit.")
  test_withdraw_giroaccount$withdraw(12)
  expect_equal(test_withdraw_giroaccount$balance, -3)
})
