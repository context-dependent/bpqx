test_that("QX request functions as expected", {
  test_req <- qx_req("surveys")
  expect_equal(
    test_req$url,
    "https://blueprintade.yul1.qualtrics.com/API/v3/surveys"
  )
  bad_req <- qx_req("should-fail")
  expect_error(bad_req |> httr2::req_perform())
})
