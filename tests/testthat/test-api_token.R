test_that("API key storage and retrieval behaving as expected", {
  save_api_token(.t = "test-value", .f = "~/.bpqx-test-auth")
  expect_equal(api_token(.f = "~/.bpqx-test-auth"), "test-value")
  expect_equal(data_center(.f = "~/.bpqx-test-auth"), "blueprintade.yul1")
})
