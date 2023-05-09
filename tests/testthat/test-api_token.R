test_that("API key storage and retrieval behaving as expected", {
  save_api_token(.t = "test-value")
  expect_equal(api_token(), "test-value")
})
