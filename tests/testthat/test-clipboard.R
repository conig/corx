test_that("clipboard writes cannot be enabled in non-interactive sessions", {
  skip_if(interactive())
  # Deliberately invalid type: even without our guard, clipr cannot write.
  expect_error(
    to_clipboard(matrix(1), allow_non_interactive = TRUE, object_type = "invalid"),
    "to_clipboard\\(\\) only works in interactive R sessions"
  )
})
