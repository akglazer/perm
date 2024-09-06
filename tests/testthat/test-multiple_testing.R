test_that("fisher works", {
  output <- fisher(seq(0.05, 0.9, length.out = 5))
  expect_equal(round(output, 2), 11.12)
})

test_that("liptak works", {
  output <- liptak(seq(0.05, 0.9, length.out = 5))
  expect_equal(round(output, 2), 0.57)
})

test_that("tippett works", {
  output <- tippett(seq(0.05, 0.9, length.out = 5))
  expect_equal(round(output, 2), 0.95)
})

test_that("npc works", {
  data <- data.frame(group = c(rep(1, 4), rep(2, 4)),
                    out1 = c(0, 1, 0, 0, 1, 1, 1, 0),
                    out2 = rep(1, 8))

  # test stat for out1 is smaller if X all 0s which about 0.015 chance so p-value should be about 0.985
  output <- npc(df = data, group_col = "group", outcome_cols = c("out1", "out2"),
                reps = 10^4, seed=42)
  expect_equal(round(output, 3), 0.986)

  # all permutations result in same test stat, so combined p-value of 1
  data <- data.frame(group = c(1, 1, 2, 2),
                     out1 = rep(0, 4),
                     out2 = rep(1, 4))
  output <- npc(df = data, group_col = "group", outcome_cols = c("out1", "out2"), reps = 10^4)
  expect_equal(round(output, 3), 1)

  # 4/24 permutations result in test stat of same size
  data <- data.frame(group = c(1, 1, 2, 2),
                     out1 = c(2, 2, 1, 1),
                     out2 = c(2, 2, 1, 1))
  output <- npc(df = data, group_col = "group", outcome_cols = c("out1", "out2"), reps = 10^4, seed = 42)
  expect_equal(round(output, 2), .17)
})
