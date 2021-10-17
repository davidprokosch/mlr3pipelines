context("PipeOpDichotomize")

test_that("PipeOpDichotomize - basic properties", {
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpDichotomize, task = task)
  op = PipeOpDichotomize$new()
  expect_pipeop(op)
  result = op$train(list(task))
  expect_task(result[[1]])
  expect_equal(result[[1]]$data(), op$predict(list(task))[[1]]$data())
})

test_that("PipeOpDichotomize - see if expected result is returned", {
  task = mlr_tasks$get("iris")
  op = PipeOpDichotomize$new()
  expect_pipeop(op)
  result = op$train(list(task))

  # Default parameters
  # Checks if there are more TRUE values than possible
  a = apply(as.data.frame(result[[1]]$data()[, 2:5]), MARGIN = 2,
            function(x) sum(x) <= (1 - op$param_set$values$quantile) * length(x))
  expect_true(all(a))

  # 0.8-quantile
  op_80 = PipeOpDichotomize$new(param_vals = list(quantile = .8))
  expect_pipeop(op_80)
  result = op_80$train(list(task))
  b = apply(as.data.frame(result[[1]]$data()[, 2:5]), MARGIN = 2,
            function(x) sum(x) <= (1 - op$param_set$values$quantile) * length(x))
  expect_true(all(b))
 })
