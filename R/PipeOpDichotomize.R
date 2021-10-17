PipeOpDichotomize = R6Class("PipeOpDichotomize",
                            inherit = PipeOpTaskPreprocSimple,
                            public = list(
                              initialize = function(id = "dichotomize", param_vals = list()) {
                                ps = ParamSet$new(params = list(
                                  ParamDbl$new("quantile", lower = 0, upper = 1, special_vals = list(NULL), tags = "train")
                                ))
                                ps$values = list(quantile = .5)
                                super$initialize(id, param_set = ps, param_vals = param_vals, packages = "stats", feature_types = c("numeric", "integer"))
                              }
                            ),
                            private = list(

                              .get_state_dt = function(dt, levels, target) {
                                # quantiles for each feature
                                quants = lapply(dt, quantile, self$param_set$values$quantile)
                                list(quants = quants)
                              },

                              .transform_dt = function(dt, levels) {
                                # transformes data with the quantiles of the training set
                                as.data.frame(Map(function(x, y) {
                                  x > y
                                }, dt, self$state$quants))
                              }
                            )
)

mlr_pipeops$add("dichotomize", PipeOpDichotomize)
