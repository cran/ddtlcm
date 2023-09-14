## ----setup,echo=FALSE,results="hide", include = FALSE-------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE, warning = FALSE, message = FALSE
)
suppressPackageStartupMessages({
library(knitr)
})
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = xfun::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})

## ----eval=FALSE, include=FALSE------------------------------------------------
#  install.packages("/Users/mengbing/Dropbox (University of Michigan)/from_box/research/R_packages/ddtlcm_0.1.1.tar.gz", repos = NULL, type="source")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("devtools",repos="https://cloud.r-project.org")
#  devtools::install_github("limengbinggz/ddtlcm")

## -----------------------------------------------------------------------------
library(ddtlcm)
# load the MAP tree structure obtained from the real HCHS/SOL data
data(data_hchs)
# unlist the elements into variables in the global environment
list2env(setNames(data_hchs, names(data_hchs)), envir = globalenv()) 

# look at items in group 1
g <- 1
# indices of the items in group 1
item_membership_list[g]
# names of the items in group 1. The name of the list element is the major food group
item_name_list[g]

## -----------------------------------------------------------------------------
# number of individuals
N <- 496
# set random seed to generate node parameters given the tree
seed_parameter = 1
# set random seed to generate multivariate binary observations from LCM
seed_response = 1
# simulate data given the parameters
sim_data <- simulate_lcm_given_tree(tree_phylo, N, 
            class_probability, item_membership_list, Sigma_by_group, 
            root_node_location = 0, seed_parameter = 1, seed_response = 1)

## ----fig.height = 4, fig.width = 6--------------------------------------------
response_matrix <- sim_data$response_matrix
dim(response_matrix)
response_prob <- sim_data$response_prob
tree_with_parameter <- sim_data$tree_with_parameter
plot_tree_with_heatmap(tree_with_parameter, response_prob, item_membership_list)

## -----------------------------------------------------------------------------
set.seed(999)
# number of latent classes, or number of leaves on the tree
K <- 6
system.time({
  result <- ddtlcm_fit(K = K, data = response_matrix, 
                     item_membership_list = item_membership_list, total_iters = 100)
})

print(result)

## -----------------------------------------------------------------------------
burnin <- 50
summarized_result <- summary(result, burnin, relabel = T, be_quiet = T)

## ----fig.height = 6, fig.width = 7--------------------------------------------
plot(x = summarized_result, item_name_list = item_name_list, plot_option = "all")

## -----------------------------------------------------------------------------
rmse <- sqrt(mean((summarized_result$response_probs_summary[, "Mean"] - sim_data$response_prob)**2))
cat("\nRMSE of the item response probabilities:", rmse)

## -----------------------------------------------------------------------------
predicted_class_assignments1 <- predict(summarized_result, response_matrix)
cat("\nFrequencies of predicted class memberships from posterior summaries:\n", tabulate(predicted_class_assignments1$class_assignments, K))

## -----------------------------------------------------------------------------
predicted_class_assignments2 <- predict(result, response_matrix, burnin)
cat("\nFrequencies of predicted class memberships from posterior predictive distribution:\n", tabulate(predicted_class_assignments2$class_assignments, K))

