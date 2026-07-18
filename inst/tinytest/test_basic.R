## -----------------------------------------------------------------------------
library("btw")


## -----------------------------------------------------------------------------
options(bt_path = "~/misc/BayesTraitsV5.0.3-Linux")
options(bt_bin = "BayesTraitsV5")


## -----------------------------------------------------------------------------
data(primates)


## -----------------------------------------------------------------------------
# commands to run multistate model ("1") in Maximum Likelihood mode ("1")
command_vec1 <- c("1", "1")


## -----------------------------------------------------------------------------
results_1 <- bayestraits(primate.discrete1, primate.tree1, command_vec1, remove_files=FALSE)


## -----------------------------------------------------------------------------
log_file_1 <- results_1$Log


## -----------------------------------------------------------------------------
model_options_1 <- log_file_1$options
model_results_1 <- log_file_1$results


## -----------------------------------------------------------------------------
# commands to run multistate model ("1") in MCMC mode ("2")
command_vec_2 <- c("1", "2")

# run analysis
results_2 <- bayestraits(primate.discrete1, primate.tree1, command_vec_2)

# extract log and schedule file results
log_2 <- results_2$Log
schedule_2 <- results_2$Schedule


## -----------------------------------------------------------------------------
# commands to run geographic model ("13") in MCMC, with only one tree
command_vec_3 <- c("13", "2")

# run analysis
results_3 <- bayestraits(primate.continuous2, primate.tree1, command_vec_3)

# extract log, schedule, and AncStates file results
log_3 <- results_3$Log
schedule_3 <- results_3$Schedule
ancestors_3 <- results_3$AncStates


## -----------------------------------------------------------------------------
# commands to run Multistate model ("1") in MCMC mode ("2") with stepping stone sampler
command_vec_4 <- c("1", "2", "Stones 100 1000")

# run analysis
results_4 <- bayestraits(primate.discrete1, primate.tree1, command_vec_4)

# extract log, schedule, and stones file results
log_4 <- results_4$Log
schedule_4 <- results_4$Schedule
stones_4 <- results_4$Stones


## -----------------------------------------------------------------------------
# commands to run continuous random walk trait model ("4") in MCMC mode ("2"), estimating lambda 
# and saving initial trees, transformed trees
command_vec_5 <- c("4", "2", "lambda", "SaveTrees")

# run analysis
results_5 <- bayestraits(primate.continuous1, primate.tree100, command_vec_5)

# extract log, schedule, and output trees
log_5 <- results_5$Log
schedule_5 <- results_5$Schedule
output_trees_5 <- results_5$OutputTrees

