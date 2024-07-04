# Load packages
library(terra, lib.loc = "/public4/group_wxk/home/wuxk/apps/envs/R/lib/R/library")
library(biomod2, lib.loc = "/public4/group_wxk/home/wuxk/apps/envs/R/lib/R/library")
library(ggplot2, lib.loc = "/public4/group_wxk/home/wuxk/apps/envs/R/lib/R/library")
library(dplyr, lib.loc = "/public4/group_wxk/home/wuxk/apps/envs/R/lib/R/library")

sessionInfo()

# Read climate data
modern_clim <- rast(list.files("/public4/group_wxk/home/wuxk/public_data/climate/worldclim/0ka_5min", pattern = ".asc$", full.names = T))

# Arrange the climate data in the order of convention
names(modern_clim) <- c(
  'Bio1', 'Bio2', 'Bio3', 'Bio4', 'Bio5', 'Bio6', 'Bio7', 'Bio8',
  'Bio9', 'Bio10', 'Bio11', 'Bio12', 'Bio13', 'Bio14', 'Bio15',
  'Bio16', 'Bio17', 'Bio18', 'Bio19'
)

# Import species distribution data
presence_all <- read.csv("./Training/pinus_filtered.csv")
presence_all <- vect(presence_all, geom = c("Longitude", "Latitude"))

pseudoabsence_data <- read.csv("./Training/pinus_absence.csv")
pseudoabsence_data <- vect(pseudoabsence_data, geom = c("Longitude", "Latitude"))

# Select the climate parameters to be retained according to the correlation and geographical common sense
modern_clim <- modern_clim[[c(
  'Bio2', 'Bio3', 'Bio4', 'Bio5', 'Bio6', 'Bio8',
  'Bio12', 'Bio13', 'Bio15', 'Bio18', 'Bio19'  # Modify according to the actual situation
)]]
cat("Kept climatic variables: ", names(modern_clim), ", number: ", length(names(modern_clim)), "\n")

# Set the seed to ensure reproducibility
set.seed(123)

# Get the length of the vector
vector_length <- length(presence_all)

# Calculate the 70% and 30% split points
train_percent <- 0.7
train_size <- round(vector_length * train_percent)

# Randomly select indices
train_indices <- sample(1:vector_length, train_size)

# Create training and validation data for presence and pseudo-absence respectively
presence_train <- presence_all[train_indices]
presence_test <- presence_all[-train_indices]
pseudoabsence_train <- pseudoabsence_data[train_indices]
pseudoabsence_test <- pseudoabsence_data[-train_indices]

# Save the above data
write.csv(presence_train, "./Training/presence_train.csv")
write.csv(presence_test, "./Training/presence_test.csv")
write.csv(pseudoabsence_train, "./Training/pseudoabsence_train.csv")
write.csv(pseudoabsence_test, "./Training/pseudoabsence_test.csv")

# Merge training and validation data
train_data <- rbind(presence_train, pseudoabsence_train)
test_data <- rbind(presence_test, pseudoabsence_test)

rm(presence_train)
rm(presence_test)
rm(pseudoabsence_train)
rm(pseudoabsence_test)

# Formating data
formatted_data <- BIOMOD_FormatingData(
  resp.name = "pinus",
  resp.var = train_data,
  expl.var = modern_clim,
  eval.resp.var = test_data,
  eval.expl.var = modern_clim,
  dir.name = "./Models"
)
formatted_data

# Set the parameters for model training to the default parameters
model_list <- c("GLM", "GBM", "CTA", "MARS", "RF", "XGBOOST", "MAXNET", "MAXENT")
modeling_option <- bm_ModelingOptions(data.type = "binary",
                                      models = model_list,
                                      strategy = "bigboss",
                                      user.base = "bigboss",
                                      bm.format = formatted_data)
evaluation_list <- c("ROC", "TSS", "ACCURACY", "POD")

# Train models
modeval_for_evaluation <- BIOMOD_Modeling(
  bm.format = formatted_data,
  modeling.id = as.character(format(Sys.time(), "%s")),    # Name the model after the current time
  models = model_list,
  OPT.user = modeling_option,
  CV.nb.rep = 16,
  CV.perc = 0.7,
  CV.do.full.models = F,
  metric.eval = evaluation_list,
  var.import = 10,
  nb.cpu = 124,
  seed.val = 123
)
modeval_for_evaluation

# get evaluation results and climate parameters importance data and save
modeval_eval <- get_evaluations(modeval_for_evaluation)
write.csv(modeval_eval, "./Evaluation/modeval_eval.csv")
modeval_varimp <- get_variables_importance(modeval_for_evaluation)
write.csv(modeval_varimp, "./Evaluation/modeval_varimp.csv")

# Save the data up to now
save.image("./RData/modeval.RData")

# Extract the row with metric.eval = "TSS"
tss_data <- modeval_eval[modeval_eval$metric.eval == "TSS", ]

# Calculate the mean and standard deviation of the evaluation column according to algo
tss_summary <- aggregate(evaluation ~ algo, tss_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Save temporary data and read
write.csv(tss_summary, "./Evaluation/modeval_eval_tss.csv", row.names = F)
tss_summary <- read.csv("./Evaluation/modeval_eval_tss.csv")

# Change column names
colnames(tss_summary) <- c("algo", "tss_mean", "tss_sd")

# Calculate the TSS weight of each algorithm
tss_weights <- with(tss_summary, tss_mean / sum(tss_mean))

# Merge the weight with the table
tss_summary <- cbind(tss_summary, tss_weights)
tss_summary

# Save data
write.csv(tss_summary, "./Evaluation/modeval_eval_tss.csv", row.names = F)

# Read data
modeval_varimp <- read.csv("./Evaluation/modeval_varimp.csv")
tss_weights <- read.csv("./Evaluation/modeval_eval_tss.csv")

# Merge model_varimp and tss_weights tables
modeval_varimp_merged <- merge(modeval_varimp, tss_weights, by = "algo")

# Calculate the weighted average
modeval_varimp_weighted <- modeval_varimp_merged %>%
  group_by(algo, expl.var) %>%
  summarize(weighted_varimp = sum(var.imp * tss_weights), .groups = "drop")
modeval_varimp_mean <- modeval_varimp_weighted %>%
  group_by(expl.var) %>%
  summarize(var.imp = sum(weighted_varimp), .groups = "drop")
modeval_varimp_mean

# Output result table
write.csv(modeval_varimp_mean, "./Evaluation/modeval_varimp_mean.csv", row.names = F)

# Read data
modeval_eval_tss <- read.csv("./Evaluation/modeval_eval_tss.csv")
modeval_varimp_mean <- read.csv("./Evaluation/modeval_varimp_mean.csv")

# Barplot
ggplot(modeval_eval_tss, aes(x = reorder(algo, -tss_mean), y = tss_mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = tss_mean - tss_sd, ymax = tss_mean + tss_sd), width = 0.1) +
  labs(x = "Algorithm", y = "TSS Mean", title = "Model Evaluation: TSS Mean and SD") +
  theme_light()
ggplot(modeval_varimp_mean, aes(x = reorder(expl.var, -var.imp), y = var.imp)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  labs(x = "Explained Variable", y = "Variable Importance", title = "Model Variable Importance") +
  theme_light()

gc()

gc()

# Select the top five algorithms
top5_algo <- head(tss_summary[order(-tss_summary$tss_mean), "algo"], 5)

# Select the top five climate factors
top10_clim <- head(modeval_varimp_mean[order(-modeval_varimp_mean$var.imp), "expl.var"], 10)

# Print the results
cat("Top 5 algorithms: ", top5_algo, "\n")
cat("Top 10 climatic variables: ", top10_clim, "\n")

# Update model list and climate factor list
model_list <- top5_algo
modern_clim <- modern_clim[[top10_clim]]

rm(modeval_for_evaluation)

# Formating data
formatted_data <- BIOMOD_FormatingData(
  resp.name = "pinus",
  resp.var = train_data,
  expl.var = modern_clim,
  eval.resp.var = test_data,
  eval.expl.var = modern_clim,
  dir.name = "./Models"
)
formatted_data

# Set the parameters for model training to the default parameters
modeling_option <- bm_ModelingOptions(data.type = "binary",
                                      models = model_list,
                                      strategy = "bigboss",
                                      user.base = "bigboss",
                                      bm.format = formatted_data)
evaluation_list <- c("ROC", "TSS", "ACCURACY", "POD")

gc()

# Train models for projection
model_for_projection <- BIOMOD_Modeling(
  bm.format = formatted_data,
  modeling.id = as.character(format(Sys.time(), "%s")),
  models = model_list,
  OPT.user = modeling_option,
  CV.nb.rep = 32,
  CV.perc = 0.7,
  CV.do.full.models = F,
  metric.eval = evaluation_list,
  var.import = 10,
  nb.cpu = 124,
  seed.val = 123
)
model_for_projection

# Get evaluation results and climate parameters importance data and save
modproj_eval <- get_evaluations(model_for_projection)
write.csv(modproj_eval, "./Evaluation/modproj_eval.csv")
modproj_varimp <- get_variables_importance(model_for_projection)
write.csv(modproj_varimp, "./Evaluation/modproj_varimp.csv")
save.image("./RData/single_models.RData")

gc()

# Process the evaluations
tss_data <- modproj_eval[modproj_eval$metric.eval == "TSS", ]
tss_summary <- aggregate(evaluation ~ algo, tss_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))
write.csv(tss_summary, "./Evaluation/modproj_eval_tss.csv", row.names = F)
tss_summary <- read.csv("./Evaluation/modproj_eval_tss.csv")
colnames(tss_summary) <- c("algo", "tss_mean", "tss_sd")
tss_weights <- with(tss_summary, tss_mean / sum(tss_mean))
tss_summary <- cbind(tss_summary, tss_weights)
tss_summary
write.csv(tss_summary, "./Evaluation/modproj_eval_tss.csv", row.names = F)
modproj_varimp <- read.csv("./Evaluation/modproj_varimp.csv")
tss_weights <- read.csv("./Evaluation/modproj_eval_tss.csv")
modproj_varimp_merged <- merge(modproj_varimp, tss_weights, by = "algo")
modproj_varimp_weighted <- modproj_varimp_merged %>%
  group_by(algo, expl.var) %>%
  summarize(weighted_varimp = sum(var.imp * tss_weights), .groups = "drop")
modproj_varimp_mean <- modproj_varimp_weighted %>%
  group_by(expl.var) %>%
  summarize(var.imp = sum(weighted_varimp), .groups = "drop")
modproj_varimp_mean
write.csv(modproj_varimp_mean, "./Evaluation/modproj_varimp_mean.csv", row.names = F)
modproj_eval_tss <- read.csv("./Evaluation/modproj_eval_tss.csv")
modproj_varimp_mean <- read.csv("./Evaluation/modproj_varimp_mean.csv")

# Barplot
ggplot(modproj_eval_tss, aes(x = reorder(algo, -tss_mean), y = tss_mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = tss_mean - tss_sd, ymax = tss_mean + tss_sd), width = 0.1) +
  labs(x = "Algorithm", y = "TSS Mean", title = "Model Evaluation: TSS Mean and SD") +
  theme_light()
ggplot(modproj_varimp_mean, aes(x = reorder(expl.var, -var.imp), y = var.imp)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  labs(x = "Explained Variable", y = "Variable Importance", title = "Model Variable Importance") +
  theme_light()

gc()

# Create an ensemble model with multiple algorithms
ensemble_model <- BIOMOD_EnsembleModeling(
  bm.mod = model_for_projection,
  models.chosen = "all",
  em.by = "all",
  em.algo = "EMwmean",
  metric.select = c("ROC", "TSS"),
  metric.select.thresh = c(0.98, 0.95),
  metric.eval = evaluation_list,
  EMwmean.decay = 2,
  var.import = 10,
  seed.val = 123,
  nb.cpu = 124
)
ensemble_model

# Get the original data of model evaluation
ensmod_eval <- get_evaluations(ensemble_model)
write.csv(ensmod_eval, "./Evaluation/ensmod_eval.csv")
ensmod_varimp <- get_variables_importance(ensemble_model)
write.csv(ensmod_varimp, "./Evaluation/ensmod_varimp.csv")
save.image("./RData/ensemble_model.RData")

gc()

# Extract four metric.eval
tss_data <- ensmod_eval[ensmod_eval$metric.eval == "TSS", ]
roc_data <- ensmod_eval[ensmod_eval$metric.eval == "ROC", ]
accuracy_data <- ensmod_eval[ensmod_eval$metric.eval == "ACCURACY", ]
pod_data <- ensmod_eval[ensmod_eval$metric.eval == "POD", ]

# Calculate the mean and standard deviation of the evaluation column according to algo
tss_summary <- aggregate(evaluation ~ algo, tss_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))
roc_summary <- aggregate(evaluation ~ algo, roc_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))
accuracy_summary <- aggregate(evaluation ~ algo, accuracy_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))
pod_summary <- aggregate(evaluation ~ algo, pod_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Save temporary data and read
write.csv(tss_summary, "./Evaluation/ensmod_eval_eval_tss.csv", row.names = F)
tss_summary <- read.csv("./Evaluation/ensmod_eval_eval_tss.csv")
write.csv(roc_summary, "./Evaluation/ensmod_eval_eval_roc.csv", row.names = F)
roc_summary <- read.csv("./Evaluation/ensmod_eval_eval_roc.csv")
write.csv(accuracy_summary, "./Evaluation/ensmod_eval_eval_accuracy.csv", row.names = F)
accuracy_summary <- read.csv("./Evaluation/ensmod_eval_eval_accuracy.csv")
write.csv(pod_summary, "./Evaluation/ensmod_eval_eval_pod.csv", row.names = F)
pod_summary <- read.csv("./Evaluation/ensmod_eval_eval_pod.csv")

# Change column names
colnames(tss_summary) <- c("algo", "tss_mean", "tss_sd")
colnames(roc_summary) <- c("algo", "roc_mean", "roc_sd")
colnames(accuracy_summary) <- c("algo", "accuracy_mean", "accuracy_sd")
colnames(pod_summary) <- c("algo", "pod_mean", "pod_sd")

# Save data
write.csv(tss_summary, "./Evaluation/ensmod_eval_eval_tss.csv", row.names = F)
write.csv(roc_summary, "./Evaluation/ensmod_eval_eval_roc.csv", row.names = F)
write.csv(accuracy_summary, "./Evaluation/ensmod_eval_eval_accuracy.csv", row.names = F)
write.csv(pod_summary, "./Evaluation/ensmod_eval_eval_pod.csv", row.names = F)

# Barplot
ggplot(tss_summary, aes(x = reorder(algo, -tss_mean), y = tss_mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = tss_mean - tss_sd, ymax = tss_mean + tss_sd), width = 0.1) +
  labs(x = "Algorithm", y = "TSS Mean", title = "Model Evaluation: TSS Mean and SD") +
  theme_light()
ggplot(roc_summary, aes(x = reorder(algo, -roc_mean), y = roc_mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = roc_mean - roc_sd, ymax = roc_mean + roc_sd), width = 0.1) +
  labs(x = "Algorithm", y = "ROC Mean", title = "Model Evaluation: ROC Mean and SD") +
  theme_light()
ggplot(accuracy_summary, aes(x = reorder(algo, -accuracy_mean), y = accuracy_mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = accuracy_mean - accuracy_sd, ymax = accuracy_mean + accuracy_sd), width = 0.1) +
  labs(x = "Algorithm", y = "Accuracy Mean", title = "Model Evaluation: Accuracy Mean and SD") +
  theme_light()
ggplot(pod_summary, aes(x = reorder(algo, -pod_mean), y = pod_mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = pod_mean - pod_sd, ymax = pod_mean + pod_sd), width = 0.1) +
  labs(x = "Algorithm", y = "POD Mean", title = "Model Evaluation: POD Mean and SD") +
  theme_light()
