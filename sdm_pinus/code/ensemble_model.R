# Load packages
library(terra, lib.loc = "/public4/group_wxk/home/wuxk/apps/envs/R/lib/R/library")
library(biomod2, lib.loc = "/public4/group_wxk/home/wuxk/apps/envs/R/lib/R/library")
library(ggplot2, lib.loc = "/public4/group_wxk/home/wuxk/apps/envs/R/lib/R/library")
library(dplyr, lib.loc = "/public4/group_wxk/home/wuxk/apps/envs/R/lib/R/library")

sessionInfo()

load('./RData/single_models.RData')

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
  nb.cpu = 1
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
