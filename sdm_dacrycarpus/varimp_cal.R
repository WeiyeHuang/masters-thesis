library(tidyverse)

setwd("~/experiments/dacrycarpus_huangwy88")

eval_data <- read.csv('./Evaluation/modeval_eval.csv') %>%
  select(full.name, metric.eval, evaluation) %>%
  filter(metric.eval == "TSS") %>%
  mutate(tss.weight = evaluation / sum(evaluation)) %>%
  select(full.name, tss.weight)
varimp_data <- read.csv('./Evaluation/modeval_varimp.csv') %>%
  select(full.name, expl.var, var.imp)
merged_data <- left_join(varimp_data, eval_data, by = "full.name")
weighted_varimp <- merged_data %>%
  group_by(expl.var) %>%
  summarise(w.varimp = weighted.mean(var.imp, w = tss.weight))

write.csv(weighted_varimp, './Evaluation/dacrycarpus_varimp_summary.csv', row.names = F)
