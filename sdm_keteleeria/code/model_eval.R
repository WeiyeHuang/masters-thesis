library(tidyverse)

setwd("~/experiments/sdm_keteleeria_huangwy88/wc")

modeval_eval <- read.csv('./Evaluation/modeval_eval.csv')

eval_cal <- function(eval_data, metric) {
  # 使用 dplyr 进行数据处理
  eval_data <- eval_data %>%
    filter(metric.eval == metric) %>%
    group_by(algo) %>%
    summarise(mean = mean(evaluation), sd = sd(evaluation))
  
  # 计算每个算法的权重，权重计算为均值占总均值的比例
  eval_weights <- eval_data %>%
    mutate(weight = mean / sum(mean)) %>%
    select(algo, weight)
  
  # 将权重列添加到聚合结果中
  eval_data <- left_join(eval_data, eval_weights, by = "algo")
  
  # 保存计算好的评估表格
  eval_name <- paste0('./Evaluation/modeval_', tolower(metric), '_summary.csv')
  write.csv(eval_data, eval_name, row.names = F)
  
  # 返回最终的聚合结果，包括算法、均值、标准差和权重
  return(eval_data)
}

modeval_tss_summary <- eval_cal(modeval_eval, "TSS")
modeval_roc_summary <- eval_cal(modeval_eval, "ROC")
modeval_accuracy_summary <- eval_cal(modeval_eval, "ACCURACY")