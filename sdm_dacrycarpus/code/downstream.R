library(biomod2)
library(terra)
library(tidyverse)
library(matrixStats)
library(writexl)

setwd('~/experiments/sdm_dacrycarpus_huangwy88')
load('./RData/single_models.RData')

# Evaluations
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
modeval_pod_summary <- eval_cal(modeval_eval, "POD")

# Plot Response Curve
resp_curve <- bm_PlotResponseCurves(model_for_projection,
                                    get_built_models(model_for_projection, algo = 'RF'))
resp_curve_data <- as.data.frame(resp_curve$tab) %>%
  select(id, expl.name, expl.val, pred.name, pred.val) %>%
  pivot_wider(names_from = pred.name, values_from = pred.val) %>%
  select(id, expl.name, expl.val, everything()) %>%
  mutate(avg_pred_val = rowMeans(select(., starts_with("dacrycarpus_allData_RUN"))),
         std_dev_pred_val = rowSds(as.matrix(select(., starts_with("dacrycarpus_allData_RUN"))))) %>%
  as.data.frame()
resp_curve_plot <- resp_curve_data %>%
  select(expl.name, expl.val, avg_pred_val, std_dev_pred_val) %>%
  mutate(avg_plus_val = avg_pred_val + std_dev_pred_val,
         avg_minus_val = avg_pred_val - std_dev_pred_val) %>%
  select(expl.name, expl.val, avg_minus_val, avg_pred_val, avg_plus_val) %>%
  as.data.frame()
resp_curve_split <- split(resp_curve_plot, resp_curve_plot$expl.name)
write_xlsx(resp_curve_split, path = "./Evaluation/resp_curve.xlsx")
