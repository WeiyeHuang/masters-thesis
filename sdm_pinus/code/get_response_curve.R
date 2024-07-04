library(biomod2)
library(tidyverse)
library(matrixStats)

setwd('~/experiments/sdm_pinus_huangwy88')
load('./RData/ensemble_model.RData')

maxent_rc <- bm_PlotResponseCurves(bm.out = model_for_projection,
                                   models.chosen = get_built_models(model_for_projection,
                                                                    algo = 'MAXNET'))
write.csv(maxent_rc$tab, './Evaluation/maxent_rc.csv')

# 转换和清理数据
maxent_rc_cleaned <- maxent_rc$tab %>%
  select(id, expl.name, expl.val, pred.name, pred.val) %>%
  pivot_wider(names_from = pred.name, values_from = pred.val) %>%
  select(id, expl.name, expl.val, everything()) %>%
  mutate(across(starts_with("RUN"), ~ suppressWarnings(as.numeric(as.character(.))), .names = "{col}_clean")) %>%
  drop_na(starts_with("RUN_clean")) %>%
  rename_with(~ gsub("_clean", "", .), starts_with("RUN_clean")) %>%
  select(-starts_with("RUN_clean"))

# 打印清理后数据的前几行，检查数据是否正常
head(maxent_rc_cleaned)

# 获取"RUN"开头的列名
run_column_names <- grep("^pinus_allData_RUN\\d+_MAXNET$", names(maxent_rc_cleaned), value = TRUE)

# 计算平均值
avg_values <- apply(maxent_rc_cleaned[, run_column_names], 1, mean, na.rm = TRUE)
maxent_rc_cleaned$avg_pred_val <- avg_values

# 计算标准差
std_dev_values <- apply(maxent_rc_cleaned[, run_column_names], 1, sd, na.rm = TRUE)
maxent_rc_cleaned$std_dev_pred_val <- std_dev_values

# 转换为数据框并输出
maxent_rc_tab <- as.data.frame(maxent_rc_cleaned)

write.csv(maxent_rc_tab[, c("expl.name", "expl.val", "avg_pred_val", "std_dev_pred_val")], './maxent_rc_plot.csv', row.names = F)
