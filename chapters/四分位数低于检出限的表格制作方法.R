library(tidyverse)
library(gt)
library(gtsummary)

# 创建一个虚拟的数据集，表示50个样本3个物质的检出浓度，并把这50个样本分为2类
set.seed(123)

data <- tibble(
  group = factor(sample(c("A", "B"), 50, replace = TRUE)),
  A = rnorm(50, 0.5, 0.1),
  B = rnorm(50, 0.6, 0.1),
  C = rnorm(50, 0.7, 0.1)
)

# 模拟检出限
lod <- tibble(
  name = c("A", "B", "C"),
  LOD = c(0.5, 0.56, 0.7)
)

# 生成汇总表，展示每个物质浓度的四分位数，其中小于检出限的四分位数用"<LOD"表示

data |>
  pivot_longer(cols = -group, names_to = "name", values_to = "value") |>
  group_by(group, name) |>
  summarise(
    Q1 = quantile(value, 0.25),
    Q2 = quantile(value, 0.5),
    Q3 = quantile(value, 0.75)
  ) |>
  mutate(across(Q1:Q3, ~round(., 3))) |>
  left_join(lod, by = "name") |>
  mutate(across(Q1:Q3, ~case_when(. < LOD ~ "<LOD", TRUE ~ as.character(.)))) |>
  mutate(ci = str_glue("{Q1}[{Q2}, {Q3}]")) |>
  select(-Q1, -Q2, -Q3, -LOD) |>
  pivot_wider(names_from = group, values_from = ci) -> summary_table

# 用gtsummary生成差异分析p值

data |>
  tbl_summary(by = group) |>
  add_overall() |>
  add_p() -> summary_table2

# 替换表格中的统计量

summary_table2$table_body

summary_table2$table_body |>
  mutate(stat_1 = summary_table$A,
         stat_2 = summary_table$B) -> summary_table2$table_body

summary_table2

# overall列同理，summarise不分组即可
