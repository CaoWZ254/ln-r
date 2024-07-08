library(tidyverse)
library(gtsummary)
library(gt)

data("trial")

table1 <- trial |>
  tbl_summary(
    by = grade, # by表示分组变量
    missing = "no", # missing表示是否显示缺失值
    digits = list(all_continuous() ~ 3), # digits表示小数位数
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", # statistic表示统计量的显示格式
      all_categorical() ~ "{n} / {N} ({p}%)" # all_categorical表示分类变量的显示格式
    )) |>
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) |> # add_p表示添加p值
  as_gt() |>
  tab_style_body(
    style = cell_text(color = "red"),
    columns = p.value,
    fn = \(x) (x < 0.05)
  ) # tab_style_body表示对表格进行样式设置

table1

table1 |>
  gtsave("table1.docx") # gtsave表示保存表格
