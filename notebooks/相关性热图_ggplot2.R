# 用ggplot2绘制相关性热图的函数，在自己的分析文件中用source("path/to/相关性热图_ggplot2.R")加载函数
# data参数指定数据框，每一列是一个变量，每一行是一个观测（不是相关性矩阵）
# vars参数指定要绘制的变量，可以用c("var1", "var2", ...)传入，对应向量的顺序就是热图中变量的排序
# type参数指定热图左下角的样式，可以是"point"（气泡图）或"tile"（矩形图）
# .col_low、.col_mid、.col_high参数指定颜色渐变的低、中、高三个颜色
# 最终出图的效果是对角线显示变量的分布，左下角为添加了显著性星号的气泡图或矩形图，右上角为相关系数值

# 最简单的示例：
# source("path/to/相关性热图_ggplot2.R")
# vars <- c("x1", "x2", "x3", "x4")
# data |> cor_plot(vars) # 默认气泡图
# data |> cor_plot(vars, "tile") # 矩形图

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}

library(tidyverse)
library(patchwork)

cor_plot <- function(data, vars, type = "point",
                     .col_low = "#2660a4", .col_mid = "#acbea3", .col_high = "#b2182b") {
  data |>
    select(all_of(vars)) |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "value") |>
    summarise(.by = var, value1 = list(value)) -> data_1

  expand_grid(var1 = vars, var2 = vars) |>
    left_join(data_1, by = c("var1" = "var")) |>
    left_join(data_1 |>
                rename(value2 = value1),
              by = c("var2" = "var")) |>
    mutate(
      cor = map2_dbl(value1, value2, ~ cor.test(.x, .y)$estimate),
      cor_p = map2_dbl(value1, value2, ~ cor.test(.x, .y)$p.value)
    ) |>
    select(-value1, -value2) -> cor

  cor |>
    distinct(cor, .keep_all = TRUE) |>
    filter(var1 != var2) |>
    bind_rows(cor |>
                filter(var1 == var2)) -> cor_for_plot

  cor |>
    select(-cor_p) |>
    rename(text = cor) |>
    full_join(cor_for_plot, by = c("var1", "var2")) |>
    mutate(text = if_else(is.na(cor), round(text, 2), NA),
           value = if_else(var1 == var2, var1, NA),
           cor = if_else(var1 == var2, NA, cor),
           a = if_else(is.na(cor), "0", "1"),
           p = case_when(
             cor_p == 0 ~ "",
             cor_p < 0.001 ~ "***",
             cor_p < 0.01 ~ "**",
             cor_p < 0.05 ~ "*",
             TRUE ~ ""
           )) -> cor_for_plot

  data |>
    select(all_of(vars)) |>
    pivot_longer(cols = everything(), names_to = "var", values_to = "value") |>
    nest(data = value) |>
    mutate(plot = map(data, ~ggplot(.x, aes(x = value)) +
                        geom_histogram(aes(y = after_stat(density)), fill = "skyblue", color = "black",
                                       alpha = 0.6, bins = 10, linewidth = 0.3) +
                        geom_density(color = "skyblue", linewidth = 0.5) +
                        theme_minimal() +
                        theme(text = element_blank(),
                              panel.grid = element_blank(),)
                              # plot.margin = margin(0, 0, 0, 0, "pt"))
    )) |>
    select(var, plot) -> dens

  cor_for_plot |>
    left_join(dens, by = join_by(value == var)) -> cor_for_plot

  if (type == "tile") {
    fig <- cor_for_plot |>
      mutate(var1 = fct_relevel(var1, {{vars}}),
             var2 = fct_relevel(var2, rev({{vars}}))) |>
      ggplot(aes(x = var1, y = var2, fill = cor, color = cor)) +
      geom_tile(aes(alpha = a, linewidth = a, width = cor, height = cor), # tile ########
                na.rm = TRUE, color = "black") +                          # tile ########
      scale_alpha_manual(labels = c("0", "1"), values = c(0, 1)) +        # tile ########
      scale_linewidth_manual(labels = c("0", "1"), values = c(0, 0.5))    # tile ########
  } else {
    fig <- cor_for_plot |>
      mutate(var1 = fct_relevel(var1, {{vars}}),
             var2 = fct_relevel(var2, rev({{vars}}))) |>
      ggplot(aes(x = var1, y = var2, fill = cor, color = cor)) +
      geom_point(aes(size = abs(cor)), na.rm = TRUE) + # point ########
      scale_size_continuous(range = c(4, 10))          # point ########
  }

  fig +
    # 显著性标记 ----
    geom_text(aes(label = p), size = 5, color = "black") +
    # 上半三角 ----
    geom_text(aes(label = text, size = abs(text), color = text), na.rm = TRUE) +
    scale_fill_gradient2(low = .col_low, mid = .col_mid, high = .col_high,
                         midpoint = 0, limits = c(-1, 1)) +
    scale_color_gradient2(low = .col_low, mid = .col_mid, high = .col_high,
                          midpoint = 0, limits = c(-1, 1)) +
    theme_bw() +
    labs(x = NULL, y = NULL,
         fill = "Correlation", color = "Correlation") +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(size = 18),
      axis.text = element_text(face = "bold", color = "black")
    ) +
    guides(size = "none", alpha = "none", linewidth = "none") -> fig

  for (i in 1:length(vars)) {
    fig +
      inset_element(
        p = dens |>
          filter(var == vars[i]) |>
          pluck("plot", 1),
        left = (i - 1) / length(vars),
        right = i / length(vars),
        bottom = 1 - i / length(vars),
        top = 1 - (i - 1) / length(vars),
        ignore_tag = TRUE
      ) -> fig
  }

  return(fig)
}


