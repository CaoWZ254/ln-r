library(tidyverse)
library(showtext)

# 绘制一个简单的散点图

set.seed(123)
data <- tibble(
  x = rnorm(100),
  y = rnorm(100)
)

data |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_minimal() +
  labs(title = "A simple scatter plot") +
  theme(text = element_text(size = 24)) -> p1

# 加载新字体

font_add(family = "Times New Roman", # 字体名称
         regular = "C:/Windows/Fonts/TIMES.TTF") # 字体文件路径
showtext_auto()

# 修改字体
p1 +
  theme(text = element_text(family = "Times New Roman", size = 24)) -> p2

# 保存图片
ggsave("scatter_plot.pdf", p2, width = 10, height = 10, device = cairo_pdf)
