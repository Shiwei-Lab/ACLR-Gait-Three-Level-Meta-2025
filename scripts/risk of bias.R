library(readxl)
library(tidyverse)
library(gridExtra)
library(grid)

# 读取数据
rob_data <- read_excel("Desktop/结果汇总表/risk plot/rob_final.xlsx")

# 设置域顺序
domain_order <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8")

# 转换为长格式
rob_long <- rob_data %>%
  pivot_longer(cols = all_of(domain_order), names_to = "Domain", values_to = "Judgement")

# Judgement 颜色
judgement_colors <- c(
  "Low" = "#AA3A49",      # 淡蓝色（自然色系）
  "Moderate" = "#D0908F",  # 橙色（自然色系）
  "Serious" = "#BE6C6D",   # 深橄榄绿色（自然色系）
  "High" = "#ADD8E6",
  "No Information" = "#DB7093"
)

rob_long$Domain <- factor(rob_long$Domain, levels = domain_order)

# 主图
main_plot <- ggplot(rob_long, aes(x = Domain, y = fct_rev(Study))) +
  geom_point(aes(fill = Judgement), shape = 21, size = 6, color = "black") +
  scale_fill_manual(values = judgement_colors) +
  theme_minimal() +
  labs(title = "Risk of Bias (ROBINS-I)", x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(size = 8, color = "darkgray"),  # 更小且深灰色的X轴标签
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# Judgement 图标替换为加号、减号、叉号（小尺寸，细字体）
main_plot <- main_plot +
  geom_text(data = rob_long %>% filter(Judgement == "Low"),
            aes(x = Domain, y = fct_rev(Study), label = "+"),
            color = "black", size = 4, fontface = "plain") +  # 调整符号大小和字体
  geom_text(data = rob_long %>% filter(Judgement == "Moderate"),
            aes(x = Domain, y = fct_rev(Study), label = "-"),
            color = "black", size = 4, fontface = "plain") +  # 调整符号大小和字体
  geom_text(data = rob_long %>% filter(Judgement == "Serious"),
            aes(x = Domain, y = fct_rev(Study), label = "X"),
            color = "black", size = 4, fontface = "plain")  # 调整符号大小和字体

# 注释 D1-D4
left_labels <- data.frame(
  x = 1.1,
  y = 4:1,
  label = c(
    "D1: Confounding",
    "D2: Selection of Participants",
    "D3: Classification of Interventions",
    "D4: Deviations from Intended Interventions"
  )
)

left_plot <- ggplot(left_labels, aes(x = x, y = y, label = label)) +
  geom_text(hjust = 0, size = 3, color = "darkgray") +  # 设置字体颜色为深灰色
  xlim(1, 3.5) +
  ylim(0.2, 4.2) +  # 调整y轴范围，减少间距
  theme_void()

# 注释 D5-D8
right_labels <- data.frame(
  x = 1.1,
  y = 4:1,
  label = c(
    "D5: Missing Data",
    "D6: Measurement of Outcomes",
    "D7: Selection of the Reported Result",
    "D8: Overall"
  )
)

right_plot <- ggplot(right_labels, aes(x = x, y = y, label = label)) +
  geom_text(hjust = 0, size = 3, color = "darkgray") +  # 设置字体颜色为深灰色
  xlim(1, 3.5) +
  ylim(0.2, 4.2) +  # 调整y轴范围，减少间距
  theme_void()

# 组合注释左右两列
annotations <- arrangeGrob(
  left_plot, right_plot,
  ncol = 2,  # 将 Judgement 竖排图移除，保持左右两列注释
  widths = c(2, 2)  # 调整每列的宽度比例，保证主图和注释部分的空间
)

# 最终拼图（调整注释部分的高度和位置）
grid.arrange(
  main_plot,
  annotations,
  nrow = 2,  # 修改为2行布局
  heights = c(10, 1)  # 调整height，使主图更大，注释部分稍小
)

