# 加载必要的包
install.packages("showtext")
library(openxlsx)
library(readxl)
library(writexl)
library(dplyr) 
library(metafor)
library(meta)
library(dmetar)
library(showtext)

# 数据导入
data1 <- read.xlsx("Desktop/步态三水平/第二版（重新大改）/结果数据集/动力学/Peak external knee extension moment /Cohen_d_results Peak external knee flexion moment.xlsx", colNames = TRUE)

# 转换为escall对象
data_escalc <- escalc(
  measure = "SMD",  # Standardized Mean Difference
  m1i = M_post_exp, 
  m2i = M_post_ctrl, 
  sd1i = SD_post_exp, 
  sd2i = SD_post_ctrl, 
  n1i = n_post_exp, 
  n2i = n_post_ctrl,
  data = data1
)

# 三水平元分析模型
full.model <- rma.mv(yi = yi, 
                     V = vi, 
                     slab = author,
                     data = data_escalc,
                     random = list(~ 1 | author/es.id), 
                     test = "t", 
                     method = "REML")

# 输出三水平元分析结果
summary(full.model)

# 二水平分析部分
res1 <- rma(yi = yi, vi = vi, data = data_escalc, method = "REML")  # 使用简单模型

# Egger 检验
regtest(res1)  # p < 0.001

# 绘制漏斗图
funnel(res1, level = c(90, 95, 99), shade = c("white", "#A6CEE3", "#1F78B4"), 
       legend = FALSE, col = "grey40", xlab = "", ylab = "", back = "grey80")


# 三水平分析部分
# 三水平元分析模型
res1 <- rma.mv(yi = yi,
               V = vi,
               data = data_escalc,
               random = ~ 1 | author/es.id, 
               method = "REML", 
               test = "t", 
               slab = author)

# 聚合每个研究的效应大小
agg <- aggregate(data_escalc, cluster = author, V = vcov(res1, type = "obs"), addk = TRUE)

# 再次进行元分析，使用EE方法
res1 <- rma(yi = yi, vi = vi, method = "EE", data = agg, digits = 3, slab = paste(author))

# Egger 检验
regtest(res1)  # p = 0.040

# 漏斗图（设置背景为白色）
funnel(res1, level = c(90, 95, 99), shade = c("white", "#A6CEE3", "#1F78B4"), 
       legend = FALSE, col = "grey40", xlab = "", ylab = "", back = "grey80")
