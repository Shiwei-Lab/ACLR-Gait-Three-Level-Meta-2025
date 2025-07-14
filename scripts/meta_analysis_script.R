##下载包
#install.packages("readxl")
#install.packages("metafor")
#install.packages("openxlsx")
#install.packages("dplyr")
#install.packages("writexl")
#install.packages("remotes") 
#install.packages(c("meta", "metafor", "dplyr", "ggplot2"))
#remotes::install_github("MathiasHarrer/dmetar")
# 加载必要的包
library(openxlsx)
library(readxl)
library(writexl)
library(dplyr) 
library(metafor)
library(dmetar)
library(meta)
library(metafor)
library(ggplot2)
data <- read_excel("~/Desktop/步态三水平/第二版（重新大改）/结果数据集/时空参数/walking speed 数据集/时空参数-步行速度.xlsx", sheet = 1)
data <- data %>%
  mutate(
    SD_post_exp = as.numeric(SD_post_exp),
    SD_post_ctrl = as.numeric(SD_post_ctrl),
    n_post_exp = as.numeric(n_post_exp),
    n_post_ctrl = as.numeric(n_post_ctrl)
  )
data %>%
  filter(is.na(as.numeric(M_post_exp)) | is.na(as.numeric(M_post_ctrl))) %>%
  select(M_post_exp, M_post_ctrl)
data <- data %>%
  mutate(
    M_post_exp = as.numeric(gsub("[^0-9.-]", "", M_post_exp)),
    M_post_ctrl = as.numeric(gsub("[^0-9.-]", "", M_post_ctrl))
  )
# 计算效应量和方差（使用向量化操作更高效）
processed_data <- data %>%
  rowwise() %>%
  mutate(
    # 计算合并标准差 (pooled SD)
    SD_pooled = sqrt(((n_post_exp - 1) * SD_post_exp^2 + (n_post_ctrl - 1) * SD_post_ctrl^2) / (n_post_exp + n_post_ctrl - 2)),
    
    # 计算实验组 vs. 对照组的标准化均差 (Cohen's d)
    d_exp_ctrl = (M_post_exp - M_post_ctrl) / SD_pooled,
    
    # 计算 d 的方差 (Var_d)
    Var_d_exp_ctrl = (n_post_exp + n_post_ctrl) / (n_post_exp * n_post_ctrl) + (d_exp_ctrl^2 / (2 * (n_post_exp + n_post_ctrl))),
  
  ) %>%
  ungroup()

# 查看计算结果
View(processed_data)

# 保存结果到Excel
write_xlsx(processed_data, "Cohen_d_results2.xlsx")

#################################################################################

#数据导入
data1<-read.xlsx("~/Desktop/步态三水平/第二版（重新大改）/结果数据集/时空参数/walking speed 数据集/Cohen_d_results walking speed.xlsx",colNames = TRUE)

full.model <- rma.mv(yi = d, 
                     V = Var, 
                     slab = author,
                     data = data1,
                     random = list(~ 1 | author/es.id), 
                     test = "t", 
                     method = "REML")

summary(full.model)
# 绘制森林图
forest(full.model,
       xlab = "Cohen's d (Effect Size)",
       mlab = "RE Model",
       cex = 0.8,                  # 缩放文字大小
       ilab.xpos = -2,            # 控制ilab的位置
       order = order(data1$d),    # 按效应量大小排序（可选）
       digits = 2,                # 保留小数位
       col = "blue")             # 点与线的颜色（可自定义）

#2.分析level1\level2\level3上的变异性分别占总变异的百分比
i2 <- var.comp(full.model)
summary(i2)
plot(i2)

#5.调节变量（连续变量）
#5.1time
time <- rma.mv(yi = d, V = Var, 
                          mods = ~ time, 
                          random = list(~ 1 | es.id, ~ 1 | author), 
                          tdist=TRUE, data=data1)
summary(time, digits=3)
Proportion <- rma.mv(yi = d, V = Var, 
               mods = ~ Proportion, 
               random = list(~ 1 | es.id, ~ 1 | author), 
               tdist=TRUE, data=data1)
summary(Proportion, digits=3)

# 绘制出版年代元回归图
# 提取有数据点的时间（去重、排序）
# 筛选 time ≤ 107 且排除 20–60 月的数据
library(metafor)
library(metaviz)

# 筛选 3–12 月的数据
data_filtered <- subset(data1, time >= 3 & time <= 12)

# 拟合模型
res <- rma(yi = d, vi = Var, mods = ~ time, data = data_filtered, method = "REML")

# 绘图
regplot(res,
        mod = "time",
        xlab = "Follow-up Time (months)",
        ylab = "Cohen's d",
        main = "Meta-Regression (3–12 months only)",
        pch = 19,
        col = "#2c7bb6",
        psize = 1/sqrt(data_filtered$Var)*0.5,
        xaxt = "n",
        ylim = c(-2, 2),
        ci = TRUE,
        pred = TRUE,
        refline = "predict")

# 自定义横坐标刻度
axis(1, at = c(3, 6, 12), labels = c("3", "6", "12"))

# 👉 去除右边和上方边框，只保留 x 轴和 y 轴
box(bty = "l")





#6.调节变量

#6.1country_Asia+country_Europe+country_Americas

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ country_Europe+country_Americas, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ country_Asia+country_Americas, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ country_Asia+country_Europe, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

#6.2 time_3+time_6+time_12+time_24
generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ time_6+time_12+time_24, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ time_3+time_12+time_24, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ time_3+time_6+time_24, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ time_3+time_6+time_12, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

#6.3Angular_Flexion+Angular_Extension+Angular_Adduction+Angular_Rotation
generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Angular_Extension+Angular_Adduction+Angular_Rotation, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Angular_Flexion+Angular_Adduction+Angular_Rotation, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Angular_Flexion+Angular_Extension+Angular_Rotation, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Angular_Flexion+Angular_Extension+Angular_Adduction, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)


#7.3Moment_Flexion+Moment_Extension+Moment_Adduction+Moment_Rotation
generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Moment_Extension+Moment_Adduction+Moment_Rotation, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Moment_Flexion+Moment_Adduction+Moment_Rotation, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Moment_Flexion+Moment_Extension+Moment_Rotation, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Moment_Flexion+Moment_Extension+Moment_Adduction, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

#6.4Walking_Speed+Walking_Length+Walking_Time
generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Walking_Length+Walking_Time, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Walking_Speed+Walking_Time, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

generaldelinquency <- rma.mv(yi = d, V = Var, 
                             mods = ~ Walking_Speed+Walking_Length, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(generaldelinquency, digits=3)

##第六、将显著的调节变量纳入同一个模型
##将显著的调节变量纳入同一个模型（要根据调节变量结果增加）
multiplemoderator1 <- rma.mv(yi = d, V = Var_d_exp_ctrl, 
                             mods = ~ Publication.year + 
                               Publicationtype2硕士论文 + 
                               country2Asia+country3Europe + 
                               THI2feedbackquality + THI3autonomysupport + THI4contentsupport+THI5combinedsupport+THI7homeworkquality + 
                               LO2Engagement + LO3completion + LO4interest+LO5management
                             +LO6purposes+LO8effort+others+expectancy+value+mastery.approach
                             +performance.approach+LO233Affective.attitude
                             +LO25Homework.procrastination, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(multiplemoderator1, digits=3)
##调整后
multiplemoderator1 <- rma.mv(yi = z, V = var.z, 
                             mods = ~ THI2feedbackquality + THI3autonomysupport + THI4contentsupport+THI5combinedsupport+THI7homeworkquality, 
                             random = list(~ 1 | es.id, ~ 1 | author), 
                             tdist=TRUE, data=data1)
summary(multiplemoderator1, digits=3)








