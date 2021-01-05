# If the comments don't load
# File/Reopen with Ecoding/UTF-8

# Для использования R необходимо в первую очередь скачать саму программу с официального сайта 
# https://www.r-project.org/
# После успешной установки необходимо скачать R studio с сайта
# https://rstudio.com/products/rstudio/download/

install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
# подключаем пакеты и библиотеки необходимые для работы
# для загрузки файла нам нужен пакет data.table
# включаем пакет dplyr, чтобы затем могли использовать функцию lar
# чтобы могли рисовать графики, включаем пакет ggplot2

setwd("C:/Users/Milica/Desktop")
# с помощью функции setwd поменяем рабочую директорию на директорию, в которой находится файл с данными

load("C:/Users/Milica/Desktop/IT4Fin2019-W2-Dataset.RData")
# загружаем файл IT4Fin2019-W2-Dataset.RData 
# указываем полный путь к файлу

df <- read.table("hist_price.csv", header = TRUE, sep = ",", dec=".", stringsAsFactors = FALSE)
# в переменную df загружаем данные из файла hist_price.csv
# данные содержат заголовок (header)
# данные в файле разделяются запятой
# символ, используемый в файле для десятичных знаков - это .
# мы не указываем строки (strings) как факторы

# меняем имена двух столбцов
names(df)[names(df)=="TIME"] <- "period"
names(df)[names(df)=="CLOSE"] <- "close"

sample <- df[c(1:(0.7*nrow(df))),]  # из датафрейма df выбираем первые 70% строк
out_of_sample <- df[c((0.7*nrow(df)):nrow(df)),]   # остальные 30% хранятся в переменной out_of_sample

sample$returns <- 0  # создаем новое поле и заполняем его
for (i in 2:length(sample$period)) {
  sample$returns[i] <- (sample$close[i] - sample$close[i-1])/sample$close[i-1]
}

sample$portfolio_return <- 0  # создаем новое поле и заполняем его
for (i in 2:length(sample$period)) {
  sample$portfolio_return[i] <- sample$portfolio_return[i-1] + sample$returns[i]
}


sample_lar <- lar(sample, 0.4, 1) # используем функцию lar для переменной sample, p_up=0.4 и p_down=1 - стратегия на рост 

sample_lar$market_return <- sample_lar$lag0[1]  # создаем новое поле и заполняем его
for (i in 2:length(sample_lar$period)) {
  sample_lar$market_return[i] <- sample_lar$market_return[i-1] + sample_lar$lag0[i]
}

# Считаем наколпенную доходность в новом поле
sample_lar$portfolio_return <- sample_lar$lag0
for (i in 2:length(sample_lar$period)) {
  sample_lar$portfolio_return[i] <- sample_lar$portfolio_return[i-1] + (sample_lar$lag0[i] * sample_lar$position[i])
}

# Строим график portfolio_return ~ period
ggplot(data = sample_lar) +
  geom_line(aes(x = period, y = portfolio_return), col = "blue") +
  geom_line(aes(x = period, y = market_return))

# Вектор вероятности prob = (0, 0.1, 0.2, ..., 0.9, 1)
prob <- seq(0, 1, by=0.1)

#Рассчитываем компоненты ROC-анализа и записываем результаты в векторе
TPR <- vector(mode = "numeric", length = 11)  # истинно-положительный результат
FPR <- vector(mode = "numeric", length = 11)  # ложно-положительный результат
FNR <- vector(mode = "numeric", length = 11)  # ложно-отрицательный результат
TNR <- vector(mode = "numeric", length = 11)  # истинно-отрицательный результат

for (j in 1:11) { sample_Rlar <- lar(sample, prob[j], 1)
fn <-0
fp <-0
tn <-0
tp <-0

for (i in 1:length(sample_Rlar$position)) ifelse(sample_Rlar$position[i] == "1" & sample_Rlar$lag0[i]<0, fp <- fp+1,
                                                ifelse(sample_Rlar$position[i] == "1" & sample_Rlar$lag0[i]>0, tp <- tp+1,
                                                       ifelse(sample_Rlar$position[i] == "0" & sample_Rlar$lag0[i]>0, fn <- fn+1,
                                                              tn <- tn+1)))
TPR[j] <- tp/(tp+fn)
FPR[j] <- fp/(fp+tn)
TNR[j] <- tn/(tn+fp)
FNR[j] <- fn/(fn+tp)
  
}

# Создаём датафрейм для графика
ROC <- as.data.frame(cbind(FPR, TPR, TNR, FNR, prob))

# График ROC-кривой
ggplot(ROC, aes(x = FPR, y = TPR)) +
  geom_point(color = 'green') +
  geom_line(color = 'red') +
  geom_abline(intercept = 0, slope = 1, color ="black")

# Считаем расстояние до оптимальной точки
ROC$dist <- sqrt((1 - ROC$TPR)^2 + (ROC$FPR)^2) 

# Минимальное расстояние до оптимальной точки
min(ROC$dist)  # 0.6263819

# Позиция этой точки
p_up_opt <- ROC$TPR[which.min(ROC$dist)]
p_down_opt <- ROC$FPR[which.min(ROC$dist)]
p_up_opt  # 0.7582938
p_down_opt  # 0.5778689

# Строим график этих расстоянии (график dist ~ prob)
ggplot(data = ROC) + 
  geom_line(aes(x = TPR, y = dist))

# Стратегия по базовой барьерной вероятности
sample_lar$total_profit <- sample_lar$position*sample_lar$lag0 # создаем новое поле
sum(sample_lar$total_profit)  # 0.02082036
max(sample_lar$total_profit)  # 0.007000206

# Строим график изменений total_profit
ggplot(data = sample_lar) +
  geom_line(aes(x = as.numeric(rownames(sample_lar)), y = total_profit))

# Выведем доходности в датафрейм Total_Profit

# Buy and hold
Total_Profit <- data.frame(Buy_and_hold = sample$portfolio_return[(length(sample$period)-length(sample_lar$period)+1):length(sample$period)])
Total_Profit$period <- seq((length(sample$period)-length(sample_lar$period)+1):length(sample$period))

# Стратегия по базовой барьерной вероятности
Total_Profit$base_strategy <- sample_lar$total_profit

# Стратегия при ROC-оптимальной барьерной вероятности
sample_lar1 <- lar(sample, p_up_opt, p_down_opt)
# Считаем наколпенную доходность в новом поле
Total_Profit$ROC_opt <- sample_lar1$lag0
for (i in 2:length(sample_lar1$period)) {
  Total_Profit$ROC_opt[i] <- Total_Profit$ROC_opt[i-1] + (sample_lar1$lag0[i] * sample_lar1$position[i])
}
rm(sample_lar1)

# Строим график того, как прибыль меняется со временем в зависимости от стратегии
ggplot(Total_Profit, aes(period, profit)) + 
  geom_line(aes(y = Buy_and_hold, group = 1, color = "Buy_and_hold")) + 
  geom_line(aes(y = base_strategy, group = 1, color = "base_strategy")) +
  geom_line(aes(y = ROC_opt, group = 1, color = "ROC_opt"))

# Значение доходности для каждой из стратегий
sum(Total_Profit$Buy_and_hold) # -20.98677
sum(Total_Profit$base_strategy) # 0.02082036
sum(Total_Profit$ROC_opt) # 0.8418362