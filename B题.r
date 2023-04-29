library("trend")
library("plyr")
library("forecast")
# 数据读入
attachment1 <- read.csv("附件1.csv", encoding = "UTF-8")
attachment1[, 1] <- as.Date(attachment1[, 1])
attachment1[, "Day"] <- as.integer(format(attachment1[, 1], "%d"))
attachment1[, "Month"] <- as.integer(format(attachment1[, 1], "%m"))

# 第一问:完成
trend_pre <- read.csv("附件1-趋势预处理.csv", encoding = "UTF-8")
# 发出
deliver <- list()
deliver_sum <- list()
# deliver_diff <- list()
# deliver_trend <- list()
for (i in LETTERS) {
    deliver[[i]] <- attachment1[attachment1$Delivering == i, ][, "PCS"]
    deliver_sum[i] <- sum(deliver[[i]])
}
# 接收
receive <- list()
receive_sum <- list()
# reveive_diff <- list()
# receive_trend <- list()
for (i in LETTERS) {
    receive[[i]] <- attachment1[attachment1$Receiving == i, ][, "PCS"]
    receive_sum[i] <- sum(receive[[i]])
}
# 趋势
trend <- list()
trend_p <- list()
trend_statistic <- list()
for (i in LETTERS) {
    trend[[i]] <- mk.test(trend_pre[, i])
    trend_p[i] <- trend[[i]]["p.value"]
    trend_statistic[i] <- trend[[i]]["statistic"]
}
# 相关
cov <- list()
cov_p <- list()
cov_statistic <- list()
for (i in LETTERS) {
    cov[[i]] <- mk.test(diff(trend_pre[, i]))
    cov_p[i] <- cov[[i]]["p.value"]
    cov_statistic[i] <- cov[[i]]["statistic"]
}
# 熵权法
topsis <- as.data.frame(cbind(unlist(deliver_sum), unlist(receive_sum), unlist(trend_statistic), unlist(cov_statistic)))
topsis[is.na(topsis)] <- 0
entropy_positive <- function(x) {
    x <- unlist(x)
    y <- (x - min(x)) / (max(x) - min(x))
    p <- y / sum(y)
    entropy <- -1 / log(length(x)) * sum(ifelse(p == 0, 0, p * log(p)))
    return(entropy)
}
entropy_data <- colwise(entropy_positive)(topsis)
entropy_weight <- (1 - entropy_data) / sum(1 - entropy_data)
# 标准化
scaled_data <- scale(topsis)
write.csv(scaled_data, "附件1-标准化.csv")
write.csv(entropy_weight, "附件1-权重.csv")
# 结果见 第一问结果.py

# 第二问:完成
index <- sample(c(TRUE, FALSE), length(attachment1), replace = TRUE, prob = c(0.75, 0.25))
train <- attachment1[index, ]
test <- attachment1[!index, ]
fit <- glm(PCS ~ Month + Day + Delivering + Receiving, data = train)
summary(fit)
plot(fit)
result <- predict(fit, newdata = test, type = "response")
result[result < 0] <- 0
# 分层运算
result <- list()
for (i in LETTERS) {
    for (j in LETTERS) {
        route <- attachment1[attachment1$Delivering == i, ]
        route <- route[route$Receiving == j, ]
        if (nrow(route)) {
            index <- sample(c(TRUE, FALSE), length(route), TRUE, c(0.75, 0.25))
            train <- route[index, ]
            test <- route[!index, ]
            fit <- glm(PCS ~ Month + Day, data = train)
            result[[paste(i, j)]] <- predict(fit, newdata = test, type = "response")
        }
    }
}
# 结果输出
fit <- glm(PCS ~ Month + Day + Delivering + Receiving, data = attachment1)
Month <-      c(4,   4,   4,   4,   4,   4,   4,   4)
Day <-        c(18,  18,  18,  18,  19,  19,  19,  19)
Delivering <- c("M", "Q", "K", "G", "V", "A", "D", "L")
Receiving <-  c("U", "V", "L", "V", "G", "Q", "A", "K")
goal_data <- data.frame(Month, Day, Delivering, Receiving)
result <- predict(fit, newdata = goal_data, type = "response")
goal4.17 <- attachment1[attachment1$X.U.FEFF.Date == "2019-4-17", ]
goal4.18 <- goal4.17
goal4.18$Day <- 18
goal4.19 <- goal4.17
goal4.19$Day <- 19
sum4.18 <- sum(predict(fit, newdata = goal4.18, type = "response"))
sum4.19 <- sum(predict(fit, newdata = goal4.19, type = "response"))
result
sum4.18
sum4.19

# 第三问:完成
attachment2 <- read.csv("附件2.csv", encoding = "UTF-8")
attachment2[, 1] <- as.Date(attachment2[, 1])
attachment2[, "Day"] <- as.integer(format(attachment2[, 1], "%d"))
attachment2[, "Month"] <- as.integer(format(attachment2[, 1], "%m"))
attachment2[, "Year"] <- as.integer(format(attachment2[, 1], "%Y"))
#正式处理
index <- sample(c(TRUE, FALSE), length(attachment2), replace = TRUE, prob = c(0.75, 0.25))
train <- attachment2[index, ]
test <- attachment2[!index, ]
fit <- glm(PCS ~ Year + Month + Day + Delivering + Receiving, data = train)
result <- predict(fit, newdata = test, type = "response")
result[result < 0] <- 0
# 时间序列
time_pre <- read.csv("附件2-时间序列预处理.csv", encoding = "UTF-8") # 可以通过截断csv文件的方法实现预测
time2 <- ts(time_pre, c(2020, 119), frequency = 273)
# 调用此函数, 便可预测路线的在下两天的通断情况
predictRouteAccess <- function(route) {
    fit <- ets(time2[, route])
    return(forecast(fit, 2))
}
predictRouteAccess("IS")
predictRouteAccess("MG")
predictRouteAccess("SQ")
predictRouteAccess("VA")
predictRouteAccess("YL")
predictRouteAccess("DR")
predictRouteAccess("JK")
predictRouteAccess("QO")
predictRouteAccess("UO")
predictRouteAccess("YW")
# 若能正常发货, 则发货数量预测
fit <- glm(PCS ~ Year + Month + Day + Delivering + Receiving, data = attachment2)
Year <-       c(2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023)
Month <-      c(4,    4,    4,    4,    4,    4,    4,    4,    4,    4)
Day <-        c(28,   28,   28,   28,   28,   29,   29,   29,   29,   29)
Delivering <- c("I",  "M",  "S",  "V",  "Y",  "D",  "J",  "Q",  "U",  "Y")
Receiving <-  c("S",  "G",  "Q",  "A",  "L",  "R",  "K",  "O",  "O",  "W")
goal_data <- data.frame(Year, Month, Day, Delivering, Receiving)
result <- predict(fit, newdata = goal_data, type = "response")
