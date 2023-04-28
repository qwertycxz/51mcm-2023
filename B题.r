# 数据读入
attachment1 <- read.csv("附件1.csv", encoding = "UTF-8")
attachment1[, 1] <- as.Date(attachment1[, 1])
attachment1[, "Day"] <- as.integer(format(attachment1[, 1], "%d"))
attachment1[, "Month"] <- as.integer(format(attachment1[, 1], "%m"))

# 第一问:未完成
deliver <- list()
deliver_sum <- list()
deliver_diff <- list()
for (i in LETTERS) {
    deliver[[i]] <- attachment1[attachment1$Delivering == i, ][, "PCS"]
    deliver_sum[i] <- sum(deliver[[i]])
    deliver_diff[[i]] <- diff(deliver[[i]], 1)
}
receive <- list()
receive_sum <- list()
reveive_diff <- list()
for (i in LETTERS) {
    receive[[i]] <- attachment1[attachment1$Receiving == i, ][, "PCS"]
    receive_sum[i] <- sum(receive[[i]])
    reveive_diff[[i]] <- diff(receive[[i]], 1)
}

# 第二问
index <- sample(c(TRUE, FALSE), length(attachment1), replace = TRUE, prob = c(0.75, 0.25))
train <- attachment1[index, ]
test <- attachment1[!index, ]
fit <- glm(PCS ~ Month + Day + Delivering + Receiving, data = train)

fit <- glm(PCS ~ Month + Day + Delivering + Receiving, data = attachment1)
summary(fit)
plot(fit)
result <- predict(fit, newdata = test, type = "response")

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
