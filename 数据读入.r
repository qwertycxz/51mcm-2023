# 大杂烩
attachment1 <- read.csv("附件1.csv", encoding = "UTF-8")
attachment1[, 1] <- as.Date(attachment1[, 1])
attachment1[, "Day"] <- as.integer(format(attachment1[, 1], "%d"))
attachment1[, "Month"] <- as.integer(format(attachment1[, 1], "%m"))
index <- sample(c(TRUE, FALSE), length(attachment1), replace = TRUE, prob = c(0.75, 0.25))
train <- attachment1[index, ]
test <- attachment1[!index, ]
fit <- glm(PCS ~ Month + Day + Delivering + Receiving, data = train)
summary(fit)
result <- predict(fit, newdata = test, type = "response")
