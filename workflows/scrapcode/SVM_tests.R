library(e1071)

train20 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/20_deg/", ".csv", full.names = TRUE)
train25 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/25_deg/", ".csv", full.names = TRUE)
train30 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/train/30_deg/", ".csv", full.names = TRUE)

test20 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/20_deg/", ".csv", full.names = TRUE)
test25 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/25_deg/", ".csv", full.names = TRUE)
test30 <- list.files("/Users/bioImaging2/Desktop/ziad_scripts/data/means/test/30_deg/", ".csv", full.names = TRUE)


trainData <- aggregateData(train20, train25, train30)
testData <- aggregateData(test20, test25, test30)

model <- svm(group~., data = trainData$data_1, kernel = "radial")
table(model$fitted, trainData$data_1$group)

# Testing initial model
dat <- data.frame(pred = model$fitted, actual = trainData$data_1$group)

datError <- ifelse(dat$pred == dat$actual,
                   NA, dat$pred) %>% na.omit()

accuracy <- (1 - (length(datError) / length(dat$actual))) * 100


# Testing against test data
pred <- predict(model, testData$data_1[,2:12])
table(pred, testData$data_1$group)

dat <- data.frame(pred = pred, actual = testData$data_1$group)

datError <- ifelse(dat$pred == dat$actual,
                   NA, dat$pred) %>% na.omit()

accuracy <- (1 - (length(datError) / length(dat$actual))) * 100


