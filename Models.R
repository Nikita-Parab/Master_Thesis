library(ggplot2)
library(caret)
library(tidyverse)
library(keras)
library(Rfast)

#All data
setwd( "C:/Users/nikita parab/Desktop/SEMESTER 3/ICT Solution/PreProcessed_Data")
charlie <- read.csv("charliehebdo.csv")
ferguson <- read.csv("ferguson.csv")
german <- read.csv("germanwings-crash.csv")
gurlitt <- read.csv("gurlitt.csv")
ottawa <- read.csv("ottawashooting.csv")
putin <- read.csv("putinmissing.csv")
sydneysiege <- read.csv("sydneysiege.csv")


#Improvement in SVM
train <- rbind(ferguson,german,ottawa,sydneysiege)
test <- charlie

#converting to factors
train$status <- factor(train$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))
test$status <- factor(test$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))

#Support Vector Machine
fit.imp <- caret::train(status~., data = train, method = "svmRadial", metric = "Accuracy")
pred.imp <- predict(fit.imp, test)
imptab <- table(pred.imp, test$status)
impc <-confusionMatrix(svmtab)
impp <- precision(svmtab)
impr <- recall(svmtab)
impf <-F_meas(svmtab)



#Creating training and testing data
testing <- charlie
training <- rbind(ferguson,german,ottawa,putin,sydneysiege)

#binary classification with Keras
x_train <- training %>% select(-status) %>% scale()
y_train <- to_categorical(training$status)

x_test <- testing %>% select(-status) %>% scale()
y_test <- to_categorical(testing$status)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 80, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 40, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model%>%fit(
  x_train, y_train,
  epochs = 60,
  batch_size = 5,
  validation_split = 0.2
)

plot(history)
summary(model)
evalkeras <- model%>%evaluate(x_test,y_test)
pred.keras <- model%>%predict_classes(x_test)
tabkeras <- table(factor(pred.keras, levels = c(0,1)),factor(testing$status, levels = c(0,1)))
kp <- precision(tabkeras)
kpr <- recall(tabkeras)
kpf <- F_meas(tabkeras)
kpc <- confusionMatrix(tabkeras)

#converting to factors
training$status <- factor(training$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))
testing$status <- factor(testing$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))

#data plots
plot <- ggplot(data = testing, mapping = aes(x =timeDiff  , y=Freq))+
  geom_line(aes(color = status))
plot + scale_colour_manual(values = c("green","red")) + 
  labs(title = "Event : Charlie Hebdo",  x = "Time Interval", y = "Interactions")


#Linear Discriminant Analysis
fit.lda1 <- caret::train(status~., data = training, method = "lda", metric = "Accuracy")
pred.lda1 <- predict(fit.lda1, testing)
ldatab <- table(pred.lda1, testing$status)
ldac <- confusionMatrix(ldatab)
ldap <- precision(ldatab)
ldar <- recall(ldatab)
ldaf <- F_meas(ldatab)

#Classification and Regression Trees (CART)
fit.cart1 <- caret::train(status~., data = training, method = "rpart", metric = "Accuracy")
pred.cart1 <- predict(fit.cart1, testing)
carttab <- table(pred.cart1, testing$status)
cartc <- confusionMatrix(carttab)
cartp <- precision(carttab)
cartr <- recall(carttab)
cartf <- F_meas(carttab)

#KNN
fit.knn1 <- caret::train(status~., data = training, method = "knn", metric = "Accuracy")
pred.knn1 <- predict(fit.knn1, testing)
knntab <- table(pred.knn1, testing$status)
knnc <- confusionMatrix(knntab)
knnp <- precision(knntab)
knnr <- recall(knntab)
knnf <-F_meas(knntab)

#Random Forest
fit.rf1 <- caret::train(status~., data = training, method = "rf", metric = "Accuracy")
pred.rf1 <- predict(fit.rf1, testing)
rftab <- table(pred.rf1, testing$status)
rfc <- confusionMatrix(rftab)
rfp <- precision(rftab)
rfr <- recall(rftab)
rff <- F_meas(rftab)

#gaussian Naive Bayes
gnbx <- as.matrix(training[,c(1,3)])
gnby <- testing[,c(1,3)]
fit.gnb1 <- Rfast::gaussian.nb(xnew = NULL, gnbx , ina = training$status)
pred.gnb1 <- Rfast::gaussiannb.pred(gnby, fit.gnb1$mu, fit.gnb1$sigma, fit.gnb1$ni)
gnbtab <- table(pred.gnb1, testing$status)
rownames(gnbtab) <- c("Non-Rumor", "Rumor")
gnbp <- precision(gnbtab)
gnbr <- recall(gnbtab)
gnbf <- F_meas(gnbtab)
gnbc <- confusionMatrix(gnbtab)

#Support Vector Machine
fit.svm <- caret::train(status~., data = training, method = "svmRadial", metric = "Accuracy")
pred.svm <- predict(fit.svm, testing)
svmtab <- table(pred.svm, testing$status)
svmc <-confusionMatrix(svmtab)
svmp <- precision(svmtab)
svmr <- recall(svmtab)
svmf <-F_meas(svmtab)


#results
results <- data.frame("Model","Interval","Accuracy", "Precision", "Recall","F1 Score")
colnames(results) <- c("Model","Interval","Accuracy", "Precision", "Recall","F1 Score")
results <- results[-1,]
results <- rbind(results, data.frame("Model" = "SVM",
                                     "Accuracy" = c(svmc[["overall"]][["Accuracy"]]),
                                     "Precision"=svmp,"Recall" =svmr, "F1 Score"=svmf))

results <- rbind(results, data.frame("Model" = "Gaussian Naive Bayes",
                                     "Accuracy" = c(gnbc[["overall"]][["Accuracy"]]),
                                     "Precision"=gnbp,"Recall" =gnbr, "F1 Score"=gnbf))

results <- rbind(results, data.frame("Model" = "KNN",
                                     "Accuracy" = c(knnc[["overall"]][["Accuracy"]]),
                                     "Precision"=knnp,"Recall" =knnr, "F1 Score"=knnf))
results <- rbind(results, data.frame("Model" = "LDA",
                                     "Accuracy" = c(ldac[["overall"]][["Accuracy"]]),
                                     "Precision"=ldap,"Recall" =ldar, "F1 Score"=ldaf))
results <- rbind(results, data.frame("Model" = "CART",
                                     "Accuracy" = c(cartc[["overall"]][["Accuracy"]]),
                                     "Precision"=cartp,"Recall" =cartr, "F1 Score"=cartf))
results <- rbind(results, data.frame("Model" = "Random Forest",
                                     "Accuracy" = c(rfc[["overall"]][["Accuracy"]]),
                                     "Precision"=rfp,"Recall" =rfr, "F1 Score"=rff))
results <- rbind(results, data.frame("Model" = "DL with Keras",
                                     "Accuracy" = c(kpc[["overall"]][["Accuracy"]]),
                                     "Precision"=kp,"Recall" =kpr, "F1 Score"=kpf))





#FERGUSON
training <- rbind(charlie,german,gurlitt,ottawa,putin,sydneysiege)
testing <- ferguson

#binary classification with Keras
x_train <- training %>% select(-status) %>% scale()
y_train <- to_categorical(training$status)

x_test <- testing %>% select(-status) %>% scale()
y_test <- to_categorical(testing$status)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 80, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 40, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model%>%fit(
  x_train, y_train,
  epochs = 60,
  batch_size = 5,
  validation_split = 0.2
)

plot(history)
summary(model)
evalkeras <- model%>%evaluate(x_test,y_test)
pred.keras <- model%>%predict_classes(x_test)
tabkeras <- table(factor(pred.keras, levels = c(0,1)),factor(testing$status, levels = c(0,1)))
kp <- precision(tabkeras)
kpr <- recall(tabkeras)
kpf <- F_meas(tabkeras)
kpc <- confusionMatrix(tabkeras)

#converting to factors
training$status <- factor(training$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))
testing$status <- factor(testing$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))

graph <- testing[c(-1,-2990),]
#data plots
plot <- ggplot(data = graph, mapping = aes(x = timeDiff, y=Freq))+
  geom_line(aes(color = status))
plot + scale_colour_manual(values = c("green","red")) + 
  labs(title = "Event: Ferguson",  x = "Time Interval", y = "Interactions")


#Linear Discriminant Analysis
fit.lda2 <- caret::train(status~., data = training, method = "lda", metric = "Accuracy")
pred.lda2 <- predict(fit.lda2, testing)
ldatab <- table(pred.lda2, testing$status)
ldac <- confusionMatrix(ldatab)
ldap <- precision(ldatab)
ldar <- recall(ldatab)
ldaf <- F_meas(ldatab)


#Classification and Regression Trees (CART)
fit.cart2 <- caret::train(status~., data = training, method = "rpart", metric = "Accuracy")
pred.cart2 <- predict(fit.cart2, testing)
carttab <- table(pred.cart2, testing$status)
cartc <- confusionMatrix(carttab)
cartp <- precision(carttab)
cartr <- recall(carttab)
cartf <- F_meas(carttab)

#KNN
fit.knn2 <- caret::train(status~., data = training, method = "knn", metric = "Accuracy")
pred.knn2 <- predict(fit.knn2, testing)
knntab <- table(pred.knn2, testing$status)
knnc <- confusionMatrix(knntab)
knnp <- precision(knntab)
knnr <- recall(knntab)
knnf <-F_meas(knntab)

#Random Forest
fit.rf2 <- caret::train(status~., data = training, method = "rf", metric = "Accuracy")
pred.rf2 <- predict(fit.rf2, testing)
rftab <- table(pred.rf2, testing$status)
rfc <- confusionMatrix(rftab)
rfp <- precision(rftab)
rfr <- recall(rftab)
rff <- F_meas(rftab)

#gaussian Naive Bayes
gnbx <- as.matrix(training[,c(1,3)])
gnby <- testing[,c(1,3)]
fit.gnb2 <- Rfast::gaussian.nb(xnew = NULL, gnbx , ina = training$status)
pred.gnb2 <- Rfast::gaussiannb.pred(gnby, fit.gnb2$mu, fit.gnb2$sigma, fit.gnb2$ni)
gnbtab <- table(pred.gnb2, testing$status)
rownames(gnbtab) <- c("Non-Rumor", "Rumor")
gnbp <- precision(gnbtab)
gnbr <- recall(gnbtab)
gnbf <- F_meas(gnbtab)
gnbc <- confusionMatrix(gnbtab)

#Support Vector Machine
fit.svm <- caret::train(status~., data = training, method = "svmRadial", metric = "Accuracy")
pred.svm <- predict(fit.svm, testing)
svmtab <- table(pred.svm, testing$status)
svmc <-confusionMatrix(svmtab)
svmp <- precision(svmtab)
svmr <- recall(svmtab)
svmf <-F_meas(svmtab)


#results
results <- rbind(results, data.frame("Model" = "SVM",
                                     "Accuracy" = c(svmc[["overall"]][["Accuracy"]]),
                                     "Precision"=svmp,"Recall" =svmr, "F1 Score"=svmf))

results <- rbind(results, data.frame("Model" = "Gaussian Naive Bayes",
                                     "Accuracy" = c(gnbc[["overall"]][["Accuracy"]]),
                                     "Precision"=gnbp,"Recall" =gnbr, "F1 Score"=gnbf))

results <- rbind(results, data.frame("Model" = "KNN",
                                     "Accuracy" = c(knnc[["overall"]][["Accuracy"]]),
                                     "Precision"=knnp,"Recall" =knnr, "F1 Score"=knnf))
results <- rbind(results, data.frame("Model" = "LDA",
                                     "Accuracy" = c(ldac[["overall"]][["Accuracy"]]),
                                     "Precision"=ldap,"Recall" =ldar, "F1 Score"=ldaf))
results <- rbind(results, data.frame("Model" = "CART",
                                     "Accuracy" = c(cartc[["overall"]][["Accuracy"]]),
                                     "Precision"=cartp,"Recall" =cartr, "F1 Score"=cartf))
results <- rbind(results, data.frame("Model" = "Random Forest",
                                     "Accuracy" = c(rfc[["overall"]][["Accuracy"]]),
                                     "Precision"=rfp,"Recall" =rfr, "F1 Score"=rff))
results <- rbind(results, data.frame("Model" = "DL with Keras",
                                     "Accuracy" = c(kpc[["overall"]][["Accuracy"]]),
                                     "Precision"=kp,"Recall" =kpr, "F1 Score"=kpf))

#GERMAN WINGS
training <- rbind(charlie,ferguson,gurlitt,ottawa,putin,sydneysiege)
testing <- german

#binary classification with Keras
x_train <- training %>% select(-status) %>% scale()
y_train <- to_categorical(training$status)

x_test <- testing %>% select(-status) %>% scale()
y_test <- to_categorical(testing$status)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 80, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 40, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model%>%fit(
  x_train, y_train,
  epochs = 60,
  batch_size = 5,
  validation_split = 0.2
)

plot(history)
summary(model)
evalkeras <- model%>%evaluate(x_test,y_test)
pred.keras <- model%>%predict_classes(x_test)
tabkeras <- table(factor(pred.keras, levels = c(0,1)),factor(testing$status, levels = c(0,1)))
kp <- precision(tabkeras)
kpr <- recall(tabkeras)
kpf <- F_meas(tabkeras)
kpc <- confusionMatrix(tabkeras)

#converting to factors
training$status <- factor(training$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))
testing$status <- factor(testing$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))

#data plots
plot <- ggplot(data = testing, mapping = aes(x = timeDiff, y=Freq))+
  geom_line(aes(color = status))
plot + scale_colour_manual(values = c("green","red")) + 
  labs(title = "Event: German Wings",  x = "Time Interval", y = "Interactions")


#Linear Discriminant Analysis
fit.lda3 <- caret::train(status~., data = training, method = "lda", metric = "Accuracy")
pred.lda3 <- predict(fit.lda3, testing)
ldatab <- table(pred.lda3, testing$status)
ldac <- confusionMatrix(ldatab)
ldap <- precision(ldatab)
ldar <- recall(ldatab)
ldaf <- F_meas(ldatab)


#Classification and Regression Trees (CART)
fit.cart3 <- caret::train(status~., data = training, method = "rpart", metric = "Accuracy")
pred.cart3 <- predict(fit.cart3, testing)
carttab <- table(pred.cart3, testing$status)
cartc <- confusionMatrix(carttab)
cartp <- precision(carttab)
cartr <- recall(carttab)
cartf <- F_meas(carttab)

#KNN
fit.knn3 <- caret::train(status~., data = training, method = "knn", metric = "Accuracy")
pred.knn3 <- predict(fit.knn3, testing)
knntab <- table(pred.knn3, testing$status)
knnc <- confusionMatrix(knntab)
knnp <- precision(knntab)
knnr <- recall(knntab)
knnf <-F_meas(knntab)

#Random Forest
fit.rf3 <- caret::train(status~., data = training, method = "rf", metric = "Accuracy" )
pred.rf3 <- predict(fit.rf3, testing)
rftab <- table(pred.rf3, testing$status)
rfc <- confusionMatrix(rftab)
rfp <- precision(rftab)
rfr <- recall(rftab)
rff <- F_meas(rftab)

#gaussian Naive Bayes
gnbx <- as.matrix(training[,c(1,3)])
gnby <- testing[,c(1,3)]
fit.gnb3 <- Rfast::gaussian.nb(xnew = NULL, gnbx , ina = training$status)
pred.gnb3 <- Rfast::gaussiannb.pred(gnby, fit.gnb3$mu, fit.gnb3$sigma, fit.gnb3$ni)
gnbtab <- table(pred.gnb3, testing$status)
rownames(gnbtab) <- c("Non-Rumor", "Rumor")
gnbp <- precision(gnbtab)
gnbr <- recall(gnbtab)
gnbf <- F_meas(gnbtab)
gnbc <- confusionMatrix(gnbtab)

#Support Vector Machine
fit.svm <- caret::train(status~., data = training, method = "svmRadial", metric = "Accuracy")
pred.svm <- predict(fit.svm, testing)
svmtab <- table(pred.svm, testing$status)
svmc <-confusionMatrix(svmtab)
svmp <- precision(svmtab)
svmr <- recall(svmtab)
svmf <-F_meas(svmtab)


#results
results <- rbind(results, data.frame("Model" = "SVM",
                                     "Accuracy" = c(svmc[["overall"]][["Accuracy"]]),
                                     "Precision"=svmp,"Recall" =svmr, "F1 Score"=svmf))

results <- rbind(results, data.frame("Model" = "Gaussian Naive Bayes",
                                     "Accuracy" = c(gnbc[["overall"]][["Accuracy"]]),
                                     "Precision"=gnbp,"Recall" =gnbr, "F1 Score"=gnbf))

results <- rbind(results, data.frame("Model" = "KNN",
                                     "Accuracy" = c(knnc[["overall"]][["Accuracy"]]),
                                     "Precision"=knnp,"Recall" =knnr, "F1 Score"=knnf))
results <- rbind(results, data.frame("Model" = "LDA",
                                     "Accuracy" = c(ldac[["overall"]][["Accuracy"]]),
                                     "Precision"=ldap,"Recall" =ldar, "F1 Score"=ldaf))
results <- rbind(results, data.frame("Model" = "CART",
                                     "Accuracy" = c(cartc[["overall"]][["Accuracy"]]),
                                     "Precision"=cartp,"Recall" =cartr, "F1 Score"=cartf))
results <- rbind(results, data.frame("Model" = "Random Forest",
                                     "Accuracy" = c(rfc[["overall"]][["Accuracy"]]),
                                     "Precision"=rfp,"Recall" =rfr, "F1 Score"=rff))
results <- rbind(results, data.frame("Model" = "DL with Keras",
                                     "Accuracy" = c(kpc[["overall"]][["Accuracy"]]),
                                     "Precision"=kp,"Recall" =kpr, "F1 Score"=kpf))
#GURLITT
training <- rbind(charlie,ferguson,german,ottawa,putin,sydneysiege)
testing <- gurlitt

#binary classification with Keras
x_train <- training %>% select(-status) %>% scale()
y_train <- to_categorical(training$status)

x_test <- testing %>% select(-status) %>% scale()
y_test <- to_categorical(testing$status)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model%>%fit(
  x_train, y_train,
  epochs = 60,
  batch_size = 5,
  validation_split = 0.2
)

plot(history)
summary(model)
evalkeras <- model%>%evaluate(x_test,y_test)
pred.keras <- model%>%predict_classes(x_test)
tabkeras <- table(factor(pred.keras, levels = c(0,1)),factor(testing$status, levels = c(0,1)))
kp <- precision(tabkeras)
kpr <- recall(tabkeras)
kpf <- F_meas(tabkeras)
kpc <- confusionMatrix(tabkeras)

#converting to factors
training$status <- factor(training$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))
testing$status <- factor(testing$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))

#data plots
plot <- ggplot(data = testing, mapping = aes(x = timeDiff, y=Freq))+
  geom_line(aes(color = status))
plot + scale_colour_manual(values = c("green","red")) + 
  labs(title = " mins interval",  x = "Time Interval", y = "Interactions")


#Linear Discriminant Analysis
fit.lda4 <- caret::train(status~., data = training, method = "lda", metric = "Accuracy")
pred.lda4 <- predict(fit.lda4, testing)
ldatab <- table(pred.lda4, testing$status)
ldac <- confusionMatrix(ldatab)
ldap <- precision(ldatab)
ldar <- recall(ldatab)
ldaf <- F_meas(ldatab)


#Classification and Regression Trees (CART)
fit.cart4 <- caret::train(status~., data = training, method = "rpart", metric = "Accuracy")
pred.cart4 <- predict(fit.cart4, testing)
carttab <- table(pred.cart4, testing$status)
cartc <- confusionMatrix(carttab)
cartp <- precision(carttab)
cartr <- recall(carttab)
cartf <- F_meas(carttab)

#KNN
fit.knn4 <- caret::train(status~., data = training, method = "knn", metric = "Accuracy")
pred.knn4 <- predict(fit.knn4, testing)
knntab <- table(pred.knn4, testing$status)
knnc <- confusionMatrix(knntab)
knnp <- precision(knntab)
knnr <- recall(knntab)
knnf <-F_meas(knntab)

#Random Forest
fit.rf4 <- caret::train(status~., data = training, method = "rf", metric = "Accuracy")
pred.rf4 <- predict(fit.rf4, testing)
rftab <- table(pred.rf4, testing$status)
rfc <- confusionMatrix(rftab)
rfp <- precision(rftab)
rfr <- recall(rftab)
rff <- F_meas(rftab)

#gaussian Naive Bayes
gnbx <- as.matrix(training[,c(1,3)])
gnby <- testing[,c(1,3)]
fit.gnb4 <- Rfast::gaussian.nb(xnew = NULL, gnbx , ina = training$status)
pred.gnb4 <- Rfast::gaussiannb.pred(gnby, fit.gnb4$mu, fit.gnb4$sigma, fit.gnb4$ni)
gnbtab <- table(pred.gnb4, testing$status)
rownames(gnbtab) <- c("Non-Rumor", "Rumor")
gnbp <- precision(gnbtab)
gnbr <- recall(gnbtab)
gnbf <- F_meas(gnbtab)
gnbc <- confusionMatrix(gnbtab)

#Support Vector Machine
fit.svm <- caret::train(status~., data = training, method = "svmRadial", metric = "Accuracy")
pred.svm <- predict(fit.svm, testing)
svmtab <- table(pred.svm, testing$status)
svmc <-confusionMatrix(svmtab)
svmp <- precision(svmtab)
svmr <- recall(svmtab)
svmf <-F_meas(svmtab)


#results
results <- rbind(results, data.frame("Model" = "SVM",
                                     "Accuracy" = c(svmc[["overall"]][["Accuracy"]]),
                                     "Precision"=svmp,"Recall" =svmr, "F1 Score"=svmf))

results <- rbind(results, data.frame("Model" = "Gaussian Naive Bayes",
                                     "Accuracy" = c(gnbc[["overall"]][["Accuracy"]]),
                                     "Precision"=gnbp,"Recall" =gnbr, "F1 Score"=gnbf))

results <- rbind(results, data.frame("Model" = "KNN",
                                     "Accuracy" = c(knnc[["overall"]][["Accuracy"]]),
                                     "Precision"=knnp,"Recall" =knnr, "F1 Score"=knnf))
results <- rbind(results, data.frame("Model" = "LDA",
                                     "Accuracy" = c(ldac[["overall"]][["Accuracy"]]),
                                     "Precision"=ldap,"Recall" =ldar, "F1 Score"=ldaf))
results <- rbind(results, data.frame("Model" = "CART",
                                     "Accuracy" = c(cartc[["overall"]][["Accuracy"]]),
                                     "Precision"=cartp,"Recall" =cartr, "F1 Score"=cartf))
results <- rbind(results, data.frame("Model" = "Random Forest",
                                     "Accuracy" = c(rfc[["overall"]][["Accuracy"]]),
                                     "Precision"=rfp,"Recall" =rfr, "F1 Score"=rff))
results <- rbind(results, data.frame("Model" = "DL with Keras",
                                     "Accuracy" = c(kpc[["overall"]][["Accuracy"]]),
                                     "Precision"=kp,"Recall" =kpr, "F1 Score"=kpf))



#OTTAWA SHOOTING
training <- rbind(charlie,ferguson,german,gurlitt,putin,sydneysiege)
testing <- ottawa

#binary classification with Keras
x_train <- training %>% select(-status) %>% scale()
y_train <- to_categorical(training$status)

x_test <- testing %>% select(-status) %>% scale()
y_test <- to_categorical(testing$status)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 80, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 40, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model%>%fit(
  x_train, y_train,
  epochs = 60,
  batch_size = 5,
  validation_split = 0.2
)

plot(history)
summary(model)
evalkeras <- model%>%evaluate(x_test,y_test)
pred.keras <- model%>%predict_classes(x_test)
tabkeras <- table(factor(pred.keras, levels = c(0,1)),factor(testing$status, levels = c(0,1)))
kp <- precision(tabkeras)
kpr <- recall(tabkeras)
kpf <- F_meas(tabkeras)
kpc <- confusionMatrix(tabkeras)

#converting to factors
training$status <- factor(training$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))
testing$status <- factor(testing$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))

#data plots
plot <- ggplot(data = testing, mapping = aes(x = timeDiff, y=Freq))+
  geom_line(aes(color = status))
plot + scale_colour_manual(values = c("green","red")) + 
  labs(title = "Event: Ottawa Shooting",  x = "Time Interval", y = "Interactions")


#Linear Discriminant Analysis
fit.lda5 <- caret::train(status~., data = training, method = "lda", metric = "Accuracy")
pred.lda5 <- predict(fit.lda5, testing)
ldatab <- table(pred.lda5, testing$status)
ldac <- confusionMatrix(ldatab)
ldap <- precision(ldatab)
ldar <- recall(ldatab)
ldaf <- F_meas(ldatab)


#Classification and Regression Trees (CART)
fit.cart5 <- caret::train(status~., data = training, method = "rpart", metric = "Accuracy")
pred.cart5 <- predict(fit.cart5, testing)
carttab <- table(pred.cart5, testing$status)
cartc <- confusionMatrix(carttab)
cartp <- precision(carttab)
cartr <- recall(carttab)
cartf <- F_meas(carttab)

#KNN
fit.knn5 <- caret::train(status~., data = training, method = "knn", metric = "Accuracy")
pred.knn5 <- predict(fit.knn5, testing)
knntab <- table(pred.knn5, testing$status)
knnc <- confusionMatrix(knntab)
knnp <- precision(knntab)
knnr <- recall(knntab)
knnf <-F_meas(knntab)

#Random Forest
fit.rf5 <- caret::train(status~., data = training, method = "rf", metric = "Accuracy" )
pred.rf5 <- predict(fit.rf5, testing)
rftab <- table(pred.rf5, testing$status)
rfc <- confusionMatrix(rftab)
rfp <- precision(rftab)
rfr <- recall(rftab)
rff <- F_meas(rftab)

#gaussian Naive Bayes
gnbx <- as.matrix(training[,c(1,3)])
gnby <- testing[,c(1,3)]
fit.gnb5 <- Rfast::gaussian.nb(xnew = NULL, gnbx , ina = training$status)
pred.gnb5 <- Rfast::gaussiannb.pred(gnby, fit.gnb5$mu, fit.gnb5$sigma, fit.gnb5$ni)
gnbtab <- table(pred.gnb5, testing$status)
rownames(gnbtab) <- c("Non-Rumor", "Rumor")
gnbp <- precision(gnbtab)
gnbr <- recall(gnbtab)
gnbf <- F_meas(gnbtab)
gnbc <- confusionMatrix(gnbtab)

#Support Vector Machine
fit.svm <- caret::train(status~., data = training, method = "svmRadial", metric = "Accuracy")
pred.svm <- predict(fit.svm, testing)
svmtab <- table(pred.svm, testing$status)
svmc <-confusionMatrix(svmtab)
svmp <- precision(svmtab)
svmr <- recall(svmtab)
svmf <-F_meas(svmtab)


#results

results <- rbind(results, data.frame("Model" = "SVM",
                                     "Accuracy" = c(svmc[["overall"]][["Accuracy"]]),
                                     "Precision"=svmp,"Recall" =svmr, "F1 Score"=svmf))

results <- rbind(results, data.frame("Model" = "Gaussian Naive Bayes",
                                     "Accuracy" = c(gnbc[["overall"]][["Accuracy"]]),
                                     "Precision"=gnbp,"Recall" =gnbr, "F1 Score"=gnbf))

results <- rbind(results, data.frame("Model" = "KNN",
                                     "Accuracy" = c(knnc[["overall"]][["Accuracy"]]),
                                     "Precision"=knnp,"Recall" =knnr, "F1 Score"=knnf))
results <- rbind(results, data.frame("Model" = "LDA",
                                     "Accuracy" = c(ldac[["overall"]][["Accuracy"]]),
                                     "Precision"=ldap,"Recall" =ldar, "F1 Score"=ldaf))
results <- rbind(results, data.frame("Model" = "CART",
                                     "Accuracy" = c(cartc[["overall"]][["Accuracy"]]),
                                     "Precision"=cartp,"Recall" =cartr, "F1 Score"=cartf))
results <- rbind(results, data.frame("Model" = "Random Forest",
                                     "Accuracy" = c(rfc[["overall"]][["Accuracy"]]),
                                     "Precision"=rfp,"Recall" =rfr, "F1 Score"=rff))
results <- rbind(results, data.frame("Model" = "DL with Keras",
                                     "Accuracy" = c(kpc[["overall"]][["Accuracy"]]),
                                     "Precision"=kp,"Recall" =kpr, "F1 Score"=kpf))
#PUTIN
training <- rbind(charlie,ferguson,german,gurlitt,ottawa,sydneysiege)
testing <- putin

#binary classification with Keras
x_train <- training %>% select(-status) %>% scale()
y_train <- to_categorical(training$status)

x_test <- testing %>% select(-status) %>% scale()
y_test <- to_categorical(testing$status)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 80, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 40, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model%>%fit(
  x_train, y_train,
  epochs = 60,
  batch_size = 5,
  validation_split = 0.2
)

plot(history)
summary(model)
evalkeras <- model%>%evaluate(x_test,y_test)
pred.keras <- model%>%predict_classes(x_test)
tabkeras <- table(factor(pred.keras, levels = c(0,1)),factor(testing$status, levels = c(0,1)))
kp <- precision(tabkeras)
kpr <- recall(tabkeras)
kpf <- F_meas(tabkeras)
kpc <- confusionMatrix(tabkeras)

#converting to factors
training$status <- factor(training$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))
testing$status <- factor(testing$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))

#data plots
plot <- ggplot(data = testing, mapping = aes(x = timeDiff, y=Freq))+
  geom_line(aes(color = status))
plot + scale_colour_manual(values = c("green","red")) + 
  labs(title = "Event: Putin Missing",  x = "Time Interval", y = "Interactions")


#Linear Discriminant Analysis
fit.lda6 <- caret::train(status~., data = training, method = "lda", metric = "Accuracy")
pred.lda6 <- predict(fit.lda6, testing)
ldatab <- table(pred.lda6, testing$status)
ldac <- confusionMatrix(ldatab)
ldap <- precision(ldatab)
ldar <- recall(ldatab)
ldaf <- F_meas(ldatab)


#Classification and Regression Trees (CART)
fit.cart6 <- caret::train(status~., data = training, method = "rpart", metric = "Accuracy")
pred.cart6 <- predict(fit.cart6, testing)
carttab <- table(pred.cart6, testing$status)
cartc <- confusionMatrix(carttab)
cartp <- precision(carttab)
cartr <- recall(carttab)
cartf <- F_meas(carttab)

#KNN
fit.knn6 <- caret::train(status~., data = training, method = "knn", metric = "Accuracy")
pred.knn6 <- predict(fit.knn6, testing)
knntab <- table(pred.knn6, testing$status)
knnc <- confusionMatrix(knntab)
knnp <- precision(knntab)
knnr <- recall(knntab)
knnf <-F_meas(knntab)

#Random Forest
fit.rf6 <- caret::train(status~., data = training, method = "rf", metric = "Accuracy")
pred.rf6 <- predict(fit.rf6, testing)
rftab <- table(pred.rf6, testing$status)
rfc <- confusionMatrix(rftab)
rfp <- precision(rftab)
rfr <- recall(rftab)
rff <- F_meas(rftab)

#gaussian Naive Bayes
gnbx <- as.matrix(training[,c(1,3)])
gnby <- testing[,c(1,3)]
fit.gnb6 <- Rfast::gaussian.nb(xnew = NULL, gnbx , ina = training$status)
pred.gnb6 <- Rfast::gaussiannb.pred(gnby, fit.gnb6$mu, fit.gnb6$sigma, fit.gnb6$ni)
gnbtab <- table(pred.gnb6, testing$status)
rownames(gnbtab) <- c("Non-Rumor", "Rumor")
gnbp <- precision(gnbtab)
gnbr <- recall(gnbtab)
gnbf <- F_meas(gnbtab)
gnbc <- confusionMatrix(gnbtab)

#Support Vector Machine
fit.svm <- caret::train(status~., data = training, method = "svmRadial", metric = "Accuracy")
pred.svm <- predict(fit.svm, testing)
svmtab <- table(pred.svm, testing$status)
svmc <-confusionMatrix(svmtab)
svmp <- precision(svmtab)
svmr <- recall(svmtab)
svmf <-F_meas(svmtab)


#results
results <- rbind(results, data.frame("Model" = "SVM",
                                     "Accuracy" = c(svmc[["overall"]][["Accuracy"]]),
                                     "Precision"=svmp,"Recall" =svmr, "F1 Score"=svmf))

results <- rbind(results, data.frame("Model" = "Gaussian Naive Bayes",
                                     "Accuracy" = c(gnbc[["overall"]][["Accuracy"]]),
                                     "Precision"=gnbp,"Recall" =gnbr, "F1 Score"=gnbf))

results <- rbind(results, data.frame("Model" = "KNN",
                                     "Accuracy" = c(knnc[["overall"]][["Accuracy"]]),
                                     "Precision"=knnp,"Recall" =knnr, "F1 Score"=knnf))
results <- rbind(results, data.frame("Model" = "LDA",
                                     "Accuracy" = c(ldac[["overall"]][["Accuracy"]]),
                                     "Precision"=ldap,"Recall" =ldar, "F1 Score"=ldaf))
results <- rbind(results, data.frame("Model" = "CART",
                                     "Accuracy" = c(cartc[["overall"]][["Accuracy"]]),
                                     "Precision"=cartp,"Recall" =cartr, "F1 Score"=cartf))
results <- rbind(results, data.frame("Model" = "Random Forest",
                                     "Accuracy" = c(rfc[["overall"]][["Accuracy"]]),
                                     "Precision"=rfp,"Recall" =rfr, "F1 Score"=rff))
results <- rbind(results, data.frame("Model" = "DL with Keras",
                                     "Accuracy" = c(kpc[["overall"]][["Accuracy"]]),
                                     "Precision"=kp,"Recall" =kpr, "F1 Score"=kpf))



#SYDNEY SIEGE
training <- rbind(charlie,ferguson,german,gurlitt,ottawa,putin)
testing <- sydneysiege

#binary classification with Keras
x_train <- training %>% select(-status) %>% scale()
y_train <- to_categorical(training$status)

x_test <- testing %>% select(-status) %>% scale()
y_test <- to_categorical(testing$status)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 80, activation = 'relu', input_shape = ncol(x_train)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 40, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model%>%fit(
  x_train, y_train,
  epochs = 60,
  batch_size = 5,
  validation_split = 0.2
)

plot(history)
summary(model)
evalkeras <- model%>%evaluate(x_test,y_test)
pred.keras <- model%>%predict_classes(x_test)
tabkeras <- table(factor(pred.keras, levels = c(0,1)),factor(testing$status, levels = c(0,1)))
kp <- precision(tabkeras)
kpr <- recall(tabkeras)
kpf <- F_meas(tabkeras)
kpc <- confusionMatrix(tabkeras)

#converting to factors
training$status <- factor(training$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))
testing$status <- factor(testing$status, levels = c(0,1), labels = c("Non-Rumor", "Rumor"))

#data plots
plot <- ggplot(data = testing, mapping = aes(x = timeDiff, y=Freq))+
  geom_line(aes(color = status))
plot + scale_colour_manual(values = c("green","red")) + 
  labs(title = "Event: Sydney Siege",  x = "Time Interval", y = "Interactions")


#Linear Discriminant Analysis
fit.lda7 <- caret::train(status~., data = training, method = "lda", metric = "Accuracy")
pred.lda7 <- predict(fit.lda7, testing)
ldatab <- table(pred.lda7, testing$status)
ldac <- confusionMatrix(ldatab)
ldap <- precision(ldatab)
ldar <- recall(ldatab)
ldaf <- F_meas(ldatab)


#Classification and Regression Trees (CART)
fit.cart7 <- caret::train(status~., data = training, method = "rpart", metric = "Accuracy")
pred.cart7 <- predict(fit.cart7, testing)
carttab <- table(pred.cart7, testing$status)
cartc <- confusionMatrix(carttab)
cartp <- precision(carttab)
cartr <- recall(carttab)
cartf <- F_meas(carttab)

#KNN
fit.knn7 <- caret::train(status~., data = training, method = "knn", metric = "Accuracy")
pred.knn7 <- predict(fit.knn7, testing)
knntab <- table(pred.knn7, testing$status)
knnc <- confusionMatrix(knntab)
knnp <- precision(knntab)
knnr <- recall(knntab)
knnf <-F_meas(knntab)

#Random Forest
fit.rf7 <- caret::train(status~., data = training, method = "rf", metric = "Accuracy" )
pred.rf7 <- predict(fit.rf7, testing)
rftab <- table(pred.rf7, testing$status)
rfc <- confusionMatrix(rftab)
rfp <- precision(rftab)
rfr <- recall(rftab)
rff <- F_meas(rftab)

#gaussian Naive Bayes
gnbx <- as.matrix(training[,c(1,3)])
gnby <- testing[,c(1,3)]
fit.gnb7 <- Rfast::gaussian.nb(xnew = NULL, gnbx , ina = training$status)
pred.gnb7 <- Rfast::gaussiannb.pred(gnby, fit.gnb7$mu, fit.gnb7$sigma, fit.gnb7$ni)
gnbtab <- table(pred.gnb7, testing$status)
rownames(gnbtab) <- c("Non-Rumor", "Rumor")
gnbp <- precision(gnbtab)
gnbr <- recall(gnbtab)
gnbf <- F_meas(gnbtab)
gnbc <- confusionMatrix(gnbtab)

#Support Vector Machine
fit.svm <- caret::train(status~., data = training, method = "svmRadial", metric = "Accuracy")
pred.svm <- predict(fit.svm, testing)
svmtab <- table(pred.svm, testing$status)
svmc <-confusionMatrix(svmtab)
svmp <- precision(svmtab)
svmr <- recall(svmtab)
svmf <-F_meas(svmtab)


#results
results <- rbind(results, data.frame("Model" = "SVM",
                                     "Accuracy" = c(svmc[["overall"]][["Accuracy"]]),
                                     "Precision"=svmp,"Recall" =svmr, "F1 Score"=svmf))

results <- rbind(results, data.frame("Model" = "Gaussian Naive Bayes",
                                     "Accuracy" = c(gnbc[["overall"]][["Accuracy"]]),
                                     "Precision"=gnbp,"Recall" =gnbr, "F1 Score"=gnbf))

results <- rbind(results, data.frame("Model" = "KNN",
                                     "Accuracy" = c(knnc[["overall"]][["Accuracy"]]),
                                     "Precision"=knnp,"Recall" =knnr, "F1 Score"=knnf))
results <- rbind(results, data.frame("Model" = "LDA",
                                     "Accuracy" = c(ldac[["overall"]][["Accuracy"]]),
                                     "Precision"=ldap,"Recall" =ldar, "F1 Score"=ldaf))
results <- rbind(results, data.frame("Model" = "CART",
                                     "Accuracy" = c(cartc[["overall"]][["Accuracy"]]),
                                     "Precision"=cartp,"Recall" =cartr, "F1 Score"=cartf))
results <- rbind(results, data.frame("Model" = "Random Forest",
                                     "Accuracy" = c(rfc[["overall"]][["Accuracy"]]),
                                     "Precision"=rfp,"Recall" =rfr, "F1 Score"=rff))
results <- rbind(results, data.frame("Model" = "DL with Keras",
                                     "Accuracy" = c(kpc[["overall"]][["Accuracy"]]),
                                     "Precision"=kp,"Recall" =kpr, "F1 Score"=kpf))
results[,2:5] <- round(results[,2:5], digits = 2)
results[is.na(results)] <- 0
write.csv(results, "C:/Users/nikita parab/Desktop/SEMESTER 3/ICT Solution/finalresults.csv", row.names = FALSE)


