library(ggplot2)
library(reshape2)
library(tidyverse)
library(caret)
library(knitr)
library(ggpubr)

# import data
lung <- read.csv("survey lung cancer.csv", header = T)
# check NA
n <-rep(NA, length(lung)) 
for (i in 1:length(lung)){
  n[i] <- sum(is.na(lung[, i]))
}; sum(n)

# investigate variables
lapply(lung, function(x) levels(factor(x)))
# analyze age
lung %>% ggplot(aes(x = LUNG_CANCER, y = AGE)) + geom_boxplot()  + 
  ggtitle("Graph 1")
t.test(lung$AGE[lung$LUNG_CANCER == "YES"], lung$AGE[lung$LUNG_CANCER == "NO"], 
       alternative = "two.sided") # not associated 
# analyze gender
chisq.test(table(lung$GENDER,lung$LUNG_CANCER)) # not associated
# association between gender and age
a <- lung %>% ggplot(aes(x = GENDER, y = AGE)) + geom_boxplot() +
  facet_grid(cols = vars(LUNG_CANCER))
a + stat_compare_means(method = "t.test") + ggtitle("Graph 2")
# analyze other variables
corr <- cor(lung %>% select(-c(AGE, GENDER, LUNG_CANCER)))
corr %>% melt() %>% ggplot(aes(x = Var1, y = Var2, fill = value, label = value)) +
  geom_tile() + labs(fill = "correlation") + ggtitle("Correlation Matrix") +
  xlab("") + ylab("") + ggtitle("Graph 3")
kable(corr %>% melt() %>% filter(value > abs(0.5) & value != 1), 
      caption = "correlated variables")

# association between LUN_CANCER and other variables
lung %>% select(-c(GENDER, AGE)) %>% pivot_longer(!LUNG_CANCER) %>% 
  ggplot(aes(factor(value), fill = LUNG_CANCER)) +
  geom_bar(position = "fill") + facet_wrap(~name) + 
  xlab("value of each factor") + ggtitle("Graph 4")

factor.p <- c(chisq.test(lung$LUNG_CANCER, lung$ALCOHOL.CONSUMING)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$ALLERGY)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$ANXIETY)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$CHEST.PAIN)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$CHRONIC.DISEASE)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$COUGHING)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$FATIGUE)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$PEER_PRESSURE)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$SWALLOWING.DIFFICULTY)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$WHEEZING)$p.value,
              chisq.test(lung$LUNG_CANCER, lung$YELLOW_FINGERS)$p.value)

tab <- matrix(factor.p)
tab <- cbind(tab, ifelse(tab[, 1]<0.05, "correlated", "not-correlated"))
colnames(tab) <- c("p.value", "correlation")
rownames(tab) <- c("alcohol", "allergy", "anxiety", "chest.pain",
                   "chronic.disease", "coughing", "fatigue", "peer.pressure",
                   "swallowing.difficulty", "wheezing", "yellow.fingers")
kable(tab, caption = "Association Between Lung Cancer and Factor")
tab_df <- as.data.frame(tab) %>% 
  mutate(p.value = round(as.numeric(p.value), 8)) %>% 
  arrange(p.value)

lung$LUNG_CANCER_b <- ifelse(lung$LUNG_CANCER == "YES", 1, 0)
# add in all potential significantly associated factor
fit.all <- summary(glm(LUNG_CANCER_b ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                         WHEEZING + COUGHING + CHEST.PAIN + PEER_PRESSURE + YELLOW_FINGERS + 
                         FATIGUE + ANXIETY, data = lung))
kable(fit.all$coefficients, caption = "P-value for Different Factors")
# delete wheezing
fit.no.wheezing <- summary(glm(LUNG_CANCER_b ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                                 COUGHING + CHEST.PAIN + PEER_PRESSURE + YELLOW_FINGERS + 
                                 FATIGUE + ANXIETY, data = lung))
# delete chest.pain
fit.no.pain <- summary(glm(LUNG_CANCER_b ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                             WHEEZING + COUGHING + PEER_PRESSURE + YELLOW_FINGERS + 
                             FATIGUE + ANXIETY, data = lung))
# delete both
fit.no.both <- summary(glm(LUNG_CANCER_b ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                             COUGHING  + PEER_PRESSURE + YELLOW_FINGERS + FATIGUE + ANXIETY, 
                           data = lung))
# compare aic
fit.aic <- matrix(c(fit.all$aic, fit.no.wheezing$aic, fit.no.pain$aic, fit.no.both$aic))
colnames(fit.aic) <- "AIC"
rownames(fit.aic) <- c("All Factors", "No Wheezing", "No Chest.Pain", "No Both")
kable(fit.aic, caption = "Comparison of AIC")

# decide the model
final.logit.model <- glm(LUNG_CANCER_b ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                           WHEEZING + COUGHING + PEER_PRESSURE + YELLOW_FINGERS + 
                           FATIGUE + ANXIETY, data = lung)

# prediction and accuracy
predict.log <- ifelse(predict(final.logit.model) >0.5, 1, 0)
confusionMatrix(as.factor(predict.log), as.factor(lung$LUNG_CANCER_b))

set.seed(10)
control <- trainControl(number = 1, p = 0.6)
cart.mod <- train(factor(LUNG_CANCER_b) ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                    WHEEZING + COUGHING + PEER_PRESSURE + YELLOW_FINGERS + 
                    FATIGUE + ANXIETY, data = lung, method = "rpart", trControl = control)
lda.mod <- train(factor(LUNG_CANCER_b) ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                   WHEEZING + COUGHING + PEER_PRESSURE + YELLOW_FINGERS + 
                   FATIGUE + ANXIETY, data = lung, method = "lda", trControl = control)

knn.mod <- train(factor(LUNG_CANCER_b) ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                   WHEEZING + COUGHING + PEER_PRESSURE + YELLOW_FINGERS + 
                   FATIGUE + ANXIETY, data = lung, method = "knn", trControl = control)

rf.mod <- train(factor(LUNG_CANCER_b) ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                  WHEEZING + COUGHING + PEER_PRESSURE + YELLOW_FINGERS + 
                  FATIGUE + ANXIETY, data = lung, method = "rf", trControl = control)

svm.mod <- train(factor(LUNG_CANCER_b) ~ ALLERGY + ALCOHOL.CONSUMING + SWALLOWING.DIFFICULTY +
                   WHEEZING + COUGHING + PEER_PRESSURE + YELLOW_FINGERS + 
                   FATIGUE + ANXIETY, data = lung, method = "svmRadial", trControl = control)
accu.tab <- rbind(cart.mod$resample, lda.mod$resample, knn.mod$resample,
                  rf.mod$resample, svm.mod$resample)[, 1:2]
.rowNamesDF(accu.tab) <- c("cart", "lda", "knn", "rf", "svm")
kable(accu.tab)

# prediction and accuracy
predict.svm <- predict(svm.mod)
confusionMatrix(as.factor(predict.svm), as.factor(lung$LUNG_CANCER_b))
              