## We have dataset on Credit Card Transactions.Here our main purpose is to build a Machine Learning model which can detect fraudulent credit transactions. Decription is given at the beginning of our analysis.Here the response variable is of dichotomous type i.e. it can take values 0 and 1.We have performed logistic regression.At the beginning dataset is divided into two parts,one is training and the other is testing.Train the classification algorithm using training dataset and predict the result using test dataset.For the evaluation of model's performance ROC curve method is used which says that AUC(The area under the curve)is 0.9687 ~ 1,that means the model performed here fits the data well. 
## Importing Dataset
Creditcard=read.csv("C:/Users/SAGNIK SAMANTA/OneDrive/Desktop/Datasets/CreditCard.csv",sep=",",header = TRUE)

## Print First 6 rows of the Dataset
head(Creditcard)

## Check the Dimension of the Dataset
dim(Creditcard)

## Therefore the Dataset(CreditCard) has 284807 Rows and 31 Columns.

## 
str(Creditcard)


Creditcard$Class=as.factor(Creditcard$Class)

## Summary of dataset in package
summary(Creditcard)

## Percentage Calculation of Fraudulent Transaction or Otherwise
attach(Creditcard)
my_table=Class

## Percentage of Fraudulent Transaction
(sum(my_table==1)/(sum(my_table==1)+sum(my_table==0)))*100
## Percentage of Otherwise
(sum(my_table==0)/(sum(my_table==1)+sum(my_table==0)))*100
## Class == 1 represents fradulent transactions
## Class == 0 represents Otherwise
## As shown in the above result, out of 284807 transactions 492 transactions are fraudulent credit card
## transactions.
## 99.82725% are Non-fraudulent and 0.1727486% are fraudulent transactions.

## Data Visualizations Using Barplot
install.packages("ggplot2")
library(ggplot2)
ggplot(Creditcard, aes(y=Amount, x=Class,fill=Class)) + 
  geom_bar(position="dodge", stat="identity",width=0.7)+
  ggtitle("Fraudulent Transaction vs Otherwise") +
  xlab("Class") +
  ylab("Amount")

## For Logistic regression
install.packages("caTools") 
library(caTools)

## Splitting dataset
split=sample.split(Creditcard, SplitRatio = 0.8)

train_reg=subset(Creditcard, split == "TRUE")
test_reg=subset(Creditcard, split == "FALSE")

## Training model
logistic_model = glm(Class ~ Time+Amount+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28,data = train_reg,family = "binomial"(link = logit))

## Summary
summary(logistic_model)

## Predict test data based on model
predict_reg=predict(logistic_model,test_reg, type = "response")

Transaction=c()
for(i in 1:length(predict_reg)){
  if(predict_reg[i] > 0.9){
    Transaction[i] = "Fraudulent Transaction"
  } else {
    Transaction[i] = "Non-Fraudulent Transaction"
  }
}

Final_data=cbind(Class=test_reg$Class,Predicted=Transaction)
Final_data=as.data.frame(Final_data)
View(Final_data)

## Checking The Model Performance
## ROC-curve using pROC library
install.packages("pROC")
library(pROC)
roc_score = roc(test_reg$Class, predict_reg)   ## AUC score
roc_score
plot(roc_score, main="ROC curve -- Logistic Regression ")
