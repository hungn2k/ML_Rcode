# Doc so lieu vao R
titanic = read.csv("titanic.csv")
str(titanic)
summary(titanic)
titanic <- subset(titanic, select = -c(cabin, boat,body,home.dest) )
titanic = titanic[complete.cases(titanic),]

# Chia mau thanh mau xay dung va mau kiem dinh su dung library caTools
# Neu chua cai dat library, can uncomment dong duoi
# install.packages("caTools")
library(caTools)
set.seed(2000)
split = sample.split(titanic$survived, SplitRatio = 0.75)
mau_xay_dung = subset(titanic, split==TRUE)
mau_kiem_dinh = subset(titanic, split==FALSE)

mau_xay_dung$survived = as.factor(mau_xay_dung$survived)
mau_kiem_dinh$survived = as.factor(mau_kiem_dinh$survived)


# Cai dat rpart library, dung cho mo hinh cay
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)


######
# Mo hinh 1: Mo hinh cay voi minbucket = 25 (Toi thieu 25 quan sat trong nhanh la)
Tree_1 = rpart(survived ~ pclass+sex+age+sibsp+parch+fare+embarked, 
               method="class", data = mau_xay_dung, control=rpart.control(minbucket=25))
prp(Tree_1)
rpart.plot(Tree_1)

# Du bao va tinh toan sai so tren mau kiem dinh
Predict_test_tree_1 = predict(Tree_1, newdata = mau_kiem_dinh, type = "class")
table(mau_kiem_dinh$survived, Predict_test_tree_1)
matrix_1 = table(mau_kiem_dinh$survived, Predict_test_tree_1)

# Do chinh xac tren mau kiem dinh
(matrix_1[1,1]+matrix_1[2,2])/(nrow(mau_kiem_dinh))

# Do chinh xac mo hinh baseline
0.59

######
# Mo hinh 2: Su dung cross-validation de lua chon tham so cp cua mo hinh

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Xac dinh cac tham so cua pp cross-validation
# 10-fold cross-validation, voi gia tri tham so cp trong khoang 0.001-0.1
fitControl = trainControl( method = "cv", number = 10)
cartGrid = expand.grid( .cp=c(1:1000)*0.001) 

train(survived ~ pclass+sex+age+sibsp+parch+fare+embarked, 
      data = mau_xay_dung,
      na.action  = na.pass, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)

# Tao mo hinh cay voi gia tri cp tao ra sai so nho nhat theo cross-validation
Tree_2 = rpart(survived ~ pclass+sex+age+sibsp+parch+fare+embarked, 
               data = mau_xay_dung,method="class", control=rpart.control(cp = 0.013))
prp(Tree_2)
rpart.plot(Tree_2)

# Du bao va tinh toan sai so tren mau kiem dinh - mo hinh Tree_2
Predict_test_tree_2 = predict(Tree_2, newdata = mau_kiem_dinh, type = "class")
table(mau_kiem_dinh$survived, Predict_test_tree_2)
matrix_2 = table(mau_kiem_dinh$survived, Predict_test_tree_2)

# Do chinh xac tren mau kiem dinh
(matrix_2[1,1]+matrix_2[2,2])/(nrow(mau_kiem_dinh))



###########
# Mo hinh 3: Xay dung cay quyet dinh phuc tap nhat, sau do pruning lai,
# su dung gia tri cp da tinh toan trong buoc cross-validation

Tree_3 <- rpart(survived ~ pclass+sex+age+sibsp+parch+fare+embarked,
             method="class", data=mau_xay_dung)
prp(Tree_3)
rpart.plot(Tree_3)

Tree_3_prune = prune(Tree_3, cp=0.013)
rpart.plot(Tree_3_prune)

Predict_3 = predict(Tree_3_prune, newdata = mau_kiem_dinh, type = "class")
table(mau_kiem_dinh$survived, Predict_3)
matrix_3 = table(mau_kiem_dinh$survived, Predict_3)

# Do chinh xac tren mau kiem dinh
(matrix_3[1,1]+matrix_3[2,2])/(nrow(mau_kiem_dinh))


##########
# Mo hinh 4: Random forest
install.packages("randomForest")
library(randomForest)
forest = randomForest(survived ~ pclass+sex+age+sibsp+parch+fare+embarked,
                      data = mau_xay_dung)
# Can bo cac quan sat co missing value de chay mo hinh randomForest


# Make predictions
PredictForest = predict(forest, newdata = mau_kiem_dinh)
table(mau_kiem_dinh$survived, PredictForest)
matrix_4 = table(mau_kiem_dinh$survived, PredictForest)
# Do chinh xac tren mau kiem dinh
(matrix_4[1,1]+matrix_4[2,2])/(nrow(mau_kiem_dinh))

