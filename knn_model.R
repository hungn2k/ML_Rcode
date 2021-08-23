# Doc so lieu vao R
mnist_data = read.csv("mnist_data.csv")
mnist_data = mnist_data[1:5000,]

mnist_data$label = as.factor(mnist_data$label)

# Chia mau thanh mau xay dung va mau kiem dinh su dung library caTools
# Neu chua cai dat library, can uncomment dong duoi
# install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(mnist_data$label, SplitRatio = 0.75)
mau_xay_dung = subset(mnist_data, split==TRUE)
mau_kiem_dinh = subset(mnist_data, split==FALSE)


# Ve ra mot so quan sat trong mau xay dung
i = 5
x = unlist(mau_xay_dung[i,2:785])
x_mat = matrix(x,nrow=28, byrow=TRUE)
image(x_mat, col = grey.colors(255))
image(t(x_mat)[,28:1], col = grey.colors(255))
print(mau_xay_dung[i,1])


# Mo hinh k-NN voi k=1
library(class)
nn1 <- knn(train=mau_xay_dung[,2:785],
           test=mau_kiem_dinh[,2:785],
           cl=mau_xay_dung[,1],
           k=1)

## Ma tran confusion matrix
table(mau_kiem_dinh[,1],nn1)
# Do chinh xac cua mo hinh tren mau kiem dinh
sum(mau_kiem_dinh[,1]==nn1)/nrow(mau_kiem_dinh)



# Mo hinh k-NN voi k=3
nn3 <- knn(train=mau_xay_dung[,2:785],
           test=mau_kiem_dinh[,2:785],
           cl=mau_xay_dung[,1],
           k=3)

## Ma tran confusion matrix
table(mau_kiem_dinh[,1],nn3)
# Do chinh xac cua mo hinh tren mau kiem dinh
sum(mau_kiem_dinh[,1]==nn3)/nrow(mau_kiem_dinh)




# Thuc hien cross-validation de lua chon tham so k toi uu

# Install cross-validation packages
install.packages("caret")
library(caret)
#install.packages("e1071")
#library(e1071)

# Xac dinh cac tham so cua pp cross-validation
# 5-fold cross-validation, voi gia tri tham so k trong khoang 1-10
fitControl = trainControl( method = "cv", number = 5)
cartGrid = expand.grid( .k=c(1:5)) 

knn_cv = train(mau_xay_dung[,2:785], 
      mau_xay_dung[,1],
      method = "knn", trControl = fitControl, tuneGrid = cartGrid)

knn_cv_predict = predict(knn_cv, mau_kiem_dinh[,2:785])

## Ma tran confusion matrix
table(mau_kiem_dinh[,1],knn_cv_predict)
# Do chinh xac cua mo hinh tren mau kiem dinh
sum(mau_kiem_dinh[,1]==knn_cv_predict)/nrow(mau_kiem_dinh)



####################
# Xem xet sai so cua mo hinh knn - k=1
## Ma tran confusion matrix
table(mau_kiem_dinh[,1],nn1)
# Do chinh xac cua mo hinh tren mau kiem dinh
sum(mau_kiem_dinh[,1]==nn1)/nrow(mau_kiem_dinh)

df = data.frame(real=mau_kiem_dinh[,1],forecast=nn1)
df$error = ifelse(df$real==df$forecast,0,1)

# Ve ra mot so quan sat trong mau kiem dinh
i = 36
x = unlist(mau_kiem_dinh[i,2:785])
x_mat = matrix(x,nrow=28, byrow=TRUE)
image(x_mat, col = grey.colors(255))
image(t(x_mat)[,28:1], col = grey.colors(255))
print(mau_kiem_dinh[i,1])
