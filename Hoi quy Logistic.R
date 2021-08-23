# Doc bo so lieu Framingham
framingham = read.csv("framingham.csv")

# Xem cau truc cua bo so lieu
str(framingham)

# Bo so lieu Framingham nghien cuu cac nhan to anh huong den kha nang mac benh tim do mach vanh
# tu do dua ra chuan doan va co bien phap dieu tri phu hop
# Cac yeu to duoc nghien cuu: thong tin ca nhan cua nguoi benh (tuoi, gioi tinh, hoc van ...)
# lich su kham benh (co tien su benh tim khong, co beo phi khong, huyet ap tai lan kham truoc)
# Cac thong so kham benh (luong cholesterol, huyet ap, chi so BMI, chi so glucose, nhip tim,...)

# Load the library caTools
library(caTools)

# Chia bo so lieu thanh mau xay dung va mau kiem dinh
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# Su dung lenh subset de chia thanh mau train va test
mauXayDung = subset(framingham, split==TRUE)
mauKiemDinh = subset(framingham, split==FALSE)

# Xay dung mo hinh Logistic su dung tat ca cac bien so
mohinh = glm(TenYearCHD ~ ., data = mauXayDung, family=binomial)
summary(mohinh)

# Du bao tren bo mau xay dung
duBaoXayDung = predict(mohinh, type="response", newdata=mauXayDung)
summary(duBaoXayDung)


# Thuc hien du bao tren bo mau kiem dinh
duBaoKiemDinh = predict(mohinh, type="response", newdata=mauKiemDinh)
# Xay dung ma tran nham lan (Confusion matrix) voi muc nguong = 0.5
table(mauKiemDinh$TenYearCHD, duBaoKiemDinh > 0.5)

# Do chinh xac cua mo hinh
(1069+11)/nrow(mauKiemDinh)

# Do chinh xac cua mo hinh co so
(1069+6)/nrow(mauKiemDinh) 


########################################
library(ROCR)

# phuong trinh du bao
ROCRpred = prediction(duBaoKiemDinh, mauKiemDinh$TenYearCHD)
# Ham thuc hien
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Ve duong ROC
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Chi so AUC cho mau kiem dinh
as.numeric(performance(ROCRpred, "auc")@y.values)
