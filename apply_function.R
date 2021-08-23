mat = matrix(c(1,2,3,4,5,6),3,2)

#Tinh tong theo tung dong
apply(mat,1,sum)

# Tinh so phan tu NA trong tung cot
apply(mat,2,count_na)

list1 = list(c(1,2,3),3,5,c(2,5,6))
# Tinh do dai cua tung phan tu trong list1
lapply(list1,length)
sapply(list1,length)

# Tinh gia tri trung binh cua tung cot trong data frame
lapply(who,mean)
sapply(who,mean)

# Tinh trung binh Population phan theo Region
tapply(who$Population,who$Region,mean)