x = 3

count = 0
# Cau truc if-elseif-else
if (x > 3){ x = x - 3} else if (x == 3){x =0}else {x = 3 - x}
if(y[i] >0){count = count+1}
}

# Cach khac de count so phan tu lon hon 0
sum(y>0)

# Count so phan tu NA cua FertilityRate
sum(is.na(who$FertilityRate))

# Count so phan tu khong phai NA
sum(!is.na(who$FertilityRate))

# Subset vector
who$FertilityRate[who$FertilityRate>6]
who$FertilityRate[who$FertilityRate>6 & !is.na(who$FertilityRate)]
who$LifeExpectancy[who$FertilityRate>6 & !is.na(who$FertilityRate)]

y = c(2,3,5,7,4,0,-1)

# Cau truc for
count = 0
for (i in 1:length(y)){
  if(y[i] >0){count = count+1}
}

# Tao ham
count_na = function(vect){
  sum(is.na(vect))
}

count_na(c(1,2,NA,NA,3,5))
count_na(who$Under15)