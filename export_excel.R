# Export to txt file
write.table(who, "D:/My Youtube course/R software/mydata.txt", sep="\t", row.names=FALSE)


# Write CSV in R
write.csv(who, file = "D:/My Youtube course/R software/MyData.csv",row.names=FALSE, na="NA")

write.table(who, file = "D:/My Youtube course/R software/MyData2.csv",row.names=FALSE, 
            na="",col.names=TRUE, sep=",")

#Export to excel spreadsheet
library('xlsx')
write.xlsx(who, "D:/My Youtube course/R software/mydata.xlsx", showNA = FALSE)