data_set = matrix(list(), nrow = 6, ncol = 3)

#importing data set
for (i in 1:6){
  for (j in 1:3) {
    data_set[[i,j]] = read.csv(paste("C:/Users/novin/Desktop/Q1/Q1data/fMRIPerson",i,"Num",j,".csv",sep = ""),header = FALSE)
  }
}

#T-Test in different areas of Bottle and Scissors
t.test(as.numeric(data_set[[5,1]][7,]), as.numeric(data_set[[5,1]][1,]), paired = TRUE)
t.test(as.numeric(data_set[[5,2]][7,]), as.numeric(data_set[[5,2]][1,]), paired = TRUE)
t.test(as.numeric(data_set[[5,3]][7,]), as.numeric(data_set[[5,3]][1,]), paired = TRUE)


#wilcox_test in different areas of Bottle and Scissors
wilcox.test(as.numeric(data_set[[5,1]][7,]), as.numeric(data_set[[5,1]][1,]), paired = TRUE, alternative = "two.sided")
wilcox.test(as.numeric(data_set[[5,2]][7,]), as.numeric(data_set[[5,2]][1,]), paired = TRUE, alternative = "two.sided")
wilcox.test(as.numeric(data_set[[5,3]][7,]), as.numeric(data_set[[5,3]][1,]), paired = TRUE, alternative = "two.sided")






#T-Test in all areas for 3rd person for rest and face
t.test(as.numeric(data_set[[3,1]][3,]), as.numeric(data_set[[3,1]][6,]), paired = TRUE)
t.test(as.numeric(data_set[[3,2]][3,]), as.numeric(data_set[[3,2]][6,]), paired = TRUE)
t.test(as.numeric(data_set[[3,3]][3,]), as.numeric(data_set[[3,3]][6,]), paired = TRUE)

#wilcox_test in all areas for 3rd person for rest and face
wilcox.test(as.numeric(data_set[[3,1]][3,]), as.numeric(data_set[[3,1]][6,]), paired = TRUE, alternative = "two.sided")
wilcox.test(as.numeric(data_set[[3,2]][3,]), as.numeric(data_set[[3,2]][6,]), paired = TRUE, alternative = "two.sided")
wilcox.test(as.numeric(data_set[[3,3]][3,]), as.numeric(data_set[[3,3]][6,]), paired = TRUE, alternative = "two.sided")







#paerson and pearmen test for person1 for House and face
cor.test(as.numeric(data_set[[1,1]][5,]), as.numeric(data_set[[1,1]][4,]), method = "pearson")
cor.test(as.numeric(data_set[[1,1]][5,]), as.numeric(data_set[[1,1]][4,]), method = "spearman")


#paerson and pearmen test for person2 for House and face
cor.test(as.numeric(data_set[[1,2]][5,]), as.numeric(data_set[[1,2]][4,]), method = "pearson")
cor.test(as.numeric(data_set[[1,2]][5,]), as.numeric(data_set[[1,2]][4,]), method = "spearman")

#paerson and pearmen test for person3 for House and face
cor.test(as.numeric(data_set[[1,3]][5,]), as.numeric(data_set[[1,3]][4,]), method = "pearson")
cor.test(as.numeric(data_set[[1,3]][5,]), as.numeric(data_set[[1,3]][4,]), method = "spearman")





