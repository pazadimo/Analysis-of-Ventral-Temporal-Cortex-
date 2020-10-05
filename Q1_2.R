library(ggplot2)
#defining variables for means and vars
area1s = matrix(list(), nrow = 9, ncol = 1)
area2s = matrix(list(), nrow = 9, ncol = 1)
area3s = matrix(list(), nrow = 9, ncol = 1)
data_set = matrix(list(), nrow = 6, ncol = 3)
Lab = c("Bottle", "Cat", "Chair", "Face" , "House","Rest", "Scissors", "Scramble", "Shoe")

#importing data set
for (i in 1:6){
  for (j in 1:3) {
    data_set[[i,j]] = read.csv(paste("C:/Users/novin/Desktop/Q1/Q1data/fMRIPerson",i,"Num",j,".csv",sep = ""),header = FALSE)
  }
}
#trying to make mean and var
for (Object in 1:9) {
  for (Case in 1:6) {
    area1s[[Object,1]] = c(area1s[[Object,1]], data_set[[Case,1]][Object,])
    area2s[[Object,1]] = c(area2s[[Object,1]], data_set[[Case,2]][Object,])
    area3s[[Object,1]] = c(area3s[[Object,1]], data_set[[Case,3]][Object,])
  }
}
for (i in 1:9) {
  print(Lab[i])
  print("in Area1 for Rest and ")
  print(Lab[i])
  print(t.test(as.numeric(area1s[[i,1]]), as.numeric(area1s[[6,1]]), paired = TRUE))
  print("in Area2 for Rest and ")
  print(Lab[i])
  print(t.test(as.numeric(area2s[[i,1]]), as.numeric(area2s[[6,1]]), paired = TRUE))
  print("in Area3 for Rest and ")
  print(Lab[i])
  print(t.test(as.numeric(area3s[[i,1]]), as.numeric(area3s[[9,1]]), paired = TRUE))
}
