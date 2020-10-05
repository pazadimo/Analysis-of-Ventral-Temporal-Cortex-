library(ggplot2)
#defining variables for means and vars
n_row1=9
n_col1=1
n_row2=6
n_col2=3
Betha=1.96
area1s = matrix(list(), nrow = 9, ncol = 1)
area2s = matrix(list(), nrow = 9, ncol = 1)
area3s = matrix(list(), nrow = 9, ncol = 1)
means_area1 = list()
means_area2 = list()
means_area3 = list()
Var_area1 = list()
Var_area2 = list()
Var_area3 = list()
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
  
  #the resulted means and vars are :
  means_area1[Object] = mean(as.numeric(area1s[[Object,1]]))
  Var_area1[Object] = sd(as.numeric(area1s[[Object,1]]))
  means_area2[Object] = mean(as.numeric(area2s[[Object,1]]))
  Var_area2[Object] = sd(as.numeric(area2s[[Object,1]]))
  means_area3[Object] = mean(as.numeric(area3s[[Object,1]]))
  Var_area3[Object] = sd(as.numeric(area3s[[Object,1]]))
}
df1 = data.frame(Lab = Lab, means = as.numeric(means_area1), sds = as.numeric(Var_area1))
df2 = data.frame(Lab = Lab, means = as.numeric(means_area2), sds = as.numeric(Var_area2))
df3 = data.frame(Lab = Lab, means = as.numeric(means_area3), sds = as.numeric(Var_area3))

ggplot(df1, aes(x = Lab, y = means)) + geom_bar(stat = "identity")+
  theme_minimal() + geom_errorbar(aes(ymin = as.numeric(means_area1) - 1.96 * as.numeric(Var_area1), ymax = as.numeric(means_area1) + 1.96 * as.numeric(Var_area1)))

ggplot(df2, aes(x = Lab, y = means)) + geom_bar(stat = "identity")+
  theme_minimal() + geom_errorbar(aes(ymin = as.numeric(means_area2) - 1.96 * as.numeric(Var_area2), ymax = as.numeric(means_area2) + 1.96 * as.numeric(Var_area2)))

ggplot(df3, aes(x = Lab, y = means)) + geom_bar(stat = "identity")+
  theme_minimal() + geom_errorbar(aes(ymin = as.numeric(means_area3) - 1.96 * as.numeric(Var_area3), ymax = as.numeric(means_area3) + 1.96 * as.numeric(Var_area3)))

