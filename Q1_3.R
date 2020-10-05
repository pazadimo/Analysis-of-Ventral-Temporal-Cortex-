library(ggplot2)
#defining variables for means and vars
area1s = matrix(list(), nrow = 9, ncol = 1)
area2s = matrix(list(), nrow = 9, ncol = 1)
area3s = matrix(list(), nrow = 9, ncol = 1)
area1_normal = matrix(list(), nrow = 9, ncol =1)
area2_normal = matrix(list(), nrow = 9, ncol =1)
area3_normal = matrix(list(), nrow = 9, ncol =1)
means_area1 = list()
means_area2 = list()
means_area3 = list()
Var_area1 = list()
Var_area2 = list()
Var_area3 = list()
means_area1_normal = list()
means_area2_normal = list()
means_area3_normal = list()
Var_area1_normal = list()
Var_area2_normal = list()
Var_area3_normal = list()

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
#We are going to normalize our means and vars
for (Object in 1:9) {
  area1_normal[[Object,1]] = (as.numeric(area1s[[Object,1]]) - as.numeric(means_area1[6]))/as.numeric(Var_area1[6])#WE assuemed that Rest is in 6th block
  means_area1_normal[Object] = mean(as.numeric(area1_normal[[Object,1]]))
  Var_area1_normal[Object] = sd(as.numeric(area1_normal[[Object,1]]))
  area2_normal[[Object,1]] = (as.numeric(area2s[[Object,1]]) - as.numeric(means_area2[6]))/as.numeric(Var_area2[6])#WE assuemed that Rest is in 6th block
  means_area2_normal[Object] = mean(as.numeric(area2_normal[[Object,1]]))
  Var_area2_normal[Object] = sd(as.numeric(area2_normal[[Object,1]]))
  area3_normal[[Object,1]] = (as.numeric(area3s[[Object,1]]) - as.numeric(means_area3[6]))/as.numeric(Var_area3[6])#WE assuemed that Rest is in 6th block
  means_area3_normal[Object] = mean(as.numeric(area3_normal[[Object,1]]))
  Var_area3_normal[Object] = sd(as.numeric(area3_normal[[Object,1]]))
}

#making data frame for this new normalized data
df1 = data.frame(Lab = Lab, means = as.numeric(means_area1_normal), sds = as.numeric(Var_area1_normal))
df2 = data.frame(Lab = Lab, means = as.numeric(means_area2_normal), sds = as.numeric(Var_area2_normal))
df3 = data.frame(Lab = Lab, means = as.numeric(means_area3_normal), sds = as.numeric(Var_area3_normal))


ggplot(df1, aes(x = Lab, y = means)) + geom_bar(stat = "identity") + geom_text(aes(label=format(round(means,3),nsmall = 3)), vjust=-0.3, color="red")+
  theme_minimal() + geom_errorbar(aes(ymin = as.numeric(means_area1_normal) - 1.96 * as.numeric(Var_area1_normal), ymax = as.numeric(means_area1_normal) + 1.96 * as.numeric(Var_area1_normal))) + geom_text(aes(label=format(round(sds,3),nsmall = 3)), vjust=1.6, color="red")


ggplot(df2, aes(x = Lab, y = means)) + geom_bar(stat = "identity") + geom_text(aes(label=format(round(means,3),nsmall = 3)), vjust=-0.3, color="red")+
  theme_minimal() + geom_errorbar(aes(ymin = as.numeric(means_area2_normal) - 1.96 * as.numeric(Var_area2_normal), ymax = as.numeric(means_area2_normal) + 1.96 * as.numeric(Var_area2_normal))) + geom_text(aes(label=format(round(sds,3),nsmall = 3)), vjust=1.6, color="red")


ggplot(df3, aes(x = Lab, y = means)) + geom_bar(stat = "identity") + geom_text(aes(label=format(round(means,3),nsmall = 3)), vjust=-0.3, color="red")+
  theme_minimal() + geom_errorbar(aes(ymin = as.numeric(means_area3_normal) - 1.96 * as.numeric(Var_area3_normal), ymax = as.numeric(means_area3_normal) + 1.96 * as.numeric(Var_area3_normal))) + geom_text(aes(label=format(round(sds,3),nsmall = 3)), vjust=1.6, color="red")
