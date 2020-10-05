data_set = matrix(list(), nrow = 6, ncol = 3)
#importing data set
r_sam = list()
for (person in 1:6){
  for (area in 1:3) {
    data_set[[person,area]] = read.csv(paste("C:/Users/novin/Desktop/Q1/Q1data/fMRIPerson",person,"Num",area,".csv",sep = ""),header = FALSE)
  }
}
for (sample_number in 1:1000) {
  #samlping and calculating corrolation
  r_sam[sample_number] = cov(sample(as.numeric(data_set[[1,2]][4,]),21), sample(as.numeric(data_set[[1,2]][5,]), 21)) / (sd(sample(as.numeric(data_set[[1,2]][5,]), 21)) *
                                                                                                                           sd(sample(as.numeric(data_set[[1,2]][4,]),21)))
}
#finding the distribution
cdf = ecdf(as.numeric(r_sam))
#the p/-value is:
p_value = cdf(cov(as.numeric(data_set[[1,2]][4,]), as.numeric(data_set[[1,2]][5,])) / (sd(as.numeric(data_set[[1,2]][4,])) * sd(as.numeric(data_set[[1,2]][5,]))))