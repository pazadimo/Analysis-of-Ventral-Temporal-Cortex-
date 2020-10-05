library(ggplot2)
#defining variables for means and vars
n_row1=9
n_col1=1
n_row2=6
n_col2=3
Betha=1.96
a = 0.05
area1s = matrix(list(), nrow = 9, ncol = 1)
diff_matrix = matrix(list(), nrow = 9, ncol = 9)
means_area1 = list()
data_set = matrix(list(), nrow = 6, ncol = 3)
p_value_1 = matrix(list(), nrow = 9, ncol = 9)
p_value_4 = matrix(list(), nrow = 9, ncol = 9)

#importing data set
for (person in 1:6){
  for (area in 1:3) {
    data_set[[person,area]] = read.csv(paste("C:/Users/novin/Desktop/Q1/Q1data/fMRIPerson",person,"Num",area,".csv",sep = ""),header = FALSE)
  }
}
#face area1
qqnorm(as.numeric(data_set[[3,1]][4,]))
qqline(as.numeric(data_set[[3,1]][4,]))
ks.test(x = as.numeric(data_set[[3,1]][4,]),y = "pnorm",mean(as.numeric(data_set[[3,1]][4,])), sd(as.numeric(data_set[[3,1]][4,])), alternative = "two.sided")

#house area1
qqnorm(as.numeric(data_set[[3,1]][5,]))
qqline(as.numeric(data_set[[3,1]][5,]))
ks.test(x = as.numeric(data_set[[3,1]][5,]),y = "pnorm",mean(as.numeric(data_set[[3,1]][5,])), sd(as.numeric(data_set[[3,1]][5,])), alternative = "two.sided")


#face area2
qqnorm(as.numeric(data_set[[3,2]][4,]))
qqline(as.numeric(data_set[[3,2]][4,]))
ks.test(x = as.numeric(data_set[[3,2]][4,]),y = "pnorm",mean(as.numeric(data_set[[3,2]][4,])), sd(as.numeric(data_set[[3,2]][4,])), alternative = "two.sided")

#house area2
qqnorm(as.numeric(data_set[[3,2]][5,]))
qqline(as.numeric(data_set[[3,2]][5,]))
ks.test(x = as.numeric(data_set[[3,2]][5,]),y = "pnorm",mean(as.numeric(data_set[[3,2]][5,])), sd(as.numeric(data_set[[3,2]][5,])), alternative = "two.sided")


person_1_area1_area2 = as.numeric(data_set[[1,1]][1,])
person_1_area1_rest = as.numeric(data_set[[1,1]][9,])

print(t.test(person_1_area1_area2,person_1_area1_rest, alternative = "two.sided", paired = TRUE))
print(wilcox.test(person_1_area1_area2,person_1_area1_rest, alternative = "two.sided", paired = TRUE))
print(ks.test(person_1_area1_area2,person_1_area1_rest, alternative = "two.sided"))

