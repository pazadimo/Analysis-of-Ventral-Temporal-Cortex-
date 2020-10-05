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
#trying to make mean and var
for (Object in 1:9) {
  for (Case in 1:6) {
    area1s[[Object,1]] = c(area1s[[Object,1]], data_set[[Case,1]][Object,])
  }
  #the resulted means and vars are :
  means_area1[Object] = mean(as.numeric(area1s[[Object,1]]))
}


for (Object1 in 1:9) {
  for(Object2 in 1:9){
    diff_matrix[[Object1,Object2]] = as.numeric(mean(as.numeric(area1s[[Object1,1]]))) - as.numeric(mean(as.numeric(area1s[[Object2,1]])))
    p_value_1[[Object1,Object2]] = t.test(as.numeric(data_set[[1,1]][Object1,]),as.numeric(data_set[[1,1]][Object2,]), paired = TRUE)$p.value
    p_value_4[[Object1,Object2]] = t.test(as.numeric(data_set[[4,1]][Object1,]),as.numeric(data_set[[4,1]][Object2,]), paired = TRUE)$p.value                                                                                                
    if(Object1 == Object2){
      p_value_1[9*(Object1 - 1) + Object2] = 0
      p_value_4[9*(Object1 - 1) + Object2] = 0
    } 
  }
}






p_value_1_adjust_bofferni = p.adjust(p_value_1, "bonferroni")
p_value_4_adjust_bofferni = p.adjust(p_value_4, "bonferroni")
p1_final = matrix(list(), nrow = 9, ncol = 9)
p4_final = matrix(list(), nrow = 9, ncol = 9)

for (Object1 in 1:9) {
  for (Object2 in 1:9) {
    if(p_value_1_adjust_bofferni[9*(Object1 - 1) + Object2] < a){
      p1_final[[Object1,Object2]] = 1
    }else{
      p1_final[[Object1,Object2]] = 0
    }
    if(p_value_4_adjust_bofferni[9*(Object1 - 1) + Object2] < a){
      p4_final[[Object1,Object2]] = 1
    }else{
      p4_final[[Object1,Object2]] = 0
    }
  }
}
print(p1_final)
print(p4_final)





p_value_1_adjust_BH = p.adjust(p_value_1, "BH")
p_value_4_adjust_BH = p.adjust(p_value_4, "BH")
p1_final_BH = matrix(list(), nrow = 9, ncol = 9)
p4_final_BH = matrix(list(), nrow = 9, ncol = 9)


for (Object1 in 1:9) {
  for (Object2 in 1:9) {
    if(p_value_1_adjust_BH[9*(Object1 - 1) + Object2] < a){
      p1_final_BH[[Object1,Object2]] = 1
    }else{
      p1_final_BH[[Object,Object2]] = 0
    }
    if(p_value_4_adjust_BH[9*(Object1 - 1) + Object2] < a){
      p4_final_BH[[Object1,Object2]] = 1
    }else{
      p4_final_BH[[Object1,Object2]] = 0
    }

  }
}
print(p1_final_BH)
print(p4_final_BH)




p_value_1_adjust_fdr = p.adjust(p_value_1, "fdr")
p_value_4_adjust_fdr = p.adjust(p_value_4, "fdr")
p1_final_fdr = matrix(list(), nrow = 9, ncol = 9)
p4_final_fdr = matrix(list(), nrow = 9, ncol = 9)

for (Object1 in 1:9) {
  for (Object2 in 1:9) {
    if(p_value_1_adjust_fdr[9*(Object1 - 1) + Object2] < a){
      p1_final_fdr[[Object1,Object2]] = 1
    }else{
      p1_final_fdr[[Object1,Object2]] = 0
    }
    if(p_value_4_adjust_fdr[9*(Object1 - 1) + Object2] < a){                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
      p4_final_fdr[[Object1,Object2]] = 1
    }else{
      p4_final_fdr[[Object1,Object2]] = 0
    }
  }
}
print(p1_final_fdr)
print(p4_final_fdr)


