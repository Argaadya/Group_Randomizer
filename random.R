library(tidyverse)

random_group <- function(n,group){
#input
sample_size <- n
num_group <- group

#number of people in each group
n_per_group <- floor(sample_size/num_group)

#data frame/tabel berisi setiap orang akan masuk grup mana (di awal grup = 0)
group_df <- data.frame(member = seq(1,n),
                       group = 0)

#define maximum number of people in each group
max_group <- data.frame(group = seq(1,num_group),
                        max_member = n_per_group)

#menambahkan 1 orang ke tiap kelompok sampai jumlah orang di semua grup sesuai jumlah sampel
for (i in 1:num_group) {
  if (sum(max_group$max_member)!=sample_size) {
    max_group$max_member[i] <- max_group$max_member[i]+1  
  }
}
  
#Assign people to group
for (i in 1:sample_size) {
  group_df$group[i] <- round(runif(n = 1,min = 1,max = num_group))
  
  #jika jumlah orang dalam suatu grup > jumlah maksimum di grup tersebut, maka random ulang sampai terpenuhi
  if (length(group_df[group_df$group %in% group_df$group[i],2]) > max_group[max_group$group %in% group_df$group[i],2]) {
    while (length(group_df[group_df$group %in% group_df$group[i],2]) > max_group[max_group$group %in% group_df$group[i],2]) {
      group_df$group[i] <- round(runif(n = 1,min = 1,max = num_group))  
    }  
  }
}
return(group_df)
}

#excecute
random_group(52,6)




