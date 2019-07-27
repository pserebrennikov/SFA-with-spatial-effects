library('openxlsx')
library('stringr')
install.packages('micropan')
library('micropan')

df_geo <- read.xlsx(xlsxFile = 'universities_city.xlsx')
fruit <- c("apple", "banana", "pear", "pinapple")
temp <- c('a', 'b', 'c', 'd')

df_2017.for_sfa <- data.frame(cbind(df_2017.for_sfa, `Широта` = rep(NA, 497), `Долгота` = rep(NA, 497)))


num_of_branch <- c()
for(i in 1:length(df_geo$`ВУЗ.(2014.год)`)){
  
  if(str_detect(df_geo$`ВУЗ.(2014.год)`[i], 'филиал') == TRUE)
    num_of_branch <- c(num_of_branch, i)
}
df_geo <- df_geo[-num_of_branch, ]


k = 0
for(i in 1:length(df_geo$`ВУЗ.(2014.год)`)){
  
  detect_vec <- str_detect(df_2017.for_sfa$`Наименование.образовательной.организации`, df_geo$`ВУЗ.(2014.год)`[i])
  if(sum(detect_vec) == 2){
    cat(df_geo$`ВУЗ.(2014.год)`[i], match(TRUE, detect_vec), '\n')
    k = k + 1
    #df_2017.for_sfa$`Широта`[match(TRUE, detect_vec)] <- df_geo$`Широта`[i]
    #df_2017.for_sfa$`Долгота`[match(TRUE, detect_vec)] <- df_geo$`Долгота`[i]
  }
  
}
cat(k, '\n')

write.xlsx(df_2017.for_sfa, file = '2017_for_sfa_test.xlsx')





