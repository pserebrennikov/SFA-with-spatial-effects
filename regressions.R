library('openxlsx')
library('dplyr')
library('mice')
library('MASS')
library('openxlsx')
#library('Rcmdr')


df_for_reg <- read.xlsx('df_for_reg.xlsx', startRow = 2)

impute_df <- mice(df_for_reg)
df_for_reg <- complete(impute_df,1)

df_sfa$dif[497] <- mean(df_sfa$dif[1:496])

df_for_reg <- data.frame(cbind(df_for_reg, dif = df_sfa$dif ))

for (i in 4:ncol(df_for_reg))
  df_for_reg[ ,i] <- as.numeric(df_for_reg[ ,i])

for (i in 4:ncol(df_for_reg))
  df_for_reg[ ,i] <- replace_zero(df_for_reg[ ,i])

ggplot(df_for_reg, aes(x = '', y = unemployment_rate )) +
  geom_boxplot() + 
  geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3, check_overlap = TRUE)

model_1 <- lm(data = df_for_reg, formula = dif ~ population + unemployment_rate + income + employ_higher + students_per_pop + 
                                                 GRP + investment + dist + life_exp + urban_pop)

model_1_log <- lm(data = df_for_reg, formula = log(dif) ~ log(population) + log(unemployment_rate) + log(income) + 
                                               log(employ_higher) + log(students_per_pop) + log(GRP) + 
                                               log(investment) + log(dist) + log(life_exp) + log(urban_pop))

model <- lm(data = df_for_reg, formula = dif ~ population + life_exp +urban_pop)

summary(model)
summary(model_1)
summary(model_1_log)



stepAIC(model_1_log, direction = 'both')

# divide our sample into more homogeneous subsamples


df_temp <- read.xlsx('me_2017.xlsx', cols = c(1,2,3,4,5,6,7,12,14,15,17))

names(df_temp)
temp_1 <- c()
temp_2 <- c()
for (i in 1:nrow(df_for_reg)){
  for (j in 1:nrow(df_temp)){
    if (df_for_reg$id[i] == df_temp$id[j]){
      temp_1 <- c(temp_1, df_temp$`Ведомственная.принадлежность`[j])
      temp_2 <- c(temp_2, df_temp$`Профиль.организации`[j])
    }
  }
}
temp_1
temp_2
