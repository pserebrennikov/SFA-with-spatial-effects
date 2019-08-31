library('openxlsx')
library('dplyr')
library('mice')
library('MASS')
#library('Rcmdr')

df_for_reg_cenc <- df_for_reg
df_for_reg_cenc <- df_for_reg_cenc[-c(1:497), ]

# make dataframe for stepwise regression 

for (i in 1:nrow(df_2017_geo_cenc)){
  for (j in 1:nrow(df_for_reg)){
    
    if (df_2017_geo_cenc$name[i] == df_for_reg$name[j])
      df_for_reg_cenc <- rbind(df_for_reg_cenc, df_for_reg[j, ])
      
  }
  
}

str(df_for_reg_cenc) # dataframe


for (i in 4:ncol(df_for_reg_cenc))
  df_for_reg_cenc[ ,i] <- as.numeric(df_for_reg_cenc[ ,i])

str(df_for_reg_cenc)

impute_df <- mice(df_for_reg_cenc, method = 'cart')
df_for_reg_cenc <- complete(impute_df, 1)


df_for_reg_cenc <- data.frame(cbind(df_for_reg_cenc, dif = df_2017_geo_cenc$efficiency ))


#for (i in 4:ncol(df_for_reg))
#  df_for_reg[ ,i] <- replace_zero(df_for_reg[ ,i])



model_1 <- lm(data = df_for_reg_cenc, formula = dif ~ population + unemployment_rate + income + employ_higher + students_per_pop + 
                                                      GRP + investment + dist + life_exp + urban_pop)

model_1_log <- lm(data = df_for_reg_cenc, formula = log(dif) ~ log(population) + log(unemployment_rate) + log(income) + 
                                                    log(employ_higher) + log(students_per_pop) + log(GRP) + 
                                                    log(investment) + dist + log(life_exp) + log(urban_pop))

model <- lm(data = df_for_reg, formula = dif ~ population + life_exp + urban_pop)

summary(model)
summary(model_1)
summary(model_1_log)



stepAIC(model_1, direction = 'both')


