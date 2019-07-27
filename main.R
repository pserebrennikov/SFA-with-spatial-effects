library('openxlsx')
library('dplyr')
library('ssfa')
library('frontier')

df_2017_geo <- read.xlsx('2017_for_sfa_geo.xlsx', startRow = 2)

for (i in 4 : (length(df_2017_geo) - 1))
  df_2017_geo[, i] <- as.numeric(df_2017_geo[, i])
  
df_temp <- read.xlsx('population.xlsx', cols = c(1, 2))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `1..Оценка.численности.постоянного.населения.по.субъектам.Российской.Федерации`,
                  population = X2)

df_2017_geo <- data.frame(cbind(df_2017_geo, population = rep(NA, nrow(df_2017_geo))))

df_temp$region[2] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_2017_geo)){
    
    if (df_2017_geo$region[j] == df_temp$region[i] )
      df_2017_geo$population[j] = df_temp$population[i]
  }
}

#################################data for regression


#population
df_for_reg <- data.frame(id = df_2017_geo$id,
                         name = df_2017_geo$name,
                         region = df_2017_geo$region,
                         population = df_2017_geo$population)


#unemployment rate
df_temp <- read.xlsx('unemployment rate.xlsx', cols = c(1, 2))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `Обновлено.27.03.2019`,
                  unemploy = X2)

df_for_reg <- data.frame(cbind(df_for_reg, unemployment_rate = rep(NA, nrow(df_for_reg))))

df_temp$region[3] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$unemployment_rate[j] = df_temp$unemploy[i]
  }
}

sum(is.na(df_for_reg$unemployment_rate))

#income per capita
df_temp <- read.xlsx('income per capita.xlsx', cols = c(1, 81))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = X1,
                  income = `81`)

df_for_reg <- data.frame(cbind(df_for_reg, income = rep(NA, nrow(df_for_reg))))

df_temp$region[1] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$income[j] = df_temp$income[i]
  }
}

sum(is.na(df_for_reg$income))

#share of employment with higher education

df_temp <- read.xlsx('share of employment with higher education.xlsx', cols = c(1, 9))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `1.1.1..Доля.занятого.населения.в.возрасте.25-64.лет,.имеющего.высшее.образование.в.общей.численности.занятого.населения.соответствующей.возрастной.группы`,
                  employ_higher = X2)

df_for_reg <- data.frame(cbind(df_for_reg, employ_higher = rep(NA, nrow(df_for_reg))))

df_temp$region[1] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$employ_higher[j] = df_temp$employ_higher[i]
  }
}

sum(is.na(df_for_reg$employ_higher))

#number of students per 10 000 population

df_temp <- read.xlsx('number of students per 10000 population.xlsx', cols = c(1, 9))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `1.1.2..Численность.студентов,.обучающихся.по.образовательным.программам.высшего.образования.-.программам.бакалавриата,.специалитета,.магистратуры,.на.10000.человек.населения`,
                  students_per_pop = X2)

df_for_reg <- data.frame(cbind(df_for_reg, students_per_pop = rep(NA, nrow(df_for_reg))))

df_temp$region[1] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$students_per_pop[j] = df_temp$students_per_pop[i]
  }
}

sum(is.na(df_for_reg$students_per_pop))

#vrp

df_temp <- read.xlsx('vrp98-17.xlsx', cols = c(1, 21))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `Обновлено.01.03.2019`,
                  GRP = X2)

df_for_reg <- data.frame(cbind(df_for_reg, GRP = rep(NA, nrow(df_for_reg))))

df_temp$region[1] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$GRP[j] = df_temp$GRP[i]
  }
}

sum(is.na(df_for_reg$GRP))

#vrp

df_temp <- read.xlsx('vrp98-17.xlsx', cols = c(1, 21))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `Обновлено.01.03.2019`,
                  GRP = X2)

df_for_reg <- data.frame(cbind(df_for_reg, GRP = rep(NA, nrow(df_for_reg))))

df_temp$region[1] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$GRP[j] = df_temp$GRP[i]
  }
}

sum(is.na(df_for_reg$GRP))

#fixed investment

df_temp <- read.xlsx('fixed investment.xlsx', cols = c(1, 15))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `Обновлено.18.03.2019`,
                  investment = X2)

df_for_reg <- data.frame(cbind(df_for_reg, investment = rep(NA, nrow(df_for_reg))))

df_temp$region[3] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$investment[j] = df_temp$investment[i]
  }
}

sum(is.na(df_for_reg$investment))

#structure of gross regional product

df_temp <- read.xlsx('struktura17.xlsx', cols = c(1, 3:21))

df_temp <- df_temp[-c(1:3), ]

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `Обновлено.01.03.2019`,
                  A = X2, B = X3, C = X4, D = X5, E = X6, F = X7, G = X8, H = X9, I = X10, J = X11, 
                  K = X12, L = X13, M = X14, N = X15, O = X16, P = X17, Q = X18, R = X19, S = X20)

df_for_reg <- data.frame(cbind(df_for_reg, A = rep(NA, nrow(df_for_reg)), B = rep(NA, nrow(df_for_reg)),
                                           C = rep(NA, nrow(df_for_reg)), D = rep(NA, nrow(df_for_reg)),
                                           F = rep(NA, nrow(df_for_reg)), G = rep(NA, nrow(df_for_reg)),
                                           H = rep(NA, nrow(df_for_reg)), I = rep(NA, nrow(df_for_reg)),
                                           J = rep(NA, nrow(df_for_reg)), K = rep(NA, nrow(df_for_reg)),
                                           L = rep(NA, nrow(df_for_reg)), M = rep(NA, nrow(df_for_reg)),
                                           N = rep(NA, nrow(df_for_reg)), O = rep(NA, nrow(df_for_reg)),
                                           P = rep(NA, nrow(df_for_reg)), Q = rep(NA, nrow(df_for_reg)),
                                           R = rep(NA, nrow(df_for_reg)), S = rep(NA, nrow(df_for_reg))))

df_temp$region[1:2] <- 0
df_for_reg <- data.frame(cbind(df_for_reg, E = rep(NA, nrow(df_for_reg))))

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$S[j] = df_temp$S[i]
    
  }
}

sum(is.na(df_for_reg$S))

#dummy variable for universities located in Moscow and Leningrad region

df_for_reg <- data.frame(cbind(df_for_reg, dist = rep(NA, nrow(df_for_reg))))

for (i in 1:nrow(df_for_reg)){
  if (df_for_reg$region[i] == 'г. Москва' | 
      df_for_reg$region[i] == 'Московская область' | 
      df_for_reg$region[i] =='г. Санкт-Петербург' |
      df_for_reg$region[i] == 'Ленинградская область')
    df_for_reg$dist[i] = 1
  
  else 
    df_for_reg$dist[i] = 0
    
}

sum(df_for_reg$dist)

#life expectancy at birth

df_temp <- read.xlsx('life expectancy at birth.xlsx', cols = c(1, 2))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `2017`,
                  life_exp = `Ожидаемая.продолжительность.жизни`)

df_for_reg <- read.xlsx('df_for_reg.xlsx', startRow = 2)
df_for_reg <- data.frame(cbind(df_for_reg, life_exp = rep(NA, nrow(df_for_reg))))

df_temp$region[3] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$life_exp[j] = df_temp$life_exp[i]
  }
}

sum(is.na(df_for_reg$life_exp))

for (i in 1:nrow(df_for_reg)){
  if (is.na(df_for_reg$life_exp[i]))
    cat(df_for_reg$region[i], '\n')
}

#share of urban population

df_temp <- read.xlsx('share of urban population.xlsx', cols = c(1, 2))

colnames(df_temp)
df_temp <- rename(df_temp,
                  region = `2017`,
                  urban_pop = `Доля.городского.населения`)

df_for_reg <- data.frame(cbind(df_for_reg, urban_pop = rep(NA, nrow(df_for_reg))))

df_temp$region[3] <- 0

for (i in 1 : nrow(df_temp)){
  for (j in 1 : nrow(df_for_reg)){
    
    if (df_for_reg$region[j] == df_temp$region[i] )
      df_for_reg$urban_pop[j] = df_temp$urban_pop[i]
  }
}

sum(is.na(df_for_reg$urban_pop))

for (i in 1:nrow(df_for_reg)){
  if (is.na(df_for_reg$urban_pop[i]))
    cat(df_for_reg$region[i], '\n')
}



write.xlsx(df_for_reg, file = 'df_for_reg_1.xlsx')
