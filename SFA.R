library('Benchmarking')
library('openxlsx')
library('dplyr')
library('ggplot2')
library('mice')
library('VIM')
library('frontier')
library('stringr')
library('GGally')

df_2017 <- read.xlsx(xlsxFile = 'me_2017.xlsx', skipEmptyCols = TRUE)
names(df_2017)

sum(df_2017$Ведомственная.принадлежность == 'Частные образовательные организации')
sum(df_2017$`головная/филиал` != 'головная образовательная организация')

# select state and head universities
df_2017.temp <- filter(df_2017, Ведомственная.принадлежность != 'Частные образовательные организации' & 
                                   `головная/филиал` == 'головная образовательная организация')

# check selection
sum(df_2017.temp$Ведомственная.принадлежность == 'Частные образовательные организации')
sum(df_2017.temp$`головная/филиал` != 'головная образовательная организация')


# select variables for sfa
df_2017.for_sfa <- with(df_2017.temp, cbind(id, 
                                       Наименование.образовательной.организации,
                                       Регион,
                                       Доходы.вуза.из.всех.источников,
                                       `Средний.балл.ЕГЭ.студентов,.принятых.по.результатам.ЕГЭ.на.обучение.по.очной.форме.по.программам.бакалавриата.и.специалитета.за.счет.средств.соответствующих.бюджетов.бюджетной.системы.РФ`,
                                       `Общий.объем.научно-исследовательских.и.опытно-конструкторских.работ.(далее.–.НИОКР)`))

Nums_of_NPR <- as.numeric(df_2017.temp$`Общая.численность.ППС.(без.внешних.совместителей.и.работающих.по.договорам.ГПХ)`) +
               as.numeric(df_2017.temp$`Общая.численность.научных.работников.(без.внешних.совместителей.и.работающих.по.договорам.ГПХ)`)

Nums_of_publications <- (as.numeric(df_2017.temp$`Число.публикаций.организации,.индексируемых.в.информационно-аналитической.системе.научного.цитирования.Web.of.Science,.в.расчете.на.100.НПР`)  +
                         as.numeric(df_2017.temp$`Число.публикаций.организации,.индексируемых.в.информационно-аналитической.системе.научного.цитирования.Scopus,.в.расчете.на.100.НПР`) +
                         as.numeric(df_2017.temp$`Число.публикаций.организации,.индексируемых.в.информационно-аналитической.системе.научного.цитирования.РИНЦ,.в.расчете.на.100.НПР`) ) * Nums_of_NPR / 100


Priv_cont <- as.numeric(df_2017.temp$`в.том.числе:по.очной.форме.обучения`) + 
        0.25*as.numeric(df_2017.temp$`по.очно-заочной.(вечерней).форме.обучения`) + 
         0.1*as.numeric(df_2017.temp$`по.заочной.форме.обучения`)

df_2017.for_sfa <- data.frame(cbind(df_2017.for_sfa, `Общая.численность.НПР` = Nums_of_NPR, 
                             `Общее.количество.публикаций` = Nums_of_publications,
                             `Приведенный.контингент` = Priv_cont))




# export to excel
write.xlsx(df_2017.for_sfa, file = '2017_for_sfa.xlsx')

############################################################
# descreptive statistics
df_2017.for_sfa <- read.xlsx(xlsxFile = '2017_for_sfa_geo.xlsx', startRow = 2)


# present data as numeric
for (i in 4:9) 
  df_2017.for_sfa[,i] <- as.numeric(as.character(df_2017.for_sfa[,i]))

str(df_2017.for_sfa)
colnames(X)
summary(df_2017.for_sfa$income)
summary(df_2017.for_sfa$mean_exam)
summary(df_2017.for_sfa$research_work)
summary(df_2017.for_sfa$total_staff)
summary(df_2017.for_sfa$total_publications)
summary(df_2017.for_sfa$total_students)


ggplot(df_2017.for_sfa, aes(x = '', y = income )) +
  geom_boxplot() + 
  geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3, check_overlap = TRUE)
ggplot(df_2017.for_sfa, aes(x = '', y = mean_exam )) +
  geom_boxplot() + 
  geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3, check_overlap = TRUE)
ggplot(df_2017.for_sfa, aes(x = '', y = research_work)) +
  geom_boxplot() + 
  geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3, check_overlap = TRUE)
ggplot(df_2017.for_sfa, aes(x = '', y = total_staff )) +
  geom_boxplot() + 
  geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3, check_overlap = TRUE)
ggplot(df_2017.for_sfa, aes(x = '', y = total_publications )) +
  geom_boxplot() + 
  geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3, check_overlap = TRUE)
ggplot(df_2017.for_sfa, aes(x = '', y = total_students )) +
  geom_boxplot() + 
  geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3, check_overlap = TRUE)

boxplot(df_2017.for_sfa$Доходы.вуза.из.всех.источников)
boxplot(df_2017.for_sfa$Средний.балл.ЕГЭ.студентов..принятых.по.результатам.ЕГЭ.на.обучение.по.очной.форме.по.программам.бакалавриата.и.специалитета.за.счет.средств.соответствующих.бюджетов.бюджетной.системы.РФ)
boxplot(df_2017.for_sfa$`Общий.объем.научно-исследовательских.и.опытно-конструкторских.работ.(далее.–.НИОКР)`)
boxplot(df_2017.for_sfa$Общая.численность.НПР)
boxplot(df_2017.for_sfa$Общее.количество.публикаций)
boxplot(df_2017.for_sfa$Приведенный.контингент)


outlier <- function(x) {
  qnt <- quantile(x, probs=c(.25,.75), na.rm=TRUE)
  caps <- quantile(x, probs=c(.05,.95), na.rm=TRUE)
  H <- 3 * IQR(x, na.rm = T)
  y <- x
  y[x < (qnt[1] - H)] <- caps[1]
  y[x > (qnt[2] + H)] <- caps[2]
  return(y)
}


for (i in 4:9) 
  df_2017.for_sfa[,i] <- outlier(df_2017.for_sfa[,i])


# sfa
X <- with(df_2017.for_sfa, cbind(mean_exam, income, total_staff, #input
                                 research_work, total_publications, total_students )) #output



md.pattern(X)
aggr(X, col=c('navyblue','red'), numbers=TRUE,
     sortVars=TRUE, labels=names(X), cex.axis=.7,
     gap=0, ylab=c("Histogram of missing data","Pattern"))
methods(mice)

impute_X <- mice(X)
X <- complete(impute_X,1)


apply(X, 2, function(x) sum(is.na(x)))

replace_zero <- function(vec){
  
  for(i in 1:length(vec)){
    if(vec[i] == 0)
      vec[i] = 1e-04
  }
  return(vec)
}

X$research_work <- replace_zero(X$research_work)



colnames(X)
sfa <- sfa(data = X, -log(income) ~ log(mean_exam/income) + log(total_staff/income) + 
                                               log(total_publications) + log(total_students) +
                                               log(research_work)) # frontier
summary(sfa, extraPar = TRUE)
sd(efficiencies(sfa))
ggplot(as.data.frame(efficiencies(sfa)), fill = "grey", color = "black") + 
  geom_histogram(aes(x = efficiencies(sfa))) + ggtitle('Distribution of effectiency`s estimations')

### change normalizing input

sfa_1 <- sfa(data = X, -log(total_staff) ~ log(mean_exam/total_staff) + log(income/total_staff) + 
                                             log(total_publications) + log(total_students) +
                                             log(research_work))

summary(sfa_1, extraPar = TRUE)
ggplot(as.data.frame(efficiencies(sfa_1)), fill = "grey", color = "black") + 
  geom_histogram(aes(x = efficiencies(sfa_1))) + ggtitle('Distribution of effectiency`s estimations')

cor(efficiencies(sfa), efficiencies(sfa_1))
mean(efficiencies(sfa)) == mean(efficiencies(sfa_1))
mean(efficiencies(sfa))
mean(efficiencies(sfa_1))
### change normalizing input

sfa_2 <- sfa(data = X, -log(mean_exam) ~ log(total_staff/mean_exam) + log(income/mean_exam) + 
                                           log(total_publications) + log(total_students) +
                                           log(research_work))
summary(sfa_2)
ggplot(as.data.frame(efficiencies(sfa_2)), fill = "grey", color = "black") + 
  geom_histogram(aes(x = efficiencies(sfa_2))) + ggtitle('Distribution of effectiency`s estimations')

cor(efficiencies(sfa), efficiencies(sfa_2))

X_cor <- data.frame(cbind(efficiencies(sfa), efficiencies(sfa_1), efficiencies(sfa_2)))
ggpairs(X_cor)

# simple DEA

colnames(X)
X <- X[ , !(names(X) %in% c('Income_of_univ.1'))]

dea_vrs <- dea(X = data.frame(cbind(X$mean_exam, X$income, X$total_staff)), 
               Y = data.frame(cbind(X$research_work, X$total_publications, X$total_students)),
               ORIENTATION = 'in', 
               RTS = 'vrs')

dea_crs <- dea(X = data.frame(cbind(X$mean_exam, X$income, X$total_staff)), 
               Y = data.frame(cbind(X$research_work, X$total_publications, X$total_students)),
               ORIENTATION = 'in', 
               RTS = 'crs')


X_cor <- data.frame(cbind(sfa_1 = as.vector(efficiencies(sfa)), 
                          sfa_2 = as.vector(efficiencies(sfa_1)), 
                          sfa_3 = as.vector(efficiencies(sfa_2)), 
                          dea_vrs = eff(dea_vrs), 
                          dea_crs = eff(dea_crs)))
ggpairs(X_cor)


