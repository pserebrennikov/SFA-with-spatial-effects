library('openxlsx')
library('mice')
library('ssfa')
library('ggplot2')
library('frontier')
library('reshape2')
library('dplyr')
library('raster')
library('geosphere')
library('REAT')



df_2017_geo_cenc <- read.xlsx(xlsxFile = '2017_for_sfa_geo_cencored.xlsx', skipEmptyCols = TRUE)

for (i in 4:15) 
  df_2017_geo_cenc[,i] <- as.numeric(df_2017_geo_cenc[,i])

for (i in 4:13) 
  df_2017_geo_cenc[,i] <- outlier(df_2017_geo_cenc[,i])

X_spatial <- with(df_2017.for_sfa_cenc, cbind(mean_exam, income, total_staff, #input
                                      research_work, total_publications, total_students, #output
                                      latitude, longitude)) 


impute_X.spatial <- mice(X_spatial)
X_spatial <- complete(impute_X.spatial,1)


for (i in 1:6) 
  X_spatial[,i] <- replace_zero(X_spatial[,i])

W <- constructW(cbind(X_spatial$longitude, X_spatial$latitude), df_2017_geo_cenc$id)
W <- rowStdrt(W)
W.e_1 <- rowStdrt(W.e_1)
sum(W[1,])
sum(W.e_1[1,])

ssfa <- ssfa(-log(income) ~ log(mean_exam/income) + log(total_staff/income) + log(total_publications) + 
                                                    log(total_students) + log(research_work), 
                                    data = X_spatial, 
                                    data_w = W, 
                                    form = "production", 
                                    par_rho=TRUE)

summary(ssfa)
ggplot(as.data.frame(eff.ssfa(ssfa)), fill = "grey", color = "black") + 
  geom_density(aes(x = eff.ssfa(ssfa))) + ggtitle('Distribution of effectiency`s estimations')


sfa.ef <- efficiencies(sfa) # efficiency estimators of classic sfa
#sfa.ef[497] <- NA
df_sfa <- data.frame(cbind(sfa = sfa.ef, ssfa = as.vector(eff.ssfa(ssfa)) ))
df_sfa <- rename(df_sfa, sfa = efficiency)
sd(eff.ssfa(ssfa))
data <- melt(df_sfa)
ggplot(data, aes(x=value, fill=variable)) + geom_density(alpha=0.25) + theme(legend.position = c(0.3, 0.75)) + 
  xlab('efficiency')



mean(eff.ssfa(ssfa) - efficiencies(sfa))

dif <- eff.ssfa(ssfa) - efficiencies(sfa)
dif[497] <- NA

df_sfa_cenc <- data.frame(cbind(df_2017_geo_cenc, dif ))
df_sfa <- data.frame(cbind(df_sfa, id = df_2017_geo$id))
df_sfa <- data.frame(cbind(df_sfa, name = df_2017_geo$name))
df_sfa <- data.frame(cbind(df_sfa, region = df_2017_geo$region))

write.xlsx(df_sfa, file = 'df_sfa_dif.xlsx')


data <- melt(df_sfa_cenc$dif)
ggplot(data, aes(x=value)) + geom_density(alpha=0.25) + theme(legend.position = c(0.3, 0.75)) + 
  xlab('dif of eff')

hist(df_sfa$dif)
boxplot(df_sfa$dif)

ggplot(df_sfa_cenc, aes(x = '', y = dif )) +
  geom_boxplot() + 
  geom_text(aes(label = id), na.rm = TRUE, hjust = -0.3, check_overlap = TRUE)




### change distance matrix

#W.geo_1 <- pointDistance(cbind(X_spatial$latitude, X_spatial$longitude), lonlat = TRUE)

DistanceMatrix <- function(coordinates, method = 'Euclidean'){
  
  distMatrix <- matrix(, nrow = nrow(coordinates), ncol = nrow(coordinates))
  
  if (method == 'geosphere'){
    
    for (i in 1:nrow(coordinates)){
      for (j in 1:nrow(coordinates))
        
          distMatrix[i, j] = distGeo(cbind(coordinates[i, 1], coordinates[i, 2]), 
                                     cbind(coordinates[j, 1], coordinates[j, 2]))
    }
  }
  else {
    
    for (i in 1:nrow(coordinates)){
      for (j in 1:nrow(coordinates)){
        
        if (j == i) # dist.calc sometimes produces NaN values trying to calculate distance between the same points 
          distMatrix[i, j] = 0
                    # just ignore now Euclidean distance matrix
        else
          distMatrix[i, j] = dist.calc(coordinates[i, 2], coordinates[i, 1], 
                                       coordinates[j, 2], coordinates[j, 1], unit = 'm')
      }
    }
  }
  
  
  return(distMatrix)
}

W.geosphere <- DistanceMatrix(cbind(X_spatial$longitude, X_spatial$latitude), method = 'geosphere')
W.euclidean <- DistanceMatrix(cbind(X_spatial$longitude, X_spatial$latitude), method = 'Euclidean')

apply(W.geosphere, 2, function (x) sum(is.nan(x)) )
apply(W.euclidean, 2, function (x) sum(is.nan(x)) )  # Euclidean distance matrix has NaN values. Why?)
                                                     # do not use this metric

W.geosphere <- rowStdrt(W.geosphere)


ssfa.geo <- ssfa(-log(income) ~ log(mean_exam/income) + log(total_staff/income) + log(total_publications) + 
                                log(total_students) + log(research_work), 
             data = X_spatial, 
             data_w = W.geosphere, 
             form = "production", 
             par_rho = TRUE)

summary(ssfa.geo)

mean(eff.ssfa(ssfa.geo) - efficiencies(sfa))

df_sfa <- data.frame(cbind(sfa = efficiencies(sfa), ssfa = as.vector(eff.ssfa(ssfa.geo)) ))
df_sfa <- rename(df_sfa, sfa = efficiency)
data <- melt(df_sfa)
ggplot(data, aes(x=value, fill=variable)) + geom_density(alpha=0.25) + theme(legend.position = c(0.3, 0.75)) + 
  xlab('efficiency')

df_2017_geo_cenc <- data.frame(df_2017_geo_cenc, dif = eff.ssfa(ssfa.geo) - efficiencies(sfa))


df_2017_geo_cenc.sort <- df_2017_geo_cenc[order(df_2017_geo_cenc$efficiency), ]

write.xlsx(df_2017_geo_cenc.sort, file = 'df_dif_cencored.xlsx')


