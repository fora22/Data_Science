library(readxl)
rm(list=ls())
setwd("D:/Trash_Room_D")
df <- read_excel("seoul_EDA.xlsx")

#df = df[-26,]
head(df)
name = df[,1]
name
dataf = data.frame(df, row.names = name$구별)
dataf = dataf[,-1]
head(dataf)
# summary(dataf)

pca_df <- prcomp(dataf, center = T, scale. = T)
summary(pca_df)
pca_df$rotation

screeplot(pca_df, main="", col="blue", type="lines", pch=3, npcs = length(pca_df$sdev))
biplot(pca_df)

print(pca_df)
#####################주성분분석 ###########################
# 1) 4개의 변수들간 산점도
library(datasets)
data("USArrests")
pairs(USArrests, panel=panel.smooth, main="USArrests data")

# 2) summary
US.prin = princomp(USArrests, cor=TRUE)
summary(US.prin)
screeplot(US.prin, npcs=4, type="lines")

# 3) Loading
loadings(US.prin)

# 4) Scores
US.prin$scores

# 5) 제 1-2주성분에 의한 행렬도
arrests.pca = prcomp(USArrests, center = TRUE, scale. = TRUE)
biplot(arrests.pca, scale = 0)