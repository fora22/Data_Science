library(readxl)
rm(list=ls())
df <- read_excel("./data/EDA_FINAL.xlsx" , na = "NA")
name = df[,1]
df = df[,-1]
df = data.frame(df, row.names = name$구별)
head(df)

pca_df <- prcomp(df, center = T, scale. = T)
summary(pca_df)
pca_df$rotation

screeplot(pca_df, main="", col="blue", type="lines", pch=3, npcs = length(pca_df$sdev))
biplot(pca_df)

print(pca_df)

