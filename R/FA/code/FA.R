library(readxl)
rm(list=ls())
df <- read_excel("./data/EDA_FINAL.xlsx" , na = "NA")
name = df[,1]
df = df[,-1]
df = data.frame(df, row.names = name$구별)
head(df)
df = scale(df)
######################################################################
# 분기점 1
# install.packages(c("psych", "GPArotation"))
library(psych)
library(GPArotation)

d.factor <- principal(df, rotate = "none")
names(d.factor)

d.factor$values
plot(d.factor$values, type = "b")

nof = 3  # number_of_factors

# rotate can "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", or "cluster"
Varimax = principal(df, nfactors = nof, rotate = "varimax")
Varimax

# rotation= options include "varimax", "promax", and "none"
# scores="regression" or "Bartlett"
# 동작 안함
fact <- factanal(df, factors = nof, rotation="varimax", scores = "regression")
fact

d.fipa <- factor.pa(df, nfactors = nof, rotate = "varimax")
d.fipa

# install.packages("nFactors")
library(nFactors)
ev <- eigen(cor(df))  # get eigenvalues
ap <- parallel(subject=nrow(df),var=ncol(df),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# 출처
# https://www.statmethods.net/advstats/factor.html
##############################################################


##############################################################
# 분기점 2
library(psych)
library(GPArotation)
X_cor <- cor(df, use="pairwise.complete.obs")
scree(X_cor, factors = FALSE)

fa.parallel(df, fm = 'minres', fa = 'fa')


anim_two_fa   <- fa(df, nfactors=2, n.obs=N, rotate="varimax")
anim_three_fa <- fa(df, nfactors=3, n.obs=N, rotate="varimax")
anim_four_fa  <- fa(df, nfactors=4, n.obs=N, rotate="varimax")
anim_five_fa  <- fa(df, nfactors=5, n.obs=N, rotate="varimax")

anova(anim_two_fa, anim_three_fa)
anova(anim_three_fa, anim_four_fa)
anova(anim_four_fa, anim_five_fa)
anim_three_fa$RMSEA
anim_three_fa$TLI
fa.plot(anim_three_fa)  # 알아보기 힘듬
print(anim_three_fa$loadings, cutoff = 0.2)

fa.diagram(anim_three_fa)

# 출처
# https://statkclee.github.io/statistics/stat-exploratory-factor-analysis.html