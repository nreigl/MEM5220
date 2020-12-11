#Sample Selection/Heckman model class for masters Econometrics

library(sampleSelection)
library(Hmisc)
library(corrplot)
library(mfx)
library(ggplot2)

# Import CSV
data_selection <- read.csv("~/Desktop/Projektid/Masters Econometrics course/data_selection.csv")
# Example based on sample of married women
summary(data_selection)
str(data_selection)
##Correlation matrix for participation
res <- rcorr(as.matrix(data_selection[,c("working","ndepchld","age","educ")]))
flattenCorrMatrix <- function(cormat, pmat){
  ut<- upper.tri(cormat)
  data.frame(
    row=rownames(cormat)[row(cormat)[ut]],
    column=rownames(cormat)[col(cormat)[ut]],
    cor=(cormat)[ut],
    p=pmat[ut]
  )
}
corrview <- flattenCorrMatrix(res$r,res$P)
corrplot(res$r,type="upper",order="hclust",p.mat = res$P,sig.level = 0.1,insig = "blank")

#Correlation matrix for wage
res2 <- rcorr(as.matrix(data_selection[,c("wage_ln","age","ndepchld","educ")]))
flattenCorrMatrix <- function(cormat, pmat){
  ut<- upper.tri(cormat)
  data.frame(
    row=rownames(cormat)[row(cormat)[ut]],
    column=rownames(cormat)[col(cormat)[ut]],
    cor=(cormat)[ut],
    p=pmat[ut]
  )
}
corrview2 <- flattenCorrMatrix(res2$r,res2$P)
corrplot(res2$r,type="upper",order="hclust",p.mat = res2$P,sig.level = 0.1,insig = "blank")

## OLS
model_OLS <- lm(wage_ln~educ+age,data=data_selection)
summary(model_OLS)

## Heckman model manually "by hand"
### Estimate the probability to work
model_sel <- glm(working~ age + educ+ndepchld, family=binomial(link="probit"), data=data_selection)
summary(model_sel)
probitmfx(working~ age + educ+ndepchld, data=data_selection)

## Calculate inverse Mills ratio manually
data_selection$linear_predictors_man <- 2.194571-0.044295*data_selection$age+0.011640*data_selection$educ-0.171465*data_selection$ndepchld
data_selection$IMR_man <- dnorm(data_selection$linear_predictors_man)/pnorm(data_selection$linear_predictors_man)
## Calculate inverse Mills ratio by using probit model options - very similar result
data_selection$linear_predictors_aut <- model_sel$linear.predictors
data_selection$IMR_aut <- dnorm(model_sel$linear.predictors)/pnorm(model_sel$linear.predictors)



## Outcome equation correcting for selection - include IMR as additional regressor
model_out <- lm(wage_ln ~ age+educ+ IMR_aut, data=data_selection, subset=(working==1))
summary(model_out)


## Heckman model automatically
library(sampleSelection)
model_heckman <- heckit(working~educ+age+ndepchld,wage_ln~educ+age,data_selection)
summary(model_heckman)

### Selection equation marginal effects after "heckit" function
z_avg <- 2.194590+0.011639*mean(data_selection$educ)-0.044295*mean(data_selection$age)-0.171465*mean(data_selection$ndepchld)
phi <- dnorm(z_avg)
phi*0.011639 #marginal effect for education
phi*-0.044295 #marginal effect for age
phi*-0.171465 #marginal effect for number of dependent children



