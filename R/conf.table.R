#' Configuration Table
#' 
#' Internal function; calculates via logistic regression the output of the Bootstrapped Robustness Recommendation
#' @import QCA bootstrap
#' @importFrom graphics hist
#' @importFrom stats glm plogis predict quantile
#' @importFrom utils flush.console
#' @param data name of the model object; the table of solutions for an application of QCA. Default set to \code{data}.
#' @param ncut configurational n levels for inclusion. Default set to \code{ncut=4}
#' @return The output of the Bootstrapped Recommendation
#' #' @export
conf.table<-function(data, ncut=ncut){
#logistic regression predicting probability of returning a spurious relationship
  if (length(ncut)==1){suppressWarnings(modp<-glm(OUT ~ CTH + CPI, family="binomial", data=data))}
  if (length(ncut)>2){suppressWarnings(modp<-glm(OUT ~ CTH + CNTH + CPI, family="binomial", data=data))}
  
  
data$pred<-predict(modp,data, type="response")

data<- cbind(data, predict(modp, newdata = data, type = "link", se = TRUE))

data$UL<-plogis(data$fit - (1.96 * data$se.fit))
data$LL<-plogis(data$fit + (1.96 * data$se.fit))
                                    

df<-data.frame(sig=c("p < .10","","p < .05","","p < .01","","p < .001",""),solution=rep(c("parsimonious","complex"),4),consistency.score.rec=rep(0,8),lower.CI=rep(0,8),
               upper.CI=rep(0,8))
output<-list(df)[rep(1L, times=length(ncut))]

names(output)<-paste("ncut=",ncut,sep="")

plevels<-c(.10,.05,.01,.001,-Inf)
# & data$pred > plevels[(i +1)]]

for (q in 1:length(ncut)){
  j<-0
for (i in 1:4){

j<-j+1
output[[q]]$consistency.score.rec[j]<-suppressWarnings(min(data$CTH[data$CPI== 0 & data$CNTH == ncut[q] & data$pred < plevels[i]],na.rm=T))
output[[q]]$upper.CI[j]<-suppressWarnings(min(data$CTH[data$CPI== 0 & data$CNTH == ncut[q] & data$LL < plevels[i]],na.rm=T))
output[[q]]$lower.CI[j]<-suppressWarnings(min(data$CTH[data$CPI== 0 & data$CNTH == ncut[q] & data$UL < plevels[i]],na.rm=T))

j<-j+1

output[[q]]$consistency.score.rec[j]<-suppressWarnings(min(data$CTH[data$CPI== 1 & data$CNTH == ncut[q] & data$pred < plevels[i]],na.rm=T))
output[[q]]$upper.CI[j]<-suppressWarnings(min(data$CTH[data$CPI== 1 & data$CNTH == ncut[q] & data$LL < plevels[i]],na.rm=T))
output[[q]]$lower.CI[j]<-suppressWarnings(min(data$CTH[data$CPI== 1 & data$CNTH == ncut[q] & data$UL < plevels[i]],na.rm=T))

}

output[[q]][,3][output[[q]][,3] == Inf]<-NA

output[[q]]$consistency.score.rec[output[[q]]$consistency.score.rec == Inf] <- NA
output[[q]]$lower.CI[output[[q]]$lower.CI == Inf] <- NA
output[[q]]$upper.CI[output[[q]]$upper.CI == Inf] <- NA

}

return(output)
}