#' Summarize Results of baQCA
#'
#' Displays results of baQCA.
#' @import QCA bootstrap utils
#' @param object Object returned by \code{\link{baQCA}}.
#' @param ... Additional parameters to pass on.
#' @return Matrix of values for percent of simulations returning result from random data, along with confidence interval.
#' @examples
#' qca.data <- rallies[,8:13]
#' rownames(qca.data)<-rownames(rallies)
#' truth<-QCA::truthTable(qca.data,outcome="P",sort.by="incl",incl.cut1=0.85,n.cut=1,show.cases=TRUE)
#' mod1 <- QCA::minimize(truth,details=TRUE,show.cases=TRUE)
#' 
#' test <- baQCA(mod1,sim=1) 
#' summary(test)
summary.baQCAtest <- function(object, ...){
  cat("Call:\n")
  print(object$call)
  cat("\n")
  cat("Bootstrapped Assessment Results:\n")
  printCoefmat(object$result)
  cat("\n")
  #cat(paste("Total number of configurations:", object$total.configurations), "\n")
  cat(paste("Total Number of Simulations:", object$total.sims), "\n")
  #cat(paste("Number of Simulations for CI:", object$total.ci.boots), "\n")
  #cat(paste("p-value adjustment method:", object$p.adj.method))
  cat("\n")
}
