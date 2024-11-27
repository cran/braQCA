#' Boostrapped Assessment
#' 
#' This function performs the the Bootstrapped Assessment for QCA (baQCA) on a given QCA model object.
#' @import QCA bootstrap
#' @importFrom graphics hist
#' @importFrom stats glm plogis predict quantile
#' @importFrom utils flush.console
#' @param mod name of the QCA model object -- the minimization of the truth table.
#' @param sim the number of simulations the baQCA function should run. Default set to \code{sim=2000}.
#' @param all logical, whether or not causal conditions AND outcome should be resampled (with replacement). Default set to \code{all=TRUE}.
#' @param include [from QCA package] ``A vector of additional output function values to be included in the minimization.'' Default set to \code{include=c("")}.
#' @param row.dom [from QCA package] ``Logical, impose row dominance as constraint on solution to eliminate dominated inessential prime implicants.'' Default set to \code{FALSE}.
#' @param omit [from QCA package] ``A vector of configuration index values or matrix of configurations to be omitted from minimization.'' Default set to \code{omit=c()}.
#' @param dir.exp [from QCA package] ``A vector of directional expectations for deriving intermediate solutions.'' Default set to \code{dir.exp=c()}.
#' @return This function returns a value which is the probability of a random QCA result (e.g. a result from random data) given the parameters set by the researcher in the model (configurational n threshold, consistency score threshold, etc), and a confidence interval around this value. This value is interpreted similarly to a p-value." 
#' @examples 
#' 
#' qca.data <- rallies[,8:13]
#' rownames(qca.data)<-rownames(rallies)
#' truth<-QCA::truthTable(qca.data,outcome="P",sort.by="incl",incl.cut1=0.85,n.cut=1,show.cases=TRUE)
#' mod1 <- QCA::minimize(truth,details=TRUE,show.cases=TRUE)
#' 
#' summary(baQCA(mod1,sim=1))
#' @export
baQCA <- function(mod, sim=2000, all=TRUE, include=c(""), row.dom=FALSE, omit=c(), dir.exp=c() ){
  ptm <- proc.time()
  set.seed(1738)
  enter <- ""
  
  nconf <- rownames(mod$IC$incl.cov) #names of the configuration(s)
  #print(paste0("list of configurations: ",nconf))
  incl.cut<-mod$tt$options$incl.cut #consistency score of the configuration
  #print(paste0("consistency score threshold: ",incl.cut))
  n.cut <- mod$tt$options$n.cut  #configurational n 
  #print(paste0("configurational n/cases per configurattion threshold: ",n.cut))
  pop <- dim(mod[[1]]$initial.data)[1] #population size
  #print(paste0("population/size of original data: ",pop))
  relation <- mod$options$relation
  #print(paste0("type of relation (sufficiency or necessity): ",relation))
  explain <- mod$options$explain
  #print(paste0("explain?: ",explain))
  
  #start.time <- Sys.time()
  notconditions <- names(mod[[1]][[1]])== c("OUT") |  names(mod[[1]][[1]])== "n" | names(mod[[1]][[1]])==   "incl" | names(mod[[1]][[1]])== "PRI"  | names(mod[[1]][[1]])== "cases"
  #end.time <- Sys.time()
  #time.taken <- end.time - start.time
  #print(time.taken)
  conditions <- names(mod$tt$tt)[!notconditions]
  outcome <- mod$tt$options$outcome
  qca.data <- mod$tt$initial.data
  neg.out <- mod$tt$options$neg.out
  
  rows <- sim*2 #total number of rows
  out <- qca.data[,outcome] #outcome vector
  qca.data <- qca.data[,(names(qca.data) %in% conditions)] #matrix of causal conditions
  data <- data.frame(cname=0,OUT=rep(NA,rows)) #empty data set to simulate into
  
  if (sum(outcome > 0 && outcome < 1) == 0) {
    type = "crisp"}
  
  if (sum(outcome > 0 && outcome < 1) != 0) {
    type = "fuzzy"}
  
  s.qca.data <- do.call("list", replicate(sim, qca.data, simplify = FALSE))
  
  #start.time <- Sys.time()
  for (j in 1:sim) {
    #print(paste0(j,"/",sim))
    
    if (type=="crisp"){
      
      
      #simulate random causal conditions
      if (all == TRUE){
        for (i in 1:length(qca.data)){ 
          prob <- c(sum(qca.data[,i]==0)/(dim(qca.data)[1]),sum(qca.data[,i]==1)/dim(qca.data)[1]) #match distributions of data set
          #print(prob) #prob of 0s in causal condition i, prob of 1s in causal condition i
          s.qca.data[[j]][,i] <- sample(c(0,1),pop,prob=prob,replace=TRUE)
          #print(s.qca.data) #simulated causal conditions based on probs of each causal condition i in the data set
        }
      }
      #use original matrix of causal conditions
      else {
        s.qca.data <- s.qca.data 
      }
      
      #simulate random outcome variable
      prob <- c(sum(out==0)/(length(out)),sum(out==1)/length(out)) #match distributions of data set
      s.qca.data[[j]]$OUT <- sample(c(0,1),pop,prob=prob,replace=TRUE)
      
    }
    
    if (type == "fuzzy"){
      
      #simulate random causal conditions
      if (all == TRUE){
        for (i in 1:length(qca.data)){ 
          ranges<-seq(from=0.1, to=1, by=.1) #better way to do this? could do a for loop
          prob<-hist(qca.data[,i])[[2]]/dim(qca.data)[1]
          s.qca.data[[j]][,i]<-sample(ranges,pop,prob=prob,replace=TRUE)
        } 
      }
      #use original matrix of causal conditions
      else {
        s.qca.data <- s.qca.data 
      }
      
      #simulate random outcome variable
      prob<-hist(out)[[2]]/length(out)
      s.qca.data[[j]]$OUT<-sample(ranges,pop,prob=prob,replace=TRUE)
    }
  }
  
  #end.time <- Sys.time()
  #time.taken <- end.time - start.time
  #print(paste0("simulating data set: ",time.taken))
  
  #start.time <- Sys.time()
  #{suppressWarnings(
  confList <- sapply(s.qca.data, function(x){ tryCatch( {#trap error
    #minimize(x,  outcome=c("OUT"), n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, neg.out=neg.out, relation=relation, explain=mod$options$explain,
    #conditions= c(names(x[,!(names(x) %in% 'OUT')]))),
    modconfigs <- minimize(x,  outcome=c("OUT"), n.cut=n.cut, incl.cut=incl.cut, neg.out=neg.out, relation=relation, explain=explain, enter=enter,
                            conditions= c(names(x[,!(names(x) %in% 'OUT')]))) 
    #print(modconfigs$initials)
  }
  , error=function(e) e
  )
  })
  #)}
  
  #end.time <- Sys.time()
  #time.taken <- end.time - start.time
  #print(paste0("running QCA on data sets: ",time.taken))
  
  pars<-rep(NA, sim)
  pars[sapply(confList, function(x) !inherits(x,"error"))]<-1
  pars[sapply(confList, function(x) inherits(x,"error"))]<-0
  #errs<-sapply(confList, function(x) grepl("Nothing to explain",x) |  grepl("All combinations have been included into analysis",x))
  #pars[sapply(errs, function(x) sum(x))==1]<-0
  #print(pars)
  
  results<-mean(pars, na.rm=TRUE)
  
  bsp<-bootstrap::bootstrap(pars,mean,nboot=1000)[[1]]
  #confInt<-quantile(bsp, prob=c(0.05, .95)) 
  confInt <- quantile(bsp, prob=c(0.025, .975)) 
  ciboots <- 1000
  
  out.mat <- matrix(, nrow = 1, ncol = 3)
  out.mat[,1] <- results
  out.mat[,2] <- confInt[1]
  out.mat[,3] <- confInt[2]
  rownames(out.mat) <- "QCA Model"
  colnames(out.mat) <- c("p", "Lower CI (2.5%)", "Upper CI (97.5%)")
  
  
  #returnme<-list(results, confInt)
  #names(returnme)<-c("Probability","Confidence Interval")
  #return(returnme)
  
  
  return.obj <- list(call = match.call(), total.sims = sim, total.ci.boots = ciboots, result = out.mat)
  class(return.obj) <- "baQCAtest"
  return.obj
  
}
