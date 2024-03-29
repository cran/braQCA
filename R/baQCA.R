#' Boostrapped Assessment
#' 
#' This function performs the the Bootstrapped Assessment for QCA (baQCA) on a given QCA model object.
#' @import QCA bootstrap
#' @importFrom graphics hist
#' @importFrom stats glm plogis predict quantile
#' @importFrom utils flush.console
#' @param mod name of the QCA eqmcc/minimize model object.
#' @param sim the number of simulations the baQCA function should run. Default set to \code{sim=2000}.
#' @param include [from QCA package] ``A vector of additional output function values to be included in the minimization.'' Default set to \code{include=c("")}.
#' @param row.dom [from QCA package] ``Logical, impose row dominance as constraint on solution to eliminate dominated inessential prime implicants.'' Default set to \code{FALSE}.
#' @param omit [from QCA package] ``A vector of configuration index values or matrix of configurations to be omitted from minimization.'' Default set to \code{omit=c()}.
#' @param dir.exp [from QCA package] ``A vector of directional expectations for deriving intermediate solutions.'' Default set to \code{dir.exp=c()}.
#' @return After some time, this function returns the probability that the data will return a random result given the parameters set by the researcher in the model (configurational n threshold, consistency score threshold, etc), as well a confidence interval around this value. This value is interpreted similarly to a p-value, i.e. a .05 value coincides with a 95\% "confidence level." 
#' @examples 
#' 
#' qca.data <- rallies[,8:13]
#' rownames(qca.data)<-rownames(rallies)
#' truth<-QCA::truthTable(qca.data,outcome="P",sort.by="incl",incl.cut1=0.85,n.cut=1,show.cases=TRUE)
#' mod1 <- QCA::minimize(truth,details=TRUE,show.cases=TRUE)
#' 
#' baQCA(mod1,sim=1) 
#' @export
baQCA<-function(mod, sim=2000, include=c(""), row.dom=FALSE, omit=c(), dir.exp=c() ){
  ptm <- proc.time()
  
  nconf<-rownames(mod$IC$incl.cov) #names of the configuration(s)
  #incl.cut1<-mod$tt$options$incl.cut #consistency score of the configuration 
  #incl.cut0<-mod$tt$options$incl.cut #consistency score of the configuration 
  incl.cut<-mod$tt$options$incl.cut #consistency score of the configuration 
  n.cut<-mod$tt$options$n.cut  #configurational n 
  pop<-dim(mod[[1]]$initial.data)[1] #population size
  #relation<-mod$relation ###what is relation?
  relation<-mod$options$relation
  explain<-mod$options$explain
  
  notconditions<-names(mod[[1]][[1]])== c("OUT") |  names(mod[[1]][[1]])== "n" | names(mod[[1]][[1]])==   "incl" | names(mod[[1]][[1]])== "PRI"  | names(mod[[1]][[1]])== "cases"
  conditions<-names(mod$tt$tt)[!notconditions]
  outcome<-mod$tt$options$outcome
  qca.data<-mod$tt$initial.data
  neg.out<-mod$tt$options$neg.out
  
  rows<-sim*2 #total number of rows
  out<-qca.data[,outcome] #outcome vector
  qca.data<-qca.data[,(names(qca.data) %in% conditions)] #matrix of causal conditions
  data<-data.frame(cname=0,OUT=rep(NA,rows)) #empty data set to simulate into
  
  if (sum(outcome > 0 && outcome < 1) == 0) {
    type = "crisp"}
  
  if (sum(outcome > 0 && outcome < 1) != 0) {
    type = "fuzzy"}
  
  s.qca.data<-do.call("list", replicate(sim, qca.data, simplify = FALSE))
  
  for (j in 1:sim) {
    
    if (type=="crisp"){
      
      for (i in 1:length(qca.data)){ #simulate random causal conditions
        prob<-c(sum(qca.data[,i]==0)/(dim(qca.data)[1]),sum(qca.data[,i]==1)/dim(qca.data)[1]) #match distributions of data set
        s.qca.data[[j]][,i]<-sample(c(0,1),pop,prob=prob,replace=TRUE)} 
      #simulate random outcome variable
      prob<-c(sum(out==0)/(length(out)),sum(out==1)/length(out)) #match distributions of data set
      s.qca.data[[j]]$OUT<-sample(c(0,1),pop,prob=prob,replace=TRUE)
    }
    
    if (type == "fuzzy"){
      
      for (i in 1:length(qca.data)){ #simulate random causal conditions
        ranges<-seq(from=0.1, to=1, by=.1) #better way to do this? could do a for loop
        prob<-hist(qca.data[,i])[[2]]/dim(qca.data)[1]
        s.qca.data[[j]][,i]<-sample(ranges,pop,prob=prob,replace=TRUE)
      } 
      #simulate random outcome variable
      prob<-hist(out)[[2]]/length(out)
      s.qca.data[[j]]$OUT<-sample(ranges,pop,prob=prob,replace=TRUE)
    }
  }
  
  
  {suppressWarnings(confList <- sapply(s.qca.data, function(x){tryCatch( #trap error
    #minimize(x,  outcome=c("OUT"), n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, neg.out=neg.out, relation=relation, explain=mod$options$explain,
          #conditions= c(names(x[,!(names(x) %in% 'OUT')]))),
    minimize(x,  outcome=c("OUT"), n.cut=n.cut, incl.cut=incl.cut, neg.out=neg.out, relation=relation, explain=explain,
          conditions= c(names(x[,!(names(x) %in% 'OUT')]))),
    error=function(e) e
  )}))}
  
  pars<-rep(NA, sim)
  pars[sapply(confList, function(x) !inherits(x,"error"))]<-1
  pars[sapply(confList, function(x) inherits(x,"error"))]<-0
  #errs<-sapply(confList, function(x) grepl("Nothing to explain",x) |  grepl("All combinations have been included into analysis",x))
  #pars[sapply(errs, function(x) sum(x))==1]<-0
  
  results<-mean(pars, na.rm=TRUE)
  
  bsp<-bootstrap(pars,mean,nboot=1000)[[1]]
  confInt<-quantile(bsp, prob=c(0.05, .95)) 
  
  returnme<-list(results, confInt)
  names(returnme)<-c("Probability","Confidence Interval")
  return(returnme)
  
  #this is where I should assign values to a ltQCA "class"
  
}

