#' QCA Case Difference
#' 
#' Provides a data frame of all cases removed from the final QCA model object, after applying recommendations from brQCA.
#' @import QCA bootstrap 
#' @importFrom dplyr filter select count %>% n
#' @param x name of one QCA model object.
#' @param y name of a second QCA model object.
#' @param show Logical, use to show the cases in the solution set for each QCA model object. Default set to \code{show=F}.
#' @return Shows the cases excluded/removed from the final, more robust QCA model object.
#' @examples
#' data(rallies)
#' P<-rallies$P
#' R<-rallies$R
#' C<-rallies$C
#' B<-rallies$B
#' 
#' qca.data<-data.frame(P,R,C,B)
#' rownames(qca.data)<-rownames(rallies)
#' truth<-truthTable(qca.data,outcome="P",sort.by="incl",incl.cut1=0.2,show.cases=TRUE,n.cut=1)
#' mod1<-minimize(truth,details=TRUE,show.cases=TRUE)
#' mod1
#' 
#' truth2<-truthTable(qca.data,outcome="P",sort.by="incl",incl.cut1=0.7,show.cases=TRUE,n.cut=3)
#' mod2<-minimize(truth2,details=TRUE,show.cases=TRUE)
#' mod2
#' 
#' QCAdiff(mod1,mod2,show=FALSE)
#' @export
QCAdiff<-function(x, y, show=FALSE){
  tt1<-x$tt$cases
  tt2<-y$tt$cases
  tt<-c(tt1,tt2)
  tt<-as.data.frame(tt)
  tt<-tt %>% count(tt)
  colnames(tt)<-c("tt","n")
  tt<-tt %>% filter(n==1)
  tt<-tt %>% select(1)
  tt<-strsplit(as.character(tt$tt),",")
  tt<-Reduce(c,tt)
  #tt<-as.data.frame(as.numeric(tt))
  tt<-as.data.frame(tt)
  tt1<-as.data.frame(tt1)
  tt2<-as.data.frame(tt2)
  names(tt)<-"Cases Excluded"
  names(tt1)<-paste("Cases in",deparse(substitute(x)),"Truth Table") #to insert the name of the model object
  names(tt2)<-paste("Cases in",deparse(substitute(y)),"Truth Table") #to insert the name of the model object
  #names(tt)<-cat("Cases\nExcluded") #to add spaces
  if (show==T){
    return(list(tt1,tt2,tt))
    }
  else{
    return(tt)
    }
}

