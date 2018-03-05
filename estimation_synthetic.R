# This code computes the one-month ahead to 18-month ahead forecasts with (i) the Bass model, (ii) the G/SG(alpha = 0.5), (iii) the Shifted Gompertz (SG) model, (iv) the G/SG(alpha), (v) the naive model with a linear trend and (vi) the model with seasonal effect, and the corresponding absolute percentage errors (ape's). It uses a synthetic data set with 67 data points. 
# Bemmaor, Albert C. and Li Zheng (2018), "The Diffusion of Mobile Social Networking: Further Study," ESSEC Business School, Cergy-Pontoise. 

install.packages("data.table")
library(data.table)
source(file.choose()) ##Open "utilities.R" file
source(file.choose()) ##Open "myfunctions.R" file
dt <- fread(file.choose()) ##Open "synthetic_data_set.csv" file



format_data <- function(dt){
  setnames(dt,1,"Country")
  melt(dt,id="Country",variable.name="Date",value.name="adopters")[,Date:=as.Date(Date)]
}
data  <- Reduce(rbind,lapply(list(dt),format_data))

setkey(data,Country,Date)
data[,adopters:=adopters][,Dadopters:=c(adopters[1],diff(adopters))/1000000,.(Country)]
data[,adopters0:=adopters]
data[,adopters:=adopters/1000000]

data[,mon:=1:(.N),Country]
data[,lmon:=mon-1]

dbass <- function(t,p,q){
  ### DD
  ## t . time value
  ## p . q  parameter of the bass >0
  ### Purpose
  ## Compute the difference of the bass F(t)-F(t-1)
  a <- -(p+q)
  b <- (q/p)
  bass <-  function(t){
    (1-exp(a*t))/(1+b*exp(a*t))
  }
  if(is.null(t)){
    return(bass)
  }
  
  bass(t)-bass(t-1)
}

dSG <- function(t,p,q){
  ### DD
  ## t . time value
  ## p . q  parameter of the SG >0
  
  ### Purpose
  ##
  a <- -(p+q)
  b <- (q/p)
  
  SG <-  function(t){
    (1-exp(a*t))*(1+b)^(-exp(a*t))
  }
  
  if(is.null(t)){
    return(SG)
  }
  
  
  SG(t)-SG(t-1)
  
}


dG_SG<- function(t,p,q){
  ### DD
  ## t . time value
  ## p . q  parameter of the SG >0
  
  ### Purpose
  ##
  a <- -(p+q)
  b <- (q/p)
  
  G_SG <-  function(t){
    (1-exp(a*t))/(1+b*(2+b)*exp(a*t))^0.5
  }
  if(is.null(t)){
    return(G_SG)
  }
  
  G_SG(t)-G_SG(t-1)
  
} 


dG_SG2<- function(t,p,q,alpha){
  ### DD
  ## t . time value
  ## p . q  parameter of the SG >0
  
  ### Purpose
  ##  Gamma-shifted Goempertz with free alpha
  a <- -(p+q)
  b <- (q/p)
  c <- alpha
  d <- 1/alpha
  G_SG2 <-  function(t){
    (1-exp(a*t))/(1+ (((1+b)^d)-1)*exp(a*t))^c
  }
  if(is.null(t)){
    return(G_SG2)
  }
  
  G_SG2(t)-G_SG2(t-1)
  
} 
modelnames <- c("dbass", "dSG", "dG_SG a=0.5", "dG_SG2")

ctry <- orderthis(unique(data[,Country]))

estim2 <- function(size,ctry,fn,step,type=NULL){
  ### Data Definition
  ## size interp. integer size of train dataset
  ## ctry interp. string one of "France|UK|Germany|US"
  ## fn interp. function one of bass|SG|G_SG
  ## step interp. integer step of forecast
  
  ### Purpose
  ## perform non-linear least square on data using one of bass|SG|G_SG|G_SG2 with variable sample
  ##:ess-bp-start::browser@nil:##
  
  dt <- data[Country==ctry,]
  
  
  if (identical(fn,dbass)){
    
    
    res <- nlsLM(Dadopters~m*fn(mon,p,q),start=list(m=300,p=0.01,q=0.2),
                 data=dt[1:size,],lower=c(0.00001,0.0000001,0.0000001),
                 trace=FALSE,na.action="na.exclude",control=nls.control(maxiter = 1000,tol=1e-05,minFactor = 1/1024))
    
    
    
    coef <- summary(res)
    mhat <- coef$coefficients[1]
    phat <- coef$coefficients[2]
    qhat <- coef$coefficients[3]
    
    
    
    nhat <- mhat*fn(NULL,phat,qhat)(size+step)
    n <- dt[(size+step),adopters]
    
    
    ape <- abs((n-nhat)/n)*100
    return(ape)
    
  }
  else if (identical(fn,dSG)){
    
    
    res <- nlsLM(Dadopters~m*fn(mon,p,q),start=list(m=1300,p=0.001,q=0.01),
                 data=dt[1:size,],lower=c(0.00001,0.0000001,0.0000001),
                 trace=FALSE,na.action="na.exclude",control=nls.control(maxiter = 1000,tol=1e-05,minFactor = 1/1024))
    
    
    
    coef <- summary(res)
    mhat <- coef$coefficients[1]
    phat <- coef$coefficients[2]
    qhat <- coef$coefficients[3]
    
    
    
    nhat <- mhat*fn(NULL,phat,qhat)(size+step)
    n <- dt[(size+step),adopters]
    
    
    ape <- abs((n-nhat)/n)*100
    return(ape)
    
  }
  else if (identical(fn,dG_SG)){
    
    
    res <- nlsLM(Dadopters~m*fn(mon,p,q),start=list(m=300,p=0.01,q=0.1),
                 data=dt[1:size,],lower=c(0.00001,0.0000001,0.0000001),
                 trace=FALSE,na.action="na.exclude",control=nls.control(maxiter = 1000,tol=1e-05,minFactor = 1/1024))
    
    
    
    coef <- summary(res)
    mhat <- coef$coefficients[1]
    phat <- coef$coefficients[2]
    qhat <- coef$coefficients[3]
    
    
    
    nhat <- mhat*fn(NULL,phat,qhat)(size+step)
    n <- dt[(size+step),adopters]
    
    
    ape <- abs((n-nhat)/n)*100
    return(ape)
    
  }
  else {
    
    res <- nlsLM(Dadopters~m*fn(mon,p,q,alpha),start=list(m=300,p=0.01,q=0.1,alpha=1),
                 data=dt[1:size,],lower=c(0.00001,0.0000001,0.0000001,0.000001),
                 trace=FALSE,na.action="na.exclude",control=nls.control(maxiter = 1000) )
    
    
    coef <- summary(res)
    mhat <- coef$coefficients[1]
    phat <- coef$coefficients[2]
    qhat <- coef$coefficients[3]
    qalpha <- coef$coefficients[4]
    
    
    nhat <- mhat*fn(NULL,phat,qhat,qalpha)(size+step)
    n <- dt[(size+step),adopters]
    
    
    ape <- abs((n-nhat)/n)*100
    return(ape)
    
  }
  
}

naive <- function(ctry,size,step,type="naive"){
  ### Purpose
  ## Compute the naive forecast error naive trend and drift over seasonal differences
  ### DD
  ## ctry interp. string one of (France, Germany, Italy, Spain, UK, USA)
  ## size interp . integer size of train dataset
  ## step interp. integer step of forecast
  ## type iterp . "naive" for naive trend "else" drift over seasonal differences
  
  dt <- data[Country==ctry]
  
  if (type=="naive"){
    theta <- sum(dt[1:size,diff(adopters0,1)])/(size-1)
  } else{
    theta <- sum(dt[1:size,diff(adopters0,12)])/(size-12)
  }
  
  yt <- dt[size,adopters0]
  ytstep <- dt[size+step,adopters0]
  yhat <- yt+(theta*step)
  
  return(abs((ytstep-yhat)/ytstep)*100)
  
}



loop2 <- function(type="naive"){
  lapply(ctry,
         function(ctry){
           lapply(1:18,function(y)
             (unlist(lapply(32:(67-y),function(x) naive(ctry,size=x,step=y,type)))))})
  
}


aa <-  lapply(list(dbass,dSG,dG_SG,dG_SG2), function(fun) {lapply(ctry,
                                                                  function(ctry){
                                                                    lapply(1:18,function(y)
                                                                      (unlist(lapply(c(32:(67-y)),function(x) estim2(x,ctry,fun,y)))))})})



med <- lapply(aa,function(x) lapply(x,function(y) rapply(y,median)))
Gmean <- lapply(aa,function(x) lapply(x,function(y) rapply(y,function(y) prod(y)^(1/length(y)))))


naive_on_trend <- loop2()
drift_seasonal <- loop2(type="seasonal")

res_naive_on_trend <- lapply(naive_on_trend,function(x)  cbind(rapply(x,median),rapply(x,function(y) prod(y)^(1/length(y)))))

res_drift_seasonal <- lapply(drift_seasonal,function(x)  cbind(rapply(x,median),rapply(x,function(y) prod(y)^(1/length(y)))))

putnamesmed <- function(med){
  names(med)  <- paste(gsub("d","",modelnames),"med",sep=".")
  res <- lapply(med,function(x)setNames(x,ctry))
}

putnamesGmean <- function(med){
  names(med)  <- paste(gsub("d","",modelnames),"Gmean",sep=".")
  res <- lapply(med,function(x)setNames(x,ctry))
}

med <- putnamesmed(med)
Gmean <- putnamesGmean(Gmean)

names(res_naive_on_trend) <- ctry
names(res_drift_seasonal) <- ctry

Geomean <- function(vect){
  return(prod(vect)^(1/length(vect)))
}

tmp <- nlapply(ctry,function(y) setnames(cbind(c(paste0((1:18)," (",35:18,")"),"pooled"),
                                               data.table(cbind(Reduce(cbind,Map(function(x,z){cbind(c(x[[y]],median(x[[y]])),c(z[[y]],Geomean(z[[y]])))},
                                                                                 x=med,z=Gmean)),
                                                                rbind(res_naive_on_trend[[y]],c(median(res_naive_on_trend[[y]][,1]),Geomean(res_naive_on_trend[[y]][,2]))),
                                                                rbind(res_drift_seasonal[[y]],c(median(res_drift_seasonal[[y]][,1]),Geomean(res_drift_seasonal[[y]][,2])))))),c("forecast",rep(c("Med","Q-mean"),6))),ctry)

write.csv2(tmp$USA1,"/Users/MacBook-Jack/Documents/ESSEC/BB/RA-JJ/RA-JJ/APE_synthetic.csv",
           row.names = F)
