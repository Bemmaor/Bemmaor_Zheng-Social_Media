install.packages("data.table")
library(data.table)
#path <- "C:/Users/buslzhe/Downloads/RA-JJ-Code (1)/RA-JJ-Code/"
#commentaire rochdi
path <- "P:/CasEtDon/Mkg/Bac2/RA JJ/RA-JJ/RA-JJ-Code/"
setwd(path)
source("utilities.R")
source("myfunctions.R")
#dt1 <- fread("Albert.csv")
#dt <- fread("Bemmaor.csv")
dt1 <- fread(paste(path,"Albert.csv",sep=""))
dt <- fread(paste(path,"Bemmaor.csv",sep=""))


format_data <- function(dt){
    setnames(dt,1,"Country")
    melt(dt,id="Country",variable.name="Date",value.name="adopters")[,Date:=as.Date(Date)]
}
data  <- Reduce(rbind,lapply(list(dt,dt1),format_data))

setkey(data,Country,Date)
data[,adopters:=adopters][,Dadopters:=c(0,diff(adopters))/1000000,.(Country)]
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

estim <- function(ctry,fn){
### Data Definition
    ## ctry interp. string one of "France|UK|Germany|US"
    ## fn iterp. function one of bass|SG|G_SG

### Purpose
    ## perform non-linear least square on data using one of bass|SG|G_SG
    
    if (!(identical(fn,dG_SG2))){
        ##:ess-bp-start::browser@nil:##
        
 res <- nls(Dadopters~m*fn(mon,p,q),start=list(m=2,p=0.1,q=0.2),
               data=data[Country==ctry,],algorithm = "port",lower=c(0.00001,0.0000001,0.0000001),
            trace=FALSE,na.action="na.exclude",control=nls.control(maxiter = 500) )
 
} else {
    ##:ess-bp-start::browser@nil:##
    
    res <- nlsLM(Dadopters~m*fn(mon,p,q,alpha),start=list(m=8,p=0.1,q=0.2,alpha=0.4),
               data=data[Country==ctry,],algorithm = "port",lower=c(0.00001,0.0000001,0.0000001,0.000001),
               trace=FALSE,na.action="na.exclude",control=nls.control(maxiter = 500) )
}
    
}


mycreateTexreg <- function(x){
### Data Definition
    ## x . class summary.nls
### Purpose
    ## format results for output
    ## format function
    
    aa <- x$coeff    
    createTexreg(rownames(aa),aa[,1],aa[,2],aa[,4],gof.names ="Nobs",
                 gof =length(x$residuals),gof.decimal=FALSE)
    }

ctry <- orderthis(unique(data[,Country]))
res <- lapply(list(dbass,dSG,dG_SG,dG_SG2), function(x) {(lapply_with_error(ctry,function(y) estim(y,x)))})
names(res) <- c("dbass", "dSG", "dG_SG", "dG_SG2")
res <- lapply(res,function(x)setNames(x,ctry))
res <- lapply(res,remove_if_null)



modelselect <- function(x){
paste("Model",c("Bass","SG","G-SG $\\alpha=0.5$","G-SG")[x])
}

cnt <- new_counter()
res1<- lapply(res, function(x) {
    mytexreg(lapply(x,function(y) mycreateTexreg(summary(y))),threeparttable=TRUE,custom.model.names=names(x),
             digits=4,stars = c(0.01, 0.05, 0.1),caption=modelselect(cnt()),caption.above=TRUE,
             mynotes="\\scriptsize{$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$}. ``m\'\' represents the estimation of the market size in million of users")})

aa <- lapply(res1,cat,append=TRUE)

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

    
    if (!(identical(fn,dG_SG2))){


 res <- nlsLM(Dadopters~m*fn(mon,p,q),start=list(m=2,p=0.1,q=0.2),
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
 
} else {
    
    res <- nlsLM(Dadopters~m*fn(mon,p,q,alpha),start=list(m=8,p=0.1,q=0.2,alpha=1),
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
    
    abs((ytstep-yhat)/ytstep)*100
    
}



loop2 <- function(type="naive"){
lapply(ctry,
           function(ctry){
               lapply(1:18,function(y)
               (unlist(lapply(33:(67-y),function(x) naive(ctry,size=x,step=y,type)))))})

}


aa <-  lapply(list(dbass,dSG,dG_SG,dG_SG2), function(fun) {lapply(ctry,
                                                 function(ctry){
                                                     lapply(1:18,function(y)
                                                     (unlist(lapply(33:(67-y),function(x) estim2(x,ctry,fun,y)))))})})


med <- lapply(aa,function(x) lapply(x,function(y) rapply(y,median)))
Gmean <- lapply(aa,function(x) lapply(x,function(y) rapply(y,function(y) prod(y)^(1/length(y)))))


naive_on_trend <- loop2()
drift_seasonal <- loop2(type="seasonal")


res_naive_on_trend <- lapply(naive_on_trend,function(x)  cbind(rapply(x,median),rapply(x,function(y) prod(y)^(1/length(y)))))

res_drift_seasonal <- lapply(drift_seasonal,function(x)  cbind(rapply(x,median),rapply(x,function(y) prod(y)^(1/length(y)))))

putnames <- function(med){
    names(med)  <- gsub("d","",modelnames)
    res <- lapply(med,function(x)setNames(x,ctry))
}


med <- putnames(med)
Gmean <- putnames(Gmean)

names(res_naive_on_trend) <- ctry
names(res_drift_seasonal) <- ctry

tmp <- nlapply(ctry,function(y) setnames(cbind(c(paste0((1:18)," (",34:17,")"),"pooled"),
                                               data.table(cbind(Reduce(cbind,Map(function(x,z){cbind(c(x[[y]],median(x[[y]])),c(z[[y]],median(z[[y]])))},
                                                                                 x=med,z=Gmean)),
                                                                rbind(res_naive_on_trend[[y]],median(res_naive_on_trend[[y]])),
                                                                rbind(res_drift_seasonal[[y]],median(res_drift_seasonal[[y]]))
                                                                ))),c("forecast",rep(c("Med","Q-mean"),6))),ctry)

addtorow <- list()
addtorow$pos <- list()

addtorow$pos[[1]] <- 0
addtorow$pos[[2]] <- 0
addtorow$pos[[3]] <- 0
addtorow$pos[[4]] <- 0


addtorow$command <-c( " \\toprule",
                     " &\\multicolumn{2}{c}{Bass}&\\multicolumn{2}{c}{SG}&\\multicolumn{2}{c}{G-SG $\\alpha=0.5$}&\\multicolumn{2}{c}{G-SG}& \\multicolumn{2}{c}{naive trend}&\\multicolumn{2}{c}{seasonal}\\\\\n",
                     "\\cmidrule(rl){2-3}\\cmidrule(rl){4-5}\\cmidrule(rl){6-7}\\cmidrule(rl){8-9}\\cmidrule(rl){10-11}\\cmidrule(rl){12-13}",
                     "forecast & Med & Q-mean & Med & Q-mean & Med & Q-mean & Med & Q-mean & Med & Q-mean & Med & Q-mean \\\\\n")

aa <- rep(c("France", "Germany", "Italy", "Spain", "UK", "USA"),each=2)
cat(aa[cnt()],"\n")

str <- "Italy"
cat("Forcasting accuracy levels",str,"\n")

no <- "France"
print.xtable(xtable(tmp[[no]]),floating = FALSE,add.to.row=addtorow,booktabs=TRUE,include.colnames=FALSE,comment=FALSE,include.rownames=FALSE)
