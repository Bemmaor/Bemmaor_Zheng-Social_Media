library(data.table)
## library(tikzDevice)
options(deparse.max.lines=100)


library(gtools)


select_if <- function(lst,test_fn) {
    ## DD . char or list
    if (class(lst)=="character"){
        unlist(lapply(lst ,function(x){if(test_fn(x)) x }))
    } else {
       lst[sapply(lst ,function(x){unlist(test_fn(x)) })]
    }
}


remove_if <- function(lst,test_fn) {
    ## DD . char or list
    if (class(lst)=="character"){
        unlist(lapply(lst ,function(x){if(!(test_fn(x))) x }))
    } else {
        ##:ess-bp-start::browser@nil:##
        
       lst[sapply(lst ,function(x){unlist(!test_fn(x)) })]
    }
}

alter_if <- function(lst,fn,mod){
## DD 
    ## mod function modifying the data
    ## fn  function testing criteria
    ## list

### purpose
    ## alter the list base on a criteria
    
    myfun  <- function(x){
        if(fn(x)){
            mod(x)
        } else {x
            }
        
        }
    lapply(lst,myfun)

    }




lapply_with_error <- function(X,FUN,...){
    
  lapply(X, function(x, ...) tryCatch(FUN(x, ...),
                                      error=function(e) NULL))
}

flapply <- function(error=NULL){
    lapply_with_error <- function(X,FUN,...){    
        lapply(X, function(x, ...) tryCatch(FUN(x, ...),
                                      error=function(e) error))
}

    }




condlapply <- function(lst, crit, fn1, fn2){
    lapply(lst, function(x) if(crit(x)) {
                                fn1(x)} else {fn2(x)})
    
    }

classp <- function(classname){
    function(x){
        any(class(x) %in% classname)
        }
    }


remove_if_null <- function(lst) {
    lst[sapply(lst,function(x) !is.null((unlist(x))))]
    }



getfnname <- function(y) as.character(print(match.call()[2]))

with_error <- function(fn,...){
   res  <- tryCatch(fn(...),
                    error=function(e) NULL)

   res
             
    }


gret <- function(pattern,text,ignore.case=TRUE){
    ## purpose
    ## return the sub string matched by the regular expression. More general that grep with return systematicaly the whole expression.
    
    regmatches(text,regexpr(pattern,text,perl=TRUE,ignore.case))
 }



str_split_as_char <-  function(x,split){
    x <- as.character(x)        
    remove_if(unique(unlist(strsplit(x,split))),is.null)

}

length_formula  <- function(y){
    ## purpose
    ### approximate the length of formula  (no character variables)
    
    ## DD
### y . formula
    length(remove_if(str_split_as_char(y," +"),function(x) grepl("\\+|\\-1|~",x)))
    }


finite  <- function(x){
    ## DD
    ### x numeric   
    apply(as.matrix(x),1,function(y) if (is.infinite(y)) {y <- NA; return(y)} else{ y})
 }



not <- function(boolean){
    !boolean
    }



split_string <- function(x,str){
    len <- length(strsplit(str,"")[[1]])
    c(substr(str,0,regexpr(x,str)),
    substr(str,regexpr(x,str),len))
    }


# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(format(utils::object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

pvalue <-  function(par,sd,nobs){
                                        # compute p-value    
    2*pt(-abs(par/sd),(nobs-1))
    }


                                        # my macro
get.packages <- defmacro(x,
                         expr={
                             install.packages(deparse(substitute(x)),repos='http://cran.us.r-project.org')
                             library(x)})




dtby <- defmacro(df,y,by,expr=df[,.(y),.(by)])

seqList <- function(character,by= 1,res=list()){
    ### sequence a list of characters by
    if (length(character)==0){
        res
    } else{
        seqList(character[-c(1:by)],by=by,res=c(res,list(character[1:by])))

    }
    }


`.l`<-defmacro(x,body,expr={function(x) {body}})





mytc <- defmacro(expression, error ="simple error", expr={
    tryCatch(
        expression,
        error=function(e){           
            cat('===>',paste(e),"===>", error ,"\n\n")
            browser()
            T
        })})

## mytc <- function(expr,error="noerror"){
##     tryCatch({
##         eval(substitute(expr))},
##         error=function(e){
           
##             cat('===>',paste(e),"\n" , error,"\n")
##             browser()
##             T
##         }
##         )


isnot.na <- function(x){
    !is.na(x)
}


twodp <- function(x)
sprintf("%.2f",x)



new_counter <- function() {
  i <- 0
  function() {
    # do something useful, then ...
    i <<- i + 1
    i
  }
}



"%notin%" <- function(x, table) !(match(x, table, nomatch = 0) > 0)


nlapply <- function(X,FUN,name="res",...){
    ## DD
    ## names . if length name=1 then generic res1 ,res2,
    ## else names apply name directly
    
    res <- lapply(X, function(x, ... ) FUN(x,...))
    if(length(name)==1){
        names(res) <- paste0(name,1:length(res))
    }else{
        names(res) <- name
        }
    res
}


count <- function(x,...) sum(!is.na(x))


mysummary <- function(x){
    a <- lapply(list(min,mean,median,max,count),function(y) y(x,na.rm=TRUE))
}



m.summary <- defmacro(dt,lst,expr={
    # lst character of the column names
    xtable(setcolorder(
        dcast(melt(cbind(var=c("min","mean","median","max","nobs"),dt[,lapply(.SD,mysummary), .SDcols=lst]),id.vars="var") ,
              variable~var),
        c(1,5,3,4,2,6)),caption="Descriptive Statistics")
    })



pxtable.type <- function(type,scalebox=1) {
pxtable <- function(obj,caption.placement="top",table.placement="!htbp",...){
    ## DD
### obj . object of class xtable
### type . character latex|html
    ## purpose
### print an xtable in latex or html
    
    if(!(class(obj) %in% "xtable")){
        stop(cat("object of class ", class(obj), '. Expected xtable'))
    }
    print.xtable(obj, type=type,comment=FALSE, sanitize.text.function=function(x) x,
                 caption.placement=caption.placement,table.placement=table.placement,scalebox=scalebox,...)
    
}
pxtable
}

ouput <- pxtable.type("latex",1)

q <- defmacro(X,expr={
    as.character(quote(X))[-1]
})

## qc <- defmacro(X="DOTS",expr={
##     c(as.character(quote(X))[-1])
##            })




factorize <- defmacro(x,expr={
    as.factor(x)
})



checkcol <- function(data,char){
    char[!(char %in%colnames(data))]
    }




mytc_length <- function(expr){
    if(length(eval(substitute(expr)))>0){
            cat('===>',eval(substitute(expr),"\n error location\n"))
            browser()
        }
        
}


subdt <- defmacro(dt,pattern,expr={
dt[,.SD,.SDcols=grep(pattern,colnames(dt),value=TRUE)]
})


sum.na <- function(x){
    ifelse(all(is.na(x)),1.9*NA,sum(x,na.rm=TRUE))
    }


expect_error <- defmacro(fn1,fn2,expr={
    ##:ess-bp-start::browser@nil:##
    
    tryCatch({fn1(x)},
             error=function(e) {mytc(fn2(x))}
)})
########

# pure macro
######


#cnt <- new_counter()

## testm <- defmacro(lst,expr={
##   unlist(lapply(quote(lst), function(x)  assign(paste0("addtorow$pos[[",cnt(),"]]"),x)))})

## addtorow$command <- c("& \\multicolumn{3}{c}{No Country} & \\multicolumn{3}{c}{Country} & \\multicolumn{5}{c}{Mundlak} \\\\\n",
##                       "\\toprule\\rowfont[c]\\bfseries  variables & estim & APE & LL & estim & APE & LL & X$_{it}$ & APE X$_{it} & X$_{i} & APE X$_{i}$ & LL  \\\\  \\midrule \\endhead \n") 


u <- function(f){
    unique(f)
    }


grayheat.colors <- function (n, alpha = 1,s=0) {
    # s for saturation
    if ((n <- as.integer(n[1L])) > 0) {
        j <- n%/%4
        i <- n - j
        c(rainbow(i, start = 0, end = 1/6, alpha = alpha), if (j > 
            0) hsv(h = 1/6, s = 0, v = 1, alpha = alpha))
    }
    else character()
}

fnstring <- function(myfun) {
### purpose
    ## get the string name of the function
    as.character((substitute(myfun)))
    }

orderthis <- function(a){
    a[order(a)]

    }
