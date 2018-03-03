library(texreg)
library(minpack.lm)
library(xtable)


mymodelnames <- function (model.list, tr.objects, model.names) 
{
    if (class(model.list)[1] != "list") {
        model.list <- list(model.list)
    }
    mnames <- names(model.list)
    if (is.null(mnames)) {
        mnames <- character(length(model.list))
        if (length(mnames) != length(tr.objects)) {
            mnames <- character(length(tr.objects))
        }
    }
    for (i in 1:length(tr.objects)) {
        nam <- tr.objects[[i]]@model.name
        if (length(nam) == 1) {
            model.names[i] <- nam
        }
    }
    if (is.null(model.names)) {
        model.names <- rep(NA, length(mnames))
    }
    else if (class(model.names)=="function"){
        mnames <- model.names(mnames)
        return(mnames)

        }
    
    else if (class(model.names) != "character") {
        stop("Model names must be specified as a vector of strings.")
    }
    else if (length(model.names) != length(tr.objects)) {
        stop(paste("There are", length(tr.objects), "models, but you provided", 
            length(model.names), "name(s) for them."))
    }
    for (i in 1:length(model.names)) {
        if (!is.na(model.names[i])) {
            mnames[i] <- model.names[i]
        }
        else if (mnames[i] == "") {
            mnames[i] <- paste("Model", i)
        }
        else {
        }
    }
    return(mnames)
}


mycustomnames  <- function (m, custom.names) 
{
    
    if (is.null(custom.names)) {
        return(m)
    }
    if(class(custom.names)=="function"){
        rownames(m)  <-  custom.names(rownames(m))
        return(m)
        }
    else if (length(custom.names) > 1) {
       
        if (class(custom.names) !="character") {
            stop("Custom coefficient names must be provided as a vector of strings!")
        }
        else if (length(custom.names) != length(rownames(m))) {
            stop(paste("There are", length(rownames(m)), "coefficients, but you provided", 
                length(custom.names), "custom names for them."))
        }
        else {
            custom.names[is.na(custom.names)] <- rownames(m)[is.na(custom.names)]
            rownames(m) <- custom.names
        }
    }
    else if (!is.na(custom.names) & class(custom.names) != "character") {
        stop("Custom coefficient names must be provided as a vector of strings.")
    }
    else if (length(custom.names) == 1 & class(custom.names) == 
        "character" & is.na(custom.names)) {
        rownames(m) <- custom.names
    }
    else if (length(custom.names) == length(rownames(m))) {
        rownames(m) <- custom.names
    }
    return(m)
}

myoverride <- 
function (models, override.coef, override.se, override.pval, 
    override.ci.low, override.ci.up) 
{
    if (class(override.se) == "list" || length(override.se) > 
        1 || override.se[1] != 0) {
        if (length(override.pval) == 1 && class(override.pval) != 
            "list" && override.pval[1] == 0) {
            warning(paste("Standard errors were provided using 'override.se',", 
                "but p-values were not replaced!"))
        }
    }
    if (class(override.pval) == "list" || length(override.pval) > 
        1 || override.pval[1] != 0) {
        if (length(override.se) == 1 && class(override.se) != 
            "list" && override.se[1] == 0) {
            warning(paste("P-values were provided using 'override.pval',", 
                "but standard errors were not replaced!"))
        }
    }
    for (i in 1:length(models)) {
        if (class(override.coef) != "list" && length(override.coef) == 
            1 && override.coef == 0) {
            cf <- models[[i]]@coef
        }
        else if (class(override.coef) == "numeric" && length(models) == 
            1 && length(override.coef) == length(models[[i]]@coef)) {
            cf <- override.coef
        }
        else if (class(override.coef) != "list") {
            warning("Coefficients must be provided as a list. Using default values.")
            cf <- models[[i]]@coef
        }
        else if (length(override.coef) != length(models)) {
            warning(paste("Number of coefficients provided does not match number of", 
                "models. Using default values."))
            cf <- models[[i]]@coef
        }
        else if (length(models[[i]]@coef) != length(override.coef[[i]])) {
            warning(paste("Number of coefficients provided does not match number of ", 
                "terms in model ", i, ". Using default values.", 
                sep = ""))
            cf <- models[[i]]@coef
        }
        else if (class(override.coef[[i]]) != "numeric") {
            warning(paste("Coefficients provided for model", 
                i, "are not numeric. Using default values."))
            cf <- models[[i]]@coef
        }
        else {
            cf <- override.coef[[i]]
        }
        models[[i]]@coef <- cf
        if (class(override.se) != "list" && length(override.se) == 
            1 && override.se == 0) {
            se <- models[[i]]@se
        }
        else if (class(override.se) == "numeric" && length(models) == 
            1 && length(override.se) == length(models[[i]]@se)) {
            se <- override.se
        }
        else if (class(override.se) != "list") {
            warning("SEs must be provided as a list. Using default SEs.")
            se <- models[[i]]@se
        }
        else if (length(override.se) != length(models)) {
            warning(paste("Number of SEs provided does not match number of models.", 
                "Using default SEs."))
            se <- models[[i]]@se
        }
        
        else if (all(is.na(override.se[[i]]))){            
            se <- models[[i]]@se
            }

        
        else if (length(models[[i]]@se) != length(override.se[[i]])) {
                
            warning(paste("Number of SEs provided does not match number of ", 
                "coefficients in model ", i, ". Using default SEs.", 
                sep = ""))
            se <- models[[i]]@se
        }
        else if (class(override.se[[i]]) != "numeric") {
            warning(paste("SEs provided for model", i, "are not numeric. Using default SEs."))
            se <- models[[i]]@se
        }
        else {
            se <- override.se[[i]]
        }
        models[[i]]@se <- se
        
        if (class(override.pval) != "list" && length(override.pval) == 
            1 && override.pval == 0) {
            pval <- models[[i]]@pvalues
        }
        else if (class(override.pval) == "numeric" && length(models) == 
            1 && length(override.pval) == length(models[[i]]@pvalues)) {
            pval <- override.pval
        }
        else if (class(override.pval) != "list") {
            warning("p values must be provided as a list. Using default p values.")
            pval <- models[[i]]@pvalues
        }
        else if (length(override.pval) != length(models)) {
            warning(paste("Number of p values provided does not match number of", 
                "models. Using default p values."))
            pval <- models[[i]]@pvalues
        }

        else if (all(is.na(override.pval[[i]]))){
            pval <- models[[i]]@pvalues

            }
        
        else if (length(models[[i]]@se) != length(override.pval[[i]])) {
                
            warning(paste("Number of p values provided does not match number of ", 
                "coefficients in model ", i, ". Using default p values.", 
                sep = ""))
                pval <- models[[i]]@pvalues
        }
        else if (class(override.pval[[i]]) != "numeric") {
            warning(paste("p values provided for model", i, "are not numeric. Using default p values."))
            pval <- models[[i]]@pvalues
        }
        else {
            pval <- override.pval[[i]]
        }
        models[[i]]@pvalues <- pval
        if (is.null(override.ci.low)) {
        }
        else if (class(override.ci.low) != "list" && length(override.ci.low) == 
            1 && override.ci.low == 0) {
            ci.low <- models[[i]]@ci.low
        }
        else if (class(override.ci.low) == "numeric" && length(models) == 
            1 && length(override.ci.low) == length(models[[i]]@coef)) {
            ci.low <- override.ci.low
        }
        else if (class(override.ci.low) != "list") {
            warning("CIs must be provided as a list. Using default CIs if available.")
            ci.low <- models[[i]]@ci.low
        }
        else if (length(override.ci.low) != length(models)) {
            warning(paste("Number of lower CIs provided does not match number of", 
                "models. Using default CIs if available."))
            ci.low <- models[[i]]@ci.low
        }
        else if (length(models[[i]]@coef) != length(override.ci.low[[i]])) {
            warning(paste0("Number of lower CIs provided does not match number of ", 
                "coefficients in model ", i, ". Using default CIs if available."))
            ci.low <- models[[i]]@ci.low
        }
        else if (class(override.ci.low[[i]]) != "numeric") {
            warning(paste("Lower CIs provided for model", i, 
                "are not numeric. Using default lower CIs."))
            ci.low <- models[[i]]@ci.low
        }
        else {
            ci.low <- override.ci.low[[i]]
        }
        models[[i]]@ci.low <- ci.low
        if (is.null(override.ci.up)) {
        }
        else if (class(override.ci.up) != "list" && length(override.ci.up) == 
            1 && override.ci.up == 0) {
            ci.up <- models[[i]]@ci.up
        }
        else if (class(override.ci.up) == "numeric" && length(models) == 
            1 && length(override.ci.up) == length(models[[i]]@coef)) {
            ci.up <- override.ci.up
        }
        else if (class(override.ci.up) != "list") {
            warning("CIs must be provided as a list. Using default CIs if available.")
            ci.up <- models[[i]]@ci.up
        }
        else if (length(override.ci.up) != length(models)) {
            warning(paste("Number of lower CIs provided does not match number of", 
                "models. Using default CIs if available."))
            ci.up <- models[[i]]@ci.up
        }
        else if (length(models[[i]]@coef) != length(override.ci.up[[i]])) {
            warning(paste0("Number of lower CIs provided does not match number of ", 
                "coefficients in model ", i, ". Using default CIs if available."))
            ci.up <- models[[i]]@ci.up
        }
        else if (class(override.ci.up[[i]]) != "numeric") {
            warning(paste("Lower CIs provided for model", i, 
                "are not numeric. Using default lower CIs."))
            ci.up <- models[[i]]@ci.up
        }
        else {
            ci.up <- override.ci.up[[i]]
        }
        models[[i]]@ci.up <- ci.up
        if (length(models[[i]]@ci.low) > 0 && length(models[[i]]@ci.up) > 
            0) {
            models[[i]]@se <- numeric()
            models[[i]]@pvalues <- numeric()
        }
    }
    return(models)
}


mytexreg <-
    function (l, file = NULL, single.row = FALSE, stars = c(0.001, 
    0.01, 0.05), custom.model.names = NULL, custom.coef.names = NULL, 
    custom.gof.names = NULL, custom.note = NULL, digits = 2, 
    leading.zero = TRUE, symbol = "\\cdot", override.coef = 0, 
    override.se = 0, override.pvalues = 0, override.ci.low = 0, 
    override.ci.up = 0, omit.coef = NULL, reorder.coef = NULL, 
    reorder.gof = NULL, ci.force = FALSE, ci.force.level = 0.95, print.star=FALSE,
    ci.test = 0, groups = NULL, custom.columns = NULL, custom.col.pos = NULL, 
    bold = 0, center = TRUE, caption = "Statistical models", 
    caption.above = FALSE, label = "table:coefficients", booktabs = FALSE, 
    dcolumn = FALSE, sideways = FALSE, longtable = FALSE, use.packages = TRUE, FE=NULL,
    table = TRUE, no.margin = FALSE, fontsize = NULL, scalebox = NULL,myheader=NULL,
    float.pos = "", threeparttable=FALSE,mynotes="",tightcenter=FALSE,...) 
{
    stars <- check.stars(stars)
    if (dcolumn == TRUE && bold > 0) {
        dcolumn <- FALSE
        msg <- paste("The dcolumn package and the bold argument cannot be used at", 
            "the same time. Switching off dcolumn.")
        if (length(stars) > 1 || stars == TRUE) {
            warning(paste(msg, "You should also consider setting stars = 0."))
        }
        else {
            warning(msg)
        }
    }
    if (longtable == TRUE && sideways == TRUE) {
        sideways <- FALSE
        msg <- paste("The longtable package and sideways environment cannot be", 
            "used at the same time. You may want to use the pdflscape package.", 
            "Switching off sideways.")
        warning(msg)
    }
    if (longtable == TRUE && !(float.pos %in% c("", "l", "c", 
        "r"))) {
        float.pos <- ""
        msg <- paste("When the longtable environment is used, the float.pos", 
            "argument can only take one of the \"l\", \"c\", \"r\", or \"\"", 
            "(empty) values. Setting float.pos = \"\".")
        warning(msg)
    }
    if (longtable == TRUE && !is.null(scalebox)) {
        scalebox <- NULL
        warning(paste("longtable and scalebox are not compatible. Setting", 
            "scalebox = NULL."))
    }
    models <- get.data(l, ...)
    gof.names <- get.gof(models)
    models <- myoverride(models, override.coef, override.se, override.pvalues, 
        override.ci.low, override.ci.up)
    models <- ciforce(models, ci.force = ci.force, ci.level = ci.force.level)
    models <- correctDuplicateCoefNames(models)
    gofs <- aggregate.matrix(models, gof.names, custom.gof.names, 
        digits, returnobject = "gofs")
    m <- aggregate.matrix(models, gof.names, custom.gof.names, 
        digits, returnobject = "m")
    decimal.matrix <- aggregate.matrix(models, gof.names, custom.gof.names, 
        digits, returnobject = "decimal.matrix")
    m <- mycustomnames(m, custom.coef.names)
    m <- replaceSymbols(m)
    m <- rearrangeMatrix(m)
    m <- as.data.frame(m)
    m <- omitcoef(m, omit.coef)
    modnames <- mymodelnames(l, models, custom.model.names)
    m <- reorder(m, reorder.coef)
    gofs <- reorder(gofs, reorder.gof)
    decimal.matrix <- reorder(decimal.matrix, reorder.gof)
    lab.list <- c(rownames(m), gof.names)
    lab.length <- 0
    for (i in 1:length(lab.list)) {
        if (nchar(lab.list[i]) > lab.length) {
            lab.length <- nchar(lab.list[i])
        }
    }
    ci <- logical()
    for (i in 1:length(models)) {
        if (length(models[[i]]@se) == 0 && length(models[[i]]@ci.up) > 
            0) {
            ci[i] <- TRUE
        }
        else {
            ci[i] <- FALSE
        }
    }
    output.matrix <- outputmatrix(m, single.row, neginfstring = "\\multicolumn{1}{c}{$-\\infty$}", 
        posinfstring = "\\multicolumn{1}{c}{$\\infty$}", leading.zero, 
        digits, se.prefix = " \\; (", se.suffix = ")", star.prefix = "^{", 
        star.suffix = "}", star.char = "*", stars, dcolumn = dcolumn, 
        symbol, bold, bold.prefix = "\\mathbf{", bold.suffix = "}", 
        ci = ci, semicolon = ";\\ ", ci.test = ci.test)
    output.matrix <- grouping(output.matrix, groups, indentation = "\\quad ", 
        single.row = single.row, prefix = "", suffix = "")
    gof.matrix <- gofmatrix(gofs, decimal.matrix, dcolumn = TRUE, 
                            leading.zero, digits)
    if (!is.null(FE)){
        gof.matrix <- rbind(gof.matrix,matrix(FE,nrow=1))
        }
    output.matrix <- rbind(output.matrix, gof.matrix)
    output.matrix <- customcolumns(output.matrix, custom.columns, 
        custom.col.pos, single.row = single.row, numcoef = nrow(m), 
        groups = groups, modelnames = FALSE)
    coltypes <- customcolumnnames(modnames, custom.columns, custom.col.pos, 
        types = TRUE)
    modnames <- customcolumnnames(modnames, custom.columns, custom.col.pos, 
        types = FALSE)
    coldef <- ""
    if (no.margin == FALSE) {
        margin.arg <- ""
    }
    else {
        margin.arg <- "@{}"
    }
    coefcount <- 0
    for (i in 1:length(modnames)) {
        if (coltypes[i] == "coef") {
            coefcount <- coefcount + 1
        }
        if (single.row == TRUE && coltypes[i] == "coef") {
            if (ci[coefcount] == FALSE) {
                separator <- ")"
            }
            else {
                separator <- "]"
            }
        }
        else {
            separator <- "."
        }
        if (coltypes[i] %in% c("coef", "customcol")) {
            alignmentletter <- "c"
        }
        else if (coltypes[i] == "coefnames") {
            alignmentletter <- "l"
        }
        if (dcolumn == FALSE) {
            coldef <- paste0(coldef, alignmentletter, margin.arg, 
                " ")
        }
        else {
            if (coltypes[i] != "coef") {
                coldef <- paste0(coldef, alignmentletter, margin.arg, 
                  " ")
            }
            else {
                if (single.row == TRUE) {
                  dl <- compute.width(output.matrix[, i], left = TRUE, 
                    single.row = TRUE, bracket = separator)
                  dr <- compute.width(output.matrix[, i], left = FALSE, 
                    single.row = TRUE, bracket = separator)
                }
                else {
                  dl <- compute.width(output.matrix[, i], left = TRUE, 
                    single.row = FALSE, bracket = separator)
                  dr <- compute.width(output.matrix[, i], left = FALSE, 
                    single.row = FALSE, bracket = separator)
                }
                coldef <- paste0(coldef, "D{", separator, "}{", 
                  separator, "}{", dl, separator, dr, "}", margin.arg, 
                  " ")
            }
        }
    }
    string <- "\n"
    if (use.packages == TRUE) {
        if (sideways == TRUE & table == TRUE) {
            string <- paste0(string, "\\usepackage{rotating}\n")
        }
        if (booktabs == TRUE) {
            string <- paste0(string, "\\usepackage{booktabs}\n")
        }
        if (dcolumn == TRUE) {
            string <- paste0(string, "\\usepackage{dcolumn}\n")
        }
        if (longtable == TRUE) {
            string <- paste0(string, "\\usepackage{longtable}\n")
        }
        if (dcolumn == TRUE || booktabs == TRUE || sideways == 
            TRUE || longtable == TRUE) {
            string <- paste0(string, "\n")
        }
    }
    if (longtable == TRUE) {
        if (center == TRUE) {
            string <- paste0(string, "\\begin{center}\n")
        }
        if (!is.null(fontsize)) {
            string <- paste0(string, "\\begin{", fontsize, "}\n")
        }
        if (float.pos == "") {
            string <- paste0(string, "\\begin{longtable}{", coldef, 
                "}\n")
        }
        else {
            string <- paste0(string, "\\begin{longtable}[", float.pos, 
                "]\n")
        }
    }
    else {
        if (table == TRUE) {
            if (sideways == TRUE) {
                t <- "sideways"
            }
            else {
                t <- ""
            }
            if (float.pos == "") {
                string <- paste0(string, "\\begin{", t, "table}\n")
            }
            else {
                string <- paste0(string, "\\begin{", t, "table}[", 
                  float.pos, "]\n")
            }
            if (center == TRUE) {
                if(tightcenter==TRUE){
                    string <- paste0(string, "\\begin{tightcenter}\n")
                }else {
                    string <- paste0(string, "\\begin{center}\n")
                }
            }
            
            if (caption.above == TRUE) {
                string <- paste0(string, "\\caption{", caption, 
                  "}\n")
            }

            if (!is.null(scalebox)) {
                string <- paste0(string, "\\scalebox{", scalebox, 
                  "}{\n")
            }
            if(threeparttable==TRUE) {
                string <- paste0(string, "\\begin{threeparttable}\n")
            }
            if (!is.null(fontsize)) {
                string <- paste0(string, "\\begin{", fontsize, 
                  "}\n")
            }

        }
        string <- paste0(string, "\\begin{tabular}{", coldef, 
            "}\n")
    }
    tablehead <- ""
    if (booktabs == TRUE) {
        tablehead <- paste0(tablehead, "\\toprule\n")
    }
    else {
        tablehead <- paste0(tablehead, "\\hline\n")
    }
    tablehead <- paste0(tablehead, modnames[1])
    if(!is.null(myheader)){
        tablehead <- paste0(tablehead,myheader)
    }
    
    if (dcolumn == TRUE) {
        
        tablehead <- paste(tablehead,paste0(" & ","\\multicolumns{c}{",modnames[-1],"}",collapse=""))
        
    }
    else {
            tablehead <- paste0(tablehead, paste0(" & ", modnames[-1],collapse=""))
        }
    
    if (booktabs == TRUE) {
        tablehead <- paste0(tablehead, " \\\\\n", "\\midrule\n")
    }
    else {
        tablehead <- paste0(tablehead, " \\\\\n", "\\hline\n")
    }
    if (longtable == FALSE) {
        string <- paste0(string, tablehead)
    }
    if (is.null(stars)) {
        snote <- ""
    }
    else if (any(ci == FALSE)) {
        st <- sort(stars)
        if (length(unique(st)) != length(st)) {
            stop("Duplicate elements are not allowed in the stars argument.")
        }
        if (length(st) == 4) {
            snote <- paste0("$^{***}p<", st[1], "$, $^{**}p<", 
                st[2], "$, $^*p<", st[3], "$, $^{", symbol, "}p<", 
                st[4], "$")
        }
        else if (length(st) == 3) {
            snote <- paste0("$^{***}p<", st[1], "$, $^{**}p<", 
                st[2], "$, $^*p<", st[3], "$")
        }
        else if (length(st) == 2) {
            snote <- paste0("$^{**}p<", st[1], "$, $^*p<", st[2], 
                "$")
        }
        else if (length(st) == 1) {
            snote <- paste0("$^*p<", st[1], "$")
        }
        else {
            snote <- ""
        }
        if (is.numeric(ci.test) && !is.na(ci.test) && nchar(snote) > 
            0 && any(ci)) {
            snote <- paste(snote, "(or", ci.test, "outside the confidence interval).")
        }
        else if (is.numeric(ci.test) && !is.na(ci.test) && any(ci)) {
            snote <- paste("$^*$", ci.test, "outside the confidence interval")
        }
    }
    else if (is.numeric(ci.test) && !is.na(ci.test)) {
        snote <- paste("$^*$", ci.test, "outside the confidence interval")
    }
    else {
        snote <- ""
    }
    if (is.null(fontsize)) {
        notesize <- "scriptsize"
    }
    else if (fontsize == "tiny" || fontsize == "scriptsize" || 
        fontsize == "footnotesize" || fontsize == "small") {
        notesize <- "tiny"
    }
    else if (fontsize == "normalsize") {
        notesize <- "scriptsize"
    }
    else if (fontsize == "large") {
        notesize <- "footnotesize"
    }
    else if (fontsize == "Large") {
        notesize <- "small"
    }
    else if (fontsize == "LARGE") {
        notesize <- "normalsize"
    }
    else if (fontsize == "huge") {
        notesize <- "large"
    }
    else if (fontsize == "Huge") {
        notesize <- "Large"
    }
    if (is.null(custom.note)&threeparttable==FALSE) {
        if(print.star!=FALSE){
        note <- paste0("\\multicolumn{", length(modnames), "}{l}{\\", 
                       notesize, "{", snote, "}}")
        } else {
            note <- ""
            }
        
    }
    else if(threeparttable==TRUE){
        note <- NULL
        
    }   else if (custom.note == "") {
        note <- ""
        
    } else{
        note <- paste0("\\multicolumn{", length(modnames), "}{l}{\\", 
                       notesize, "{", custom.note, "}}")
        note <- gsub("%stars", snote, note, perl = TRUE)
    }
    if (longtable == TRUE) {
        note <- paste0(note, "\\\\\n")
    }
    else {
        note <- paste0(note, "")
    }
    if (booktabs == TRUE) {
        bottomline <- "\\bottomrule\n"
    }
    else {
        bottomline <- "\\hline\n"
    }
    if (longtable == TRUE) {
        if (caption.above == TRUE) {
            string <- paste0(string, "\\caption{", caption, "}\n", 
                "\\label{", label, "}\\\\\n", tablehead, "\\endfirsthead\n", 
                tablehead, "\\endhead\n", bottomline, "\\endfoot\n", 
                bottomline, note, "\\endlastfoot\n")
        }
        else {
            string <- paste0(string, tablehead, "\\endfirsthead\n", 
                tablehead, "\\endhead\n", bottomline, "\\endfoot\n", 
                bottomline, note, "\\caption{", caption, "}\n", 
                "\\label{", label, "}\n", "\\endlastfoot\n")
        }
    }
    max.lengths <- numeric(length(output.matrix[1, ]))
    for (i in 1:length(output.matrix[1, ])) {
        max.length <- 0
        for (j in 1:length(output.matrix[, 1])) {
            if (nchar(output.matrix[j, i]) > max.length) {
                max.length <- nchar(output.matrix[j, i])
            }
        }
        max.lengths[i] <- max.length
    }
    for (i in 1:length(output.matrix[, 1])) {
        for (j in 1:length(output.matrix[1, ])) {
            nzero <- max.lengths[j] - nchar(output.matrix[i, 
                j])
            zeros <- rep(" ", nzero)
            zeros <- paste(zeros, collapse = "")
            output.matrix[i, j] <- paste0(output.matrix[i, j], 
                zeros)
        }
    }
    for (i in 1:(length(output.matrix[, 1]) - length(gof.names))) {
        for (j in 1:length(output.matrix[1, ])) {
            string <- paste0(string, output.matrix[i, j])
            if (j == length(output.matrix[1, ])) {
                string <- paste0(string, " \\\\\n")
            }
            else {
                string <- paste0(string, " & ")
            }
        }
    }
    if (length(gof.names) > 0) {
        if (booktabs == TRUE) {
            string <- paste0(string, "\\midrule\n")
        }
        else {
            string <- paste0(string, "\\hline\n")
        }
        
        for (i in (length(output.matrix[, 1]) - (length(gof.names) - 
            1)):(length(output.matrix[, 1]))) {
            for (j in 1:length(output.matrix[1, ])) {
                string <- paste0(string, output.matrix[i, j])
                if (j == length(output.matrix[1, ])) {
                  string <- paste0(string, " \\\\\n")
                }
                else {
                  string <- paste0(string, " & ")
                }
            }
        }
    }
    if (longtable == FALSE) {
        string <- paste0(string, bottomline)
        string <- paste0(string, note, "\\end{tabular}\n")
    }
    if (longtable == TRUE) {
        string <- paste0(string, "\\end{longtable}\n")
        if (!is.null(fontsize)) {
            string <- paste0(string, "\\end{", fontsize, "}\n")
            
        }
        if (center == TRUE) {
                    string <- paste0(string, "\\end{center}\n")
        }
    
    }    
    else if (table == TRUE) {
        
        if (!is.null(fontsize)) {
            string <- paste0(string, "\\end{", fontsize, "}\n")
        }
        if (threeparttable==TRUE){
            
            string <- paste0(string, "\\begin{tablenotes}[flushleft] \n \\scriptsize \n \\item ", snote ," ",mynotes,"\n \\end{tablenotes} \n")
        }


        if(threeparttable==TRUE){
            string <-  paste0(string,"\\end{threeparttable}\n")

        }
        if (center == TRUE) {
            if(tightcenter==TRUE){
                string <- paste0(string, "\\end{tightcenter}\n")}
            else{
                    string <- paste0(string, "\\end{center}\n")}
        }        

        if (!is.null(scalebox)) {
            string <- paste0(string, "}\n")
        }
        if (caption.above == FALSE) {
            string <- paste0(string, "\\caption{", caption, "}\n")
        }
        string <- paste0(string, "\\label{", label, "}\n")   
         

        if (sideways == TRUE) {
            t <- "sideways"
        }
        else {
            t <- ""
        }
        string <- paste0(string, "\\end{", t, "table}\n")
    }
    if (is.null(file) || is.na(file)) {
        class(string) <- c("character", "texregTable")
        return(string)
    }
    else if (!is.character(file)) {
        stop("The 'file' argument must be a character string.")
    }
    else {
        sink(file)
        cat(string)
        sink()
        message(paste0("The table was written to the file '", 
            file, "'.\n"))
    }
}

environment(mycustomnames) <- asNamespace('texreg')
environment(mymodelnames) <- asNamespace('texreg')
environment(mytexreg) <- asNamespace('texreg')
environment(myoverride) <- asNamespace('texreg')



pxtable.type <- function(type,scalebox=1) {
pxtable <- function(obj,caption.placement="top",table.placement="!htbp",...){
    ## DD
### obj . object of class xtable
### type . character latex|html
    ## purpose
### print an xtable in latex or html
    
    if(all(!(class(obj) %in% "xtable"))){
        stop(cat("object of class ", class(obj), '.Expected xtable'))
    }
    print.xtable(obj, type=type,comment=FALSE, sanitize.text.function=function(x) x,include.rownames =FALSE,
                 caption.placement=caption.placement,table.placement=table.placement,scalebox=scalebox,...)
    
}
pxtable
}

output <- pxtable.type("latex",1)
