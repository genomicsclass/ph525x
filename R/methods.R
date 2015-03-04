.nomethods <-
    function()
{
    data.frame(generic=character(), signature=character(),
               package=character(), stringsAsFactors=FALSE)
}

.s3methods <-
    function(x)
{
    results <- attr(x, "info")
    method <- rownames(results)
    package <- sub("package:", "", results[["from"]])
    generic <- sub("(.*)\\.[[:alnum:]]+$", "\\1", method)
    class <- sub(".*\\.([[:alnum:]]+)$", "\\1", method)
    sig <- vapply(generic, function(elt) names(formals(elt))[[1]], character(1))
    signature <- sprintf("%s=\"%s\"", sig, class)
    data.frame(generic=generic, signature=signature, package=package,
               stringsAsFactors=FALSE)
}

.s4methods <-
    function(x)
{
}

.s3methodsForClass <-
    function(generic.function, class)
{
    .s3methods(utils::methods(class=class))
}

.s4methodsForClass <- 
    function(generic.function, class)
{
    results <- showMethods(class=class, where=search(), printTo=FALSE)
    idx <- grep("^Function:", results)

    Arith <- c("\\+", "\\-", "\\*", "\\^", "%%", "%/%", "/")
    Compare <- c("==", ">", "<", "\\!=", "<=", ">=")
    Logic <- c("\\&", "\\|", "\\!")
    In <- "%[[:alpha:]]+%"
    ops <- paste(c(Arith, Compare, Logic, In), collapse="|")


    funpat <- sprintf("([[:alnum:]\\.]+|\\[|%s)(<-)*", ops)
    generic <- sub(sprintf("^Function: (%s).*", funpat), "\\1", results[idx])
    package <- sub(".*\\(package ([[:alnum:]\\.]+)\\)$", "\\1", results[idx])
    signature <- results[idx + 1]
    data.frame(generic=generic, signature=signature, package=package,
               stringsAsFactors=FALSE)
}

.methodsForClass <-
    function(generic.function, class)
{
    s3 <- .s3methodsForClass(generic.function, class)
    s4 <- .s4methodsForClass(generic.function, class)
    .methods_result(s3, s4)
}

.s3methodsForFunction <-
    function(generic.function, class)
{
    .s3methods(utils::methods(generic.function, class))
}

.s4methodsForFunction <-
    function(generic.function, class)
{
    results <- showMethods(generic.function, where=search(), printTo=FALSE)
    if (nzchar(results[1])) {
        results <- results[nzchar(results)]
        signature <- results[-1]
        package <- sub(".*\\(package ([[:alnum:]]+)\\)$", "\\1", results[1])
        data.frame(generic=generic.function, signature=signature,
                   package=package, stringsAsFactors=FALSE)
    } else {
        .nomethods()
    }
}

.methodsForFunction <-
    function(generic.function, class)
{
    if (existsMethod(generic.function, "ANY")) {
        s3 <- .s3methodsForFunction(generic.function, class)
        s3$package <- sub(sprintf(" for %s", generic.function), "", s3$package)
    } else {
        s3 <- .nomethods()
    }
    s4 <- .s4methodsForFunction(generic.function, class)
    .methods_result(s3, s4)
}

.methods_result <-
    function(s3, s4)
{
    dup <- (s3$generic %in% s4$generic) & (s3$signature %in% s4$signature)
    s3 <- s3[!dup,, drop=FALSE]
    df <- rbind(s3, s4)
    df <- df[order(df$generic),,drop=FALSE]
    rownames(df) <- NULL
    df
}

methods <-
    function(generic.function, class, where=search())
{
    if (!missing(generic.function)) {
        FUN <- if (!is.character(generic.function))
            deparse(substitute(generic.function))
        else generic.function
        if (!isS4(get(FUN)))
            DataFrame(.s3methodsForFunction(FUN, class))
        else
            DataFrame(.methodsForFunction(FUN, class))
    } else if (!missing(class)) {
        def <- tryCatch(getClass(class), error=function(...) NULL)
        if (is.null(def))
            DataFrame(.s3methodsForClass(generic.function, class))
        else
            DataFrame(.methodsForClass(generic.function, class))
    }
}
