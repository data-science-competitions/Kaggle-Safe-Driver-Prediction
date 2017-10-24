#' ---
#' title: "Helper Functions"
#' author: "Harel Lustiger"
#' ---

# Gini code from kaggle
SumModelGini <- function(solution, submission) {
    df = data.frame(solution = solution, submission = submission)
    df <- df[order(df$submission, decreasing = TRUE),]
    df$random = (1:nrow(df))/nrow(df)
    totalPos <- sum(df$solution)
    df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
    df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion 
    # of positive examples found ("Model Lorentz")
    df$Gini <- df$Lorentz - df$random   # will store Lorentz minus random
    return(sum(df$Gini))
}# SumModelGini()

#' @title Custom metric for the caret package
#' @example trainControl(method="repeatedcv", number=3, repeats=1,
#'                       summaryFunction=NormalizedGini, verboseIter=TRUE) 
NormalizedGini <- function(data, lev=NULL, model=NULL) {
    solution=data$obs
    submission=data$pred
    result=SumModelGini(solution, submission) / SumModelGini(solution, solution)
    names(result) <- "Gini"
    return(result)
}# NormalizedGini()