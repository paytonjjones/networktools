#' net_reduce
#'
#' This function takes predefined pairs of colinear variables in a dataset and a) combines them via PCA or
#' b) picks the "better" variable and eliminates the other variable
#'
#' @param data a data frame consisting of n rows (participants) and j columns (variables)
#' @param badpairs pairs of variables to be combined. Input may consist of:
#' -an object of class "goldbricker" (all bad pairs are combined)
#' -a vector of item names, each consecutive pair will be considered a bad pair
#' -a matrix with 2 columns where each bad pair takes up 1 row
#' @param method method for combining variables. PCA takes the first principal component of the two
#' variables and defines it as a new variable. best_goldbricker requires that the input of "badpairs" be an object of class
#' "goldbricker" it selects the more unique variable, and eliminates the other variable in the pair.
#'
#' @details
#'
#' In a given psychometric network, two nodes may be redundantly measuring the same underlying construct. If this is the case,
#' both variables should not appear in the same network, or network properties will be inaccurate. These variable pairs can be
#' reduced by combining them, or by eliminating one of them. net_reduce automates this process when given a list of "bad pairs"
#'
#' If the same variable appears in multiple "bad pairs" (e.g., "x" and "y" is a bad pair, and so is "x" and "z"), only the first
#' of these pairs which appears in the badpairs argument will be reduced by the function.
#'
#' @examples
#' \donttest{
#' gb_depression <- goldbricker(depression, threshold=0.5)
#'
#' reduced_depression_PCA <- net_reduce(data=depression, badpairs=gb_depression)
#' reduced_depression_best <- net_reduce(data=depression,
#'                            badpairs=gb_depression, method="best_goldbricker")
#'
#'}
#'
#' @return \code{\link{goldbricker}} returns a dataframe of n rows (participants) and j - x columns,
#' where j is the number of variables in the original dataframe, and x is the number of bad pairs to reduce.
#'
#'
#'@export
net_reduce <- function(data, badpairs, method=c("PCA","best_goldbricker")){
  if(class(badpairs)=="goldbricker"){
    bp_full <- unlist(strsplit(names(badpairs$suggested_reductions), split= " & "))
  } else if (is.list(badpairs)){
    bp_full <- unlist(badpairs)
  } else {bp_full <- as.vector(t(badpairs))}
  bp_mat <- matrix(bp_full, ncol=2, byrow=TRUE)
  bp_unable_vec <- matrix(duplicated(bp_full), ncol=2, byrow=TRUE)[,1] | matrix(duplicated(bp_full), ncol=2, byrow=TRUE)[,2]
  bp_unable <- bp_mat[bp_unable_vec,]
  if(all(is.na(bp_unable))){
    out_unable <- NULL
    bp <- bp_mat
  } else if (is.vector(bp_unable)) {
    out_unable <- paste(bp_unable[1], bp_unable[2], sep=" & ")
  } else {
    out_unable <- paste(bp_unable[,1], bp_unable[,2], sep=" & ")
  }
  if(!all(is.na(bp_unable))){
    bp <- bp_mat[!bp_unable_vec,]
    warning("The following pairs were not reduced because they contained duplicates found in prior pairs: \n", paste(out_unable, collapse=", "))
  }
  newcols <- matrix(numeric(), nrow(data),nrow(bp))
  if(method[1]=="PCA"){
    if(TRUE %in% is.na.data.frame(data)){
      warning("NAs in data: median values are imputed by default")
      data <- data.frame(lapply(data,function(x) {
        if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))
    }
    for(i in 1:nrow(bp)){
      newcols[,i] <- prcomp(data[,bp[i,1:2]], scale.=TRUE, rank.=1)$x
      if(cor(newcols[,i], data[,bp[i,1]]) < 0 & cor(newcols[,i], data[,bp[i,2]]) < 0){
        newcols[,i] <- -newcols[,i] # flip direction of pc if backwards
      }
    }
    bp_colnames <- paste("PCA", bp[,1], bp[,2], sep=".")
  } else if (method[1]=="best_goldbricker") {
    if(class(badpairs)!="goldbricker"){stop("Argument badpairs must be of class goldbricker to use method best_goldbricker")}
    bp_colnames <- vector()
    for(i in 1:nrow(bp)){
      if(mean(na.omit(badpairs$proportion_matrix[,bp[i,1]])) >= mean(na.omit(badpairs$proportion_matrix[,bp[i,2]]))){
        newcols[,i] <- data[,bp[i,1]]
        bp_colnames[i] <- bp[i,1]
      } else {newcols[,i] <- data[,bp[i,2]]
        bp_colnames[i] <- bp[i,2]
      }
    }
  }
  colnames(newcols) <- bp_colnames
  data_stripped <- data[,!colnames(data)%in%as.vector(bp)]
  newdata <- cbind(data_stripped, newcols)
  if(is.vector(data_stripped)){colnames(newdata)[1] <- colnames(data)[!colnames(data)%in%as.vector(bp)]}
  return(newdata)
}
