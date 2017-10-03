

#' Linear Regression
#'
#' Finds betas
#' @param xVector Vector with independent variable
#' @param yVector Vector with dependent variable
#' @param numCols The number of colums you want. Standard is one column of 1's and another that is  xVector
#' @return A vector of betas, where the first cell is your y-int and the second cell represents your beta - "the slope"
#' @export
linear.regression <- function(xVector, yVector, numCols){

  x <- matrix(0, nrow= NROW(xVector),  ncol= numCols)
  x[,1] <- 1
  x[,2] <- xVector
  y <- (t(t(yVector)))

  betas = solve(t(x) %*% x) %*% t(x) %*% y # (X^T * X)^(-1) * X^T * Y # X^T is X transpose and ^(-1) is the inverse
  return(betas) # first cell is the constant (y - intercept), second is slope
}




#' Linear Regression Multi
#'
#' Finds betas (with more than one Beta)
#' @param xMatrix Vector with independent variables
#' @param yVector Vector with dependent variable
#' @param numCols The number of colums you want. Standard is one column of 1's and another that is  xVector
#' @return A vector of betas, where the first cell is your y-int and the second cell represents your beta - "the slope"
#' @export

linear.regression.multi <- function(xMatrix, yVector, numCols){
  
  x <- matrix(0, nrow= NROW(xMatrix),  ncol= numCols)
  x[,1] <- 1
  for(i in 1:(numCols-1)){
    print(colnames(x[,i+1]))
    x[,i+1] <- xMatrix[,i]
    print(x)
  }
  y <- (t(t(yVector)))
  
  betas = solve(t(x) %*% x) %*% t(x) %*% y # (X^T * X)^(-1) * X^T * Y # X^T is X transpose and ^(-1) is the inverse
  return(betas) # first cell is the constant (y - intercept), second is slope
}




#' Calculate the monthly payments of a loan
#'
#' @param n the number of payments expected
#' @param p initial loan balance
#' @param rm monthly rate
#' @return The static monthly payment for a loan
#' @export

monthly.payments <- function(p, rm, n) {
  maf = 1 / (rm) - 1 / (rm * (1 + rm) ^ n)
  return(p / maf)
}



#' To build a matrix with our info
#'
#' @param n the number of payments expected
#' @param p initial loan balance
#' @param mp monthly payments is  xVector
#' @return Returns a matrix with the info I want
#' @export

new.mat<-function(n, p, mp, rm){
  mt=matrix(nrow=n+1, ncol=6)
  colnames(mt) <- c("Month", "Initial Bal", "Interest on Bal", "Monthly Payment", "Amoritiz of Loan", "End   Balance")

  for(i in 1:n){
    mt[i,1]=i
    mt[i,2]=p
    mt[i,3]=p*rm
    mt[i,4]=mp
    mt[i,5]=mp-mt[i,3]
    mt[i,6]=p-mt[i,5]
    p=mt[i,6]
  }

  return(mt)
}

