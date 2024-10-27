#' Linear Regression Using OLR Method
#'
#' This function fits the data for linear regression
#'  
#' @param x covariate
#' @param y response
#' @param conflev confidence level for producing the CI of beta1. Default 0.95.
#' @param new.x a value of x for prediction of y. Default NULL.
#' 
#' @return An object having values including
#'  \itemize{
#'  \item  Sxx: XX sum of square sum_(x_i-xbar)^2
#'  \item  Sxy: XY sum of square sum_(x_i-xbar)(y_i-ybar)
#'  \item  beta0: coefficient for intercept; beta0=ybar-beta1*xbar
#'  \item  beta1: coefficient for x; beta1 = Sxy/Sxx
#'  \item  SST: Total sum of squares
#'  \item  SSR: Regression sum of squares
#'  \item  SSE: Error sum of squares
#'  \item  r2:  Coefficient of determination, r2 = SSR / SST
#'  \item  r2adj:  Adjusted coefficient of determination, 1-(SSE / (n-k))/(SST/(n-1)) = 1 - MSE/MST
#'  \item  anova: ANOVA table
#'  \item  sigma.hat residual standard error, sqrt(MSE)
#'  \item  se.beta1 se(beta1)
#'  \item  t0: t-test statistic
#'  \item  p: p value from the t-test
#'  \item F0: F value from the F-test for the covariate x
#'  \item p.F.test: p value from the F test (df1=1, df2=n-2).
#'  \item CI: Confidence interval of beta1. Default 95\% CI
#'  \item new.y: Predicted value of y given x = new.x
#'  \item CI.new.x: Confidence interval of population mean of new.y when x = new.x
#'  \item predCI.new.x: Prediction interval of new.y when x = new.x
#'  }
#'  
#' @examples
#' #Example
#' set.seed(2023)
#' x <- rnorm(50, mean=10, sd=2)
#' y <- rnorm(50, mean=2*x+3, sd=2)
#' OLR(x, y)
#' 
#' OLR(x, y, new.x=20)
#' 
#' @export 
#'
OLR <- function(x, y, conflev=0.95, new.x=NULL){
  xbar = mean(x)
  ybar = mean(y)
  
  sxx = sum((x-xbar)^2) 
  sxy = sum((x-xbar)*(y-ybar))
  
  beta1 = sxy / sxx; beta0 = mean(y) - beta1 * mean(x)
  
  n = length(y)
  SST = sum(y^2) - (sum(y))^2 / n
  SSR1 = (sum(x*y) -sum(x)*sum(y)/n)^2
  SSR2 = sum(x^2) - (sum(x))^2 / n
  SSR = SSR1 / SSR2
  SSE = SST - SSR
  
  MSE = SSE/(n-2)
  MST = SST / (n-1)
  
  #r2: coefficient of determination
  r2 = SSR / SST
  
  #adjusted r2
  r2adj = 1-MSE/MST
  
  #anova
  LinearReg <- lm(y ~ x) #Create the linear regression
  reg.anova = anova(LinearReg)
  
  #residual se
  sigma.hat = sqrt(MSE)
  
  #beta1 se
  se.beta1 = sigma.hat / sqrt(sxx)
  
  #test statistic value t0
  t0 = (beta1 - 0) / se.beta1
  
  #p value from t-test
  p = 2*(1-pt(abs(t0), df= n-2))
  
  #F test statistic
  F0 = reg.anova$`F value`[1]
  
  #F test p value
  p.F.test = reg.anova$`Pr(>F)`[1]
  
  #CI for beta1
  alpha=1-conflev
  margin = qt(1-alpha/2, df=n-2) * se.beta1
  CI = c(beta1 - margin, beta1+margin)
  
  #Prediction
  if (!is.null(new.x)){
    new.y = beta0 + beta1*new.x
    
    #Confidence interval of population mean of new.y when x = new.x
    margin.ci.x <- qt(1-alpha/2, df=n-2) * sigma.hat * sqrt(1/n + (new.x-xbar)^2/sxx)
    CI.new.x = c(new.y - margin.ci.x, new.y + margin.ci.x)
    
    #Prediction interval of new.y when x = new.x
    margin.predci.x <- qt(1-alpha/2, df=n-2) * sigma.hat * sqrt(1+1/n + (new.x-xbar)^2/sxx)
    predCI.new.x = c(new.y - margin.predci.x, new.y + margin.predci.x)
  }
  
  o = list()
  o$Sxx = sxx; o$Sxy = sxy; o$beta0 = beta0; o$beta1 = beta1
  o$SST = SST; o$SSR = SSR; o$SSE = SSE; o$r2 = r2; o$r2adj = r2adj
  o$anova = reg.anova
  o$sigma.hat = sigma.hat
  o$se.beta1 = se.beta1
  o$t0 = t0
  o$p = p
  o$F0 = F0
  o$p.F.test = p.F.test
  o$CI = CI
  if (!is.null(new.x)) {
    o$new.y = new.y
    o$CI.new.x = CI.new.x
    o$predCI.new.x = predCI.new.x
  }
  
  return(o)
}    


