#' @title Stochastic Gradient Descent algorithm using R
#' @description Fit the independent variable X and dependent variable Y linearly by  using stochastic gradient descent algorithm 
#' @param y the dependent variable data.
#' @param x the independent variable data.
#' @param learning_rate Rate of stochastic gradient descent
#' @return Linear function 
#' @examples
#' \dontrun{
#'    Gradient_Descent(c(1,2,3,4,5),c(2,4.1,5.9,7.9,10.01),0.00001)
#' }
#' @export
Stochastic_GD<-function(x,y,learning_rate){
  x0=rep(1,length(x))
  x_data=cbind(x0,x)
  y_data=matrix(y,length(y),1)
  theta=matrix(c(1,1),2,1)
  i=sample(1:nrow(x_data),1)
  diff=x_data[i,]%*%theta-y_data[i,]
  gradient=(x_data[i,])%*%diff
  while (!((abs(gradient[1,1])<1e-5)&(abs(gradient[2,1])<1e-5))){
    theta=theta-learning_rate*gradient
    j=sample(1:nrow(x_data),1)
    diff=x_data[j,]%*%theta-y_data[j,]
    gradient=(x_data[j,])%*%diff
  }
  func=paste('y=',theta[1,1],'+',theta[2,1],'x')
  return (func)
}