#' @title Gradient Descent algorithm using R
#' @description Fit the independent variable X and dependent variable Y linearly by  using gradient descent algorithm 
#' @param y the dependent variable data.
#' @param x the independent variable data.
#' @param learning_rate Rate of gradient descent
#' @return Linear function
#' @examples
#' \dontrun{
#'    Gradient_Descent(c(1,2,3),c(2,4.1,5.9),0.01)
#' }
#' @export
Gradient_Descent<-function(x,y,learning_rate){
  x0=rep(1,length(x))
  x_data=cbind(x0,x) 
  y_data=matrix(y,length(y),1)
  theta=matrix(c(1,1),2,1)
  diff=x_data%*%theta-y_data 
  gradient=(1/length(x))*(t(x_data)%*%diff)
  while (!((abs(gradient[1,1])<0.0001)&(abs(gradient[2,1])<0.0001))) {
    theta=theta-learning_rate*gradient
    diff=x_data%*%theta-y_data 
    gradient=(1/length(x))*(t(x_data)%*%diff)
  }
  func=paste('y=',theta[1,1],'+',theta[2,1],'x')
  return (func)
}