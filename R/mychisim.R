#' Title
#'
#' @param n1 this is also just a fake description
#' @param sigma1 this is also just a fake description
#' @param mean1 this is also just a fake description
#' @param iter this is also just a fake description
#' @param ymax this is also just a fake description
#' @param ... This is a fake description
#'
#' @return this is also just a fake description
#' @export
#'
#' @examples
#' @importFrom stats density sd rnorm apply var dchisq
#' @importFrom graphics locator hist lines curve legend
utils::globalVariables("x")

mychisim <-
  function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,...){    # adjust ymax to make graph fit
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1

  ssq1=apply(data1.mat,2,var) # ssq1 is s squared

  w=(n1-1)*ssq1/sigma1^2      #chi-sq stat

  hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(dchisq(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  title=expression(chi^2==frac((n[1]-1)*s^2,sigma^2)) #mathematical annotation -see ?plotmath
  legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #
  return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq")) # some output to use if needed
}
