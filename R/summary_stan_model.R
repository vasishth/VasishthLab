#' Produce a summary of a Stan model for paper
#'
#' @param mod A Stan model.
#' @param nfixefs Number of so-called fixed effects.
#' @seealso  \url{http://mc-stan.org}.
#' @return A dataframe with
#' \itemize{
#'   \item \strong{rows} containing intercept and other fixed effects
#'   \item \strong{columns} containing comparison, mean, lower, upper,
#'   probability that effect b < 0
#' }
#' @export
#'
#' @examples
#' # See SafaviEtAl2016-vignette.Rmd for complete examples
#' stanm1_tab<-summary_stan_model(stanm1,nfixefs=3)
summary_stan_model<-function(mod,nfixefs=NULL){
  if(is.null(nfixefs)){stop("Please specify number of fixed effects!")}
  samples_m1 <- as.data.frame(mod)
  nfixefs<-nfixefs+1 ## Always include intercept
  mns<-rep(NA,nfixefs) ## Store means
  ci95<-matrix(rep(NA,nfixefs*2),ncol=2) ## Extract Credible Intervals
  for(i in 1:nfixefs){
    condnames<-colnames(samples_m1)[1:nfixefs]
    condnames[1]<-"Intercept"
    mns[i]<-round(mean(samples_m1[,i]),digits=4)
    ci95[i,]<-round(quantile(probs=c(0.025,0.975),samples_m1[,i]),
                    digits=4)
  }
  ## prob less than 0
  prob_less<-rep(NA,nfixefs)
  for(i in 1:nfixefs){
    prob_less[i]<-round(mean(samples_m1[,i]<0),digits=4)
  }
  res<-as.data.frame(cbind(condnames,mns,ci95,prob_less))
  colnames(res)<-c("comparison","mean","lower","upper","P(b<0)")
  res
}
