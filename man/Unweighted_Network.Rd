\name{Unweighted_Network}
\alias{Unweighted_Network}
\title{Compute permutation-based FDR of importance scores and return estimated interactions.}
\usage{
Unweighted_Network(out.iJRF,out.perm,TH)
}
\arguments{
\item{out.iJRF}{Output from object of class \code{iJRF}.}

\item{out.perm}{Output from object of class \code{iJRF_Perm}.}

\item{TH}{Threshold for FDR.}
}

\value{
List of estimated interactions. 
}
\description{
This function computes permutation-based FDR of importance scores and returns interactions.  
}
\examples{


 # --- Generate data sets
 nclasses=2               # number of data sets / classes
 n1<-n2<-20               # sample size for each data sets
 p<-5                   # number of response variables 
 M<-10                   # number of predictor variables 
 W<-abs(matrix(rnorm(M*p),M,p))    # generate sampling scores

 Res1<-matrix(rnorm(p*n1),p,n1)       # generate response for class 1
 Res2<-matrix(rnorm(p*n2),p,n2)       # generate response for class 2
 Cov1<-matrix(rnorm(M*n1),M,n1)       # generate predictors for class 1
 Cov2<-matrix(rnorm(M*n2),M,n2)       # generate predictors for class 2
 
 # --- Standardize variables to mean 0 and variance 1
  Res1 <- t(apply(Res1, 1, function(x) { (x - mean(x)) / sd(x) } ))
  Res2 <- t(apply(Res2, 1, function(x) { (x - mean(x)) / sd(x) } ))


# --- Run iJRF and obtain importance score of interactions
out.iJRF<-iJRF(X=list(Cov1,Cov2),Y=list(Res1,Res2),W=W)

# --- Run iJRF for P permutated data sets
out.perm<-iJRF_Perm(X=list(Cov1,Cov2),Y=list(Res1,Res2),W=W,P=2)

# --- Derive final networks
final.net<-Unweighted_Network(out.iJRF,out.perm,0.001)

}
\references{
Petralia, F. et al (2017) A new method to study the change of miRNA-mRNA interactions due to environmental exposures, Submitted.

Petralia, F., Wang, P., Yang, J., and Tu Z. (2015) Integrative random forest for gene regulatory network inference. \bold{31}(12), i197-i205.

Petralia, F., Song, W.M., Tu, Z. and Wang, P. (2016). New method for joint network analysis reveals common and different coexpression patterns among genes and proteins in breast cancer. \emph{Journal of proteome research}, \bold{15}(3), pp.743-754.

Some of the functions utilized are a modified version of functions contained in R package randomForest:
A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. \emph{R News} \bold{2}, 18--22.

}
