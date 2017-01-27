Unweighted_Network <- function(out.iJRF,out.perm,TH) {
 
  p<-length(unique(out.iJRF[,2])) # number of response variables
  M<-length(unique(out.iJRF[,1])) # number of predictors
  nclasses<-dim(out.perm)[3]
  P<-dim(out.perm)[2]; out<-list()

    for (j in 1:p){ # -- over response 
      int.resJ<-out.iJRF[seq((j-1)*M+1,j*M),]  # -- extract importance score for j-th response
      for (net in 1:nclasses) { # -- over classes
        j.np<-sort(int.resJ[,2+net],decreasing=TRUE)
        FDR<-rep(0,M); 
        for (s in 1:length(j.np)){ 
          FP<-sum(sum(out.perm[seq((j-1)*M+1,j*M),,net]>=j.np[s]))/P
          FDR[s]<-FP/s;
          if (FDR[s]>TH) {th<-j.np[s];  break;}
          }
      if (j==1) out[[net]]<-int.resJ[int.resJ[,2+net]>=th,seq(1,2)]
      if (j>1) out[[net]]<-rbind(out[[net]],int.resJ[int.resJ[,2+net]>=th,seq(1,2)])
      }
  }
  return(out)
}