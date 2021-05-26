## ----type I error-------------------------------------------------------------
library(RABR)
library(parallel)
library(doParallel)

RABR.null.fit = RABRcontinuous(MeanVec = c(0, 0, 0, 0), 
               SdVec = c(1, 1, 1, 1), 
               M = 60, 
               N = 120, 
               R = c(3, 4, 2, 1), 
               Nitt = 10^4, 
               Alpha = 0.025, 
               Ncluster = 2, 
               Seed = 12345, 
               MultiMethod = "dunnett")

## Probability of rejecting each elementary hypothesis without multiplicity adjustment
print(RABR.null.fit$ProbUnadj)

## Probability of rejecting each elementary null hypothesis with multiplicity adjustment
print(RABR.null.fit$ProbAdj)

## Probability of rejecting at least one elementary null hypothesis with multiplicity adjustment
print(RABR.null.fit$ProbAdjOverall)

## ----power--------------------------------------------------------------------
RABR.alter.fit = RABRcontinuous(MeanVec = c(0.43, 0.48, 0.63, 1.2), 
               SdVec = c(1, 1, 1, 1), 
               M = 60, 
               N = 120, 
               R = c(9, 9, 1, 1), 
               Nitt = 10^3, 
               Alpha = 0.025, 
               Ncluster = 2, 
               Seed = 12345, 
               MultiMethod = "dunnett")

## Probability of selecting (if unadjusted p-value is the smallest among all active treatment groups) AND confirming (if the adjusted p-value is smaller than the significance level) the efficacy of each active treatment group. 
print(RABR.alter.fit$ProbAdjSelected)

## ASN Average sample size of placebo and selected treatment groups (S1, S2, S3). 
print(RABR.alter.fit$ASN)

## ----sensitivity--------------------------------------------------------------
output.mat = matrix(NA, nrow = 5, ncol = 10)
colnames(output.mat) = c("M", "R", "Prob_D1", "Prob_D2", "Prob_D3", "Prob_ALO",
                         "ASN_PBO", "ASN_S1", "ASN_S2", "ASN_S3")
output.mat = data.frame(output.mat)

for (scen.ind in 1:5){
  
  if (scen.ind==1){M.cand = 40; R.cand = c(8, 8, 3, 1)}
  if (scen.ind==2){M.cand = 60; R.cand = c(9, 9, 1, 1)} 
  if (scen.ind==3){M.cand = 24; R.cand = c(9, 9, 5, 1)} 
  if (scen.ind==4){M.cand = 40; R.cand = c(16, 16, 7, 1)} 
  if (scen.ind==5){M.cand = 40; R.cand = c(4, 4, 1, 1)} 
  
  RABR.sen.fit = RABRcontinuous(MeanVec = c(0.43, 0.48, 0.63, 1.2), 
               SdVec = c(1, 1, 1, 1), 
               M = M.cand, 
               N = 120, 
               R = R.cand, 
               Nitt = 10^3, 
               Alpha = 0.025, 
               Ncluster = 2, 
               Seed = 12345, 
               MultiMethod = "dunnett")
  
  output.mat$M[scen.ind] = M.cand
  output.mat$R[scen.ind] = paste0("(", paste0(R.cand,collapse = ","), ")")
  output.mat[scen.ind, c("Prob_D1", "Prob_D2", "Prob_D3")] = RABR.sen.fit$ProbAdjSelected
  output.mat$Prob_ALO[scen.ind] = RABR.sen.fit$ProbAdjOverall 
  output.mat[scen.ind, c("ASN_PBO", "ASN_S1", "ASN_S2", "ASN_S3")] = RABR.sen.fit$ASN
}
print(output.mat)

