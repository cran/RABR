% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RABRcontinuous.R
\name{RABRcontinuous}
\alias{RABRcontinuous}
\title{Simulate RABR for continuous endpoints to evaluate operating characteristics}
\usage{
RABRcontinuous(
  MeanVec,
  SdVec,
  M,
  N,
  R,
  Nitt,
  Alpha,
  Ncluster = 1,
  Seed = 12345,
  MultiMethod
)
}
\arguments{
\item{MeanVec}{Vector of response mean for placebo and active treatment groups.}

\item{SdVec}{Vector of standard deviation for placebo and active treatment groups.}

\item{M}{Total sample size of burn-in period.}

\item{N}{Total sample size of RABR. Must be larger than M.}

\item{R}{Randomization vector for placebo and active treatment groups.}

\item{Nitt}{Number of simulation iterations.}

\item{Alpha}{One-sided significance level.}

\item{Ncluster}{Number of clusters for parallel computing.}

\item{Seed}{Random seed.}

\item{MultiMethod}{Multiplicity adjustment method. Must be one of the following values "holm", "hochberg", "hommel", "bonferroni", or "dunnett".}
}
\value{
ProbUnadj: Probability of rejecting each elementary null hypothesis without multiplicity adjustment

ProbAdj: Probability of rejecting each elementary null hypothesis with multiplicity adjustment

ProbAdjSelected: Probability of selecting and confirming the efficacy of each active treatment group

ProbAdjOverall: Probability of rejecting at least one elementary null hypothesis with multiplicity adjustment

ASN: Average sample size of placebo and active treatment groups
}
\description{
Simulate RABR for continuous endpoints to evaluate operating characteristics
}
\details{
The \code{MeanVec} is a vector of response mean for placebo and active treatment groups, while \code{SdVec} is for standard deviation. They should be with the same length. The current package supports 2 or 3 active treatment groups. Note that a larger response corresponds to a better outcome.

The \code{M} is the total sample size of burn-in period with equal randomization. The total sample size \code{N} should be larger than N. The choice of \code{M} can be selected by comparing simulations from several candidate values. The \code{R} is a pre-specified randomization vector, where the first element is for placebo, and the next one for the best performing group, up to the worst performing group.

The \code{Alpha} is the one-sided significance level. The \code{MultiMethod} can be set at "holm" for Holm, "hochberg" for Hochberg, "hommel" for Hommel, "bonferroni" for Bonferroni, or "dunnett" for Dunnett procedures.
}
\examples{
## Consider an example with three active treatment
## groups and a placebo. Suppose that the response
## mean for placebo is 0.43 and 0.48, 0.63, and 1.2
## for three active treatment groups. The standard
## deviation is 1 for all groups. The total sample
## size is N = 120 with a burn-in period M = 60. We
## use the randomization vector of (8, 9, 2, 1),
## which means that placebo, the best performing
## group, the second-best group, and the worst group
## have randomization probabilities 8/20, 9/20, 2/20
## 1/20, respectively. The one-sided significance
## level is considered at 2.5\%. Nitt = 100 is for
## demonstration, and should be increased to 10^5
## in practice.
##
library(parallel)
library(doParallel)
RABR.fit = RABRcontinuous(
           MeanVec = c(0.43, 0.48, 0.63, 1.2),
           SdVec = c(1, 1, 1, 1),
           M = 60,
           N = 120,
           R = c(8, 9, 2, 1),
           Nitt = 100,
           Alpha = 0.025,
           Ncluster = 2,
           Seed = 12345,
           MultiMethod = "dunnett")
##
## Probability of rejecting each elementary null
## hypothesis without multiplicity adjustment
   print(RABR.fit$ProbUnadj)
##
## Probability of rejecting each elementary null
## hypothesis with multiplicity adjustment
   print(RABR.fit$ProbAdj)
##
## Probability of selecting and confirming the
## efficacy of each active treatment group
   print(RABR.fit$ProbAdjSelected)
##
## ProbAdjOverall Probability of rejecting at
## least one elementary null hypothesis
## with multiplicity adjustment
   print(RABR.fit$ProbAdjOverall)
##
## ASN Average sample size of placebo and active
## treatment groups
   print(RABR.fit$ASN)




}
\references{
Zhan, T., Cui, L., Geng, Z., Zhang, L., Gu, Y., & Chan, I. S. (2021). A practical response adaptive block randomization (RABR) design with analytic type I error protection. Statistics in Medicine, 40(23), 4947-4960.

Cui, L., Zhan, T., Zhang, L., Geng, Z., Gu, Y., & Chan, I. S. (2021). An automation-based adaptive seamless design for dose selection and confirmation with improved power and efficiency. Statistical Methods in Medical Research, 30(4), 1013-1025.
}
\author{
Tianyu Zhan (tianyu.zhan.stats@gmail.com)
}
