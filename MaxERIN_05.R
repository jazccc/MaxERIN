# Maximum Expected Retirement Income Need

MaxERIN_05 <- function() {
  
  # ++++ Calculate the adjusted expected return matrices (after AFER) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  AFER                      <-          BasicInputs$AFER
  
  adj.Asset1                <-          Asset1 - AFER
  
  adj.Asset2                <-          Asset2 - AFER
  
  adj.Asset3                <-          Asset3 - AFER
  
  adj.Asset4                <-          Asset4 - AFER
  
  adj.Asset5                <-          Asset5 - AFER
  
  adj.Asset6                <-          Asset6 - AFER
  
  adj.Asset7                <-          Asset7 - AFER
  
  adj.Asset8                <-          Asset8 - AFER
  
  adj.Asset9                <-          Asset9 - AFER
  
  adj.Asset10               <-          Asset10 - AFER
  
  # ++++ Pre-retirement Modeling ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
  pre.salary.nq <- pre.salary$Split.to.NQ[pre.salary$Split.to.NQ>0] # pre salary contribute to NQ
  pre.salary.q <- pre.salary$Split.to.Q[pre.salary$Split.to.Q>0]    # pre salary contribute to Q
  
  preYears <- length(pre.salary.nq) # Num of years in Pre-stage
  
  # Weighted return for NQ
  wr.all.nq <- t((1 + t(adj.Asset1) %*% diag(Allocations.nq[,1])) + 
                   (1 + t(adj.Asset2)) %*% diag(Allocations.nq[,2]) +
                   (1 + t(adj.Asset3)) %*% diag(Allocations.nq[,3]) + 
                   (1 + t(adj.Asset4)) %*% diag(Allocations.nq[,4]) +
                   (1 + t(adj.Asset5)) %*% diag(Allocations.nq[,5]) + 
                   (1 + t(adj.Asset6)) %*% diag(Allocations.nq[,6]) +
                   (1 + t(adj.Asset7)) %*% diag(Allocations.nq[,7]) + 
                   (1 + t(adj.Asset8)) %*% diag(Allocations.nq[,8]) +
                   (1 + t(adj.Asset9)) %*% diag(Allocations.nq[,9]) + 
                   (1 +t(adj.Asset10)) %*% diag(Allocations.nq[,10]))
  # Weighted return for Q
  wr.all.q <- t((1 + t(adj.Asset1) %*% diag(Allocations.q[,1])) + 
                  (1 + t(adj.Asset2)) %*% diag(Allocations.q[,2]) +
                  (1 + t(adj.Asset3)) %*% diag(Allocations.q[,3]) + 
                  (1 + t(adj.Asset4)) %*% diag(Allocations.q[,4]) +
                  (1 + t(adj.Asset5)) %*% diag(Allocations.q[,5]) + 
                  (1 + t(adj.Asset6)) %*% diag(Allocations.q[,6]) +
                  (1 + t(adj.Asset7)) %*% diag(Allocations.q[,7]) + 
                  (1 + t(adj.Asset8)) %*% diag(Allocations.q[,8]) +
                  (1 + t(adj.Asset9)) %*% diag(Allocations.q[,9]) + 
                  (1 +t(adj.Asset10)) %*% diag(Allocations.q[,10]))
  # Truncate weighted return for pre-stage
  pre.wr.nq <- wr.all.nq[1:preYears, ]
  pre.wr.q <- wr.all.q[1:preYears, ]
  
  pre.wr.nq <- as.data.frame(pre.wr.nq)
  pre.wr.q <- as.data.frame(pre.wr.q)
  # Accumulated return in each year
  pre.wr.nq.cum <- cumprod(pre.wr.nq[preYears : 1, ])[preYears : 1, ]
  pre.wr.q.cum <- cumprod(pre.wr.q[preYears : 1, ])[preYears : 1, ]
  
  # Accumulated salary in NQ / Q
  preEndVal.salary.nq <- apply(pre.salary.nq * pre.wr.nq.cum, 2, sum) 
  preEndVal.salary.q <- apply(pre.salary.q * pre.wr.q.cum, 2, sum)
  
  # Get NQ and Q initial balance
  # q.init <- BasicInputs$Qualified.Initial.Portfolios  
  # nq.init <- BasicInputs$Initial.Portfolios - q.init
  
  q.init <- BasicInputs$Qual
  nq.init <- BasicInputs$NQ
  
  # Total accumulated value in NQ / Q at retirement
  preEndVal.nq <- nq.init * pre.wr.nq.cum[1, ] + preEndVal.salary.nq
  preEndVal.q <- q.init * pre.wr.q.cum[1, ] + preEndVal.salary.q
  
  # Calculate the starting total assets b0 at retirement age
  
  b0 <- preEndVal.nq + preEndVal.q
  
  # ++++ Post-retirement Modeling +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # truncate the wr matrix by preYears in order to get the adjusted wr matrix for the post-retirement modeling
  
  wr                        <-          wr.all.nq[(preYears + 1) : nrow(adj.Asset1), ]
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  n                         <-          c(which(InfusionOfCapital$Infusion.of.Capital != 0), BasicInputs$Evaluation.Age - BasicInputs$Retirement.Age)
  ERIN.Collection           <-          rep(0, ncol(Asset1))
  
  for (i in 1 : length(n)) {
    
    # f1
    
    f1                        <-          colProds(wr[1 : n[i], ])
    
    # f2
    
    IT                        <-          InflationTaper$CumuIT[2 : n[i]]
    
    wrTemp                    <-          wr[2 : n[i], ][c(nrow(wr[2 : n[i], ]) : 1), ]
    
    wrProd                    <-          apply(wrTemp, 2, cumprod)
    
    wrAdj                     <-          wrProd[c(nrow(wr[2 : n[i], ]) : 1), ]
    
    ITwr                      <-          IT * wrAdj
    
    f2                        <-          colSums(ITwr)
    
    # f3
    
    SPIC                      <-          (SocSec$Soc.Sec..After.Tax. + Pension$Pension..After.Tax. + InfusionOfCapital$Infusion.of.Capital)[2 : n[i]]
    
    SPICwr                    <-          SPIC * wrAdj
    
    f3                        <-          colSums(SPICwr)
    
    # f6
    
    ISPIA                     <-          AnnuitiesAfterTax$Cumu.I.SPIA[2 : n[i]]     
    
    IPIAwr                    <-          ISPIA * wrAdj
    
    f6                        <-          colSums(IPIAwr)
    
    # solution
    
    ERIN.Vec                  <-          (as.vector(unlist(b0)) * f1 - 
                                          (SocSec$Soc.Sec..After.Tax. + Pension$Pension..After.Tax. + InfusionOfCapital$Infusion.of.Capital)[1] * f2 +
                                          f3) / (f1 + f2) +
                                          AnnuitiesAfterTax$Annuities..After.Tax.[1] * (- f2 + f6) / (f1 + f2)
    
    ERIN.Collection           <-          cbind(ERIN.Collection, ERIN.Vec)
    
  }
  
  ERIN.Collection           <-          as.matrix(ERIN.Collection[ , -1])
  
  # compare ERIN.Vec and ERIN.Vec.IC(s), choose the minimum to be the final solustion of ERIN (expected retirement living needs)
  
  d0                        <-          quantile(apply(ERIN.Collection, 1, min), BasicInputs$Ruin.Target)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  result                    <-          list(Customized.Maximum.Expected.Retirement.Income.Need     =     d0)
  
  return(result)
  
}