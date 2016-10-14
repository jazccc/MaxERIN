# Maximum Expected Retirement Income Need

MaxERIN_05 <- function() {
  
  # ++++ Calculate the adjusted expected return matrices (after AFER) +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  AFER                      <-          TaxFactor$VAFER
  
  assetList <- list(adj.Asset1 = Asset1 - AFER,
                    adj.Asset2 = Asset2 - AFER,
                    adj.Asset3 = Asset3 - AFER,
                    adj.Asset4 = Asset4 - AFER,
                    adj.Asset5 = Asset5 - AFER,
                    adj.Asset6 = Asset6 - AFER,
                    adj.Asset7 = Asset7 - AFER,
                    adj.Asset8 = Asset8 - AFER,
                    adj.Asset9 = Asset9 - AFER,
                    adj.Asset10= Asset10 - AFER)

  
  #  ++++ Prepare weighted returns ++++++++++++++++++++++++++++++++++++++++++++++++++
  assets.nq <- which(colSums(Allocations.nq != 0) >0) # which assets are used in NQ?
  assets.q <- which(colSums(Allocations.q != 0) >0)   # which assets are used in Q?
  
  wr.temp <- matrix(0, nrow = 2000, ncol = 100) # templete for wr
  # sum w_i * r_i for NQ
  for (index in assets.nq) {
    wr.temp <- wr.temp + t(assetList[[index]] + 1) %*% diag(Allocations.nq[,index])
  }
  wr.all.nq <- as.data.frame(t(wr.temp))
  
  wr.temp <- wr.temp * 0  # Reset templete
  # sum w_i * r_i for Q
  for (index in assets.q) {
    wr.temp <- wr.temp + t(assetList[[index]] + 1) %*% diag(Allocations.q[,index])
  }
  wr.all.q <- as.data.frame(t(wr.temp))  
  # ++++ Pre-retirement Modeling ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  preYears <- BasicInputs$Retirement.Age - BasicInputs$Current.Age # Num of years in Pre-stage
  pre.salary.nq <- pre.salary$Split.to.NQ[1 : preYears] # pre salary contribute to NQ
  pre.salary.q <- pre.salary$Split.to.Q[1 : preYears]    # pre salary contribute to Q
  
  # Truncate weighted return for pre-stage
  pre.wr.nq <- (wr.all.nq[1:preYears, ] - 1) * (preYears > 0) + 1
  pre.wr.q <- (wr.all.q[1:preYears, ] - 1) * (preYears > 0) + 1
  
  # Accumulated return in each year
  pre.wr.nq.cum <- cumprod(pre.wr.nq[preYears : 1, ])[preYears : 1, ]
  pre.wr.q.cum <- cumprod(pre.wr.q[preYears : 1, ])[preYears : 1, ]
  
  # Accumulated salary in NQ / Q
  preEndVal.salary.nq <- apply(pre.salary.nq * pre.wr.nq.cum, 2, sum) 
  preEndVal.salary.q <- apply(pre.salary.q * pre.wr.q.cum, 2, sum)
  
  # Get NQ and Q initial balance
  nq.init <- BasicInputs$NQ
  q.init <- BasicInputs$Qual
  
  # Total accumulated value in NQ / Q at retirement
  preEndVal.nq <- nq.init * pre.wr.nq.cum[1, ] + preEndVal.salary.nq
  preEndVal.q <- q.init * pre.wr.q.cum[1, ] + preEndVal.salary.q
  
  # Calculate the starting total assets b0 at retirement age
  
  b0 <- preEndVal.nq + preEndVal.q
  
  # ++++ Post-retirement Modeling +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # truncate the wr matrix by preYears in order to get the adjusted wr matrix for the post-retirement modeling
  
  wr                        <-          wr.all.nq[(preYears + 1) : nrow(wr.all.nq), ]
  
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
  
  #d0                        <-          unname(quantile(apply(ERIN.Collection, 1, min), BasicInputs$Ruin.Target))
  probs                     <-          c(BasicInputs$Ruin.Target,0.05, 0.25, 0.5)
  multi                     <-          unname(quantile(apply(ERIN.Collection, 1, min), probs))
  
  Retirement.Year               <-          BasicInputs$Retirement.Age - BasicInputs$Current.Age
  Infl.Adj                      <-          InflationTaper$InflAdj[Retirement.Year + 1]
  #Infl.Adj.d0                   <-          d0 / Infl.Adj
  Infl.Adj.multi                <-          multi / Infl.Adj
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  result                    <-          Infl.Adj.multi + SocSec$Soc.Sec..After.Tax.[1] +
                                            Pension$Pension..After.Tax.[1] + AnnuitiesAfterTax$Annuities..After.Tax.[1]
  
  return(result)
  
}