
## Get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## Calculate parallel encouragement
mediate.ped2 <- function(outcome, mediator, treat, encourage, data) {
  
  varnames <- c(outcome, mediator, treat, encourage)
  data <- data[,varnames]
  if(sum(is.na(data))){
    warning("NA's in data. Observations with missing data removed.")
    data <- na.omit(data)
  }
  data <- sapply(data, function(x) as.numeric(as.character(x)))
  
  check.1 <- apply(data[,1:3], 2, function(x) identical(sort(unique(x)), c(0,1)))
  #check.2 <- identical(sort(unique(data[,4])), c(-1,0,1))
  check.2 <- identical(sort(unique(data[,4])), c(0,1))
  if(sum(check.1, check.2) != ncol(data)) {
    stop("Invalid values in variables.")
  }
  
  out <- mechanism.bounds(data[,outcome], data[,mediator], data[,treat],
                          data[,encourage], design = "PED")
  out
}

#Bounds Function for parallel, parallel encouragement, and single experiment designs
mechanism.bounds <- function(outcome, mediator, treatment, DorZ, design) {
  d <- matrix(, nrow=length(outcome), ncol=3)
  
  d[,1] <- outcome
  d[,2] <- treatment
  d[,3] <- mediator
  d <- as.data.frame(d)
  names(d) <- c("Y","T","M")
  nobs <- nrow(d)
  
  ## "DorZ" indicates the variable that assigns manipulation direction.
  ## Z in the JRSSA paper. This is left to null for the single experiment design.
  
  # Single Experiment
  if(design=="SED") {
    d$Z <- 0
    P111 <- sum(d$Y==1 & d$M==1 & d$Z==0 & d$T==1)/sum(d$T==1 & d$Z==0)
    P101 <- sum(d$Y==1 & d$M==0 & d$Z==0 & d$T==1)/sum(d$T==1 & d$Z==0)
    P001 <- sum(d$Y==0 & d$M==0 & d$Z==0 & d$T==1)/sum(d$T==1 & d$Z==0)
    P011 <- sum(d$Y==0 & d$M==1 & d$Z==0 & d$T==1)/sum(d$T==1 & d$Z==0)
    
    P110 <- sum(d$Y==1 & d$M==1 & d$Z==0 & d$T==0)/sum(d$T==0 & d$Z==0)
    P100 <- sum(d$Y==1 & d$M==0 & d$Z==0 & d$T==0)/sum(d$T==0 & d$Z==0)
    P000 <- sum(d$Y==0 & d$M==0 & d$Z==0 & d$T==0)/sum(d$T==0 & d$Z==0)
    P010 <- sum(d$Y==0 & d$M==1 & d$Z==0 & d$T==0)/sum(d$T==0 & d$Z==0)
    
    l.delta.t <- max(-P001-P011, -P000-P001-P100, -P011-P010-P110)
    u.delta.t <- min(P101+P111, P000+P100+P101, P010+P110+P111)
    l.delta.c <- max(-P100-P110, -P001-P101-P100, -P011-P111-P110)
    u.delta.c <- min(P000+P010, P011+P111+P010, P000+P001+P101)
    S.t <- cbind(l.delta.t,u.delta.t)
    S.c <- cbind(l.delta.c,u.delta.c)
    
    # Parallel
  } else if(design=="PD") {
    ## D indicates if there was manipulation
    d$D <- DorZ
    Z111 <- sum(d$Y==1 & d$T==1 & d$M==1 & d$D==1)/sum(d$T==1 & d$M==1 & d$D==1)
    Z011 <- sum(d$Y==0 & d$T==1 & d$M==1 & d$D==1)/sum(d$T==1 & d$M==1 & d$D==1)
    Z001 <- sum(d$Y==0 & d$T==1 & d$M==0 & d$D==1)/sum(d$T==1 & d$M==0 & d$D==1)
    Z110 <- sum(d$Y==1 & d$T==0 & d$M==1 & d$D==1)/sum(d$T==0 & d$M==1 & d$D==1)
    Z010 <- sum(d$Y==0 & d$T==0 & d$M==1 & d$D==1)/sum(d$T==0 & d$M==1 & d$D==1)
    Z000 <- sum(d$Y==0 & d$T==0 & d$M==0 & d$D==1)/sum(d$T==0 & d$M==0 & d$D==1)
    Z101 <- sum(d$Y==1 & d$T==1 & d$M==0 & d$D==1)/sum(d$T==1 & d$M==0 & d$D==1)
    Z100 <- sum(d$Y==1 & d$T==0 & d$M==0 & d$D==1)/sum(d$T==0 & d$M==0 & d$D==1)
    
    P111 <- sum(d$Y==1 & d$M==1 & d$T==1 & d$D==0)/sum(d$T==1 & d$D==0)
    P101 <- sum(d$Y==1 & d$M==0 & d$T==1 & d$D==0)/sum(d$T==1 & d$D==0)
    P001 <- sum(d$Y==0 & d$M==0 & d$T==1 & d$D==0)/sum(d$T==1 & d$D==0)
    P011 <- sum(d$Y==0 & d$M==1 & d$T==1 & d$D==0)/sum(d$T==1 & d$D==0)
    P110 <- sum(d$Y==1 & d$M==1 & d$T==0 & d$D==0)/sum(d$T==0 & d$D==0)
    P100 <- sum(d$Y==1 & d$M==0 & d$T==0 & d$D==0)/sum(d$T==0 & d$D==0)
    P000 <- sum(d$Y==0 & d$M==0 & d$T==0 & d$D==0)/sum(d$T==0 & d$D==0)
    P010 <- sum(d$Y==0 & d$M==1 & d$T==0 & d$D==0)/sum(d$T==0 & d$D==0)
    
    P.l.delta.t <- max(-P001-P011, -P011-P010-P110-P001+Z001, -P000-P001-P100-P011+Z011, -P001-P011+Z001-Z111, -P001+P101-Z101, -P011+P111-Z111)
    P.u.delta.t <- min(P101+P111, P010+P110+P101+P111-Z101, P000+P100+P101+P111-Z111, P101+P111+Z001-Z111, P111-P011+Z011, P101-P001+Z001)
    P.l.delta.c <- max(-P100-P110, -P011-P111-P110-P100+Z000, -P001-P101-P100-P110+Z110,-P100-P110+Z100-Z010, -P110+P010-Z010, -P100+P000-Z100)
    P.u.delta.c <- min(P000+P010, P011+P111+P010+P000-Z100, P000+P001+P101+P010-Z010, P000+P010+Z100-Z010, P010-P110+Z110, P000-P100+Z100)
    P.t <- cbind(P.l.delta.t,P.u.delta.t)
    P.c <- cbind(P.l.delta.c,P.u.delta.c)
    
    #Parallel Encouragement
  } else if(design=="PED") {
    d$Z <- DorZ
    dP000 <- sum(d$Y==0 & d$M==0 & d$T==0 & d$Z==-1)/sum(d$T==0 & d$Z==-1)
    dP001 <- sum(d$Y==0 & d$M==0 & d$T==1 & d$Z==-1)/sum(d$T==1 & d$Z==-1)
    dP010 <- sum(d$Y==0 & d$M==1 & d$T==0 & d$Z==-1)/sum(d$T==0 & d$Z==-1)
    dP011 <- sum(d$Y==0 & d$M==1 & d$T==1 & d$Z==-1)/sum(d$T==1 & d$Z==-1)
    dP100 <- sum(d$Y==1 & d$M==0 & d$T==0 & d$Z==-1)/sum(d$T==0 & d$Z==-1)
    dP101 <- sum(d$Y==1 & d$M==0 & d$T==1 & d$Z==-1)/sum(d$T==1 & d$Z==-1)
    dP110 <- sum(d$Y==1 & d$M==1 & d$T==0 & d$Z==-1)/sum(d$T==0 & d$Z==-1)
    dP111 <- sum(d$Y==1 & d$M==1 & d$T==1 & d$Z==-1)/sum(d$T==1 & d$Z==-1)
    
    tP000 <- sum(d$Y==0 & d$M==0 & d$T==0 & d$Z==0)/sum(d$T==0 & d$Z==0)
    tP001 <- sum(d$Y==0 & d$M==0 & d$T==1 & d$Z==0)/sum(d$T==1 & d$Z==0)
    tP010 <- sum(d$Y==0 & d$M==1 & d$T==0 & d$Z==0)/sum(d$T==0 & d$Z==0)
    tP011 <- sum(d$Y==0 & d$M==1 & d$T==1 & d$Z==0)/sum(d$T==1 & d$Z==0)
    tP100 <- sum(d$Y==1 & d$M==0 & d$T==0 & d$Z==0)/sum(d$T==0 & d$Z==0)
    tP101 <- sum(d$Y==1 & d$M==0 & d$T==1 & d$Z==0)/sum(d$T==1 & d$Z==0)
    tP110 <- sum(d$Y==1 & d$M==1 & d$T==0 & d$Z==0)/sum(d$T==0 & d$Z==0)
    tP111 <- sum(d$Y==1 & d$M==1 & d$T==1 & d$Z==0)/sum(d$T==1 & d$Z==0)
    
    sP000 <- sum(d$Y==0 & d$M==0 & d$T==0 & d$Z==1)/sum(d$T==0 & d$Z==1)
    sP001 <- sum(d$Y==0 & d$M==0 & d$T==1 & d$Z==1)/sum(d$T==1 & d$Z==1)
    sP010 <- sum(d$Y==0 & d$M==1 & d$T==0 & d$Z==1)/sum(d$T==0 & d$Z==1)
    sP011 <- sum(d$Y==0 & d$M==1 & d$T==1 & d$Z==1)/sum(d$T==1 & d$Z==1)
    sP100 <- sum(d$Y==1 & d$M==0 & d$T==0 & d$Z==1)/sum(d$T==0 & d$Z==1)
    sP101 <- sum(d$Y==1 & d$M==0 & d$T==1 & d$Z==1)/sum(d$T==1 & d$Z==1)
    sP110 <- sum(d$Y==1 & d$M==1 & d$T==0 & d$Z==1)/sum(d$T==0 & d$Z==1)
    sP111 <- sum(d$Y==1 & d$M==1 & d$T==1 & d$Z==1)/sum(d$T==1 & d$Z==1)
    
    
    P0 <- c(dP000,dP010,dP100,dP110,tP000,tP010,tP100,tP110,sP000,sP010,sP100,sP110)
    P1 <- c(dP001,dP011,dP101,dP111,tP001,tP011,tP101,tP111,sP001,sP011,sP101,sP111)
    Q0 <- c(dP010+dP110,tP010+tP110,sP010+sP110)
    Q1 <- c(dP011+dP111,tP011+tP111,sP011+sP111)
    
    #Linear programming functions
    bounds.bidir.cmpl <- function(P,Q,dir){
      f.obj <- c(rep(0,16), 0,0,1,0, 0,0,1,0, 0,-1,0,0, 0,-1,0,0,
                 0,0,-1,0, 0,0,-1,0, 0,1,0,0, 0,1,0,0, rep(0,16))
      f.con <- matrix(c(
        rep(c(1,1,1,0),8), rep(0,32), #dP00t
        rep(c(rep(c(0,0,0,1),4), rep(0,16)),2), #dP01t
        rep(0,32), rep(c(1,1,1,0),8), #dP10t
        rep(c(rep(0,16), rep(c(0,0,0,1),4)),2), #dP11t
        rep(c(1,1,0,0),8), rep(0,32), #tP00t
        rep(c(rep(c(0,0,1,1),4), rep(0,16)),2), #tP01t
        rep(0,32), rep(c(1,1,0,0),8), #tP10t
        rep(c(rep(0,16), rep(c(0,0,1,1),4)),2), #tP11t
        rep(c(1,0,0,0),8), rep(0,32), #sP00t
        rep(c(rep(c(0,1,1,1),4), rep(0,16)),2), #sP01t
        rep(0,32), rep(c(1,0,0,0),8), #sP10t
        rep(c(rep(0,16), rep(c(0,1,1,1),4)),2), #sP11t
        rep(c(rep(0,12), rep(1,4)),4), #dQ
        rep(c(rep(0,8), rep(1,8)),4), #tQ
        rep(c(rep(0,4), rep(1,12)),4), #sQ
        rep(1,64) #sum=1
      ), nrow=16, byrow=T)
      f.dir <- rep("=", 16)
      f.rhs <- c(P,Q,1)
      r <- lp(dir, f.obj, f.con, f.dir, f.rhs)
      r$objval
    }
    
    bounds.bidir.popl <- function(P,Q,dir){
      f.obj <- c(rep(0,16), 0,0,1,1, 0,0,1,1, -1,-1,0,0, -1,-1,0,0,
                 0,0,-1,-1, 0,0,-1,-1, 1,1,0,0, 1,1,0,0, rep(0,16))
      f.con <- matrix(c(
        rep(c(1,1,1,0),8), rep(0,32), #dP00t
        rep(c(rep(c(0,0,0,1),4), rep(0,16)),2), #dP01t
        rep(0,32), rep(c(1,1,1,0),8), #dP10t
        rep(c(rep(0,16), rep(c(0,0,0,1),4)),2), #dP11t
        rep(c(1,1,0,0),8), rep(0,32), #tP00t
        rep(c(rep(c(0,0,1,1),4), rep(0,16)),2), #tP01t
        rep(0,32), rep(c(1,1,0,0),8), #tP10t
        rep(c(rep(0,16), rep(c(0,0,1,1),4)),2), #tP11t
        rep(c(1,0,0,0),8), rep(0,32), #sP00t
        rep(c(rep(c(0,1,1,1),4), rep(0,16)),2), #sP01t
        rep(0,32), rep(c(1,0,0,0),8), #sP10t
        rep(c(rep(0,16), rep(c(0,1,1,1),4)),2), #sP11t
        rep(c(rep(0,12), rep(1,4)),4), #dQ
        rep(c(rep(0,8), rep(1,8)),4), #tQ
        rep(c(rep(0,4), rep(1,12)),4), #sQ
        rep(1,64) #sum=1
      ), nrow=16, byrow=T)
      f.dir <- rep("=", 16)
      f.rhs <- c(P,Q,1)
      r <- lp(dir, f.obj, f.con, f.dir, f.rhs)
      r$objval
    }
    
    BE.t <- c(bounds.bidir.popl(P1, Q0, "min"), bounds.bidir.popl(P1, Q0, "max"))
    BE.c <- c(-bounds.bidir.popl(P0 ,Q1, "max"), -bounds.bidir.popl(P0, Q1, "min"))
    num.BET.t.lo <- bounds.bidir.cmpl(P1, Q0, "min")
    num.BET.t.up <- bounds.bidir.cmpl(P1, Q0, "max")
    num.BET.c.lo <- -bounds.bidir.cmpl(P0, Q1, "max")
    num.BET.c.up <- -bounds.bidir.cmpl(P0, Q1, "min")
    denom.BET.t <- P1[1] + P1[3] - P1[9] - P1[11]
    denom.BET.c <- P0[1] + P0[3] - P0[9] - P0[11]
    BET.t <- c(num.BET.t.lo/denom.BET.t, num.BET.t.up/denom.BET.t)
    BET.c <- c(num.BET.c.lo/denom.BET.c, num.BET.c.up/denom.BET.c)
    
  }#end computation section
  
  # Output
  if(design=="SED"){
    out <- list(d1 = S.t, d0 = S.c, design=design, nobs=nobs)
  } else if(design=="PD"){
    out <- list(d1 = P.t, d0 = P.c, design=design, nobs=nobs)
  } else if(design=="PED") {
    out <- list(d1=BE.t, d0=BE.c,  d1.p=BET.t, d0.p=BET.c, design=design, nobs=nobs)
  }
  
  rm(d)
  class(out) <- "mediate.design"
  out
}

