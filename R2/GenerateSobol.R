source("SetValues.R")

no.points <- 1e3
nstart <- 1


# ab <- rep(-2,10)
# names(ab) <- c(paste0("a",seq(0,4)),paste0("b",seq(0,4)))
ic <- rep(0,length(statenames))
names(ic) <- paste0(statenames,0)

LowerBounds <- c(p=2000, omega=0.1, delta=0.5,
                 mu_e=0.02618, mu_ql=0.0068, mu_el=0.001428, mu_qn=0.0034, mu_en=0.000476,
                 mu_qa=0.00136, mu_ea=0.000408, mu_h=1/365,
                 beta_nh=0.9, beta_hl=0.8, beta_hn=0.8, 
                 # tau=0.1, 
                 lambda_l=0.005,lambda_n=0.005,lambda_a=0.005, alpha=0.01, f_l=0.1, f_n=0.1, f_a=0.1,kappa=0.0001, 
                 c=0.4, Tf=4, obsprob=0.2, T_min_l = 5,gamma=1/100,
                 ic)
UpperBounds <- LowerBounds    
UpperBounds["omega"] <- 10
# UpperBounds["tau"] <- 1
UpperBounds["lambda_l"] <- 0.1
UpperBounds["lambda_n"] <- 0.1
UpperBounds["lambda_a"] <- 0.1
UpperBounds["alpha"] <- 100
UpperBounds["f_l"] <- 1
UpperBounds["f_n"] <- 1
UpperBounds["f_a"] <- 1 
UpperBounds["kappa"] <- 0.1
UpperBounds["obsprob"]<-0.5
UpperBounds["T_min_l"]<-11
UpperBounds["gamma"] <- 0.5

UpperBounds[names(ic)] <- c(rep(100,11),0.5,0.5)

ParamRanges<-sobolDesign(LowerBounds, UpperBounds, no.points)
ParamRanges <- ParamRanges[(nstart):(no.points),]
ParamSetFile <- paste0("ParamSet_TBE.csv")
write.csv(ParamRanges,ParamSetFile,row.names=FALSE)
setwd(mainDir)
