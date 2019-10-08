rm(list=ls())
source("SetValues.R")
library(RColorBrewer)
library(xtable)

modeltype <- 1
# 
# runs <- seq(3,10)
# runs <- c(seq(3,7),9,10,14)
runs <- 3

save_plot <- T
matrix_plot <- F
tol <- 100
no_profile <- 101
lscale <- 0

alpha_fit <- 0.10
span_tol <- 30

theta <- 0.2
cutoff <- qchisq(p=0.95,df=1)/2


ymin <- -1320
ymax <- -1280
table_MLE <- NULL

for (run in runs) {
  
  table_values <- NULL

  source("DetermineRunType.R")
  setwd(mainDir)
  if (lscale==0) {
    form <- as.formula(paste0("LogLik~",param))
  } else {
    form <- as.formula(paste0("LogLik~log(",param,")"))
  }
  print(param)
  setwd(mainDir)
  
  file <- paste0("TBE_all.csv")
  print(file)
  dataset <- read.table(file, header=TRUE,sep=",")
  
  k1 <- ceiling(max(dataset[,"LogLik"]))
  dataset <- subset(dataset,subset=LogLik>k1-tol&LogLik<0)
  print (nrow(dataset))
  if (lscale==1) {
    param_array <- exp(seq(log(param_lims[1]),log(param_lims[2]),length=no_profile))
  } else {
    param_array <- seq(param_lims[1],param_lims[2],length=no_profile)
  }
    
  param_profile <- NULL
  for (q in 2:(length(param_array))) {
    ind1 <- which(dataset[,param]>=param_array[q-1] & dataset[,param]<=param_array[q])
    if (length(ind1)>0) {
      set1 <- dataset[ind1,]
      set1 <- set1 [order ( set1[,"LogLik"],decreasing=TRUE),]
      param_profile <- rbind(param_profile,cbind(set1[1,],endTime = endTime))
    }
  }
  print (nrow(param_profile))
  
  fit<- loess(form,param_profile,span=alpha_fit)
  i0 <- which.max(param_profile[,"LogLik"])
  k0 <- param_profile[i0,"LogLik"]
  x0 <- param_profile[i0,param]
  if (lscale==0) {
    f <- function(x) {
      out <- predict(fit,x)
      return(out)
    }
  } else {
    f <- function(x) {
      out <- predict(fit,log10(x))
      return(out)
    }
  }  
  # # k0 <- f(x0)
  # temp <- optimize(f,lower=(1-theta)*x0 + theta*param_lims[1],
  #                  upper=(1-theta)*x0 + theta*param_lims[2],maximum=TRUE)
  #   
  # rm(temp)
    
  if (lscale==1) {
    param_array <- exp(seq(log10(min(param_profile[,param])),log10(max(param_profile[,param])),length=10*no_profile))
  } else {
    param_array <- seq(min(param_profile[,param]),max(param_profile[,param]),length=10*no_profile)
  }
  # xmin <- param_array[min(which(f(param_array) > k0 - qchisq(p=0.95,df=1)/2))]
  # xmax <- param_array[max(which(f(param_array) > k0 - qchisq(p=0.95,df=1)/2))]
    
  
  # MLE <- cbind.data.frame(MLE=x0,param=param,run=run,
  #                         LogLik=k0,xmin=xmin,xmax=xmax)
  # table_MLE <- rbind(table_MLE,MLE)
    
  table_values <- rbind(table_values,cbind.data.frame(unname(param_profile[,c(param,"LogLik")]),param=param,run=run))
    

  
  
  
  table_values <- as.data.frame(table_values)
  
  colnames(table_values) <- c("Values","LogLik", "Parameter", "run")
  
  
  table_values[,"Values"] <- as.numeric(table_values[,"Values"])
  table_values[,"run"] <- as.factor(table_values[,"run"])
  
  setwd(mainDir)
  if (save_plot) {
    pdf(paste0("Profile_type", paste0(run), "_",param,".pdf"),width=6,height=4)      
  }
  
  
  cols <- brewer.pal(8, "Set2")
  # cols <- cols[modeltypes]
  
  # table_rect <- MLE[,c("run", "xmin", "xmax", "LogLik")]
  # table_rect[,"ymin"] <- ymin
  # table_rect[,"ymax"] <- ymax
  # table_rect <- as.data.frame(table_rect)
  
  
  
  
  if (lscale==0) {
    p <- ggplot(table_values,aes_string(x="Values",y="LogLik")) +
      # geom_rect(data=table_rect,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),inherit.aes=F,alpha=0.2) +
      geom_point(aes(colour=run)) + 
      scale_colour_manual(values = cols,labels=runs) +
      geom_smooth(method="loess",span=alpha_fit,colour="black") +
      # geom_vline(aes(xintercept=MLE), data=MLE[,c("MLE","run","LogLik")]) +
      xlab(param)+
      theme_bw() + 
      scale_y_continuous(limits = c(ymin,ymax)) +
      scale_x_continuous(limits = c(param_lims[1],param_lims[2]))  +
      theme(legend.position = "none")
    # scale_y_continuous(limits = c(floor(min(table_values[,"LogLik"])), ceiling(max(table_values[,"LogLik"]))))
    plot(p)
    
  } else {
    p <- ggplot(table_values,aes(x=log(Values),y=LogLik)) +
      geom_point(aes(colour=run)) + 
      scale_colour_manual(values = cols,labels=runs) +
      geom_smooth(method="loess",span=alpha_fit,colour="black") +
      # facet_grid(runs~., scale="free") +
      geom_vline(aes(xintercept=log(MLE)), data=table_MLE[,c("MLE","run","LogLik")]) +
      theme(legend.position = "none")
    # scale_y_continuous(limits = c(floor(min(table_values[,"LogLik"])), ceiling(max(table_values[,"LogLik"]))))
    plot(p)
    
  }
  
  if (save_plot) {
    dev.off()
    plot(p)
  }
  
  
  
}
# rm(run,ind1)
# 
# rownames(table_MLE) <- runs
# colnames(table_MLE) <- c("MLE","Parameter","run","LogLik","xmin","xmax")
# 
# # table_MLE[,"run"] <- as.factor(table_MLE[,"run"])
# # table_MLE[,"MLE"] <- as.numeric(table_MLE[,"MLE"])
# table_MLE <- as.data.frame(table_MLE)
# table_MLE[,"run"] <- as.factor(table_MLE[,"run"])
# print(table_MLE)
# xtable(table_MLE[,c("MLE","LogLik","xmin","xmax")])
# 
# # plot(param_profile[,c("alpha", "f_a", "lambda_a", "kappa", param, "LogLik")])
