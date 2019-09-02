rm(list=ls())
source("SetValues.R")


save_plot <- T
samplename <- "TestRun0"


source("RunModel.R")
      
file <- paste0("TBE_all.csv")
temp <- read.table(file,header=TRUE,sep=",")
setwd(mainDir)
      
if (samplename=="TestRun0") {
  mp <- temp
} 
rm(temp)
params = unlist(mp[1,])
      
coef(po1) <- params[c(params.noic, params.ic)]
coef(po1,"tau") <- 1e-6


ymax <- signif((1.4*max(subset(data,select=reports),na.rm=TRUE)),0)
ymax <- 15


titleset <- params.noic
t <- NULL
for (q in 1:(length(params.noic)+length(params.ic))) {
  if (names(coef(po1)[q]) %in% titleset) {
    if (is.null(t)) {
      t <- paste0(names(coef(po1)[q]), "=",signif(coef(po1)[q],2))
    } else if ( names(coef(po1)[q]) %in% c("beta_nh") ){
      t <- paste0(t, ",\n", names(coef(po1)[q]), "=",signif(coef(po1)[q],2))
    } else {
      t <- paste0(t, ", ", names(coef(po1)[q]), "=",signif(coef(po1)[q],2))
    }
  }
}
OrigLogLik <- params["LogLik"] 
t <- paste0(t, ", LogLik=", signif(OrigLogLik,4))
      
x <- trajectory(po1,as.data.frame=TRUE)
reports <-  x[,"cases"] *(x[,"cases"]>=0)

x <- cbind(x,reports)
x[,"time"] <- x[,"time"]/365 + startTime
covars[,"time"] <- covars[,"time"]/365 + startTime
data[,"time"] <- data[,"time"]/365 + startTime

x_table <- subset(x, select=c("time", "reports")) 
      
x_table[,"type"] <- "traj"
data <- data[-which(is.na(data[,"reports"])),]
x_table <- rbind(x_table,cbind(data,type=rep("data",nrow(data))))

if (save_plot) {
  pdf(paste0("TBE_simulation.pdf"),width=8,height=4)
}

p <- ggplot(subset(x_table,time>1997+1/52),aes(x=time,y=reports)) +
      geom_line(aes(color=type),size=0.65,alpha=0.6) +
      coord_cartesian(ylim=c(0,ymax),xlim=c(1997+1/52,endTime)) +
      ggtitle(t) +
      theme_bw() +
      theme(legend.position = "right", axis.text = element_text(size = 5)) +
      theme(plot.title=element_text(size=5)) 
plot(p)    
if (save_plot) {
  dev.off()
  plot(p)
}


x <- subset(x,subset=time>1997)
x2_table <-melt(x[,c("time",statenames)],id="time")
      
if (save_plot) {
  pdf(paste0("TBE_states.pdf"),width=8,height=8)
}


p <- ggplot(x2_table,aes(x=time,y=value)) +
  geom_line(aes(color=variable)) +
  facet_grid(variable ~ ., scale="free_y") + 
  ggtitle(t) +
  theme_bw() +
  theme(legend.position = "right", axis.text = element_text(size = 8)) +
  theme(plot.title=element_text(size=8)) 
plot(p)    

if (save_plot) {
  dev.off()
  plot(p)
}

p <- ggplot(covars,aes(x=time,y=temperature))+geom_line()+theme_bw()
plot(p)