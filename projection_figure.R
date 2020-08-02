library(dplyr)
library(ggplot2)


# organize data
repfile<-scan("19_snow_gmacs_rbar/mcoutSSB.REP")
hist_ssb<-matrix(repfile,ncol=length(M[[1]]$mod_yrs),byrow=T)
colnames(hist_ssb)<-M[[1]]$mod_yrs

projfile<-as.data.frame(matrix(scan("19_snow_gmacs_rbar/mcoutPROJ.REP"),nrow=600,byrow=TRUE))
names(M[[1]])
length(M[[1]]$log_fbar)
colnames(projfile)<-c("Draw","Replicate","Treatment",paste("F",seq(1,length(M[[1]]$log_fbar))),
                   "B35",paste("proj_ssb_",seq(2019,2025)))

ind<-grep('ssb',colnames(projfile))
adj_proj<-filter(projfile,Replicate==1)
all_ssb<-cbind(hist_ssb,adj_proj[,ind])
colnames(all_ssb)<-seq(1982,2025)

#==plot
.THEME    = theme_bw(base_size = 12, base_family = "") +
  theme(strip.text.x = element_text(margin= margin(1,0,1,0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(color="white",fill="white")) 
.COL =  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

ribbon_prep<-function(tmp,adj=1,Quantity_name="MMB",F_mort="NA")
{
adj<-adj
tmp<-apply(tmp,2,sort)
med_ch<-as.numeric(tmp[nrow(tmp)/2,])/adj
up_ch<-as.numeric(tmp[round(nrow(tmp)*0.025),])/adj
dn_ch<-as.numeric(tmp[round(nrow(tmp)*0.975),])/adj
years<-seq(from=min(colnames(tmp)),to=max(colnames(tmp)))
out<-data.frame(MMB=med_ch,upper=up_ch,lower=dn_ch,Year=years,
                Quantity=Quantity_name,F_mort=as.character(F_mort))
return(out)
}

adj_proj<-filter(projfile,Replicate==1 & Treatment==1)
all_ssb<-cbind(hist_ssb,adj_proj[,ind])
colnames(all_ssb)<-seq(1982,2025)
out<-ribbon_prep(tmp=all_ssb,F_mort=0)

adj_proj<-filter(projfile,Replicate==1 & Treatment==2)
all_ssb<-cbind(hist_ssb,adj_proj[,ind])
colnames(all_ssb)<-seq(1982,2025)
out2<-ribbon_prep(all_ssb,F_mort=1)

adj_proj<-filter(projfile,Replicate==1 & Treatment==3)
all_ssb<-cbind(hist_ssb,adj_proj[,ind])
colnames(all_ssb)<-seq(1982,2025)
out3<-ribbon_prep(all_ssb,F_mort=2)

in_dat<-as.data.frame(rbind(out,out2,out3))



both_proj<-ggplot() +
  geom_ribbon(data=in_dat,aes(x=Year,ymin = lower, ymax = upper, fill = F_mort),alpha=0.1) +
  geom_line(data=in_dat,aes(x=Year,y=MMB,col=F_mort)) +
  .THEME +
  theme(legend.position = c(.2,.8)) + 
  geom_hline(yintercept=M[[1]]$spr_bmsy,linetype=2) +
  expand_limits(y=0)
print(both_proj)



png("plots/tot_fig_RAM.png",height=5,width=4,res=600,units='in')
print(both_proj)
dev.off()

