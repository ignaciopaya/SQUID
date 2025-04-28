# ------------------------------------------------------------------------
# Script for running HSquid.Rmd
# ------------------------------------------------------------------------
#   Ignacio Payá, September 2024
# -----------------------------------------------------------------------

## purl to get R file
knitr::purl("SC12 WP HSquid_HTML.Rmd", "SC12 WP HSquid_HTML.R", documentation = 2)

pars=read.table("HSquid_Par_1.csv",sep=";",header=FALSE)  
ncases=3
cases=1:ncases
season.pars.cases=popu.pars.cases=ind.pars.cases=N1.cases=R.cases=SBT.cases=BT.cases=vector()

for (case in 1:ncases){
# rename input file
file.copy(paste("HSquid_Par_",case,".csv",sep=""),"HSquid_Par.csv",overwrite = T)
if (case>1) {pars=rbind(pars,read.table(paste("HSquid_Par_",case,".csv",sep=""),sep=";",header=FALSE))}  
# Run  
source("SC12 WP HSquid_HTML.R")

if(case==1){
  N1.cases=N[,1]
  R.cases=R
  SBT.cases=SBT
  BT.cases=BT
  S.cases=S
  S1.cases=S1
  S2.cases=S2
  S3.cases=S3
  YT.cases=YT
  YT1.cases=YT1
  YT2.cases=YT2
  YT3.cases=YT3
  ind.pars.cases=t(ind.pars)
  popu.pars.cases=popu.prs
  season.pars.cases=season.prs
  fish.pars.cases=fish.prs
  }
else {
  N1.cases=cbind(N1.cases,N[,1])  
  R.cases=cbind(R.cases,R)  
  SBT.cases=cbind(SBT.cases,SBT)  
  BT.cases=cbind(BT.cases,BT)
  S.cases=cbind(S.cases,S)
  S1.cases=cbind(S1.cases,S1)
  S2.cases=cbind(S2.cases,S2)
  S3.cases=cbind(S3.cases,S3)  
  YT.cases=cbind(YT.cases,YT)
  YT1.cases=cbind(YT1.cases,YT1)
  YT2.cases=cbind(YT2.cases,YT2)
  YT3.cases=cbind(YT3.cases,YT3)  
  ind.pars.cases=rbind(ind.pars.cases,t(ind.pars))
  popu.pars.cases=rbind(popu.pars.cases,popu.prs)
  season.pars.cases=rbind(season.pars.cases,season.prs)
  fish.pars.cases=rbind(fish.pars.cases,fish.prs)
  }
}  ## cases end

# write tables
write.csv(N1.cases,"N1_Cases.csv")
write.csv(R.cases,"R_Cases.csv")
write.csv(SBT.cases,"SBT_Cases.csv")
write.csv(BT.cases,"BT_Cases.csv")
write.csv(S.cases,"S_Cases.csv")
write.csv(S1.cases,"S1_Cases.csv")
write.csv(S2.cases,"S2_Cases.csv")
write.csv(S3.cases,"S3_Cases.csv")
write.csv(YT.cases,"YT_Cases.csv")
write.csv(YT1.cases,"YT1_Cases.csv")
write.csv(YT2.cases,"YT2_Cases.csv")
write.csv(YT3.cases,"YT3_Cases.csv")
write.csv(ind.pars.cases,"ind.pars_Cases.csv")
write.csv(popu.pars.cases,"popu.pars_Cases.csv")
write.csv(season.pars.cases,"season.pars_Cases.csv")
write.csv(fish.pars.cases,"fish.pars_Cases.csv")
write.csv(pars,"Cases_Pars.csv")

#save figures
par(mar=c(4,4,1,1))
png('CasesFig_N1.png')
matplot(ags,N1.cases,t="l",xlab="Age (Year)",ylab="N at first Month")
text(ags[10],max(max(N1.cases)),"Case 1",col=1)        
text(ags[10],0.9*max(max(N1.cases)),"Case 2",col=2)        
text(ags[10],0.8*max(max(N1.cases)),"Case 3",col=3)        
dev.off()

png('CasesFig_R.png')
R.cases[which(R.cases==0)]=NaN
matplot(tbins,R.cases,t="l",xlab="Year_Month",ylab="Recruits",
        ylim=c(0,max(max(R.cases,na.rm=TRUE))))
text(tbins[40],max(max(R.cases,na.rm=TRUE)),"Case 1",col=1)        
text(tbins[40],0.90*max(max(R.cases,na.rm=TRUE)),"Case 2",col=2)        
text(tbins[40],0.80*max(max(R.cases,na.rm=TRUE)),"Case 3",col=3)        
dev.off()


png('CasesFig_BT.png')
matplot(tbins,BT.cases,t="l",xlab="Year_Month",ylab="BT (tons)",
        ylim=c(0,max(max(BT.cases))))
text(tbins[10],0.9*max(max(BT.cases)),"Case 1",col=1)        
text(tbins[10],0.85*max(max(BT.cases)),"Case 2",col=2)        
text(tbins[10],0.80*max(max(BT.cases)),"Case 3",col=3)        
dev.off()

png('CasesFig_SBT.png')
matplot(tbins,SBT.cases,t="l",xlab="Year_Month",ylab="SBT (tons)",
        ylim=c(0,max(max(SBT.cases))))
text(tbins[10],0.9*max(max(SBT.cases)),"Case 1",col=1)        
text(tbins[10],0.85*max(max(SBT.cases)),"Case 2",col=2)        
text(tbins[10],0.8*max(max(SBT.cases)),"Case 3",col=3)        
dev.off()

png('CasesFig_SB_R.png')
par(mar=c(4,4,1,1))
plot(SBT.cases[1:(ntbins-1),1],R.cases[2:ntbins,1],
ylim=c(0,max(max(R.cases,na.rm=T))), xlim=c(0,max(max(SBT.cases))),xlab="SBT",ylab="Recrutis")
for (i in 1:cases){
x=SBT.cases[1:(ntbins-1),i]  
y=R.cases[2:ntbins,i]
y[which(y==0)]=NaN
x[which(y==0)]=NaN
points(x,y,col=i)
}
text(0.5*x[10],max(max(R.cases,na.rm=T)),"Case 1",col=1)        
text(0.5*x[10],0.90*max(max(R.cases,na.rm=T)),"Case 2",col=2)        
text(0.5*x[10],0.80*max(max(R.cases,na.rm=T)),"Case 3",col=3) 
dev.off()

png('CasesFig_S1.png')
matplot(ags,S1.cases,t="l",xlab="Age(year)",ylab="Fleet 1 Selectivity",
        ylim=c(0,max(max(S1.cases))))
text(ags[10],0.9*max(max(S1.cases)),"Case 1",col=1)        
text(ags[10],0.8*max(max(S1.cases)),"Case 2",col=2)        
text(ags[10],0.7*max(max(S1.cases)),"Case 3",col=3)        
dev.off()

png('CasesFig_S2.png')
matplot(ags,S2.cases,t="l",xlab="Age(year)",ylab="Fleet 2 Selectivity",
        ylim=c(0,max(max(S2.cases))))
text(ags[10],0.9*max(max(S2.cases)),"Case 1",col=1)        
text(ags[10],0.8*max(max(S2.cases)),"Case 2",col=2)        
text(ags[10],0.7*max(max(S2.cases)),"Case 3",col=3)        
dev.off()

png('CasesFig_S3.png')
matplot(ags,S3.cases,t="l",xlab="Age(year)",ylab="Fleet 3 Selectivity",
        ylim=c(0,max(max(S3.cases))))
text(ags[10],0.9*max(max(S3.cases)),"Case 1",col=1)        
text(ags[10],0.8*max(max(S3.cases)),"Case 2",col=2)        
text(ags[10],0.7*max(max(S3.cases)),"Case 3",col=3)        
dev.off()

png('CasesFig_YT.png')
matplot(tbins,YT.cases,t="l",xlab="Year_Month",ylab="Catch (tons)",
        ylim=c(0,max(max(YT.cases))))
text(tbins[20],0.9*max(max(YT.cases)),"Case 1",col=1)        
text(tbins[20],0.85*max(max(YT.cases)),"Case 2",col=2)        
text(tbins[20],0.8*max(max(YT.cases)),"Case 3",col=3)        
dev.off()

png('CasesFig_YT1.png')
matplot(tbins,YT1.cases,t="l",xlab="Year_Month",ylab="Fleet 1 Catch (tons)",
        ylim=c(0,max(max(YT1.cases))))
text(tbins[20],0.9*max(max(YT1.cases)),"Case 1",col=1)        
text(tbins[20],0.85*max(max(YT1.cases)),"Case 2",col=2)        
text(tbins[20],0.8*max(max(YT1.cases)),"Case 3",col=3)        
dev.off()

png('CasesFig_YT2.png')
matplot(tbins,YT2.cases,t="l",xlab="Year_Month",ylab="Fleet 2 Catch (tons)",
        ylim=c(0,max(max(YT2.cases))))
text(tbins[20],0.9*max(max(YT2.cases)),"Case 1",col=1)        
text(tbins[20],0.85*max(max(YT2.cases)),"Case 2",col=2)        
text(tbins[20],0.8*max(max(YT2.cases)),"Case 3",col=3)        
dev.off()

png('CasesFig_YT3.png')
matplot(tbins,YT3.cases,t="l",xlab="Year_Month",ylab="Fleet 3 Catch (tons)",
        ylim=c(0,max(max(YT3.cases))))
text(tbins[20],0.9*max(max(YT3.cases)),"Case 1",col=1)        
text(tbins[20],0.85*max(max(YT3.cases)),"Case 2",col=2)        
text(tbins[20],0.8*max(max(YT3.cases)),"Case 3",col=3)        
dev.off()

