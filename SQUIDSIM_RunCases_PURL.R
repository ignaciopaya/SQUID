# ------------------------------------------------------------------------
# Script for running SQUIDSIM 1.02
# ------------------------------------------------------------------------
#   Ignacio Payá, September 2024
# -----------------------------------------------------------------------

## purl to get R file
knitr::purl("SQUIDSIM_1.02.Rmd", "SQUIDSIM_1.02.R", documentation = 2)

pars=read.table("HSquid_Par_1.csv",sep=";",header=FALSE)  
ncases=3
cases=1:ncases
season.pars.cases=popu.pars.cases=ind.pars.cases=N1.cases=R.cases=SBT.cases=BT.cases=vector()
wm_m1.cases=wm_m2.cases=wm_m3.cases=CPUE1.cases=CPUE2.cases=CPUE3.cases=vector()
Bacus1.cases=Bacus2.cases=wm_m1_acus.cases=wm_m2_acus.cases=vector()

for (case in 1:ncases){
# rename input file
file.copy(paste("HSquid_Par_",case,".csv",sep=""),"HSquid_Par.csv",overwrite = T)
if (case>1) {pars=rbind(pars,read.table(paste("HSquid_Par_",case,".csv",sep=""),sep=";",header=FALSE))}  
# Run  
source("SQUIDSIM_1.02.R")

if(case==1){
  N1.cases=N[,1]
  R.cases=R
  SBT.cases=SBT
  BT.cases=BT
  S.cases=S
  S1.cases=S1
  S2.cases=S2
  S3.cases=S3
  S4.cases=S4
  S5.cases=S5
  wm_m1.cases=wm_m1
  wm_m2.cases=wm_m2
  wm_m3.cases=wm_m3 
  YT.cases=YT
  YT1.cases=YT1
  YT2.cases=YT2
  YT3.cases=YT3
  CPUE1.cases=CPUE1
  CPUE2.cases=CPUE2
  CPUE3.cases=CPUE3
  Bacus1.cases =Bacus1
  Bacus2.cases =Bacus2
  wm_m1_acus.cases=wm_m1_acus
  wm_m2_acus.cases=wm_m2_acus
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
  S4.cases=cbind(S4.cases,S4)
  S5.cases=cbind(S5.cases,S5)
  wm_m1.cases=cbind(wm_m1.cases,wm_m1)
  wm_m2.cases=cbind(wm_m2.cases,wm_m2)
  wm_m3.cases=cbind(wm_m3.cases,wm_m3)
  YT.cases=cbind(YT.cases,YT)
  YT1.cases=cbind(YT1.cases,YT1)
  YT2.cases=cbind(YT2.cases,YT2)
  YT3.cases=cbind(YT3.cases,YT3)
  CPUE1.cases=cbind(CPUE1.cases,CPUE1)
  CPUE2.cases=cbind(CPUE2.cases,CPUE2)
  CPUE3.cases=cbind(CPUE3.cases,CPUE3)
  Bacus1.cases =cbind(Bacus1.cases,Bacus1)
  Bacus2.cases =cbind(Bacus2.cases,Bacus2)
  wm_m1_acus.cases=cbind(wm_m1_acus.cases,wm_m1_acus)
  wm_m2_acus.cases=cbind(wm_m2_acus.cases,wm_m2_acus)
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
write.csv(wm_m1.cases,"wm_m1.cases.csv")
write.csv(wm_m2.cases,"wm_m2.cases.csv")
write.csv(wm_m3.cases,"wm_m3.cases.csv")
write.csv(CPUE1.cases,"CPUE1.cases.csv")
write.csv(CPUE2.cases,"CPUE2.cases.csv")
write.csv(CPUE3.cases,"CPUE3.cases.csv")
write.csv(YT.cases,"YT_Cases.csv")
write.csv(YT1.cases,"YT1_Cases.csv")
write.csv(YT2.cases,"YT2_Cases.csv")
write.csv(YT3.cases,"YT3_Cases.csv")
write.csv(Bacus1.cases,"Bacus1.cases.csv")
write.csv(Bacus2.cases,"Bacus2.cases.csv")
write.csv(wm_m1_acus.cases,"wm_m1_acus.cases.csv")
write.csv(wm_m2_acus.cases,"wm_m2_acus.cases.csv")
write.csv(ind.pars.cases,"ind.pars_Cases.csv")
write.csv(popu.pars.cases,"popu.pars_Cases.csv")
write.csv(season.pars.cases,"season.pars_Cases.csv")
write.csv(fish.pars.cases,"fish.pars_Cases.csv")
write.csv(pars,"Cases_Pars.csv")

#save figures
par(mar=c(4,4,1,1))
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

plotcases<-function(x,data,xLAB,yLAB){
  png(paste("Cases_",yLAB,".png"))
  matplot(x,data,t="l",xlab=xLAB,ylab=yLAB,
          ylim=c(min(min(data)),max(max(data))))
  text(x[20],0.9*max(max(data)),"Case 1",col=1)        
  text(x[20],0.8*max(max(data)),"Case 3",col=3)        
  text(x[20],0.85*max(max(data)),"Case 2",col=2)
  dev.off()
}

plotcases(tbins,CPUE1.cases,"Year_Month","CPUE 1")
plotcases(tbins,CPUE2.cases,"Year_Month","CPUE 2")
plotcases(tbins,CPUE3.cases,"Year_Month","CPUE 3")
plotcases(tbins,YT1.cases,"Year_Month","Fleet 1 Catch")
plotcases(tbins,YT2.cases,"Year_Month","Fleet 2 Catch ")
plotcases(tbins,YT3.cases,"Year_Month","Fleet 3 Catch ")
plotcases(tbins,YT.cases,"Year_Month","Whole Catch")
plotcases(tbins,SBT.cases,"Year_Month","Spawning Biomass (tons)")
plotcases(tbins,BT.cases,"Year_Month","Biomass (tons)")
plotcases(tbins,R.cases,"Year_Month","Recruits")
plotcases(tbins,Bacus1.cases,"Year_Month","Acosutic Biomass 1")
plotcases(tbins,Bacus2.cases,"Year_Month","Acosutic Biomass 2")
plotcases(tbins,wm_m1.cases,"Year_Month","Fleet 1 Mean weight (Kg)")
plotcases(tbins,wm_m2.cases,"Year_Month","Fleet 2 Mean weight (Kg)")
plotcases(tbins,wm_m3.cases,"Year_Month","Fleet 3 Mean weight (Kg)")
plotcases(tbins,wm_m1_acus.cases,"Year_Month","Acoustic 1 mean weight (Kg)")
plotcases(tbins,wm_m2_acus,"Year_Month","Acoustic 2 mean weight (Kg)")

plotcases(ags,S1.cases,"Age(year)","Fleet 1 Selectivity")
plotcases(ags,S2.cases,"Age(year)","Fleet 2 Selectivity")
plotcases(ags,S3.cases,"Age(year)","Fleet 3 Selectivity")
plotcases(ls,S4.cases,"Length (cm)","Acoustic 1 Selectivity")
plotcases(ls,S5.cases,"Length (cm)","Acoustic 2 Selectivity")
plotcases(ags,N1.cases,"Age(year)","N at first Month")




