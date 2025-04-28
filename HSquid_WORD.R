## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ----message=FALSE, warning=FALSE, paged.print=FALSE-------------------
# Read External Parametrer input file
input=read.table("HSquid_Par.csv",sep=";")
prs=t(as.vector(input$V3))
colnames(prs)<-input$V2
input.prs<-data.frame(prs)
attach(input.prs)

# Internal Parameters
bin=1/12
months=1:12
ls=seq(1:110)
#ags=seq(bin,1.5,bin)
ags=seq(bin,Longv,bin)
#yrs=seq(2000,2021)
yrs=seq(Fyear,Lyear)
#tbins=seq(2000,2020,bin)
tbins=seq(Fyear,Lyear,bin)
nls=length(ls)
nags=length(ags)
nyrs=length(yrs)
ntbins=length(tbins)

# Parameter for short print of simualtions 1=Yes; other long print
printbins=1:ntbins
shortprint=1
if (shortprint==1) printbins=c(1:12,61:72,121:132,229:240)

## Natural mortality
M_bin=M_Annual*bin
SdM=M_bin*M_cv
## Growth
w=l=vector()
for (i in 1:nags){
if(gm==1){
# large springArguelles et al 2001. Fisheries Research 54.    
#a_expo=309.11
#b_expo=0.0029
   l[i] = a_expo *exp(b_expo*ags[i]*365)/10
ind.pars<-rbind(a_expo,b_expo,a,b)   
}else{
 # Paya
 #loo=83.94;r=0.93;t0=0.0
 #a=0.000023067;b=3.077
  l[i]=loo*(1-exp(-r*(ags[i]-t0)))
ind.pars<-rbind(loo,r,t0,a,b)  
}
w[i]=a*l[i]^b
}


## ----------------------------------------------------------------------
par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(ags,l,t="l",ylim=c(0,max(l)),xlab="Age",ylab="ML (cm)")
plot(ags,w,t="l",ylim=c(0,max(w)),xlab="Age",ylab="weight (kg)")


## ----------------------------------------------------------------------
# Maturity at length
pm=ls/ls
ind.pars<-rbind(ind.pars,lm50,lmrange)
for (i in 1:nls){
pm[i]= 1 / (1 + exp(-log(19)*(ls[i]-lm50)/lmrange) )
}
# Maturity at age
pm_age=vector()
for (i in 1:nags){
pm_age[i] = 1 / (1 + exp(-log(19)*(l[i]-lm50)/lmrange) )}

par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(ls,pm,t="l", ylim=c(0,1),xlab="ML (cm)",ylab="Maturity proportion")
plot(ags,pm_age ,t="l", ylim=c(0,1),xlab="Age",ylab="Maturity proportion")


## ----------------------------------------------------------------------
library(knitr)
if(gm==1){growth.pars<-data.frame(a_expo,b_expo,a,b)
} else {growth.pars<-data.frame(loo,r,t0,a,b)}
mat.pars<-data.frame(lm50,lmrange)
knitr::kable(growth.pars, caption= 'Growth Parameters') 


## ----------------------------------------------------------------------
knitr::kable(mat.pars, caption= 'Maturity Parameters') 
#knitr::kable(t(ind.pars)) 


## ----------------------------------------------------------------------
SB=B=N=M=matrix(NA,nags,ntbins)
for (i in 1:nags) M[i,]=M_bin+rnorm(ntbins)*SdM
#  Reclutas
# In-bin recruitment pattern
inbinR=vector("numeric",ntbins)
inbinR[1]=1

# select type of recruitment pattern
# 1 =  Constant
# 2 = Exponential decreasing 
# >2 = Sinuosidal 
# tr=3
if (RSeason==1) {
  R_Seasonal="Constant"
  inbinR=rep(1,ntbins)
} else if (RSeason==2){
  R_Seasonal="Exponential Decreasing"
  for (i in 2:ntbins){
  inbinR[i]=inbinR[i-1]*exp(-0.20)
  for( j in 1:nyrs){
  if(tbins[i-1]==yrs[j]) inbinR[i]=1
  }
  }
} else {
  R_Seasonal="Sinusoidal"
  patron=sin(4:15)
  patron[which(patron<0)]=0
  patron0=patron
  for(j in 1:nyrs-1){
  patron0=c(patron0,patron)
  }
  inbinR=patron0[1:ntbins]
}

# add error to inbinR
# Recruitment
#R_sd=0.00000
#inbinR=inbinR * rnorm(length(inbinR), 1, SdSeason)
#plot(inbinR,typ="p")
# recruitment correlation
#corre=1
#cv=0.3
#AR=vector(ntbins/12)
#AR[1,i]=N[1,i-1]*(corre + rnorm(1)*CVSeaon*CorrSeason)


## ----message=FALSE-----------------------------------------------------
image(tbins,ags,t(M),xlab="Year_Month",ylab="Ages (year)")


## ----------------------------------------------------------------------
# R0
Neq=rep(1,nags)
for (i in 2:nags){
Neq[i] = Neq[i-1]*exp(-M[i-1,1])}
SBPR=sum(w*pm_age*Neq)/1000  # tons
R0=SB0/SBPR
# Stock-Recruitment Model
if(SRModel==1){
# Ricker
alpha=1.25*log(5*h)-log(SBPR)
beta=1.25*log(5*h)/SB0
} else {  
# B-H Model
alpha=((1-h)/(4*h))*SBPR
beta=(5*h-1)/(4*h*SB0)*SBPR
}

# Initial N (at the first year)
SB=N=matrix(1:nags*ntbins,nags,ntbins)
# Previuos recruitment
preR=rep(R0,nags)*exp(rnorm(nags,0,prev.rsigma)-prev.rsigma^2/2)*inbinR[nags:1]
plot(ags,preR,t="l",xlab="Age (Years) ",ylab="Previous  Recrutiment")


## ----------------------------------------------------------------------
N[1,1]=R0
for (i in 2:nags)
  { N[i,1] =preR[nags-i+1]*exp(-M[i-1,1]*(i-1))}
plot(N[,1],xlab=" Age (Months)",ylab=" N",ylim=c(0,max(N[,1])),t="l")


## ----------------------------------------------------------------------
plot(1:12,inbinR[1:12],type="l",xlab="Month",ylab="Recruitment pattern")


## ----------------------------------------------------------------------
S=S1=S2=rep(0,nags)
for (i in 1:nags){
if( ags[i]<bs1){
S1[i]= exp(-0.5*((ags[i]-bs1)^2)/as1^2)
} else {
S1[i]= exp(-0.5*((ags[i]-bs1)^2)/cs1^2)  
}
}
for (i in 1:nags){
if( ags[i]<bs2){
S2[i]= exp(-0.5*((ags[i]-bs2)^2)/as2^2)
} else {
S2[i]= exp(-0.5*((ags[i]-bs2)^2)/cs2^2)  
}
}
# Fishing Mortality
C=C1=C2=F=F1=F2=matrix(0,nags,ntbins)
ones=rep(1,ntbins)
F1=S1%*%t(ones)*Fref1*bin
F2=S2%*%t(ones)*Fref2*bin
F=F1+F2
S=(F1[,1]+F2[,1])/max((F1[,1]+F2[,1]))
plot(ags,S1,t="l",col=2,ylab="Selectivity",xlab="Age (year)")
lines(ags,S2,t="l",col=3,lty=2,lwd=2)
lines(ags,S,t="l",lty=1,lwd=2,col=1)
text(0.2,.9,"___ Fleet 1",col=2)
text(0.2,.8,"- - - Fleet 2",col=3)
text(0.2,.7,"- - - Total",col=1)


## ----------------------------------------------------------------------
R=SBT=rep(1,ntbins)
N[1,1]=R[1]=R0
SBT[1]=SB0
#rsigma=0.0
for (j in 1:ntbins){
    if(j>1){
      if(SRModel==1){
      # Ricker
      N[1,j]=R[j] = SBT[j-1]*exp(alpha-beta*SBT[j-1])    
      }else{   
      # Beverton&Holt
      N[1,j]=R[j] = SBT[j-1]/(alpha+beta*SBT[j-1])
      }
   for (i in 2:nags) N[i,j]=N[i-1,j-1]*exp(-M[i-1,j-1]--F[i-1,j-1])
   }
   R[j] =N[1,j]=N[1,j]*exp(rnorm(1,0,rsigma)-rsigma^2/2)*inbinR[j]
   SB[,j]=N[,j]*w/1000*pm_age
   SBT[j]=sum(SB[,j])
   C[,j]=F[,j]/(F[,j]+M[,j])*N[,j]*(1-exp(-M[,j]-F[,j]))
   C1[,j]=F1[,j]/(F[,j]+M[,j])*N[,j]*(1-exp(-M[,j]-F[,j]))
   C2[,j]=F2[,j]/(F[,j]+M[,j])*N[,j]*(1-exp(-M[,j]-F[,j]))
     } 
R_NA=R
R_NA[which(R_NA==0)]=NA
plot(SBT[1:(ntbins-1)],R_NA[2:ntbins],xlab="Spawning Biomass (t-1)",ylab="Recruitment (age1)", ylim=c(0,max(N[1,2:ntbins])),xlim=c(0,max(SBT[1:(ntbins-1)]*1.1)))


## ----------------------------------------------------------------------
par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(tbins-10,inbinR,type="l",xlab="Year_Month",ylab="Seasonal pattern",ylim=c(0,max(inbinR) ))
plot(ags,N[,1],typ="l",xlab="Age",ylab="N at first month")


## ----------------------------------------------------------------------
par(mfrow=c(6,1),mar=c(1,2,1,1))
for(j in printbins) {  
barplot(N[,j],names.arg=as.character(1:nags), xlab="",axisnames =TRUE,space=NULL, ylab=NULL)
text(30,0.7*max(N[,j]), paste(as.character(floor(tbins[j])),as.character(round((tbins[j]-floor(tbins[j]))*12)+1))) 
#}
}


## ----------------------------------------------------------------------
par(mfrow=c(6,1),mar=c(1,2,1,1))
for(j in printbins) {  
barplot(C[,j],names.arg=as.character(1:nags), xlab="",axisnames =TRUE,space=NULL, ylab=NULL)
text(30,0.7*max(C[,j]), paste(as.character(floor(tbins[j])),as.character(round((tbins[j]-floor(tbins[j]))*12)+1))) 
#}
}


## ----------------------------------------------------------------------
par(mfrow=c(6,1),mar=c(1,2,1,1))
for(j in printbins) {  
barplot(C1[,j],names.arg=as.character(1:nags), xlab="",axisnames =TRUE,space=NULL, ylab=NULL)
text(30,0.7*max(C1[,j]), paste(as.character(floor(tbins[j])),as.character(round((tbins[j]-floor(tbins[j]))*12)+1))) 
#}
}


## ----------------------------------------------------------------------
par(mfrow=c(6,1),mar=c(1,2,1,1))
for(j in printbins) {  
barplot(C2[,j],names.arg=as.character(1:nags), xlab="",axisnames =TRUE,space=NULL, ylab=NULL)
text(30,0.7*max(C2[,j]), paste(as.character(floor(tbins[j])),as.character(round((tbins[j]-floor(tbins[j]))*12)+1))) 
#}
}


## ----------------------------------------------------------------------
alkey<-matrix(0,nags,nls)
for(i in 1:nags){
alkey[i,]<-dnorm(ls, mean = l[i], sd = l[i]*CVGrowth)
alkey[i,]/sum(alkey[i,])
}
matplot(t(alkey),t="l",xlab="ML (cm)",ylab="Probability")


## ----------------------------------------------------------------------
par(mfrow=c(6,1),mar=c(1,1,1,1))
#plot(colSums(N[,20]%*%alkey))
Nl=matrix(0,nls,ntbins)
for(j in seq(1,ntbins)) Nl[,j]<-colSums(N[,j]%*%alkey)
for(j in printbins) {  
barplot(Nl[,j],names.arg=as.character(ls), xlab="",axisnames =TRUE,space=NULL, ylab=NULL)
text(90,0.7*max(Nl[,j]), paste(as.character(floor(tbins[j])),as.character(round((tbins[j]-floor(tbins[j]))*12)+1))) }


## ----------------------------------------------------------------------
par(mfrow=c(6,1),mar=c(1,1,1,1))
#plot(colSums(N[,20]%*%alkey))
Cl=Cl1=Cl2=matrix(0,nls,ntbins)
for(j in seq(1,ntbins)) {  
Cl[,j]<-colSums(C[,j]%*%alkey)
Cl1[,j]<-colSums(C1[,j]%*%alkey)
Cl2[,j]<-colSums(C2[,j]%*%alkey)}
for(j in printbins) {  
barplot(Cl[,j],names.arg=as.character(ls), xlab="",axisnames =TRUE,space=NULL, ylab=NULL)
text(20,0.7*max(Cl[,j]), paste(as.character(floor(tbins[j])),as.character(round((tbins[j]-floor(tbins[j]))*12)+1))) }


## ----------------------------------------------------------------------
par(mfrow=c(6,1),mar=c(1,1,1,1))
for(j in printbins) { 
barplot(Cl1[,j],names.arg=as.character(ls), xlab="",axisnames =TRUE,space=NULL, ylab=NULL)
text(20,0.7*max(Cl1[,j]), paste(as.character(floor(tbins[j])),as.character(round((tbins[j]-floor(tbins[j]))*12)+1)))
}


## ----------------------------------------------------------------------
par(mfrow=c(6,1),mar=c(1,1,1,1))
for(j in printbins) { 
barplot(Cl2[,j],names.arg=as.character(ls), xlab="",axisnames =TRUE,space=NULL, ylab=NULL)
text(20,0.7*max(Cl2[,j]), paste(as.character(floor(tbins[j])),as.character(round((tbins[j]-floor(tbins[j]))*12)+1)))
}


## ----------------------------------------------------------------------
par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(ags,N[,1],ylim=c(0,max(N[,1], na.rm = TRUE)),xlab="Age",ylab="N at the first Month",t="l")
plot(tbins,N[1,],ylim=c(0,max(N[1,], na.rm = TRUE)),xlab="Year_Month",ylab="R",t="l")


## ----------------------------------------------------------------------
ones=rep(1,ntbins)
w.m=w%*%t(ones)
#mad.m=pm_age%*%t(ones)
B=N*w.m/1000
Y=C*w.m/1000
Y1=C1*w.m/1000
Y2=C2*w.m/1000
#BD=N*w.m*mad.m
NT=colSums(N)
BT=colSums(B)
YT=colSums(Y)
YT1=colSums(Y1)
YT2=colSums(Y2)
par(mar=c(4,4,1,1))
plot(tbins,NT,typ="l",ylim=c(0,max(NT)),xlab="Year_Month",ylab="Whole Number")


## ----------------------------------------------------------------------
par(mar=c(4,4,1,1))
plot(tbins,YT,typ="l",ylim=c(0,max(YT)),xlab="Year_Month",ylab="Catch (tons)",col=1)
lines(tbins,YT1,typ="l",lty=2,col=2)
lines(tbins,YT2,typ="l",lty=3,col=3)
text(2000,10,"Whole",col=1)
text(2005,10,"Fleet 1",col=2)
text(2010,10,"Fleet 2",col=3)


## ----------------------------------------------------------------------
#image(ags,tbins,N)
image(tbins,ags,t(N),xlab="Year_Month",ylab="Age (Months)")


## ----message=FALSE-----------------------------------------------------
require(lattice)
wireframe(N,xlab = "Age", ylab = "Year_Month",
#          main = "Number",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60))


## ----------------------------------------------------------------------
plot(tbins,BT,typ="l",ylim=c(0,max(BT,na.rm=TRUE)),xlab="Year_Month",ylab="Biomass (t)")


## ----------------------------------------------------------------------
plot(tbins,SBT,typ="l",ylim=c(0,max(SBT)),xlab="Year_Month",ylab="Spawning Biomass")


## ----------------------------------------------------------------------
popu.prs<-data.frame(M_Annual,M_bin,M_cv,SB0,SRModel,h,prev.rsigma,rsigma)
season.prs<-data.frame(RSeason,SdSeason)
#popu.pars
knitr::kable(season.prs, caption = 'Recruitment Seasonal Parameters') 


## ----------------------------------------------------------------------

knitr::kable(popu.prs,caption = 'M and Stock Recruitment Parameters') 



## ----------------------------------------------------------------------
fish.prs<-data.frame(Fref1,as1,bs1,cs1,Fref2,as2,bs2,cs2)
#popu.pars
knitr::kable(fish.prs, caption = 'Fishery Parameters') 


