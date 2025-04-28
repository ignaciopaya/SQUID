# read data in ascci format
#fldaysex.txt
fldat <-read.table("fldaysex.txt", header = TRUE)
attach(fldat)
require(lattice)

fldat$Sex[fldat$Sex=="1"] <- "Male"
fldat$Sex[fldat$Sex=="2"] <- "Female"

#fldat$Prop[fldat$Prop=="0"] <- as.character(NA)

trellis.par.set(col.whitebg())  
#postscript("MLFreqDay.eps",horizontal=TRUE,onefile=FALSE)

levelplot(-Prop ~ Day * ML | Sex, 
          	data=fldat,panel = "panel.levelplot",
		cuts=10,
		labels= as.character(seq(0, 0.25, length.out=10 )),
		#col.regions = trellis.par.get("regions")$col,
		colorkey = FALSE,	
          	xlab="Day of the Year",ylab="Dorsal mantle length (cm)",layout=c(1,2))

win.metafile('MLFreqDay.emf')
levelplot(-Prop ~ Day * ML | Sex, 
          	data=fldat,panel = "panel.levelplot",
		cuts=10,
		labels= as.character(seq(0, 0.25, length.out=10 )),
		#col.regions = trellis.par.get("regions")$col,
		colorkey = FALSE,	
          	xlab="Day of the Year",ylab="Dorsal mantle length (cm)",layout=c(1,2))
dev.off()
detach(fldat)