#Use LINTUL2 to compare potential and water limited production for a set of years
#Download map with weather data and .r listings and copy all intro one directory.
#set working directory

source('lINTUL2_code.r')	
source('lINTUL2_input.r')	
source('plot.poly.reg.r')	#used for plotting x vs y including a simple regression line
require('deSolve')    #used for solving ODEs

#---------------------------------Run control---------------------------------------#
#                                                                                   #
#-----------------------------------------------------------------------------------#
wdirectory <- paste0("M://weather/")
country   <- "NLD"
station   <- "1"
STTIME  <- 30         # d     :     start time of simulation
FINTIM  <- 300       # d     :     finish time 
DELT    <- 1         # d     :     time step 
FYR <- 1961          # first year to simulate
LYR <- 2017          # last year to simulate
##=====================================================================================##

VAR_POT=NULL
VAR_WLIM=NULL

psand<-LINTUL2_parameters(irri=FALSE,soiltype="sand")
pclay<-LINTUL2_parameters(irri=FALSE,soiltype="clay")
psand$ROOTDM <- 0.6
pclay$ROOTDM <- 0.6

pdf(file=paste0("Results/LINTUL Potential vs water limited for ",FYR, "-", LYR, ".pdf",sep = "", collapse = NULL))

for(year in FYR:LYR){
  print(year)
  yr=toString(year)

  #Load weather data
  wdata <- get_weather(directory=wdirectory ,country=country ,station=station,year=substr(yr,2,4))
  #LINTUL with irrigation: POTENTIAL PRODUCTION
  state_pot <- ode(LINTUL2_iniSTATES(), 
                   seq(STTIME, FINTIM, by = DELT), 
                   LINTUL2, 
                   LINTUL2_parameters(irri=TRUE,soiltype="sand"),
                   WDATA = wdata,
                   method = "euler")
  #LINTUL without irrigation: water limited
  state_wlim_sand <- ode(LINTUL2_iniSTATES(), 
                    seq(STTIME, FINTIM, by = DELT), 
                    LINTUL2, 
                    psand,
                    WDATA = wdata,
                    method = "euler")
  #LINTUL without irrigation: water limited
  state_wlim_clay <- ode(LINTUL2_iniSTATES(), 
                    seq(STTIME, FINTIM, by = DELT), 
                    LINTUL2, 
                    pclay,
                    WDATA = wdata,
                    method = "euler")
  #plot selected variables
  plot.new()
  par(mfrow=c(3,1),mar=c(1,4,3,1))
  plot(state_pot[, "time"], state_pot[,"LAI"],
       xlab="Day number",ylab="LAI",xlim=c(60,220),ylim=c(0,8),
       type="l",col="black",lty=1)
  lines(state_wlim_sand[, "time"], state_wlim_sand[,"LAI"],
        col="red",lty=2)
  lines(state_wlim_clay[, "time"], state_wlim_clay[,"LAI"],
        col="blue",lty=2)

  par(mar=c(2,4,2,1))
  plot(state_pot[, "time"], 0.01*rowSums(subset(state_pot,select=c("WRT","WST","WLV","WSO"))),
       xlab="Day number",ylab="Biomass, tDM/ha",xlim=c(60,220),ylim=c(0,25),
       type="l",col="black",lty=1)
  lines(state_wlim_sand[, "time"], 0.01*rowSums(subset(state_wlim_sand,select=c("WRT","WST","WLV","WSO"))),
        col="red",lty=2)
  lines(state_wlim_clay[, "time"], 0.01*rowSums(subset(state_wlim_clay,select=c("WRT","WST","WLV","WSO"))),
        col="blue",lty=2)
  
  par(mar=c(4,4,1,1))
  plot(state_pot[, "time"], 0.01*state_pot[,"WSO"],
       xlab="Day number",ylab="Grain yield, tDM/ha",xlim=c(60,220),ylim=c(0,12),
       type="l",col="black",lty=1)
  lines(state_wlim_sand[, "time"], 0.01*state_wlim_sand[,"WSO"],
        col="red",lty=2)
  lines(state_wlim_clay[, "time"], 0.01*state_wlim_clay[,"WSO"],
        col="blue",lty=2)
  legend("topleft",legend=c(paste0(yr,"Irrigated"),paste0(yr,"WL sand"),paste0(yr,"WL clay")),col=c("black","red","blue"),lty=c(1,2,3),bty="n")
  
  plot(state_pot,state_wlim_sand,state_wlim_clay,select=c("WRT","WST","WLV","WSO"))
  legend("topleft",legend=c(paste0(yr,":irrigated"),paste0(yr,":WL-sand"),paste0(yr,":WL-clay")),col=c("black","red","green"),lty=c(1,2,3),bty="n")

  
  #get all states, but alos rates and some auxillary variables
  var_wlim_sand <- get_results(state_wlim_sand, year, wdata, STTIME,FINTIM)
  write.csv(var_wlim_sand,file=paste0("Results/Run details for LINTUL2 without irrigation for ",yr, ".csv",sep = "", collapse = NULL))
  var_pot <- get_results(state_pot, year, wdata, STTIME,FINTIM)
  write.csv(var_pot,file=paste0("Results/Run details for LINTUL2 with irrigation for ",yr, ".csv",sep = "", collapse = NULL))
  #add these to the data frame with multi-year values
  VAR_WLIM <-rbind(VAR_WLIM, var_wlim_sand)
  VAR_POT <-rbind(VAR_POT, var_pot)
}
dev.off()
#

#Plot all years in one plot
plot_years(data=VAR_WLIM, FYR=FYR, LYR=LYR, VAR="WSO", YTITLE="Grain yield, g dry matter m-2")
plot_years(data=VAR_WLIM, FYR=FYR, LYR=LYR, VAR="LAI", YTITLE="LAI, m2 leaf m-2 soil")
plot_years(data=VAR_WLIM, FYR=FYR, LYR=LYR, VAR="TRAN", YTITLE="Cum. transpiration, mm")
#dev.off() 

#Compare years
fdVAR_POT <-subset(VAR_POT,time==300,select = c("year","WSOTHA","HI","WSO","WLV","WLVG","WLVD","WRT","WST","TRAN","EVAP","TRAIN","PAR"),rownames=FALSE)
#print(fdVAR_POT)
fdVAR_WLIM <-subset(VAR_WLIM,time==300,select = c("year","WSOTHA","HI","WSO","WLV","WLVG","WLVD","WRT","WST","TRAN","EVAP","TRAIN","PAR"))
#print(fdVAR_WLIM)



#determine means
StateMeans <- rbind(round(colMeans(fdVAR_POT),2),round(colMeans(fdVAR_WLIM),2))
colnames(StateMeans)[1]<- "scenario"
StateMeans[1,1] <- "Total irrigated" 
StateMeans[2,1] <- "Total iRainfed  "   
print(StateMeans)

#Save into excel file
write.csv(fdVAR_POT,file=paste0("Results/Annual totals for LINTUL2_with irrigation",toString(FYR),":",toString(LYR),".csv",sep = "", collapse = NULL))
write.csv(fdVAR_WLIM,file=paste0("Results/Annual totals for LINTUL2_",toString(FYR),":",toString(LYR),".csv",sep = "", collapse = NULL))

#Look at trends
pdf(file=paste0("Results/Water limited and potential yield trends.pdf",sep = "", collapse = NULL))
  plot.poly.reg(x=fdVAR_POT[["year"]],y=fdVAR_POT[["WSO"]]*10,add.1_1.line=FALSE,order="linear",
              xlim=c(FYR,LYR)  ,ylim=c(0,10000),pch=16,pos="topleft",col="black",xlab="",ylab="")
  par(new=TRUE)
  plot.poly.reg(fdVAR_WLIM[["year"]],fdVAR_WLIM[["WSO"]]*10,add.1_1.line=FALSE,order="linear",
                xlim=c(FYR,LYR)  ,ylim=c(0,10000),pch=16,pos="bottomleft",
                xlab="Year",ylab="Grain yield, kg dry matter ha-1",col="grey",
                main = "Spring wheat in the Netherlands 1961-2011")
  legend("bottomright", pch = 21, lty = 0,col=c("black","grey"),pt.bg=c("black","grey"),legend=c("Irrigated","Rainfed"))
dev.off() 

#in season rainfall and evapotranspiration, and PAR
pdf(file=paste0("Results/Evapotranspiration trends.pdf",sep = "", collapse = NULL))
  plot.poly.reg(fdVAR_WLIM[["year"]], fdVAR_WLIM[["EVAP"]], dd.1_1.line=FALSE,order="linear",
                xlim=c(FYR,LYR)  ,ylim=c(0,400),pch=16,pos="topleft",col="black",xlab="",ylab="")  
  par(new=TRUE)
  plot.poly.reg(fdVAR_WLIM[["year"]],fdVAR_WLIM[["TRAN"]],add.1_1.line=FALSE,order="linear",
                xlim=c(FYR,LYR)  ,ylim=c(0,400),pch=16,pos="bottomleft",
                xlab="Year",ylab="Evapo- and transpiration, mm",col="grey",
                main = "Spring wheat in the Netherlands 1961-2011")
  legend("bottomright", pch = 21, lty = 0,
         col=c("black","grey",pt.bg=c("black","grey")),
         legend=c("Evap. rainfed","Trans. rainfed"))

#plot change in season average PAR   
  plot.poly.reg(fdVAR_WLIM[["year"]],fdVAR_WLIM[["PAR"]]/(FINTIM-STTIME),add.1_1.line=FALSE,order="linear",
                xlim=c(FYR,LYR)  ,pch=16,pos="bottomleft",
                xlab="Year",ylab="PAR, MJ m-2 d-1",col="black",
                main = "NLD1, the Netherlands 1961-2011")

dev.off()
