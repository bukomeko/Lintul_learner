source(here::here('scripts', 'lINTUL2_input.r'))	
source(here::here('scripts','lINTUL2_code.r'))	
require('deSolve')    #used for solving ODEs

#---------------------------------Run control---------------------------------------#
#                                                                                   #
#-----------------------------------------------------------------------------------#
#GENERAL SETTINGS
wdirectory <- here::here("Weather")
country   <- "NLD"
station   <- "1"
SOILTYPE="sand" #only sand or clay can be selected at this moment
STTIME  <- 30        # d     :     start time of simulation
FINTIM  <- 300       # d     :     finish time 
DELT    <- 1         # d     :     time step 
YR      <- 2011      # y     :     year to simulate
#-----------------------------------------------------------------------------------#

yr      <- toString(YR)
#Load weather data for selected year
wdata <- get_weather(directory=wdirectory, country=country, station=station,year=substr(yr,2,4))
#solve differential equations with EULER
#LINTUL with irrigation: POTENTRIAL PRODUCTION
state_pot <- ode(LINTUL2_iniSTATES(), 
                 seq(STTIME, FINTIM, by = DELT), 
                 LINTUL2, 
                 LINTUL2_parameters(irri=TRUE,soiltype=SOILTYPE),
                 WDATA = wdata,
                 method = "euler")

#LINTUL without irrigation: water limited
state_wlim <- ode(LINTUL2_iniSTATES(), 
                 seq(STTIME, FINTIM, by = DELT), 
                 LINTUL2, 
                 LINTUL2_parameters(irri=FALSE,soiltype=SOILTYPE),
                 WDATA = wdata,
                 method = "euler")

#plot state variables
pdf(here::here("Results","Plots of selected variable.pdf"))
    par(mfrow=c(2,2),mar=c(2,4,1,1))
  plot(state_pot,state_wlim,select=c("WRT","WST","WLV","WSO"),ylab ="gDM m-2",xlim=c(30,300))
  legend("topleft",legend=c("Irrigated","Rainfed"),col=c("black","red"),lty=c(1,2),bty="n")

  par(mfrow=c(2,2),mar=c(2,4,1,1))
  plot(state_pot,state_wlim,select=c("ROOTD","WA","LAI","TRAIN"),xlim=c(30,300))
  legend("topleft",legend=c("Irrigated","Rainfed"),col=c("black","red"),lty=c(1,2),bty="n")
dev.off()

#Add rates to the state variables and write to file
var_pot <- get_results(state_pot, yr, wdata, STTIME,FINTIM)
data.table::fwrite(var_pot, file = here::here("Results", paste0("Run details for LINTUL2 with irrigation for ", yr, ".csv",sep = "", collapse = NULL)))

var_wlim <- get_results(state_wlim, yr, wdata, STTIME,FINTIM)
write.csv(var_wlim,file=here::here("Results", paste0("Run details for LINTUL2 without irrigation for ",yr, ".csv",sep = "", collapse = NULL)))

