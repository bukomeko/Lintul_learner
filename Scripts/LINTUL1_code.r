#This file contains functions: 
#
# LINTUL1,                    containing the LINTUL code 
# LINTUL1_DEFAUL_PARAMETERS,  containing all default settings
# LINTUL1_iniSTATES,          providing intial state values
# GLA,                        to computes daily increase of leaf area index  
# plot_years,                 function for plotting multiple years (max 15) in one graph
# get_weather,                function to read CABO weather file
#
# Part of this code was developed by Mink Zijlstra and adapted to work with the deSolve package.
#
# Tom Schut, 2016

### Lintul1---------------------------------------------------------------------------#
LINTUL1 <-function(Time, State, Pars, WDATA){
  with(as.list(c(State, Pars)), {
    
    DTEFF  <- max(0, WDATA$DAVTMP[Time] - TBASE)     # effective daily temperature (for crop development a treshold temperature (TBASE) needs to be exceeded)
    RPAR <- FPAR * WDATA$DTR[Time]                    # PAR MJ m-2 d-1
    
    #determine rates when crop is still growing
    if(TSUM < FINTSUM){
      # Once the emergence date is reached and enough water is available the crop emerges (1), once the crop is established is does not disappear again (2)
      if((Time - DOYEM + 1) > 0 ) { 	# (1)
        emerg1 <- 1 } else { emerg1 <- 0 }
      if(LAI > 0) {									# (2)
        emerg2 <- 1 } else { emerg2 <- 0}
      # Emergence of the crop is used to calculate the accumulated temperature sum.
      EMERG  <- max(emerg1,emerg2)
      
      RTSUM  <- DTEFF * EMERG

      # Light interception (MJ m-2 d-1) and total crop growth rate (g m-2 d-1)
      PARINT <- RPAR * (1 - exp(-K * LAI))
      GTOTAL <- LUE * PARINT
      
      # Relative death rate (d-1) due to aging
      if((TSUM-TSUMAN) < 0) {
        RDRDV <- 0
      } else {
        RDRDV <- approx(RDRT.TEMP,RDRT.RDR,WDATA$DAVTMP[Time])$y
      }
      
      # Relative death rate (d-1) due to self shading
      RDRSH <- RDRSHM * (LAI-LAICR) / LAICR
      if(RDRSH < 0) {
        RDRSH <- 0
      } else if(RDRSH >= RDRSHM) {
        RDRSH <- RDRSHM
      }
      
      # Effective relative death rate (1; d-1) and the resulting decrease in LAI (2; m2 m-2 d-1) and leaf weight (3; g m-2 d-1)
      RDR   <- max(RDRDV, RDRSH) 	# (1)
      DLAI  <- LAI * RDR  			  # (2)
      RWLVD <- WLVG * RDR	# (3)
      
      # Allocation to roots (2), leaves (4), stems (5) and storage organs (6)
      FRT    <- approx(x = FRTTB.TSUM,y = FRTTB.FRT,xout = TSUM)$y 		# (2)
      FLV    <- approx(x = FLVTB.TSUM,y = FLVTB.FLV,xout = TSUM)$y  	# (4)
      FST    <- approx(x = FSTTB.TSUM,y = FSTTB.FST,xout = TSUM)$y   	# (5)
      FSO    <- approx(x = FSOTB.TSUM,y = FSOTB.FSO,xout = TSUM)$y 		# (6)

      # Change in biomass (g m-2 d-1) for leaves (1), green leaves (2), stems (3), storage organs (4) and roots (5)
      RWLV    <- GTOTAL * FLV 			  # (1)
      RWLVG  <- GTOTAL * FLV - RWLVD 	# (2)
      RWST   <- GTOTAL * FST			  # (3)
      RWSO   <- GTOTAL * FSO			  # (4)
      RWRT   <- GTOTAL * FRT			  # (5)
      
      GLAI <- gla(Time,DOYEM,DTEFF,TSUM,LAII,RGRL,DELT,SLA,LAI,RWLV)
      # Change in LAI (m2 m-2 d-1) due to new growth of leaves
      RLAI <- GLAI - DLAI
    }
    else{
      #all plant related rates are set to 0
      RWLV <- 0
      RTSUM <- 0
      RLAI <- 0
      RWLVG <- 0
      RWLVD <- 0
      RWST <- 0
      RWSO <- 0
      RWRT <- 0
      RWLV <- 0 
    }
    return(list(c(RTSUM,RPAR,RLAI,RWLV,RWLVG,RWLVD,RWST,RWSO,RWRT) )) 
    
  })
}

#-----------------------------Initial conditions------------------------------------#
#                                                                                   #
# A number of variables need to be given an initial value. These variables are used #
# by the model before they are calculated during the first time step. During all    #
# subsequent time steps the updated values will be used.                            # 
#                                                                                   #
#-----------------------------------------------------------------------------------#
LINTUL1_iniSTATES <-function(Pars){
  with(as.list(Pars),{
  return( c(TSUM    = 0,                 # deg. C:    temperature sum 
            PAR     = 0,                 # MJ m-2:    PAR sum
            LAI     = 0,                 # m2 m-2:    leaf area index 
            WLV     = LAII/SLA,          # g m-2 :    initial green leaf dry weigth (at crop emergence)
            WLVG    = LAII/SLA,          # g m-2 :    dry weight of green leaves 
            WLVD    = 0,                 # g m-2 :    dry weight of dead leaves
            WST     = 0,                 # g m-2 :    dry weight of stems
            WSO     = 0,                 # g m-2 :    dry weight of storage organs
            WRT     = 0))                # g m-2 :    dry weight of roots
  })
}


#---------------------------------------------------------------------#
# FUNCTION LINTUL1_DEFAULT_PARAMETERS_SPRING_WHEAT                    #
# Purpose: Listing the default input parameters for Lintul1           #
#---------------------------------------------------------------------#
LINTUL1_DEFAULT_PARAMETERS_SPRING_WHEAT <-function() {
  #All defaults
  return(c( LAII    = 0.012,  # m2 m-2      :     initial leaf area index 
            SLA     = 0.022,  # m2 g-1      :     specific leaf area 
            TBASE   = 0,      # deg. C      :     base temperature 
            LUE     = 3.0,    # g MJ-1      :     light use efficiency
            K 	    = 0.6,    # (-)         :     extinction coefficient for PAR
            DOYEM   = 32,     # d           :     daynumber at crop emergence. 
            RGRL    = 0.009,  # 1/(deg. C d):     relative growth rate of LAI during exponential growth
            TSUMAN  = 1110,   # deg. C      :     temperature sum at anthesis
            FINTSUM = 2080,   # deg. C      :     temperature sum at which simulation stops
            LAICR   = 4,      # m2 m-2      :     critical LAI
            RDRSHM  = 0.03,   # d-1         :     max relative death rate of leaves due to shading
            FPAR    = 0.5,    # (-)         :     Fraction PAR, MJ PAR/MJ DTR
            MMWET   = 0.25,    # mm          :     Rain intercepted per leaf layer
            #Look-up tables   oC,  RDR
            RDRT    = data.frame(TEMP=c(  -1,   10,   15,   30,   50),
                                 RDR=c(0.03, 0.03, 0.04, 0.09, 0.09)),
            #Partitioning    TSUM,  fraction of daily growth to roots: FRT as a function of TSUM
            FRTTB = data.frame(TSUM=c(   0,  110,  275,  555,  780, 1055, 1160,1305,2500),
                               FRT=c(0.50, 0.50, 0.34, 0.12, 0.07, 0.03, 0.02, 0.0, 0.0)),  
            #Partitioning    TSUM,  fraction of daily growth to leaves
            FLVTB = data.frame(TSUM=c(   0,  110, 275,  555, 780,1055,2500),
                               FLV=c(0.33, 0.33,0.46, 0.44,0.14, 0.0, 0.0)), 
            #Partitioning    TSUM,  fraction of daily growth to stem
            FSTTB = data.frame(TSUM=c(   0,  110, 275,  555, 780, 1055, 1160,2500), 
                               FST=c(0.17, 0.17,0.20, 0.44,0.79, 0.97,  0.0, 0.0)), 
            #Partitioning    TSUM,  fraction of daily growth to storage organs
            FSOTB = data.frame(TSUM=c(  0,1055, 1160, 1305,2500),
                               FSO=c(0.0, 0.0, 0.98,  1.0, 1.0))   ))
}

# ---------------------------------------------------------------------#
#  SUBROUTINE GLA                                                      #
#  Purpose: This subroutine computes daily increase of leaf area index #
#           (ha leaf/ ha ground/ d)                                    #
# ---------------------------------------------------------------------#

gla <-function(TIME,DOYEM,DTEFF,TSUM,LAII,RGRL,DELT,SLA,LAI,GLV) {
  
  # Growth during maturation stage:
  GLAI <-SLA * GLV
  
  # Growth during juvenile stage:
  if(TSUM < 330 && LAI < 0.75) {
    GLAI <-LAI * (exp(RGRL * DTEFF * DELT) - 1) / DELT 
  }
  
  # Growth at day of seedling emergence:
  if(TIME >= DOYEM && LAI == 0 ) {
    GLAI <- LAII / DELT
  }
  
  # Growth before seedling emergence:
  if(TIME < DOYEM) {
    GLAI <-0
  }
  
  return(GLAI)
}


#------------------------------------------------------------------------------------------------------#
# help FUNCTION                                                                                        #
# Purpose: creating full output of states rates and relevant auxillary variables                       #
# Adds all rates and 
#------------------------------------------------------------------------------------------------------#
get_results <- function(state_out, year, wdata, STTIME,FINTIM){
  #get only relevant weather data
  WDATA <- wdata[STTIME:FINTIM,]

  state_out = data.frame(state_out)
  #Determine rate variables from change in states. 
  dt= state_out[2:nrow(state_out),1] - state_out[1:nrow(state_out)-1,1]
  dStates = (state_out[2: nrow(state_out),   2:ncol(state_out)] 
            - state_out[1:(nrow(state_out)-1),2:ncol(state_out)])
  #Add 0s to first day and determine rates of change
  rate_out = rbind(0,dStates/dt)
  colnames(rate_out)<-c("RTSUM","RPAR","RLAI", 
                        "RWLV", "RWLVG", "RWLVD", 
                        "RWST","RWSO","RWRT")
  
  #Determine harvest index
  HI <- state_out[["WSO"]] / (state_out[["WSO"]]+state_out[["WLV"]]+state_out[["WRT"]]+state_out[["WST"]])
  #convert from g m-2 to t ha-1 with factor 10E-2: t/g=10E-6 * factor m2/ha=10E4 = 0.01
  WSOTHA <- state_out[["WSO"]] * 0.01
  
  result <- data.frame(cbind(year, state_out, rate_out,HI))
  
  return(result) 
}

#---------------------------------------------------------------------#
# FUNCTION get_weather                                              #
# Purpose: Listing the weather data for Lintul1                       #
#---------------------------------------------------------------------#

get_weather <- function(directory="e:/userdata/weather/",country="NLD",station="1",year="954"){
  weather   <- matrix(data=as.numeric(unlist(scan(file=paste(directory,country,station,".",year,sep=""),
                      what=list("","","","","","","","",""),comment.char='*',fill=TRUE,quiet=TRUE))),ncol=9)
  RDD   = as.vector(weather[-1,4])    # kJ m-2 d-1:     daily global radiation     
  TMMN  = as.vector(weather[-1,5])    # deg. C   :     daily minimum temperature  
  TMMX  = as.vector(weather[-1,6])    # deg. C   :     daily maximum temperature  
  
  WDATA <- data.frame(
    DTR    = RDD / 1e+03,                # incoming radation is converted from kJ to MJ
    DAVTMP = 0.5 * (TMMN + TMMX)         # daily average temperature (degrees C)
  )
}
