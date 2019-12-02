#' ---
#' title: "LINTUL2_code.r"
#' author: "Tom Schult, Mink Zijlstra and Hannington Bukomeko"
#' date: "29th Nov 2019"
#' ---

#This file contains functions: 
#
# LINTUL2,                    containing the LINTUL code 
# LINTUL2_DEFAUL_PARAMETERS,  containing all default settings
# LINTUL2_iniSTATES,          providing intial state values
# PENMAN,                     computation of the PENMAN EQUATION 
# EVAPTR,                     to compute actual rates of evaporation and transpiration
# GLA,                        to computes daily increase of leaf area index  
# DRUNIR,                     to compute rates of drainage, runoff and irrigation
# get_weather,                function to read CABO weather file
#
# Part of this code was developed by Mink Zijlstra and adapted to work with the deSolve package.
#
# Tom Schut, 2016


#--------------------------------------Lintul---------------------------------------#
#                                                                                   # 
#-----------------------------------------------------------------------------------#
LINTUL2 <-function(Time, State, Pars, WDATA){

  #intergration with delay for RROOTD
  with(as.list(c(State, Pars)), {
    
    RTRAIN <- WDATA$RAIN[Time]                       # rain rate, mm d-1
    DTEFF  <- max(0, WDATA$DAVTMP[Time] - TBASE)     # effective daily temperature (for crop development a treshold temperature (TBASE) needs to be exceeded)
    RPAR <- FPAR * WDATA$DTR[Time]                    # PAR MJ m-2 d-1
    
    #determine rates when crop is still growing
    if(TSUM < FINTSUM){
      # Determine water content of rooted soil
      WC  <- 0.001 * WA/ROOTD
      # Once the emergence date is reached and enough water is available the crop emerges (1), once the crop is established is does not disappear again (2)
      if((Time - DOYEM + 1) > 0 && (WC-WCWP) > 0) { 	# (1)
        emerg1 <- 1 } else { emerg1 <- 0 }
      if(LAI > 0) {									# (2)
        emerg2 <- 1 } else { emerg2 <- 0}
      # Emergence of the crop is used to calculate the accumulated temperature sum.
      EMERG  <- max(emerg1,emerg2)
      
      RTSUM  <- DTEFF * EMERG

      # If soil water content drops to, or below, wilting point fibrous root growth stops.
      # As long as the crop has not reached its maximum rooting depth and has not started flowering yet, fibrous root growth continues.
      if((ROOTDM-ROOTD) > 0 && (TSUMAN-TSUM) > 0 && (WC-WCWP) >= 0) {
        # The rooting depth (m) is calculated from a maximum rate of change in rooting depth, the emergence of the crop and the constraints mentioned above.
        RROOTD <- RRDMAX * EMERG 
      }else{ RROOTD = 0}
      
      EXPLOR <- 1000 * RROOTD * WCFC                   # exploration rate of new soil water layers by root depth growth (mm d-1)
      
      RNINTC <- min(RTRAIN,MMWET * LAI)                # interception of rain by the canopy (mm d-1)
      
      # Potential evaporation (mm d-1) and transpiration (mm d-1) are calculated according to Penman-Monteith
      PENM   <- penman(WDATA$DAVTMP[Time],WDATA$VP[Time],WDATA$DTR[Time],LAI,WDATA$WN[Time],RNINTC)
      RPTRAN <- PENM$PTRAN
      RPEVAP <- PENM$PEVAP
      
      # Actual evaporation (mm d-1) and transpiration (mm d-1)
      EVA  <- evaptr(RPEVAP,RPTRAN,ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST,TRANCO,DELT)
      RTRAN <- EVA$TRAN
      REVAP <- EVA$EVAP
      
      # The transpiration reduction factor is defined as the ratio between actual and potential transpiration
      if(RPTRAN == 0) {
        TRANRF <- 1
      }else{
        TRANRF <- RTRAN / RPTRAN
      }
      
      # Drainage (below the root zone; mm d-1), surface water runoff (mm d-1) and irrigation rate (mm d-1)
      DRUNIR    <- drunir(RTRAIN,RNINTC,REVAP,RTRAN,IRRIGF,DRATE,DELT,WA,ROOTD,WCFC,WCST)
      
      # Rate of change of soil water amount (mm d-1)
      RWA <- (RTRAIN + EXPLOR + DRUNIR$IRRIG) - (RNINTC + DRUNIR$RUNOFF + RTRAN + REVAP + DRUNIR$DRAIN)
      
      # Light interception (MJ m-2 d-1) and total crop growth rate (g m-2 d-1)
      PARINT <- RPAR * (1 - exp(-K * LAI))
      GTOTAL <- LUE * PARINT * TRANRF
      
      # Relative death rate (d-1) due to aging
      if((TSUM-TSUMAN) < 0) {
        RDRDV <- 0
      } else {
#        RDRDV <- approx(RDRT[,1],RDRT[,2],WDATA$DAVTMP[Time])$y
        RDRDV <- approx(RDRT.TEMP,RDRT.RDR,WDATA$DAVTMP[Time])$y
      }
      
      # Relative death rate (d-1) due to self shading
      RDRSH <- RDRSHM * (LAI-LAICR) / LAICR
      if(RDRSH < 0) {
        RDRSH <- 0
      } else if(RDRSH >=RDRSHM) {
        RDRSH <- RDRSHM
      }
      
      # Effective relative death rate (1; d-1) and the resulting decrease in LAI (2; m2 m-2 d-1) and leaf weight (3; g m-2 d-1)
      RDR   <- max(RDRDV, RDRSH) 	# (1)
      DLAI  <- LAI * RDR  			  # (2)
      RWLVD   <- WLVG * RDR	# (3)
      
      # Allocation to roots (2), leaves (4), stems (5) and storage organs (6)
      # fractions allocated are modified for water availability (1 and 3)
      FRTMOD <- max(1, 1/(TRANRF+0.5))						    # (1)
      FRT    <- approx(FRTTB.TSUM,FRTTB.FRT,TSUM)$y * FRTMOD		# (2)
      FSHMOD <- (1 - FRT) / (1 - FRT / FRTMOD)				    # (3)
      FLV    <- approx(FLVTB.TSUM,FLVTB.FLV,TSUM)$y * FSHMOD		# (4)
      FST    <- approx(FSTTB.TSUM,FSTTB.FST,TSUM)$y * FSHMOD		# (5)
      FSO    <- approx(FSOTB.TSUM,FSOTB.FSO,TSUM)$y * FSHMOD		# (6)
      
      # Change in biomass (g m-2 d-1) for leaves (1), green leaves (2), stems (3), storage organs (4) and roots (5)
      RWLV   <- GTOTAL * FLV 			    # (1)
      RWLVG  <- GTOTAL * FLV - RWLVD 	# (2)
      RWST   <- GTOTAL * FST			    # (3)
      RWSO   <- GTOTAL * FSO			    # (4)
      RWRT   <- GTOTAL * FRT			    # (5)
      
      GLAI <- gla(Time,DOYEM,DTEFF,TSUM,LAII,RGRL,DELT,SLA,LAI,RWLV,TRANRF,WC,WCWP)

      # Change in LAI (m2 m-2 d-1) due to new growth of leaves
      RLAI <- GLAI - DLAI
    }
    else{
      #all plant related rates are set to 0
      RROOTD <- 0
      RWLV <- 0
      RWA <- 0
      RTSUM <- 0
      RLAI <- 0
      RWLVG <- 0
      RWLVD <- 0
      RWST <- 0
      RWSO <- 0
      RWRT <- 0
      RWLV <- 0 
      RTRAN <- 0 
      REVAP <- 0 
      RPTRAN <- 0 
      RPEVAP <- 0
    }
    return(list(c(RROOTD,RWA,RTSUM,RTRAIN,RPAR,RLAI,RWLV,RWLVG,RWLVD,RWST,RWSO,RWRT,RTRAN,REVAP,RPTRAN,RPEVAP) )) 
    
  })
}

#-----------------------------Initial conditions------------------------------------#
#                                                                                   #
# A number of variables need to be given an initial value. These variables are used #
# by the model before they are calculated during the first time step. During all    #
# subsequent time steps the updated values will be used.                            # 
#                                                                                   #
#-----------------------------------------------------------------------------------#
LINTUL2_iniSTATES <-function(){
  #'@title LINTUL2_iniSTATES
  #'@description A number of variables need to be given an initial value. 
  #' These variables are used by the model before they are calculated during the first time step. 
  #' During all subsequent time steps, the updated values will be used.
  #' @param ROOTDI Initial rooting depth at crop emergence measured in metres (M)
  #' @param WA Initial soil water amount measured in millimetres (mm)
  #' @param TSUM Temperature sum measured in degrees Celcius (deg. C)
  #' @param TRAIN Rain sum measured in millimetres (mm)
  #' @param PAR PAR sum measured in Mega jules per meter squared (MJ m-2)
  #' @param LAI Leaf Area Index measured in metres squared of leaf per metres squared of land (m2 m-2)
  #' @param WLV Initial green leaf dry weight (at crop emergence) measured in grams per meter squared (g m-2)
  #' @param WLVG Dry weight of green leaves measured in grams per meter squared (g m-2)
  #' @param WLVD Dry weight of dead leaves measured in grams per meter squared (g m-2)
  #' @param WST Dry weight of stems measured in grams per meter squared (g m-2)
  #' @param WSO Dry weight of storage organs measured in grams per meter squared (g m-2)
  #' @param WRT Dry weight of roots measured in grams per meter squared (g m-2)
  #' @param TRAN Actual transpiration measured in millimetres (mm)
  #' @param EVAP Actual evaporation measured in millimetres (mm)
  #' @param PTRAN Potential transpiration measured in millimetres (mm)
  #' @param PEVAP Potential evaporation measured in millimetres (mm)
  #' @return a vector of starting values for each of the listed parameters.
  parvalues <- c(LINTUL2_DEFAULT_PARAMETERS_SOIL(),
                 LINTUL2_DEFAULT_PARAMETERS_SPRINGWHEAT()
  )
return( c(ROOTD   = parvalues[["ROOTDI"]],                             # M     :    initial rooting depth (at crop emergence)
            WA      = 1000 * parvalues[["ROOTDI"]] * parvalues[["WCI"]], # mm    :    initial soil water amount
            TSUM    = 0,                                                 # deg. C:    temperature sum 
            TRAIN   = 0,                                                 # mm   :    rain sum
            PAR     = 0,                                                 # MJ m-2:    PAR sum
            LAI     = 0,                                                 # m2 m-2:    leaf area index 
            WLV     = parvalues[["LAII"]] / parvalues[["SLA"]],          # g m-2 :    initial green leaf dry weigth (at crop emergence)
            WLVG    = parvalues[["LAII"]] / parvalues[["SLA"]],          # g m-2 :    dry weight of green leaves 
            WLVD    = 0,                                                 # g m-2 :    dry weight of dead leaves
            WST     = 0,                                                 # g m-2 :    dry weight of stems
            WSO     = 0,                                                 # g m-2 :    dry weight of storage organs
            WRT     = 0,                                                 # g m-2 :    dry weight of roots
            TRAN    = 0,                                                 # mm    :    actual transpiration
            EVAP    = 0,                                                 # mm    :    actual evaporation
            PTRAN   = 0,                                                 # mm    :    potential transpiration
            PEVAP   = 0                                                  # mm    :    potential evaporation
  ))
}

LINTUL2_DEFAULT_PARAMETERS_SOIL <-function() {
  #'@title LINTUL2_DEFAULT_PARAMETERS_SOIL
  #'@description A number of variables need to be given an initial value. 
  #' These variables are used by the model before they are calculated during the first time step. 
  #' During all subsequent time steps, the updated values will be used.
  #'@param DELT Time step in days (d)
  #'@param ROOTDI Initial rooting depth at crop emergence measured in metres (M)
  #'@param WCI Initial soil water content measured in m3 m-3
  #'@param WCAD Soil water content at air dryness measured in m3 m-3
  #'@param WCWP Soil water content at wilting point measured in m3 m-3
  #'@param WCFC Soil water content at field capacity measured in m3 m-3
  #'@param WCWET Critical soil water content for transpiration reduction due to waterlogging measured in m3 m-3
  #'@param WCST Soil water content at full saturation measured in m3 m-3
  #'@param DRATE Max drainage rate measured in (mm d-1)
  #'@param IRRIGF Irrigation rate relative to the rate required to keep soil water status at field capacity
  #'@param FPAR Fraction PAR,
  #'@param MMWET  Rain intercepted per leaf layer (mm d-1)
  #'@return a vector of starting values for each of the listed parameters.
  #All defaults
  return(c( DELT    = 1.0,    # Day          
            ROOTDI  = 0.1,    # M           :    initial rooting depth (at crop emergence)
            WCI     = 0.36,   # (m3 m-3)      :     initial soil water content 
            WCAD    = 0.08,   # m3 m-3      :     soil water content at air dryness 
            WCWP    = 0.23,   # m3 m-3      :     soil water content at wilting point
            WCFC    = 0.36,   # m3 m-3      :     soil water content at field capacity 
            WCWET   = 0.48,   # m3 m-3      :     critical soil water content for transpiration reduction due to waterlogging
            WCST    = 0.55,   # m3 m-3      :     soil water content at full saturation 
            DRATE   = 50,     # mm d-1      :     max drainage rate
            IRRIGF  = 0,      # (-)         :     irrigation rate relative to the rate required to keep soil water status at field capacity
            FPAR    = 0.5,    # (-)         :     Fraction PAR, MJ PAR/MJ DTR
            MMWET   = 0.25    # mm         :     Rain intercepted per leaf layer
  ))
}

#---------------------------------------------------------------------#
# FUNCTION LINTUL2_DEFAUL_PARAMETERS                                               #
# Purpose: Listing the default input parameters for Lintul2
# Spring Wheat, based on report from Spitters, 1990#
#---------------------------------------------------------------------#
LINTUL2_DEFAULT_PARAMETERS_SPRINGWHEAT <-function() {
  #'@title LINTUL2_DEFAULT_PARAMETERS_SPRINGWHEAT
  #'@description This function lists Listing the default input parameters for Lintul2 Spring Wheat, based on report from Spitters, 1990
  #'@param ROOTDI Initial rooting depth at crop emergence measured in M
  #'@param LAII  Initial leaf area index measured in m2 m-2
  #'@param SLA  Specific leaf area measured in m2 g-1
  #'@param ROOOTDM Max rate increase of rooting depth measured in M
  #'@param RRDMAX Max rate increase of rooting depth measured in m d-1
  #'@param TRANCO Transpiration constant (indicating level of drought tolerance) measured in mmd-1
  #'@param TBASE Base temperature measured in deg. C
  #'@param LUE Light use efficiency measured in g MJ-1
  #'@param K Extinction coefficient for PAR 
  #'@param DOYEM Daynumber at crop emergence measured in d
  #'@param RGRL Relative growth rate of LAI during exponential growth measured in 1/(deg.C d)
  #'@param TSUMAN Temperature sum at anthesis measured in deg. C
  #'@param FINTSUM Temperature sum at anthesis measured in deg. C
  #'@param LAICR Critical LAI measured in m2 m-2
  #'@param RDRSHM Max relative death rate of leaves due to shading measured in d -1
  #'@param FPAR
  #'@return The dataframes: (i) FRTTB. Partitioning    TSUM, fraction of daily growth to roots: FRT as a function model;   
  #' (ii) FLVTB Partitioning    TSUM, fraction of daily growth to leaves;  
  #' (iii) FSTTB Partitioning    TSUM, fraction of daily growth to stem and;   
  #' (iv) FSOTB Partitioning    TSUM, fraction of daily growth to storage organs
  #All defaults
  return(c( ROOTDI  = 0.1,    # M           :    initial rooting depth (at crop emergence)
            LAII    = 0.012,  # m2 m-2      :     initial leaf area index 
            SLA     = 0.022,  # m2 g-1      :     specific leaf area 
            ROOTDM  = 0.6,    # m           :     maximum rooting depth
            RRDMAX  = 0.012,  # m  d-1      :     max rate increase of rooting depth
            TRANCO  = 8,      # mm d-1      :     transpiration constant (indicating level of drought tolerance)
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
                                FSO=c(0.0, 0.0, 0.98,  1.0, 1.0)) 
  ))
}

#---------------------------------------------------------------------#
# SUBROUTINE PENMAN                                                   #
#---------------------------------------------------------------------#
penman <-function(DAVTMP,VP,DTR,LAI,WN,RNINTC) {
  #'@title penman.
  #'@description The function computes the PM equation. the function takes 6 arguments or parameters as decribed below.
  #'@param DAVTMP The daily average temperature.
  #'@param VP The vapour Pressure.
  #'@param DTR Daily total radiation.
  #'@param LAI  The leaf area index.
  #'@param WN The wind speed.
  #'@param RNINTC The amount of rainfall intercepted.
  #'@return Output is a dataframe called PENM with 2 columns of computed (i) potential evaporation and, (ii) potential transpiration corrected for leaf wetness.
  
  DTRJM2 <-DTR * 1E6        # J m-2 d-1    :    Daily radiation in Joules 
  BOLTZM <-5.668E-8 	      # J m-1 s-1 K-4:    Stefan-Boltzmann constant 
  LHVAP  <-2.4E6            # J kg-1       :    Latent heat of vaporization 
  PSYCH  <-0.067            # kPa deg. C-1 :    Psychrometric constant

  # J m-2 d-1   :     Black body radiation  
  BBRAD  <-BOLTZM * (DAVTMP+273)^4 * 86400   
  
  # kPa         :     Saturation vapour pressure
  SVP    <-0.611 * exp(17.4 * DAVTMP / (DAVTMP + 239))
  
  # kPa dec. C-1:     Change of SVP per degree C
  SLOPE  <-4158.6 * SVP / (DAVTMP + 239)^2       
  
  # J m-2 d-1   :     Net outgoing long-wave radiation
  RLWN   <-BBRAD * pmax(0, 0.55 * (1 - VP / SVP))   
  
  # kg m-2 d-1  :     Wind function in the Penman equation
  WDF    <-2.63 * (1.0 + 0.54 * WN)                  
  
  # Net radiation (J m-2 d-1) for soil (1) and crop (2)
  NRADS  <-DTRJM2 * (1 - 0.15) - RLWN     # (1)
  NRADC  <-DTRJM2 * (1 - 0.25) - RLWN     # (2)
  
  # Radiation terms (J m-2 d-1) of the Penman equation for soil (1) and crop (2)
  PENMRS <-NRADS * SLOPE / (SLOPE + PSYCH)    # (1)
  PENMRC <-NRADC * SLOPE / (SLOPE + PSYCH)    # (2)
  
  # Drying power term (J m-2 d-1) of the Penman equation
  PENMD  <-LHVAP * WDF * (SVP - VP) * PSYCH / (SLOPE + PSYCH)
  
  # Potential evaporation and transpiration are weighed by a factor representing the plant canopy (exp(-0.5 * LAI)).
  PEVAP  <-exp(-0.5 * LAI)  * (PENMRS + PENMD) / LHVAP
  PTRAN  <-(1 - exp(-0.5 * LAI)) * (PENMRC + PENMD) / LHVAP
  PTRAN  <-pmax(0, PTRAN - 0.5 * RNINTC)                        # Potential transpiration is corrected for leaf wetness. The value of 0.5 the amount of rain intercepted is taken from Singh & Sceicz (1979).
  
  PENM = data.frame(cbind(PEVAP,PTRAN))
  
  return(PENM)
}

#---------------------------------------------------------------------#
# SUBROUTINE EVAPTR                                                   #
#---------------------------------------------------------------------#

evaptr <-function(PEVAP,PTRAN,ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST,TRANCO,DELT) {
  #' @title evaptr
  #' @description evaptr calculates actual evaporation
  #' @param PEVAP The potential evaporation.
  #' @param PTRAN The potential transpiration.
  #' @param ROOTD rooting depth at crop emergence measured in metres (M)
  #' @param WA The initial amount of soil water.
  #' @param WCAD The water content at air dryness.
  #' @param WCWP The water content at wilting point.
  #' @param WCFC The water content at field capacity.
  #' @param WCWET The critical soil water content for transpiration
  #' @param WCST The amount of soil water at full saturation
  #' @param TRANCO The transpiration constant (indicating level of drought tolerance)
  #' @param DELT The time step
  #' @return Output is a data frame called EVA that has actual transpiration and actual evaporation

  # Soil water content (m3 m-3) and the amount of soil water (mm) at air dryness (AD) and field capacity (FC).     
  WC   <-0.001 * WA / ROOTD
  WAAD <-1000 * WCAD * ROOTD
  WAFC <-1000 * WCFC * ROOTD
  
  # Evaporation is decreased when water content is below field capacity, but continues until WC = WCAD.
  limit.evap <-(WC-WCAD)/(WCFC-WCAD)
  #Ensure to stay within 0-1 range
  limit.evap <- pmin(1,pmax(0,limit.evap))
  EVAP <-PEVAP * limit.evap
  
  # A critical soil water content (m3 m-3) is calculated below which transpiration is reduced.
  WCCR <-WCWP + pmax(0.01, PTRAN/(PTRAN+TRANCO) * (WCFC-WCWP))
  
  # If water content is below the critical soil water content a correction factor is calculated that reduces 
  # transpiration until it stops at WC = WCWP.
  FR <-(WC-WCWP) / (WCCR - WCWP)
  
  # If water content is above the critical soil water content a correction factor is calculated that reduces 
  # transpiration when the crop is hampered by waterlogging (WC > WCWET).
  FRW <- (WCST-WC) / (WCST - WCWET)

  #Replace values for wet days with high WC values, above WCCR 
  FR[WC > WCCR] <- FRW[WC > WCCR]
  
  #Ensure to stay within the 0-1 range
  FR=pmin(1,pmax(0,FR))

  TRAN <-PTRAN * FR
 
  aux <- EVAP+TRAN
  aux[aux <= 0] <- 1
  
  # A final correction term is calculated to reduce evaporation and transpiration when evapotranspiration exceeds 
  # the amount of water in soil present in excess of air dryness.
  AVAILF <- pmin(1, (WA-WAAD)/(DELT*aux))
  EVA <- data.frame(EVAP = EVAP * AVAILF,
                    TRAN = TRAN * AVAILF)
  return(EVA)
}     

# ---------------------------------------------------------------------#
#  SUBROUTINE GLA                                                      #
#  Purpose: This subroutine computes daily increase of leaf area index #
#           (ha leaf/ ha ground/ d)                                    #
# ---------------------------------------------------------------------#

gla <-function(TIME,DOYEM,DTEFF,TSUM,LAII,RGRL,DELT,SLA,LAI,GLV,TRANRF,WC,WCWP) {
  #'@title gla
  #'@param TIME Undefined
  #'@param DOYEM Temperature sum measured in degrees Celcius (deg. C)
  #'@param DTEFF effective daily temperature
  #'@param TSUM Temperature sum measured in degrees Celcius (deg. C)
  #'@param LAII Initial Leaf Area Index measured  in m2 m-2 
  #'@param RGRL Relative growth rate of LAI during exponential growth measured in 1/(deg.C d)
  #'@param DELT Time step in days (d)
  #'@param SLA Specific leaf area measured in m2 g-1
  #'@param LAI Leaf area index
  #'@param GLV Undefined
  #'@param TRANRF A transpiration reduction factor
  #'@param WC Soil water content
  #'@param WCWP soil water content at wilting point measured in m3 m-3
  #'
  # Growth during maturation stage:
  GLAI <-SLA * GLV
  
  # Growth during juvenile stage:
  if(TSUM < 330 && LAI < 0.75) {
    GLAI <-LAI * (exp(RGRL * DTEFF * DELT) - 1) / DELT * TRANRF
  }
  
  # Growth at day of seedling emergence:
  if(TIME >= DOYEM && LAI == 0 && WC > WCWP) {
    GLAI <- LAII / DELT
  }
  
  # Growth before seedling emergence:
  if(TIME < DOYEM) {
    GLAI <-0
  }
  
  return(GLAI)
}

# ---------------------------------------------------------------------#
#  SUBROUTINE DRUNIR                                                   #
#  Purpose: To compute rates of drainage, runoff and irrigation        #
# ---------------------------------------------------------------------#

drunir <-function(RAIN,RNINTC,EVAP,TRAN,IRRIGF,DRATE,DELT,WA,ROOTD,WCFC,WCST) {
  #'@title drunir
  #'@description Calcualates drainage, runoff and water irrigated
  #'@param RAIN Precipitation measured in mm
  #'@param RNINTC The amount of rainfall intercepted
  #'@param EVAP Actual evaporation measured in millimetres (mm)
  #'@param TRAN Actual transpiration measured in millimetres (mm)
  #'@param IRRIGF Irrigation rate relative to the rate required to keep soil water status at field capacity 
  #'@param DRATE Max drainage rate measured in (mm d-1)
  #'@param DELT DELT Time step in days (d)
  #'@param WA Initial soil water amount measured in millimetres (mm)
  #'@param ROOTD Rooting depth at crop emergence measured in metres (M)
  #'@param WCFC Soil water content at field capacity measured in m3 m-3
  #'@param WCST Soil water content at full saturation measured in m3 m-3
  #'@return A data frame DRUNIR containing DRAIN, RUNOFF and IRRIG
  
  # Soil water content (m3 m-3) and the amount of soil water (mm) at field capacity (FC) and full saturation (ST).     
  WC   <-0.001 * WA / ROOTD
  WAFC <-1000 * WCFC * ROOTD
  WAST <-1000 * WCST * ROOTD
  
  # Drainage below the root zone occurs when the amount of water in the soil exceeds field capacity or when the amount of rainfall 
  # in excess of interception and evapotranspiration fills up soil water above field capacity.
  DRAIN <-(WA-WAFC)/DELT + (RAIN - (RNINTC + EVAP + TRAN))
  if(DRAIN < 0) {
    DRAIN <-0
  } else if(DRAIN >= DRATE) {
    DRAIN <-DRATE
  }
  # Surface runoff occurs when the amount of soil water exceeds total saturation or when the amount of rainfall 
  # in excess of interception, evapotranspiration and drainage fills up soil water above total saturation.
  RUNOFF = max(0, (WA - WAST) / DELT + (RAIN - (RNINTC + EVAP + TRAN + DRAIN)))
  
  
  # The irrigation rate is the extra amount of water that is needed to keep soil water at a fraction of field capacity that 
  # is defined by setting the parameter IRRIGF. If IRRIGF is set to 1, the soil will be irrigated every timestep to keep the 
  # amount of water in the soil at field capacity. IRRIGF = 0 implies rainfed conditions.
  IRRIG  = IRRIGF * max(0, (WAFC - WA) / DELT - (RAIN - (RNINTC + EVAP + TRAN + DRAIN + RUNOFF)))
  
  
  DRUNIR <- data.frame(DRAIN = DRAIN,RUNOFF=RUNOFF, IRRIG=IRRIG)
  
  return(DRUNIR)
}

#------------------------------------------------------------------------------------------------------#
# help FUNCTION                                                                                        #
# Purpose: creating full output of states rates and relevant auxillary variables                       #
# Adds all rates and 
#------------------------------------------------------------------------------------------------------#

get_results <- function(state_out, year, wdata, STTIME,FINTIM){
  #'@title get_results
  #'@description 
  #'@param state_out Undefined
  #'@param year Undefined
  #'@param wdata A subset of weather data (WDATA) that specific for specified time period as designated by @param STTIME and @param FINTIM . 
  #'@param STTIME Starting time for the weather data
  #'@param FINTIM Finishing time for the weather data
  #'@return The output is a dataframe called results that holds data of variables: `year, state_out, rate_out,TRANRF,HI,WSOTHA`.
  
  #get only relevant weather data
  WDATA <- wdata[STTIME:FINTIM,]
  parv <- LINTUL2_parameters()
  
  state_out = data.frame(state_out)
  #Determine rate variables from change in states. 
  dt= state_out[2:nrow(state_out),1] - state_out[1:nrow(state_out)-1,1]
  dstate = (state_out[2: nrow(state_out),   2:ncol(state_out)] 
            - state_out[1:(nrow(state_out)-1),2:ncol(state_out)])
  #Add 0s to first day and determine rates of change
  rate_out = rbind(0,dstate/dt)
  colnames(rate_out)<-c("RROOTD","RWA","RTSUM","RTRAIN","RPAR","RLAI", 
                        "RWLV", "RWLVG", "RWLVD", "RWST","RWSO","RWRT",
                        "RTRAN","REVAP","RPTRAN","RPEVAP")
  
  # Determine water content of rooted soil
  WC  <- 0.001 * state_out$WA/state_out$ROOTD
  
  RNINTC <- pmin(WDATA$RAIN,0.25 * state_out$LAI)         # interception of rain by the canopy (mm d-1)
  
  # Potential evaporation (mm d-1) and transpiration (mm d-1) are calculated according to Penman-Monteith
  PENM   <- penman(WDATA$DAVTMP,WDATA$VP,WDATA$DTR,state_out$LAI,WDATA$WN,RNINTC)
  
  # Actual evaporation (mm d-1) and transpiration (mm d-1)
  EVA  <- evaptr(PENM$PEVAP,PENM$PTRAN,state_out$ROOTD,state_out$WA,
                 parv[["WCAD"]],parv[["WCWP"]],parv[["WCFC"]],parv[["WCWET"]],parv[["WCST"]],parv[["TRANCO"]],1)
  
  # The transpiration reduction factor is defined as the ratio between actual and potential transpiration
  PENM$PTRAN[PENM$PTRAN== 0] <- 1 
  TRANRF <- EVA$TRAN / PENM$PTRAN
  
  #Determine harvest index
  HI <- state_out[["WSO"]] / (state_out[["WSO"]]+state_out[["WLV"]]+state_out[["WRT"]]+state_out[["WST"]])
  #convert from g m-2 to t ha-1 with factor 10E-2: t/g=10E-6 * factor m2/ha=10E4 = 0.01
  WSOTHA <- state_out[["WSO"]] * 0.01
  
  result <- data.frame(cbind(year, state_out, rate_out,TRANRF,HI,WSOTHA))
  
  return(result) 
} #wait

#---------------------------------------------------------------------#
#get_weather: Extracts weather data from weather files
#---------------------------------------------------------------------#
get_weather <- function(directory="..\\weather\\", seperator = "/", country="NLD",station="1",year="954"){
  #'@title get_weather.
  #'@description The function takes arguements of directory,country,station and year.
  #'@param directory The directory where the weather data is stored relative to the working.
  #'@param country The country where the data was recorded with default as netherlands abbreviated as NLD.
  #'@param station The weather station number.
  #'@param year The year in which the data was recorded.
  #'@return Output is a dataframe called WDATA. it contains data on vapour pressure(VP), wind speed (WN),precipitation (RAIN),daily total radiation (DTR) and daily average temperature (DAVTMP).
  weather   <- matrix(data=as.numeric(unlist(scan(paste(directory,seperator, country,station,".",year,sep=""),
                      what=list("","","","","","","","",""),comment.char='*',fill=TRUE,quiet=TRUE))),ncol=9)
  RDD   = as.vector(weather[-1,4])       # kJ m-2 d-1:     daily global radiation     
  TMMN  = as.vector(weather[-1,5])       # deg. C   :     daily minimum temperature  
  TMMX  = as.vector(weather[-1,6])       # deg. C   :     daily maximum temperature  
  # RDD,TMMN and TMMX are 4th,5th and 6th elements of matrix.
  # ?why the -1?
  WDATA <- data.frame(
    VP 	  = as.vector(weather[-1,7]),    # kPa      :     vapour pressure            
    WN 	  = as.vector(weather[-1,8]),    # m s-1    :     wind speed                 
    RAIN  = as.vector(weather[-1,9]),    # mm       :     precipitation              
    DTR    = RDD / 1e+03,                # incoming radation is converted from kJ to MJ
    DAVTMP = 0.5 * (TMMN + TMMX)         # daily average temperature (degrees C)
  )
}

 