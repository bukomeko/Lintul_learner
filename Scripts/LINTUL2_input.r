#---------------------------------------------------------------------#
# FUNCTION adapted parameters                                               #
# Purpose: Listing the input parameters for Lintul2                   #
#---------------------------------------------------------------------#
LINTUL2_parameters <-function(irri=FALSE,soiltype="clay") {
  #get the true defaults
  CROP_PARAM <- LINTUL2_DEFAULT_PARAMETERS_SPRINGWHEAT() 
  SOIL_PARAM <- LINTUL2_DEFAULT_PARAMETERS_SOIL()
  PARAM <- c(CROP_PARAM,SOIL_PARAM)

  #change what is desired
  PARAM[["DOYEM"]]  <- 60    # day nr of emergence
  
  if(irri==TRUE){ PARAM[["IRRIGF"]] <- 1 }

  if(soiltype=="clay"){
    PARAM[["ROOTDM"]]  <- 1.2    # m           :     maximum rooting depth
    PARAM[["WCAD"]]    <- 0.08   # m3 m-3      :     soil water content at air dryness 
    PARAM[["WCWP"]]    <- 0.20   # m3 m-3      :     soil water content at wilting point
    PARAM[["WCFC"]]    <- 0.46   # m3 m-3      :     soil water content at field capacity 
    PARAM[["WCWET"]]   <- 0.49   # m3 m-3      :     critical soil water content for transpiration reduction due to waterlogging
    PARAM[["WCST"]]    <- 0.52   # m3 m-3      :     soil water content at full saturation 
    PARAM[["DRATE"]]   <- 50     # mm d-1      :     max drainage rate
  }else  if( soiltype=="sand"){
    PARAM[["ROOTDM"]]  <- 0.6    # m           :     maximum rooting depth
    PARAM[["WCAD"]]    <- 0.05   # m3 m-3      :     soil water content at air dryness 
    PARAM[["WCWP"]]    <- 0.08   # m3 m-3      :     soil water content at wilting point
    PARAM[["WCFC"]]    <- 0.16   # m3 m-3      :     soil water content at field capacity 
    PARAM[["WCWET"]]   <- 0.40   # m3 m-3      :     critical soil water content for transpiration reduction due to waterlogging
    PARAM[["WCST"]]    <- 0.42   # m3 m-3      :     soil water content at full saturation 
    PARAM[["DRATE"]]   <- 50     # mm d-1      :     max drainage rate
  }else  if( soiltype=="sandPRAC"){
    PARAM[["DOYEM"]]  <- 32    # day nr of emergence
  PARAM[["ROOTDM"]]  <- 0.3    # m           :     maximum rooting depth
  PARAM[["WCAD"]]    <- 0.04   # m3 m-3      :     soil water content at air dryness 
  PARAM[["WCWP"]]    <- 0.09   # m3 m-3      :     soil water content at wilting point
  PARAM[["WCFC"]]    <- 0.28   # m3 m-3      :     soil water content at field capacity 
  PARAM[["WCWET"]]   <- 0.32   # m3 m-3      :     critical soil water content for transpiration reduction due to waterlogging
  PARAM[["WCST"]]    <- 0.38   # m3 m-3      :     soil water content at full saturation 
  PARAM[["DRATE"]]   <- 50     # mm d-1      :     max drainage rate
}
  
  return(PARAM)  
}

