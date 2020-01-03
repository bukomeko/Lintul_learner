#The name of the file was too long, namely
#"PAL_HaAfter30Dec2014VanGodfrey20Okt2014GT_JUN2014_EX_PALJun2012_Water_limited.fst"
#Ichanged it into "PAL_HaAfter30Dec2014VanGodfrey20Okt2014.fst".
#All Area units in ha.
###Search for "###PAL:" to see remarks/adaptations from Peter.
###Search for "ADDED : KN" to see remarks/adaptations from Kenneth.
###Search for "ADDED : GT" to see remarks/adaptations from Godfrey.
### Development affected by water stress in lines 964 - 978.
############################################################################################
  ########## Version ". . . . . . . . . . . . . . . . . ." . . . .2013 #######################
  ########## Version "PAL25June2012Vs18RADINT1_7.fst", dd 25 June 2012 #######################
  ########## Version "PAL13Aug2009Vs17RADINT1_7.fst", dd 13 August 2009 ######################
  ###Definitions of subroutines and functions.

#####ADDED: GT
DEFINE_CALL PENMAN(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,...
                   INPUT,INPUT,INPUT,INPUT,INPUT, ...
                   OUTPUT,OUTPUT,OUTPUT,OUTPUT,OUTPUT,OUTPUT)

DEFINE_CALL EVAPTR(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT, ...
                   INPUT,INPUT,INPUT,INPUT,INPUT,...
                   OUTPUT,OUTPUT,OUTPUT,OUTPUT,OUTPUT,OUTPUT)

DEFINE_CALL DRUNIR(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT, ...
                   INPUT,INPUT,INPUT,              OUTPUT,OUTPUT,OUTPUT)
######STOP: GT
####pal 24fEB2015
DEFINE_FUNCTION TRARFfu(INPUT, INPUT, INPUT, INPUT)
####STOP: PAL

DEFINE_CALL GLA(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,...
                INPUT,INPUT,                                OUTPUT)

DEFINE_FUNCTION Fsu2(INPUT,INPUT,INPUT,INPUT)

DEFINE_FUNCTION prt1(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT,INPUT)

DEFINE_FUNCTION prt2(INPUT,INPUT,INPUT,INPUT,INPUT,INPUT)

DEFINE_CALL INTERPOL(STRING,STRING,STRING,STRING,                     ...
                     INPUT_ARRAY,INPUT_ARRAY,INPUT_ARRAY,INPUT_ARRAY ,...
                     INTEGER_INPUT,INPUT,OUTPUT,OUTPUT,OUTPUT,OUTPUT)

###Definitions of Arrays for the functions that are linearly interpolated.
ARRAY PCOTB(1:K), PSTTB(1:K), PLVTB(1:K), PBUTB(1:K)

############################################################################################
  
  TITLE Water-limited production in East African highland banana cropping system

###Remarks
###This program assumes an established banana plantation.
###Initialisation should always be between TSUM1 is 2262 and 2423(referring to plant 1
                                                                  ###- mother).In that physiological time period, a mother plant and sucker 1, are present,
###but not yet sucker 2. Thus, only sucker 2 goes through the juvenile stage and the
###prejuvenile stage, where it has no functionalleaves yet.
###Units used throughout the program are hectare (ha), kg, degree celcius (C),
###MJ for radiation and days (d).

INITIAL

############################################################################################
  
  #####ADDED: GT
###initial water content in mulch

#Peter30Jan2015
# MSTCUI can be taken as MULSWS#DMmulch / 10000. assuming that the mulch is maximally wetted, or
# it can be taken equal to the minimum amount of water held by the mulch, which equals MSTMIN, so
# not zero. (The "/10000." is not in the below equation because it is now expressed in ha and not
             # in m2 anymore.)
#INCON MSTCUI = 0.3

#MSTCUI = MSTMIN

MSTCUI = MULSWS # DMmulch_I

#ENDPeter30Jan2015

###Initial amount of water in rooting depth layer

#Peter30Jan2015
#It is easier to initialise the amount of water via the moisture content.
#Perhaps best to take WCI equal to WCFC as starting moisture content. Under the current soil water
#characteristics WAI is calculated as:
  #WAI = 0.33 (mm H2O / mm Soil) # 0.5 (m Soil) # 1000. (mm Soil / m Soil) = 165.0 mm H2O.
#If IRRIG = 0.0 any WCI can be taken; if IRRIG=1.0 WCI should be taken equal to WCFC.
#INCON WAI    = 165.
PARAM WCI = 0.33
WAI   = WCI # ROOTD # 1000. # 1.E4
#ENDPeter30Jan2015

#####STOP: GT

### Parameters that should not be changed. The initial parameters TSUMi, LAIi, DMi,
### Wrti, Wshi, Wleafi, Wcormi, Wpsstmi, with i 1 to 3, and Wbunch1 are related because
### they belong to a certain development stage and must thus be chosen in coherence.
### For example, if TSUM1I would be taken 2423 Cd (and TSUM2I=1121 and TSUM3I=0)
### switch SWFSU2 should be 0.0, because DM1 is then not supplied anymore to DM2.
INCON TSUM1I          = 2262.
# Initial temperature sum of plant 1 (C d)
INCON TSUM2I          = 960.
# Initial temperature sum of plant 2 - sucker 1(C d)
INCON TSUM3I          = 0.
# Initial temperature sum of plant 3 - sucker 2(C d)

LAI1I           = Pleaf1I # SLA1 # Wsh1I # Fleaf_green1
# Initial leaf area index of plant 1(ha leaf ha-1 soil)

LAI2I           = Pleaf2I # SLA2 # Wsh2I # Fleaf_green2
#Initial leaf area index of plant 2(ha leaf ha-1 soil)

LAI3I           = Pleaf3I # SLA3 # Wsh3I # Fleaf_green3
#Initial leaf area index of plant 3(ha leaf ha-1 soil): is always 0.0 at start.

###We have now introduced the fraction green leaves (Fleaf_greeni) and the fraction
###dead leaves (1.0 - Fleaf_greeni) (so no new name Fleaf_deadi). Since dead
###leaves are hanging along the (pseudo)stem of the plant, they hardly contribute
###to the shadow in the plant, and the functional part is only the green portions
###of the leaves. Thus, also the LAIi's must be multiplied by the green fraction.
###This means, of course, that the initial growth rates will decrease as well.

###PAL:
###We would take the Fleaf_greeni's 1.0, as if there had just been a harvest.
###The DMmulch_I should than have a value. We agreed on running the program
###and seeing what its value is at e.g. FINTIM, and use that value. Why not done?
  ###PAL-end
PARAM Fleaf_green1    = 0.56
#PARAM Fleaf_green1    = 1.0
#The fraction of green leaves on plant 1 at initialisation (2262 Cd)
PARAM Fleaf_green2    = 0.75
#PARAM Fleaf_green2    = 1.0
#The fraction of green leaves on plant 2 at initialisation (960 Cd)
PARAM Fleaf_green3    = 0.00
#The fraction of green leaves on plant 3. Plant 3 emerges later in the growth process
#thus Fleaf_green3 is of no importance and taken zero


INCON DM1I            = 4902.
# Initial total dry matter of plant 1(kg ha-1)

INCON DM2I            = 1757.
# Initial total dry matter of plant 2(kg ha-1)

INCON DM3I            = 0.
# Initial total dry matter of plant 3(kg ha-1)

INCON HarvestDM_I     = 0.0
#Initial value of the integral keeping track of "harvested" dry matter of all
#above ground plant parts. These contain the dry matter of pseudostem, leaves
#(both green and dead) and bunch at the moment of harvest (3600 Cd) Plus the
#pruned dead leaves, all in (kg DM ha-1). The corm is not harvested.

############################################################################(ADDED : KN)
###PAL:
  #I ran the simulation and the mulch appears to start each time around
#10000 kg/ha = 1 kg/m^2, which is in the order of magnitude of the small
#experiment to assess soil coverage by mulch by Piet and . . . dated about
#July-August 2009
###PAL-end
INCON DMmulch_I       = 10000.0
#Initial amount of mulch (kg ha-1) (is 1 kg per m-2).

###PAL
#The initialization of DMmulch_I is NOT in accordance with the established
#plantation (yet).
###PAL-end
#Initial value of the integral keeping track of "harvested" dry matter of all
#above ground plant parts, except the bunch and corm. These thus contain the
#dry matter of pseudostem and leaves (both green and dead) at the moment of
#harvest (3600 Cd), in (kg DM ha-1). Besides, due to pruning, after each
#period "Days_between_prunings" (about 30 days), dead leaf dry matter
#is also added to the state DMmulch. The DMmulch is decomposed continuously
#with a (at the moment) constant relative decomposition rate.

INCON HarvestWpsstm_I = 0.0
#Initial value of the integral keeping track of "harvested" dry matter of
#pseudostem
INCON HarvestWLeaf_I  = 0.0
#Initial value of the integral keeping track of "harvested" dry matter of
#green leaves
INCON HarvestWLeafD_I = 0.0
#Initial value of the integral keeping track of "harvested" dry matter of
#dead leaves
INCON HarvestWbunch_I = 0.0
#Initial value of the integral keeping track of "harvested" dry matter of
#bunches

####################################################################################(ADDED : KN)
#HarvestDM_pruiI, added to keep track pruned leaves of plant i=1-3, to enable balance
#shoot dry matter of the individual plants. WleafDi is re-set to 0.0 after pruning and
#HarvestWLeafD captures the total pruned leaves and harvested leaves

###PAL:
  ###Why are these HarvestDM_pruiI introduced? Only to keep track of the amounts of
###pruned leaves? You do not do anything with it in the balances, so no need for that.
###But you let the contents shift: why is this useful? There are also no single
###decompositions of the harvested leaves (and we agreed on that, so that is fine
                                           ###(the only decomposition is of the DMmulch)).
###Further, we have already HarvestWLeafD_I (that indeed contains both pruned AND
                                             ###harvested leaves, which may be green as well).
###PAL-end
INCON HarvestDM_pru1I = 0.0
#Initial value of the integral keeping track of pruned leaf dry matter of plant 1
INCON HarvestDM_pru2I = 0.0
#Initial value of the integral keeping track of pruned leaf dry matter of plant 2
INCON HarvestDM_pru3I = 0.0
#Initial value of the integral keeping track of pruned leaf dry matter of plant 3
#Actually, we assume that leaves of plant 3 do not die

#################################################################################(ADDED : KN)

PARAM TSUMSUC         = 1302.
#Temperature sum referring to physiological time of sucker 1, when plant 3
#(sucker 2) starts to grow.
PARAM STGRLV          = 360.
#Temperature sum for the start of growth of photosynthetically active
#leaves of plant 3 (sucker 2). TSUM3=360 corresponds to TSUM2=1662 of sucker1.
PARAM TSUMendfulldepe = 1662.
#Temperature sum at the end of complete dependence of sucker 2 on sucker 1 (C d)
PARAM TSUMshfhv       = 2298.
#Temperature sum of plant 2 at harvest of plant 1 (C d). Hereafter, the
#function FSU2RED2_2 must be read by TSUM1, whereas before that physiological
#time it was read by TSUM2. Therefore, it is there called FSU2RED2_1.
PARAM TSUMfloinit     = 2423.
#Temperature sum at flower initiation (C d) of the mother plant
PARAMETER TSUMflower  = 2663.
#Temperature sum at flowering (C d)
PARAM TSUM3stop_exp   = 960.
PARAM LAIstop_exp     = 0.88
#LAI above which exponential leaf area growth stops. Used in subroutine GLA.

PARAMETER psh_wish    = 1.0
#Desired partitioning to the roots: psh_wish must stay 1.0.
Wrt3I                 = 0.0
#Initial dry matter of roots of plant 3 (kg ha-1)
Wsh3I                 = 0.0
#Initial dry matter of shoot of plant 3 (kg ha-1)


###PAL:  #####################################################################CHECK BELOW AGAIN

INCON Wcorm1H_I = 0.0

INCON Wcorm1_at_Harvest_I  = 0.0
#Initial value of the integral keeping track of the weight of corm of plant 1
#at harvest.
INCON Corm1_to_Wsh1I = 0.0
#Initial amount of dry matter re-distributed from corm of harvested plant to the
#shoot of the new plant 1 (kg ha-1)
INCON Corm1_After_HarvI      = 0.0
#Initial value of the weight of corm after harvest (kg ha-1)
INCON Corm1_Lost_After_HarvI = 0.0
#Initial value of the weight of corm lost after harvest (kg ha-1)
#   (PAL22Jun2012: this value should not be zero, because the plantation is established and so,
     #    there will be something left from the previous harvest.)

PARAM Plant_distance  = 3.0
#Distance between plants (m) taking a 3#3 spacing to calculate the # of leaves per
#plant as an indication. This # can easily be compared to field observations.

### The SET variables connect to events, especially to emergence and the start
### of growth of sucker2 (plant 3), and to accumulation of harvested products.
### The switch SWFSU2 must be taken 1.0, because it regulates that there is
### some dry matter, that is produced by the motherplant, going to sucker 1.
### This switch is normally set to 1 after the harvest, but since we start
### just in the middle of crop development it is still on (so it is 1.0) from
### the "previous" harvest.
### The same holds for SWFrt1, the switch that indicates that Function prt2
### should be read between 360 Cd and 2663 (Tsumflower(ing)). Before 360 Cd
### Function prt1 is read. Because of the initialization timing between
### 2262 and 2423 Cd SWFrt1 should be on (so equal to 1.0).
SET SWemerg3          = 0.0
SET SWstdm3           = 0.0
SET SWFSU2            = 1.0
SET SWFrt1            = 1.0

###Balances
INCON ZERO            = 0.0

##########################################################################################
  
  ### Parameters that can be changed.

PARAM TBASE           = 14.
#Base temperature for banana growth (degree C)

###PAL 1mRT2015
PARAM CHOICE_Trans_or_Photo = 1.0
###         If the parameter CHOICE_Trans_or_Photo = 0.0: "no transpiration -> no growth"
###         If the parameter CHOICE_Trans_or_Photo = 1.0: "no transpiration -> still some growth"
###         (in fact CHOICE_Trans_or_Photo may be any other value than 0.0 to obtain
             ###         the second option)
###STOP PAL 1mRT2015

###General remark: the RGRLi, Ki, LUEi, SLAi, RDRi and LWRi, where i = 1,2,3, have
###been given separate names, so that later, in e.g. a sensitivity analysis,
###these could be simply adapted. In principle, however, these parameters are
###similar for i=1, 2 and 3.
PARAM RGRL1_0         = 0.0077 ; RGRL2_0 = 0.0077 ; RGRL3_0 = 0.0077
#relative growth rate of leaves during the exponential growth phase (Cd)-1
PARAM K1              = 0.7
#Light extinction coefficient for plant 1 (ha soil ha-1 leaf)
PARAM K2              = 0.7
#Light extinction coefficient for plant 2 (ha soil ha-1 leaf)
PARAM K3              = 0.7
#Light extinction coefficient for plant 3 (ha soil ha-1 leaf)

LUE1_0       = 3.33 # 1.E-3
LUE2_0       = 3.33 # 1.E-3
LUE3_0       = 3.33 # 1.E-3
#Light use efficiency (kg MJ-1 PAR)

PARAM SLA1            = 0.0012 ; SLA2  = 0.0012 ; SLA3  = 0.0012
#Specific leaf area (ha leaf kg-1 leaf DM)
#PARAM LWR1            = 0.2    ; LWR2  = 0.2    ; LWR3  = 0.2
#Leaf weight ratio (kg leaf kg-1 DM. Usually, this value is around 0.5.
                    ##LWRi taken out, see previous reasoning.
                    
                    ## Relative death rate of Leaves  - (actual relative rates to be determined
                                                         ## more precisely later)
                    #RDR1 and RDR2 are to be established
                    #Plant 3 is heavily shaded by plants 1 and 2, therefore
                    #GDM3 and GWleaf3 are small. We therefore assume a small RDR for plant 3.
                    PARAM RDR1            = 0.0214
                    #Relative death rate of leaves of plant 1 (d-1)
                    PARAM RDR2            = 0.0094
                    #Relative death rate of leaves of plant 2 (d-1)
                    PARAM RDR3            = 0.00
                    #PARAM RDR3            = 0.10 (PAL22Jun2012 used for testing)
                    #Relative death rate of leaves of plant 3 (d-1)
                    PARAM Days_between_prunings = 30.
                    #Days_between_prunings affects the pruning of leaves and is a management parameter,
                    #here set at 30 days.
                    
                    PARAM RDcR            = 0.0175
                    #Relative decomposition rate of mulch (d-1)
                    #Estimated as average of relative decomposition rates of dry leaves and pseudostems from
                    #experiments during the rain season by Lekasi et al., 1999. Decomposition of crop residues
                    #in banana-based cropping systems of Uganda. Biological Agriculture and Horticulture 17:1-10.
                    ###PAL:23Feb2015: perhaps make RDcR dependent on moisture content Mulch (and temperature?).
                    
                    ###########################################################################################(ADDED K:N)
                    ###PAL:
                      PARAM RDRrt1_After_Harv = 0.051
                    #Relative death rate of roots of plant 1 after harvest (d-1)
                    #After 3 months (90 days), only 1% of the roots are left
                    #RDRroots1 = 1n(0.01)/(-90) = 0.051
                    
                    #Roots of harvested plant 1 will decay, and the roots of the next harvested plant are added to
                    #this pool of roots (roots of harvested plant 1 plus roots of new plant 1 harvested)
                    
                    ###PAL-end
                    
                    ###########################################################################################(ADDED K:N)
                    
                    PARAM FSU2max         = 0.05
                    #Fraction of DM that goes from sucker 1 to sucker 2 during the stage where
                    #sucker 2 does not have functional leaves (0<TSUM3<360) and fully obtains DM
                    #from sucker 1 expressed in (kg DM suck-2) / (kg DM suck-1)
                    
                    ### We only have to change prt_wish_0 and Factor to test the program for water
                    ### stress and the functioning of the partitioning of dry matter over root and
                    ### shoot.
                    prt_wish_0  = 0.2
                    ##Parameter prt_wish_0 is the assumed rt:sh ratio if there is no water stress.
                    #        prt_wish_high = 0.4
                    prt_wish_high = 0.2
                    #Parameter prt_wish_high represents at the moment the highest rt:sh ratio if
                    #water stress occurs. This is rudimentary introduced in the start of the DYNAMIC
                    #by an INSW, and will be adapted by the water stress factor, probably TRANRF.
                    #A prt_wish = 1.0 would mean that there is as much dry matter in the roots as in
                    #the shoot, and the fraction of the growth rate to be partitioned to the roots
                    #and shoot is (1/(1+1) = 0.5 and (1-0.5)=0.5). A value of 0.2 means that a
                    #fraction of 0.2 of dry matter is in the roots. The fraction to be partitioned
                    #from the actual growth rate to the roots is now 0.2/(1+0.2)=1/6 and 1-1/6=5/6
                    #to the shoot. These calculations are automated in the function routines prt1
                    #and prt2.
                    #Desired partitioning to the roots: psh_wish must stay 1.0, it is therefore
                    #placed in the "not-to-be-changed-section" and here it is, as a reminder
                    #placed as a comment.
                    #PARAMETER psh_wish    = 1.0
                    
                    prt_cal         = prt_wish_0 / (psh_wish + prt_wish_0)
                    Frtmax          = Factor # prt_cal
                    ###prt_wish_0 is introduced because of the initialization of prt_cal to mimick
                    #the water stress (or not) at the beginning of the simulation "_0", and to
                    #calculate the initialization of dry matter over root and shoot. (Technical
                                                                                      #remark: since prt_wish is dynamically used later, we must specify a new name. We
                                                                                      #choose therefore prt_wish_0.)
                    #prt_wish_high is the consequence of a maximum stress of water during the
                    #simulation. Both prt_wish_0 and prt_wish_high will be controlled by water
                    #stress later on. In fact only prt_wish will be made a function of water stress.
                    ### Frtmax is used to initialize the start of the dry matter distribution over
                    #the plant organs, adjusted to the different initial TSUM's, using prt1 and prt2.
#PARAM Factor          = 2.5
#PARAM Factor          = 1.8
#PARAM Factor          = 1.4
PARAM Factor          = 1.0

DummyRt               = prt1(TIME,STTIME,prt_wish_0,psh_wish,Frtmax,...
                                                      STGRLV,TSUM3I,TSUM3I)
#Function prt1 is called to calculate slope Afixed that is then placed in the
#COMMON/SLOPE / in the function, and can be use by function prt2, in which
#this COMMON is also included.

Wrt1I                 = FrtRED1I # DM1I
#Initial dry matter of roots of plant 1 (kg ha-1)
FrtRED1I              = prt2(prt_wish_0,psh_wish,TSUM3I,STGRLV,TSUM1I,TSUMflower)

Wrt2I                 = FrtRED2I # DM2I
#Initial dry matter of roots of plant 2 (kg ha-1)
FrtRED2I              = prt2(prt_wish_0,psh_wish,TSUM3I,STGRLV,TSUM2I,TSUMflower)

#Wrt3I                 = 0.0
#Initial dry matter of roots of plant 3 (kg ha-1)
#Note that this value must always be 0.0. It is therefore placed in the
#"not-to-be-changed-section" and here it is, as a reminder, placed as a
#comment. Wrt3I is always 0.0, because sucker 2 emerges somewhere in the growth
#process, so after the shift it is also gone. This does not hold for the roots
#and shoots of sucker 1 and the motherplant, which always have some initial
#positive value, calculated from the rt:sh ratio.
#The roots of sucker 2 that "go" after the shift in fact come in an older
#class, in which there will be a different (higher/lower?!) relative death rate
#of roots. The same holds for the roots from sucker 1 to motherplant. The roots
#from the motherplant at harvest will NOT be harvested (is just not possible or
#at least is never done inpractice), but these have the highest relative death
#rate. This highest relative death rate is due to the fact that the roots are not
#attached anymore to the above ground mother-plant parts.

#################################################################################(ADDED : KN)
###PAL:
#The WrtiDI should get reasonable values, similarly as the starting condition
#of the DMmulch

#################################################################################(ADDED : KN)
#Difficult to estimate dead roots, we assume that there are no dead roots. Most studies
#report living roots, because dead roots rot away fast.
###PAL-end
INCON Wrt1DI = 0.
#Initial weight of dead roots of plant 1 (kg ha-1)
INCON Wrt2DI = 0.
#Initial weight of dead roots of plant 2 (kg ha-1)
INCON Wrt3DI = 0.
#Initial weight of dead roots of plant 3 (kg ha-1)

#PAL:
INCON Wrt1_After_HarvI = 0.
#Initial weight of dead roots of plant 1 (kg ha-1) just after harvest.
#Estimated from the model runs, because we start with an established plantation

PARAM  RDRrt2 = 0.0218
#Relative death rate of roots of plant 2 (d-1)
PARAM  RDRrt3 = 0.0
#Relative death rate of roots of plant 3 (d-1)

Wsh1I                 = (1.0 - FrtRED1I) # DM1I
#Initial dry matter of shoot of plant 1 (kg ha-1)
Wsh2I                 = (1.0 - FrtRED2I) # DM2I
#Initial dry matter of shoot of plant 2 (kg ha-1)
#Wsh3I                 =  0.0
#Initial dry matter of shoot of plant 3 (kg ha-1)
#Note that this value must always be 0.0. It is therefore placed in the
#"not-to-be-changed-section" and here it is, as a reminder placed as a
#comment. Further see note at Wrt3I above.

#######################################################################################(ADDED : KN)
PARAM RDRCorm1_After_Harv = 0.03838
#Relative decomposition rate of the corm after harvest(d-1)
#PARAM pcorm1_to_Wsh1      = 0.5
PARAM RALR_Corm1_To_DM1  = 0.03838
#this value for the moment taken equal to RDRCorm1_After_Harv
PARAM pcorm1_To_ReAllocation      = 0.5
#Approximate proportion of corm dry matter of harvested plant that has been stored
#at the harvest moment and that wil be re-distributed to the new plant 1.
#The name "pcorm1_To_ReAllocation" is new (PAL22Jun2012)

#After about 4 months (120 days), 1% of the original corm dry matter will be left
#RDRCorm1_After_Harv = 1n(0.01)/(-120) = 0.038 d-1
#Assumption: No death of corm during growth
#There is (22Jun2012) corm-decay and corm redistribution. To this purpose the corm1 at
#harvest is stored in Corm1_After_Harv and Corm1_Lost_After_Harv. Then Corm1_After_Harv is
#re-allocated to DM1 that has just become the new plant, and Corm1_Lost_After_Harv is decomposed
#as organic matter. The (relative)decay rate to control the decrease of the dead corm (with
#1% left after 4 months) is 0.03838.
######PALOLD:
####There is corm-decay and corm redistribution. If we take the (relative)decay rate to
####control the decrease of the corm as a whole (with 1% left after 4 months), the fraction
####"Corm1Dm_transferprop" goes to the plant1 Wsh1, and the remainder "(1 - Corm1Dm_transferprop)"
####goes to the organic matter of the soil, that is not further specified.
######PAL-end

###PAL   dead leaves added here
Wcorm1I               = Pcorm1I  # Wsh1I
#Initial dry matter of corms of plant 1 (kg ha-1)
Wpsstm1I              = Ppsstm1I # Wsh1I
#Initial dry matter of pseudostem of plant 1 (kg ha-1)
Wleaf1I               = Pleaf1I  # Wsh1I #  Fleaf_green1
#Initial dry matter of green leaves of plant 1 (kg ha-1)
WleafD1I              = Pleaf1I  # Wsh1I # (1.0 - Fleaf_green1)
#Initial dry matter of dead leaves of plant 1 (kg ha-1)
Wbunch1I              = Pbunch1I # Wsh1I
#Initial dry matter of the bunch of plant 1 (kg ha-1)
CALL INTERPOL('PCOTB','PSTTB','PLVTB','PBUTB',...
               PCOTB ,PSTTB  ,PLVTB  ,PBUTB  ,...
               K,TSUM1I, Pcorm1I,Ppsstm1I,Pleaf1I,Pbunch1I)

Wcorm2I               = Pcorm2I  # Wsh2I
#Initial dry matter of corms of plant 2 (kg ha-1)
Wpsstm2I              = Ppsstm2I # Wsh2I
#Initial dry matter of pseudostem of plant 2 (kg ha-1)
Wleaf2I               = Pleaf2I  # Wsh2I #  Fleaf_green2
#Initial dry matter of green leaves of plant 2 (kg ha-1)
WleafD2I              = Pleaf2I  # Wsh2I # (1.0 - Fleaf_green2)
#Initial dry matter of dead leaves of plant 2 (kg ha-1)
CALL INTERPOL('PCOTB','PSTTB','PLVTB','PBUTB',...
               PCOTB ,PSTTB  ,PLVTB  ,PBUTB  ,...
               K,TSUM2I, Pcorm2I,Ppsstm2I,Pleaf2I,DummyPbunch2I)

Wcorm3I               = Pcorm3I  # Wsh3I
#Initial dry matter of corms of plant 3 (kg ha-1)
Wpsstm3I              = Ppsstm3I # Wsh3I
#Initial dry matter of pseudostem of plant 3 (kg ha-1)
Wleaf3I               = Pleaf3I  # Wsh3I #  Fleaf_green3
#Initial dry matter of green leaves of plant 3 (kg ha-1)
WleafD3I              = Pleaf3I  # Wsh3I # (1.0 - Fleaf_green3)
#Initial dry matter of dead leaves of plant 3 (kg ha-1)
CALL INTERPOL('PCOTB','PSTTB','PLVTB','PBUTB',...
               PCOTB ,PSTTB  ,PLVTB  ,PBUTB  ,...
               K,TSUM3I, Pcorm3I,Ppsstm3I,Pleaf3I,DummyPbunch3I)

TotWleafD_I           = WleafD1I + WleafD2I + WleafD3I
#Initial value of the integral collecting the dead leaves over time. This
#state is periodically harvested and becomes mulch. Also prunings come into this state.

ARRAY_SIZE K=20

#PARAMETER PCOTB(1:19) = 0.,0.14, 440.,0.14, 1038.,0.11, 1434.,0.09, 2168.,0.04, 2423.,0.03,...
#                        2663.,0.03, 2800.,0.00, 3132.,0.00, 3600.; PCOTB(20:K)=0.00

#PARAMETER PSTTB(1:19) = 0.,0.33, 440.,0.33, 1038.,0.31, 1434.,0.36, 2168.,0.50, 2423.,0.51,...
#                        2663.,0.47, 2800.,0.00, 3132.,0.00, 3600.; PSTTB(20:K)=0.00

#PARAMETER PLVTB(1:19) = 0.,0.53, 440.,0.53, 1038.,0.58, 1434.,0.55, 2168.,0.46, 2423.,0.46,...
#                        2663.,0.43, 2800.,0.00, 3132.,0.00, 3600.; PLVTB(20:K)=0.00

#PARAMETER PBUTB(1:19) = 0.,0.00, 440.,0.00, 1038.,0.00, 1434.,0.00, 2168.,0.00, 2423.,0.00,...
#                        2663.,0.07, 2800.,1.00, 3132.,1.00, 3600.;PBUTB(20:K)=1.00

PARAMETER PCOTB(1:19) = 0.,0.47, 440.,0.33, 1038.,0.10, 1434.,0.09, 2168.,0.06, 2423.,0.05,...
                        2663.,0.03, 2800.,0.00, 3132.,0.00, 3600.; PCOTB(20:K)=0.00

PARAMETER PSTTB(1:19) = 0.,0.53, 440.,0.37, 1038.,0.38, 1434.,0.47, 2168.,0.50, 2423.,0.51,...
                        2663.,0.47, 2800.,0.00, 3132.,0.00, 3600.; PSTTB(20:K)=0.00

PARAMETER PLVTB(1:19) = 0.,0.00, 440.,0.30, 1038.,0.52, 1434.,0.44, 2168.,0.44, 2423.,0.44,...
                        2663.,0.43, 2800.,0.00, 3132.,0.00, 3600.; PLVTB(20:K)=0.00

PARAMETER PBUTB(1:19) = 0.,0.00, 440.,0.00, 1038.,0.00, 1434.,0.00, 2168.,0.00, 2423.,0.00,...
                        2663.,0.07, 2800.,1.00, 3132.,1.00, 3600.;PBUTB(20:K)=1.00

#The partitioning functions are taken equal for all plants (Mother, sucker 1
#and sucker 2). If there are 10 coordinate pairs, there are 20 numerical values,
#and the ARRAY must be declared 20 long (K=20).

### Run control. STTIME=1 corresponds to January 1. FINTIM=1000 means that the
### calculations continue throughout the first, second and part of the third
### year (Now: 2004 (leap year), 2005 and 269 days of 2006: see weather
### initialization 15 lines further).

####ADDED: GT

#PARAMETER ALBS    = 0.15; ALBC    = 0.25 ; ECCOFC  = 05. ; ECCOFM = 04. ; PINTC = 0.25; MULSPR = 0.00038 ; ...
PARAMETER ALBS    = 0.15; ALBC    = 0.25 ; ECCOFC  = 0.5 ; ECCOFM = 0.4 ; PINTC = 0.25E4; MULSPR = 0.00038 ; ...
          MULSWS  = 3.5 ; ROOTD   = 0.5 ; MSTMIN = 0.3E4; ...
          WCAD    = 0.01; WCWP    = 0.11 ; WCFC    = 0.33; WCWET  = 0.37; WCST   = 0.42; ...
          TRANCO  = 5.E4  ; DRATE   = 50.E4  ; IRRIGF = 0.
###(test)          TRANCO  = 0.0001E4  ; DRATE   = 50.E4  ; IRRIGF = 0.


###NOTE: If IRRIGF = 1.0, the WCI must be taken as WCFC, to correctly start the calculations, because the
###                       starting point should also be the irrigated situation.
###      If IRRIGF = 0.0, the WCI can be taken as any value, including WCFC, to correctly start the calculations.

####STOP: GT

TIMER STTIME = 1.; FINTIM = 1200.; DELT = 0.25; PRDEL = 1.0
#TIMER STTIME = 1.; FINTIM = 365.; DELT = 0.25; PRDEL = 1.0
#TIMER STTIME = 1.; FINTIM = 365.; DELT = 1.0; PRDEL = 1.0
#TIMER STTIME = 1.; FINTIM = 272.; DELT = 0.25; PRDEL = 1.0
#TIMER STTIME = 1.; FINTIM = 365.; DELT = 0.0001; PRDEL = 1.0
#TIMER STTIME = 1.; FINTIM = 365.; DELT = 0.00005; PRDEL = 1.0
TRANSLATION_GENERAL DRIVER='RKDRIV' ; DELMAX = 0.25; TRACE = 4
#TRANSLATION_GENERAL DRIVER='RKDRIV' ; DELMAX = 1.0; TRACE = 4

#TRANSLATION_GENERAL DRIVER='EUDRIV'                            ; TRACE = 4

#TRANSLATION_GENERAL DRIVER='EUDRIV'


### Output
#In 19Aug2009Version below
#PRINT  TSUM1,LAI1,DM1,BalDmHlp1,Wcorm1,Wcorm1I, Wpsstm1,Wpsstm1I,PARINT1,Wleaf1,Leafnum_plant1,...
#       Wleaf1I,Wbunch1,NDM1, NDM2, NDM3, Wbunch1I,Wsh1,GWsh1,WleafD1I,WshPlnt1Tot,Wrt1,...
#       WrtD1_Tot,GWrt1,DWrt1, NWrt1, DWleaf1,Wcorm1H,                              ...
#       HarvestDM1_Pru,TSUM2,LAI2,DM2,BalDmHlp2,Wcorm2,Wcorm2I, Wpsstm2,Wpsstm2I, Wleaf2,Wleaf2I,...
#       Wsh2,WleafD2I,WshPlnt2Tot,Wrt2,WrtD2_Tot,DWleaf2,GWsh2,WleafD2,BalRtShHlp2,BalShHlp2,...
#       HarvestDM2_Pru,TSUM3,LAI3,DM3,Wcorm3,Wcorm3I, Wpsstm3,Wpsstm3I, Wleaf3,Wleaf3I,...
#       Wsh3,WleafD3I,WshPlnt3Tot,Wrt3,WrtD3_Tot,DWleaf3,GWsh3,WleafD3,BalRtShHlp3,BalShHlp3,...
#       BalDmHlp4, TotWrtD, BalDmHlp5,            BalRtShHlp2,BalRtShHlp3,               ...
#       HarvestDM, HarvestWLeaf, HarvestWLeafD, HarvestWpsstm, WleafD1,HarvestWbunch, DMmulch,...
#       TotWleafD,Wrt1_After_Harv,RtShratio1, RtShratio2, RtShratio3, Corm1_After_Harv,...
#       Corm1_Lost_After_Harv,               CheckOne, SWstdm3, SWemerg3,...
#      CUMEVP, TOTEVP, PEVAPS, PEVAPM, PEVAPT, PTRANT, TRAIN, ...
#      PTRAN1, PTRAN2, PTRAN3, RAI, RAIN,RINTC, RINTM, MULBAL, RINTC1, RINTC2, RINTC3,...
#      FR,WCCR,WCWET,WCST,EVAPS,TRAN1,TRAN2,TRAN3, TRANT,...
#      DRAIN,RUNOFF,WA,WC,RWA,SWABAL, NMULST, CUEVTA, MAI,...
#      MSTMAX, MSTCUR, EVAPM, INFILT, IRRIG, TRARF1, TRARF2, TRARF3, MSTDEF

####STOP: GT

PRINT  FrtRED3, FrtRED2, FrtRED1,...
       TSUM1,LAI1,DM1,BalDmHlp1,Wcorm1,Wcorm1I, Wpsstm1,Wpsstm1I,PARINT1,Wleaf1,Leafnum_plant1,...
       Wleaf1I,Wbunch1,NDM1, NDM2, NDM3, Wbunch1I,Wsh1,GWsh1,WleafD1I,WshPlnt1Tot,Wrt1,...
       WrtD1_Tot,GWrt1,DWrt1, NWrt1, DWleaf1,Wcorm1H, ...
       HarvestDM1_Pru,TSUM2,LAI2,DM2,BalDmHlp2,Wcorm2,Wcorm2I, Wpsstm2,Wpsstm2I, Wleaf2,Wleaf2I,...
       Wsh2,WleafD2I,WshPlnt2Tot,Wrt2,WrtD2_Tot,DWleaf2,GWsh2,WleafD2,BalRtShHlp2,BalShHlp2,...
       HarvestDM2_Pru,TSUM3,LAI3,DM3,Wcorm3,Wcorm3I, Wpsstm3,Wpsstm3I, Wleaf3,Wleaf3I,...
       Wsh3,WleafD3I,WshPlnt3Tot,Wrt3,WrtD3_Tot,DWleaf3,GWsh3,WleafD3,BalRtShHlp3,BalShHlp3,...
       BalDmHlp4, TotWrtD, BalDmHlp5, BalRtShHlp2,BalRtShHlp3, ...
       HarvestDM, HarvestWLeaf, HarvestWLeafD, HarvestWpsstm, WleafD1,HarvestWbunch, DMmulch,...
       TotWleafD,Wrt1_After_Harv,RtShratio1, RtShratio2, RtShratio3, Corm1_After_Harv,...
       Corm1_Lost_After_Harv,  CheckOne, SWstdm3, SWemerg3, ...
        Psum1,Pbunch1,Psum2,DummyPbunch2,Psum3,DummyPbunch3, DTEFF, DAYTEFF,...
BalRtShHlp1 , BalDmHlp4, BalDmHlp3
#Corm1_to_Wsh1, BalRtShHlp1,BalShHlp1,  BalRtShHlp4,

#In 20Aug2009Version below
#PRINT  TSUM1,LAI1,DM1,BalDmHlp1,Wcorm1,Wcorm1I, Wpsstm1,Wpsstm1I,PARINT1,Wleaf1,Leafnum_plant1,Wleaf1I,Wbunch1,...
#       Wbunch1I,Wsh1,GWsh1,WleafD1I,WshPlnt1Tot,Wrt1,WrtD1_Tot,GWrt1,DWrt1, NWrt1, DWleaf1,...
#       Wcorm1H,  HarvestDM1_Pru,...
#       TSUM2,LAI2,DM2,BalDmHlp2,Wcorm2,Wcorm2I, Wpsstm2,Wpsstm2I, Wleaf2,Wleaf2I,...
#       Wsh2,WleafD2I,WshPlnt2Tot,Wrt2,WrtD2_Tot,DWleaf2,GWsh2,WleafD2,HarvestDM2_Pru,...
#       TSUM3,LAI3,DM3,Wcorm3,Wcorm3I, Wpsstm3,Wpsstm3I, Wleaf3,Wleaf3I, ...
#       Wsh3,WleafD3I,WshPlnt3Tot,Wrt3,WrtD3_Tot,DWleaf3,GWsh3,WleafD3, ...
#       HarvestDM, HarvestWLeaf, HarvestWLeafD, HarvestWpsstm, WleafD1,HarvestWbunch, DMmulch,  TotWleafD, ...
#       Wrt1_After_Harv,RtShratio1, RtShratio2, RtShratio3, RtShratioAll, Corm1_After_Harv, Corm1_Lost_After_Harv

### Below outputs were used for the water balances.
#PRINT  MSTMAX, MSTCUR, MSTMIN, DMmulch
#PRINT  WCFC, WC, WA, TRARF1, TRARF2, TRARF3, PTRAN3, PTRAN2, PTRAN1, FR, PEVAPS, EVAPM, RAI, RAIN,PEVAPM, ...
#       EVAPS,EVAPT,TRAN1,TRAN2,TRAN3, TRANT,CUMRAIN,WCCR,RGRL1,RGRL2,RGRL3, LUE1,LUE2,LUE3, DTEFF, DAYTEFF
#### The balances are gathered in a separate PRINT statement.
PRINT  WATBALPETER_S,WATBALPETER_M_1,WATBALPETER_M_2,WATBALPETER_M_3,WATBALPETER_System,WATBALPETER_Syst_Rel,...
       WATBALPETER_System_A, WATBALPETER_Syst_Rel_A
#       CUMTRANT, CUMEVAPS, CUMRUNOFF, CUMDRAIN, CUMINFILT, CRINTC, CUMRAIN, CUMIRRIG, CRINTM, CRINTMdif, CEVAPM, CUMEVP, CUEVTA, ...
#       WATBALPETER_M_1A
####END outputs for the water balances.

### Below outputs were used for the water balances.
#PRINT BalDmHlp1, BalDmHlp2, BalDmHlp4, BalDmHlp5, BalRtShHlp2, BalShHlp2, BalRtShHlp3,BalShHlp3, BalRtShHlp2, BalRtShHlp3,...
#      DMTotdm_plant_tot,DMTotdm_plant_1, DMWshdm_plant_1, Wsh1, DTEFF, DAYTEFF
###END outputs for the water balances.

##New for the tests 26Jan2015
#PRINT  RUNOFF,RINTC, RINTM, RINTC1, RINTC2, RINTC3,INFILT, RAI,RAIN,MSTMAX,MSTCUR,NMULST,RINTM,EVAPM,INFILT, ...
#       WAAD,WAFC,PTRAN3, PTRAN2, PTRAN1, FR, PEVAPS, PEVAPM, ...
#       EVAPS,EVAPT,TRAN1,TRAN2,TRAN3, TRANT,CUMRAIN,WCCR,...
#       RUNOFF,DRAIN,RWA,WA,WC,MSTMAX, MSTCUR, EVAPM,PEVAPM,WATBALPETER_S,WATBALPETER_M,WATBALPETER_System,WATBALPETER_Syst_Rel, ...
#       Evapototal, CUMTRANT, CUMEVAPS, CEVAPM, RAI, RAIN,MaxTran, CRINTC, ...
#       TRARF1,TRARF2,TRARF3,RLWN
#END New for the tests 26Jan2015

#PRINT  HI1,HIoverall, DMTotdm, DMTotRtSh, RtShratioAll, SWemerg3, SWstdm3, SWFSU2,SWFrt1,...
#       FSU2RED2_1, FSU2RED2_2, FrtRED3, FrtRED2, FrtRED1, FrtGR, GDM1HLP, GDM1, GDM2HLP,...
#       GDM2, G2from2_1, G2from2_2, G2from1, GDM3, G3from2_1, G3from2_2, G3Photos,...
#       GWrt1, GWrt2, GWrt3, DTEFF, GLAI1, GLAI2, GLAI3,...
#       PARINT1, PARINT2, PARINT3, PARIN1, PARIN2, PARIN3, PAROUT1, PAROUT2, PAROUT3


DYNAMIC
###PAL16Mrt2015: Added to see which values are reached for the threshold for the DM where a plant may flower.
        DMTotdm_plant_tot = DMTotdm / (10000./(Plant_distance)##2)
        DMTotdm_plant_1   = DM1 / (10000./(Plant_distance)##2)
        DMWshdm_plant_1   = Wsh1 / (10000./(Plant_distance)##2)
###Values were above 4 kg per plant, so far above the 1.5 mentioned by Godfrey.
####ADDED: GT

# Water stress-limited LAI relative growth rates

#       RGRL1  = LIMIT(0., 1., 0.0077 # TRARF1)
#       RGRL2  = LIMIT(0., 1., 0.0077 # TRARF2)
#       RGRL3  = LIMIT(0., 1., 0.0077 # TRARF3)

#New for the tests 26Jan2015
### If there is enough water in the soil the ratio TRANi/PTRANi should stay 1.0, down to
### very low values of the evaporative demand. Even if TRANi and PTRANi are both extremly small,
### even 0.0, their ratio should be 1.0 (still only if enough water is present in the soil).
### So, if there is hardly any evaporative demand, e.g. the Penman equation gives a zero result,
### growth is not reduced because of below ground water effects. I adapted that a bit via the
### multiplication with FR: see FunctionRoutine TRARFfu.

      RGRL1   = RGRL1_0 # TRARF1

      RGRL2   = RGRL2_0 # TRARF2

      RGRL3   = RGRL3_0 # TRARF3
#END New for the tests 26Jan2015

#       RGRL1  = LIMIT(0., 1., 0.0077)
#       RGRL2  = LIMIT(0., 1., 0.0077)
#       RGRL3  = LIMIT(0., 1., 0.0077)


# Water stress-limited light use efficiency (kg MJ-1 PAR)

#      LUE1    = LIMIT(0., 1., TRARF1 # 3.33 # 1.E-3)
#      LUE2    = LIMIT(0., 1., TRARF2 # 3.33 # 1.E-3)
#      LUE3    = LIMIT(0., 1., TRARF3 # 3.33 # 1.E-3)


      LUE1    = LUE1_0 # TRARF1
      LUE2    = LUE2_0 # TRARF2
      LUE3    = LUE3_0 # TRARF3
#END New for the tests 26Jan2015

#      LUE1    = LIMIT(0., 1., 3.33 # 1.E-3)
#      LUE2    = LIMIT(0., 1., 3.33 # 1.E-3)
#      LUE3    = LIMIT(0., 1., 3.33 # 1.E-3)

#Peter19Sept

      MaxTran = (RDD#1.E-6)/2.45

#########Begin PETER 29Aug2014

      Evapototal = CUMTRANT + (CUMEVAPS + CEVAPM)

#########End PETER 29Aug2014

#########Begin PETER 15Aug2014

#TIMER STTIME = 1.; FINTIM = 365.; DELT = 1.; PRDEL = 1.

#the accumulation of RUNOFF, TRANT (via 3 leaf-layers), EVAPSoil, INFILTration, and DRAIN for the water balance.

      CUMTRANT  = INTGRL(ZERO , TRANT  )
      CUMEVAPS  = INTGRL(ZERO , EVAPS  )
      CUMRUNOFF = INTGRL(ZERO , RUNOFF )
      CUMDRAIN  = INTGRL(ZERO , DRAIN  )
      CUMINFILT = INTGRL(ZERO , INFILT )

### Water balance_Soil: the deviation of the balance relative to the amount of water present (not a percentage,
### but a fraction is calculated):

      WATBALPETER_S = ( ( WA + (CUMTRANT + CUMEVAPS + CUMRUNOFF + CUMDRAIN) - CUMINFILT - CUMIRRIG) - WAI ) / WA

#the accumulation of RAIN, IRRIG, RUNOFF and DRAIN for the Mulch water balance, including the green leaves.
# RAI = 0.8 # RAIN # 1.E4
 RAI = RAIN # 1.E4
      CUMRAIN   = INTGRL(ZERO , RAI   )
      CUMIRRIG  = INTGRL(ZERO , IRRIG  )

#Mulch water balance:
#Water balance_Mulch: the deviation of the balance relative to the amount of water present:

      WATBALPETER_M_1 = ( ( MSTCUR + (CEVAPM + CUMINFILT) - (CUMRAIN-CRINTC) ) - MSTCUI ) / MSTCUR
      WATBALPETER_M_1A = ( MSTCUR + CEVAPM + CUMINFILT - CUMRAIN+CRINTC - MSTCUI ) / MSTCUR
      WATBALPETER_M_2 = ( ( MSTCUR + (CEVAPM + CUMINFILT) - (CRINTM) ) - MSTCUI ) / MSTCUR
      WATBALPETER_M_3 =  ( MSTCUR + (CEVAPM + CUMINFILT) - (CRINTM) ) - MSTCUI

CRINTMdif = CUMRAIN-CRINTC

#The overall system water balance:

      WATBALPETER_System = ( (WA + MSTCUR) + (CUMTRANT + CUMEVAPS + CUMRUNOFF + CUMDRAIN + CEVAPM) - ...
                                                                    ((CUMRAIN-CRINTC) + CUMIRRIG) ) - (WAI + MSTCUI)
      WATBALPETER_Syst_Rel  = WATBALPETER_System/(WA+MSTCUR)

      WATBALPETER_System_A = ( (WA + MSTCUR) + (CUMTRANT + CUMEVAPS + CUMRUNOFF + CUMDRAIN + CEVAPM) - ...
                                                                    ((CRINTM) + CUMIRRIG) ) - (WAI + MSTCUI)
      WATBALPETER_Syst_Rel_A  = WATBALPETER_System_A/(WA+MSTCUR)


#########End PETER 15Aug2014

#Rate of change in soil water storage, RWA (mm/d) is computed as the difference
#between inflow via INFILT, and outfow through runoff RUNOFF (mm/d),
#transpiration TRANT (mm/d), evaporation form soil EVAPS (mm/d) and drainage DRAIN (mm/d)

      RWA = IRRIG + INFILT - (RUNOFF + TRANT + EVAPS + DRAIN)

#Computing total rainfall interception rate in the canopy RINTC as the sum of
#interception from plant 1, 2 and 3 i.e. RINTC1, RINTC2 and RINTC3, respectively

      RINTC1 = MIN(RAI, PINTC # LAI1)
      RINTC2 = MIN( MAX(0., RAI - RINTC1), PINTC # LAI2 )
      RINTC3 = MIN( MAX(0., RAI - RINTC1 - RINTC2), PINTC # LAI3)

      RINTC  = RINTC1 + RINTC2 + RINTC3

# Keeping track of rainfall interception in crop canopy as CRINTC

      CRINTC = INTGRL(ZERO,RINTC)

#Computing current mulch water storage as the integral of initial mulch water content
#MSTCUI and rate of rainfall inflow to the mulch (NMULST, mm/d), which in turn is the
#difference between rainfall interception in mulch (RINTM, mm/d) and evaporation from mulch (EVAPM, mm/d)

        MSTCUR = INTGRL(MSTCUI, NMULST)

#####Beginning PAL26Aug2014

        NMULST = RINTM - EVAPM - INFILT

#####End PAL26Aug2014

####Start GT 20 Aug 2014

#Computing mulch interception rate (RINTM, mm/d) as the difference between rainfall (RAIN, mm/d)
#and interception in the canopy (RINTC, mm/d) = rate of inflow into the soil

       RINTM  = RAI - RINTC

#Computing outflow from mulch or infiltration into the soil (INFILT, mm/d) from storage in mulch, which in turn is the difference
#between current mulch storage (MSTCUR, mm) and maximum storage in mulch (MSTCUR, mm), and rate of change in mulch storage

#####Beginning PAL5Sept2014

       INFILT = MAX( 0.0 , (MSTCUR-MSTMAX)/DELT + (RINTM - EVAPM) )

#####Beginning PAL5Sept2014

####Stop GT

#Computing mulch area index (MAI ha/ha) as a product of the adjusted specific mulch area (MULSPR, ha/kg DM) and
#mulch cover (DMmulch, kg DM/ha)

       MAI    = MULSPR # DMmulch

#Computing the maximum water storage by mulch from its specific water storage
#MULSWS (kg H2O/kg DM) and mulch soil surface cover (kg DM/ha soil surface area)

       MSTMAX = MULSWS # DMmulch

#Computing actual evaporation from mulch (EVAPM, mm/d)

#####Beginning PAL5Sept2014

       EVAPM = MAX( 0.0, PEVAPM # (MSTCUR - MSTMIN) / (MSTMAX - MSTMIN) )

#####End PAL5Sept2014

#Keeping track of interception in mulch as CRINTM and evaporation from mulch as CEVAPM

       CRINTM = INTGRL(ZERO,RINTM)
       CEVAPM = INTGRL(ZERO,EVAPM)

#Integral keeping track of cumulative reference evapotranspiration CUMEVP (mm)

      CUMEVP = INTGRL(ZERO,TOTEVP)

#Rate of change in cumulative reference evapotranspiration TOTEVP (mm/d) as a sum of
#reference total evaporation PEVAPT (mm/d) and reference transpiration PTRANT (mm/d)

      TOTEVP = PEVAPT + PTRANT

#reference total evaporation as a sum of reference evaporation from soil PEVAPS (mm/d)
#and reference evaporation from mulch PEVAPM (mm/d)

      PEVAPT = PEVAPS + PEVAPM

#Reference total transpiration as a sum of reference transpiration from plant 1, 2 and 3
#or PTRAN1, PTRAN2 and PTRAN3 (mm/d), respectively

      PTRANT = PTRAN1 + PTRAN2 + PTRAN3

#Integral keeping track of the cumulative actual total evapotranspiration

      CUEVTA = INTGRL(ZERO,TOEVTA)

#Computing rate of change in cumulative actual evapotranspiration TOEVTA (mm/d) as a sum of
#actual total evaporation EVAPT (mm/d) and actual transpiration TRANT (mm/d)

      TOEVTA = EVAPT  + TRANT

#Computing actual total evaporation as a sum of actual eveporation from soil EVAPS (mm/d)
#and actual evaporation from mulch EVAPM (mm/d)

      EVAPT  = EVAPS  + EVAPM

#Actual total transpiration as a sum of actual transpiration from plant 1, 2 and 3
#or TRAN1, TRAN2 and TRAN3 (mm/d), respectively

      TRANT  = TRAN1  + TRAN2  + TRAN3

# ---------------------------------------------------------------------#
#Auxiliary variables required to compute the actual evaporation
#and actual transpiration rates as well as runoff and drainage
# ---------------------------------------------------------------------#

#Computing the amount of water stored in the rooting zone WA (mm) from initial amount WAI (mm)

      WA     = INTGRL( WAI,RWA)

#Computing the water content of the root zone WC (mm/mm) from amount stored (mm)
#and root depth ROOTD (m)

      WC     = 1.E-4 # 0.001 # WA/ROOTD

#Computing the amount of water in the root zone at field capacity WAFC (mm)
#from the water content at field capacity WCFC, (mm/mm) and rooting depth, ROOTD (m)

      WAFC   = 1.E4 # 1000. # WCFC # ROOTD

#Computing the amount of water in the root zone at saturation WAST (mm)
#from the water content at saturation WCST, (mm/mm) and rooting depth, ROOTD (m)

      WAST   = 1.E4 # 1000. # WCST # ROOTD

#Computing the amount of water in the root zone at air-dry state WAAD (mm)
#from the air-dry water content, WCAD (mm/mm) and root depth, ROOTD (m)

      WAAD   = 1.E4 # 1000. # WCAD # ROOTD

CALL PENMAN(DAVTMP,VP,DTRPEN,WN,ECCOFC,LAI1,LAI2,LAI3,ECCOFM,MAI, ...
            ALBS,ALBC,RINTC1,RINTC2,RINTC3, ...
            PEVAPS,PEVAPM,PTRAN1,PTRAN2,PTRAN3,RLWN)

CALL EVAPTR(PEVAPS,PTRAN1,PTRAN2,PTRAN3,...
                       ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST,...
                       TRANCO,DELT,...
                       WCCR,EVAPS,TRAN1,TRAN2,TRAN3,FR)

CALL DRUNIR(INFILT,EVAPS,TRANT,IRRIGF,...
            DRATE,DELT,WA,ROOTD,WCFC,WCST,...
            DRAIN,RUNOFF,IRRIG)

#Computing the transpiration reduction factor for plant 1, 2 and 3
#TRARF1, TRARF2 and TRARF3, respectively

#      TRARF1 = TRAN1 / NOTNUL(PTRAN1)
#      TRARF2 = TRAN2 / NOTNUL(PTRAN2)
#      TRARF3 = TRAN3 / NOTNUL(PTRAN3)
####STOP: GT

###PAL24Feb2015:
      TRARF1 = TRARFfu(CHOICE_Trans_or_Photo, FR, TRAN1, PTRAN1)
      TRARF2 = TRARFfu(CHOICE_Trans_or_Photo, FR, TRAN2, PTRAN2)
      TRARF3 = TRARFfu(CHOICE_Trans_or_Photo, FR, TRAN3, PTRAN3)
####STOP: PAL

###########################################################################################

### Environmental data, the driving variables of the system. Note: RDD in the
### weather file are in kJ m-2 d-1, but come to the FST program in J m-2 d-1.
WEATHER WTRDIR='C:\SYS\WEATHER\'; CNTR='UGA'; ISTN=1; IYEAR=2006
#     Reading weather data from weather file:
#     RDD    Daily global radiation        kJ m-2 d-1
#     TMMN   Daily minimum temperature     degree C
#     TMMX   Daily maximum temperature     degree C
#     VP     Vapour pressure               kPa
#     WN     Wind speed                    m s-1
#     RAIN   Precipitation                 mm

DTR             = (RDD/1.E+6) # 1.E+4
#DTR             = 0.0
#test whether zero light gives no growthDTR=0.001

#To convert daily total radiation as generated from the weather system in
#J m-2 d-1 to MJ ha-1 d-1

####ADDED: GT

#To be input in the Penman equation

         DTRPEN = RDD/1.E+6
#         DTRPEN = 0.0

####STOP: GT

DAVTMP          = 0.5 # (TMMN + TMMX) - DELTMP
PARAM DELTMP = 0.0
#Daily average temperature
#DTEFF           = MAX (0., DAVTMP-TBASE)
#Daily effective temperature

###PAL:28Apr2015
# Modification of accumulation of TSUM, because drought is assumed to affect the criop
# development before flowering. We use an equation of the type:
# DAYTEFF = INSW(TSUM - TSUMflowering , DTEFF#RFP , DTEFF)
# I kept DTEFF at it was and introduced DAYTEFF to keep the changes as minimal as possible.

DAYTEFF         = MAX (0., DAVTMP-TBASE)
#DTEFF           = INSW(TSUM1 - TSUMflower , DAYTEFF#TRARF1 , DAYTEFF)
DTEFF           = INSW(TSUM1 - TSUMfloinit , DAYTEFF#TRARF1 , DAYTEFF)

###EndPAL:28Apr2015  and change to TSUMfloinit on May 15, 2015

##############################################################################################

### Water stress is now introduced (as if) by defining prt_wish as different
### from the initial setting.
### In this main program the INSW will be replaced by e.g. the TRANRF function.
prt_wish        = INSW(TSUM3-180.0,prt_wish_0,prt_wish_high)

###############################################################################################

### Balances
### For Dry Matter DM:
# All BalDmHlp1/2/4/5 should be 0.0000. BalDmHlp5 is relative to BalDmHlp3.
# BalDmHlp1 and 2 check the rates after and before splitting these over the plants.
# BalDmHlp3 accumulates all principal rates of dry matter production, which are
# compared to the summed DMi's. BalDmHlp4 is the absolute difference; BalDmHlp5 is
                    # the absolute balance relative to the total (BalDmHlp3).
                    
                    ###GT: Subtracted dCorm1_ReAll_After_Harv from GDM1HLP in order to correct BalDmHlp1
                    
                    BalDmHlp1       = G2from1 + GDM1 - GDM1HLP - dCorm1_ReAll_After_Harv
                    BalDmHlp2       = G2from2_1 + G2from2_2 + G3from2_1 + G3from2_2 - GDM2HLP
                    # DM1+DM2+DM3=DMTotdm should equal of the integral of GDM1HLP+GDM2HLP+G3Photos
                    BalDmHlp3       = INTGRL(ZERO, GDMTOT)
                    GDMTOT          = GDM1HLP + GDM2HLP + G3Photos
                    
                    ###PAL
                    #################################################################Balance BalDmHlp4 is correct upto first harvest
                    
                    #BalDmHlp4       = (DMTotdm - (DM1I+DM2I+DM3I))- BalDmHlp3  ##########Dead leaves must be included!
                      
                      
                      BalDmHlp4        = ((DMTotdm) + (TotWrtD) + (TotWleafD - TotWleafD_I) + (HarvestDM1_Pru-HarvestDM_Pru1I) + ...
                                          (HarvestDM2_Pru-HarvestDM_Pru2I)) - (BalDmHlp3) - (DM1I+DM2I+DM3I) + (HarvestWleafD-HarvestWleafD_I)
                    #+ ...  this did not work better.
                    #             (Wbunch1 - Wbunch1I)
                    
                    #BalDmHlp4        = ((DMTotdm) + (TotWrtD) + (TotWleafD - TotWleafD_I) + (HarvestDM1_Pru-HarvestDM_Pru1I)+ ...
                                         #                    (HarvestDM2_Pru-HarvestDM_Pru2I))- BalDmHlp3 - (DM1I+DM2I+DM3I) + (HarvestWleafD-HarvestWleafD_I) + ...
                    #                    (HarvestWpsstm-HarvestWpsstm_I) + Corm1_After_Harv + Wrt1_After_Harv
                    
                    ####PAL-end
                    
                    BalDmHlp5       = BalDmHlp4 / NOTNUL(BalDmHlp3)
                    ### BalDmHlp3 must also be reset at harvest, but only with respect to the shoot
                    ### part (DM1 - Wrt1). This part should be subtracted, because that is harvested.
                    ### This is implemented in the EVENT section as (HELPDM1-HELPWrt1).
                    
                    ################################################################################################
                      
                      ### Balances
                    ### For Root and Shoot Dry Matter:
                      # BalRtShHlp1, 2 and 3 are individual balances with respect to the motherplant,
                    # sucker 1 and sucker 2, respectively. BalRtShHlp4 is the overall balance check,
                    # including all 3 plant parts in one calculation.
                    # BalRtShHlp4 is the balance calculated relative to the total DM production.
                    # All balances should be 0.0000.
                    
                    ###PAL
                    ### The balance BalShHlp1,2 and 3 are completely OK, the other two, 4 need closer
                    ### inspection, although in the horizontal parts of the "curves" the balances
                    ### are fine.
                    ###PAL-end
                    
                    ###balance BalRtShHlp1, 2, 3 and BalRtShHlp4 OK.
                    
                    
                    #BalRtShHlp1     = ( ((Wrt1+Wsh1+Corm1_to_Wsh1) - (Wrt1I+Wsh1I+Corm1_to_Wsh1I)) - (DM1 - DM1I) ) / DM1
                    ###PAL11May2015
                    BalRtShHlp1     = ( ((Wrt1+Wsh1) - (Wrt1I+Wsh1I)) - (DM1 - DM1I) ) / DM1
                    ###END PAL11May2015
                    
                    BalRtShHlp2     = ( ((Wrt2+Wsh2) - (Wrt2I+Wsh2I)) - (DM2 - DM2I) ) / DM2
                    BalRtShHlp3     = ( ((Wrt3+Wsh3) - (Wrt3I+Wsh3I)) - (DM3 - DM3I) ) / NOTNUL(DM3)
                    
                    
                    #BalRtShHlp4     = ( (DMTotRtSh - ((Wrt1I+Wrt2I+Wrt3I) + (Wsh1I+Wsh2I+Wsh3I)))-...
                                         #                  (DMTotdm   - (DM1I+DM2I+DM3I) - (Corm1_to_Wsh1-Corm1_to_Wsh1I)) )   / DMTotdm
                    
                    ###PAL
                    #### Is there anything to remark about the possible resetting of balances in
                    #### the EVENT section? Yes: BalRtShHlp1 and BalRtShHlp4 were around zero before the
                    #### splitting of the shoot into its different parts was done. Reason: before the
                    #### splitting up in parts the shoot was harvested as a whole, and this was obvious
                    #### from the line "NEWVALUE Wsh1 = HELPWsh2", where the whole of the Wsh1 is
                    #### replaced by Wsh2 (at that moment contained in the variable HELPWsh2). But after
                    #### the splitting up in corm, psstm, leaves and bunch, the corm is not harvested.
                    #### It is remaining in the soil, instead. Therefore, the balances BalRtShHlp1 and
                    #### BalRtShHlp4 are not working properly anymore. This could only be changed if
                    #### a helpvariable would be introduced that also represents Wsh1 as a whole.
                    #### This has not been done, however, so BalRtShHlp1 and BalRtShHlp4 are not
                    #### correct anymore after the introduction of the different shoot parts.
                    #### After a complete review by PAL on 4 August 2009, with respect to the
                    #### formulation of a number of pruning events and harvest events the balances
                    #### were OK again!!!.
                    ###PAL-end
                    
                    ### Balances for Shoot dry Matter partitioning:
                      # BalShHlp1, 2 and 3 are individual shoot balances with respect to the motherplant,
                    # sucker 1 and sucker 2, respectively.
                    # All balances should be 0.0000.
                    ###PAL
                    ##The balance BalShHlp3 is completely OK, the other two need closer inspection.
                    ###PAL-end
                    ###############################################################################BalShHlp1 - corrected (KN)
                    
                    #BalShHlp1       = (  ((Wcorm1 + Wpsstm1 + Wleaf1 + Wbunch1 + WleafD1 + Corm1_to_Wsh1)  - ...
                                           #                      (Wcorm1I+Wpsstm1I+Wleaf1I+Wbunch1I+WleafD1I+Corm1_to_Wsh1I)) - ...
                                          #                      ( (Wsh1 - Wsh1I) + (WleafD1-WleafD1I) + (HarvestDM1_Pru-HarvestDM_Pru1I)) ) / Wsh1
                    
                    BalShHlp2       = (  ((Wcorm2 +Wpsstm2 +Wleaf2 +WleafD2 )  - ...
                                          (Wcorm2I+Wpsstm2I+Wleaf2I+WleafD2I)) - ...
                                         ( (Wsh2 - Wsh2I) + (WleafD2 - WleafD2I) + (HarvestDM2_Pru-HarvestDM_Pru2I) ) ) / Wsh2
                    
                    BalShHlp3       = (  ((Wcorm3 +Wpsstm3 +Wleaf3 +WleafD3 )  - ...
                                          (Wcorm3I+Wpsstm3I+Wleaf3I+WleafD3I)) - ...
                                         ( (Wsh3 - Wsh3I) + (WleafD3 - WleafD3I) + (HarvestDM3_Pru-HarvestDM_Pru3I) ) ) / NOTNUL(Wsh3)
                    
                    # Note that the above balances are taken relative to the amount present, so Wshi.
                    # This is in fact the approximate amount, because the roots that have died should
                    # be added. So: (...) / ( Wshi + (WleafDi - WleafDiI) ). This will, however, not
                    # give a better insight.
                    
                    ### Crop development is temperature-sum driven
                    TSUM1           = INTGRL(TSUM1I, RTSUM12)
                    #Temperature sum of plant 1 (C d)
                    TSUM2           = INTGRL(TSUM2I, RTSUM12)
                    #Temperature sum of plant 2 (C d)
                    TSUM3           = INTGRL(TSUM3I, RTSUM3)
                    #Temperature sum of plant 3 (C d)
                    
                    RTSUM12         =  DTEFF
                    # Rate of change of temperature sum for plant 1 and 2. These rates of change
                    # are not triggered by an emergence, because we start with an established
                    # banana crop consisting of a mother plant and a first sucker (sucker 1).
                    RTSUM3          =  DTEFF # SWemerg3
                    # Rate of change of temperature sum for plant 3 (sucker 2).
                    # This temperature sum starts to accumulate at emergence of sucker2,
                    # as indicated by SWemerg3. It is triggered by TSUM2 - TSUMSUC, where TSUMSUC
                    # is taken as 1302 Cd.
                    EVENT
                    ZEROCONDITION TSUM2 - TSUMSUC
                    ###Switch to indicate emergence of plant 3 (sucker 2)
                    NEWVALUE SWemerg3   = 1.0
                    ENDEVENT
                    
                    ### Leaf area calculations and light interception
                    LAI1            = INTGRL(LAI1I, NLAI1)
                    #LAI1 - leaf area index of plant 1 (ha leaf ha-1 soil)
                    LAI2            = INTGRL(LAI2I, NLAI2)
                    #LAI2 - leaf area index of plant 2 (ha leaf ha-1 soil)
                    LAI3            = INTGRL(LAI3I, NLAI3)
                    #LAI3 - leaf area index of plant 3 (ha leaf ha-1 soil)
                    
                    ###Net increase of Leaf area index
                    NLAI1           = GLAI1 - DLAI1
                    #Daily net rate of change of LAI for plant 1 (ha leaf ha-1 soil d-1)
                    NLAI2           = GLAI2 - DLAI2
                    #Daily net rate of change of LAI for plant 2 (ha leaf ha-1 soil d-1)
                    NLAI3           = GLAI3 - DLAI3
                    #Daily net rate of change of LAI for plant 3 (ha leaf ha-1 soil d-1)
                    
                    ###Death rates of Leaf area index
                    DLAI1           = LAI1 # RDR1
                    #Death rate of leaf area index of plant 1 (ha leaf ha-1 soil d-1)
                    DLAI2           = LAI2 # RDR2
                    #Death rate of leaf area index of plant 2 (ha leaf ha-1 soil d-1)
                    DLAI3           = LAI3 # RDR3 # SWstdm3
                    #Death rate of leaf area index of plant 3 (ha leaf ha-1 soil d-1) is assumed to
                    #only start after 360 Cd, and is therefore triggered by SWstdm3.
                    #No death of LAI for period TSUM3 0-360 Cd, where (non-functional) LAI3 is formed
                    #by dry matter from sucker1.
                    
                    #############################################################################################
                      
                      CALL GLA(SLA1,Pleaf1,GWsh1,SWstdm3,TSUM1,TSUM3stop_exp,LAI1,...
                               LAIstop_exp,RGRL1,DTEFF,                     GLAI1)
                    #     Growth rate of LAI plant 1 (ha leaf ha-1 soil d-1)
                    CALL GLA(SLA2,Pleaf2,GWsh2,SWstdm3,TSUM2,TSUM3stop_exp,LAI2,...
                             LAIstop_exp,RGRL2,DTEFF,                     GLAI2)
                    #     Growth rate of LAI plant 2 (ha leaf ha-1 soil d-1)
                    CALL GLA(SLA3,Pleaf3,GWsh3,SWstdm3,TSUM3,TSUM3stop_exp,LAI3,...
                             LAIstop_exp,RGRL3,DTEFF,                     GLAI3)
                    #     The growth of the leaves for plant 3 (sucker 2) due to photosynthesis does
                    #     not directly start at emergence, but is retarded by a temperature sum
                    #     (STGRLV) which corresponds with 360 Cd (about two months). (PAL22Jun2012: this is taken
                                                                                      #     care of via switch SWstdm3: see 5-7 lines below.) Plant 3 emerges
                    #     with no functional leaves, and obtains the necessary DM to establish its
                    #     weight (and leaf area growth (in ha leaf ha-1 soil d-1)) from sucker 1 up
                    #     to STGRLV=360 Cd.
                    
                    EVENT
                    ZEROCONDITION TSUM3-STGRLV
                    NEWVALUE SWstdm3    = 1.0
                    ###In this event section the Dry Matter SWitch is set to indicate the end of
                    ###the full support of sucker 1 to sucker 2. Switch SWstdm3 also indicates
                    ###the start of the linear decrease of the contribution of DM of sucker 1 to
                    ###sucker 2, and the start of photosynthesis of sucker 2. STGRLV is 360 Cd.
                    ENDEVENT
                    
                    ### Light interception by the banana canopy (three levels - plants 1, 2 and 3)
                    PARIN1          = 0.5 # DTR
                    #Total PAR incident for plant 1
                    #PAR is 50% of DTR, expressed in MJ ha-1 d-1. This fraction of 0.5 is well
                    #established (Sinclair & Muchow, 1999) and therefore hard-coded.
                    PAROUT1         = PARIN1 # EXP(-K1#LAI1)
                    #Total PAR transmitted by plant 1
                    PARINT1         = PARIN1 - PAROUT1
                    #Total PAR intercepted by plant 1
                    
                    PARIN2          = PAROUT1
                    #Total PAR incident for plant 2
                    PAROUT2         = PARIN2 # EXP(-K2#LAI2)
                    #Total PAR transmitted by plant 2
                    PARINT2         = PARIN2 - PAROUT2
                    #Total PAR intercepted by plant 2
                    
                    PARIN3          = PAROUT2
                    #Total PAR incident for plant 3
                    PAROUT3         = PARIN3 # EXP(-K3#LAI3)
                    #Total PAR transmitted by plant 3
                    PARINT3         = PARIN3 - PAROUT3
                    #Total PAR intercepted by plant 3
                    
                    ##############################################################################################
                      
                      ### Crop growth or dry matter production
                    DM1             = INTGRL (DM1I, NDM1)
                    #Total dry matter of plant 1 (kg ha-1)
                    DM2             = INTGRL (DM2I, NDM2)
                    #Total dry matter of plant 2 (kg ha-1)
                    DM3             = INTGRL (DM3I, NDM3)
                    #Total dry matter of plant 3 (kg ha-1)
                    
                    DMTotdm         = DM1 + DM2 + DM3
                    
                    HarvestDM       = INTGRL(HarvestDM_I    , RHarvestDM)
                    #HarvestDM - "harvested" dry matter: contains the whole of the shoot part that is
                    #harvested, so the pseudostem, the leaves (both green and dead) and the bunch, Plus
                    #the pruned dead leaves. This state variable is shockwise filled by dead leaf weight
                    #material, usually from prunings, and filled instantaneously by the pseudostem material,
                    #green and dead leaves of motherplant and bunch at harvest. Because of the shockwise
                    #additions, the continuous day-to-day rate (RHarvestDM) is set to zero.
                    #{[The Harvested material used for mulching the soil (kg DM ha-1), is taken care of
                       #in the separate DMmulch state variable.]}
                    RHarvestDM      = 0.0
                    
                    #Cummulative weight of leaves of plant 1 pruned (kg ha-1)
                    HarvestDM1_Pru       = INTGRL(HarvestDM_pru1I    , RHarvestDM1_pru)
                    RHarvestDM1_pru      = 0.0
                    #Cummulative weight of leaves of plant 2 pruned (kg ha-1)
                    HarvestDM2_Pru       = INTGRL(HarvestDM_pru2I    , RHarvestDM2_pru)
                    RHarvestDM2_pru      = 0.0
                    #Cummulative weight of leaves of plant 3 pruned (kg ha-1)
                    #Actually no leaves die, used to help with the re-settings
                    HarvestDM3_Pru       = INTGRL(HarvestDM_pru3I    , RHarvestDM3_pru)
                    RHarvestDM3_pru      = 0.0
                    
                    DMmulch  = INTGRL(DMmulch_I, DcRDMmulch)
                    #DMmulch - "harvested" dry matter: contains the shoot part that is not
                    #of economic interest, so the pseudostem and the leaves (both green and dead).
                    #Harvest occurs at (TSUM2=3600 Cd) and it is used for mulching the soil (kg DM ha-1).
                    ###PAL
                    #(TSUM2=3600 Cd) leaves come in the HarvestLAI by that event, the rate
                    ###TSUM2=3600 should be TSUM1=3600??
                      ###PAL-end
                    #This state variable is more or less dynamically filled by dead leaf weight material,
                    #usually from prunings, and filled instantaneously by the pseudostem material and
                    #all remaining leaves (dead and green) at harvest of only the motherplant (so no
                                                                                               #dead leaves from the 2 suckers at harvest).
                    #Since the prunings take place at regular time intervals, usually each month, the
                    #state variable DMmulch is filled shockwise. Dead leaves are gathered in an extra
                    #state variable called "TotWleafD", which is WleafD1 + WleafD2 + WleafD3.
                    
                    DcRDMmulch = - RDcR # DMmulch
                    #Decomposition rate of mulch (kg ha-1 d-1) is taken proportional to the amount
                    #present (exponential decay). This means that the assumption is that the
                    #different materials (pruned dead leaves, harvested green and dead leaves and
                                          #pseudostem) have a comparable relative decomposition rate.
                    #The RDcR could be redefined later as a function of moisture content and
                    #temperature, of course.
                    
                    ##############################################################################################
                      #EVENT
                    #   ZEROCONDITION TSUM2-(TSUMshfhv-Frac)
                    #   NEWVALUE SWFSU2     = 1.0
                    ####Switch to activate the function-subroutine FSU2 at the moment of harvest of
                    ####motherplant 1 (at TSUM2=TSUMshfhv=2298 Cd) (and the concomittent shift of
                                                                    ####sucker 1 to the status of motherplant and sucker 2 to sucker 1 status).
                    #PARAM Frac = 1.0E-2
                    ####Technical remark: apparently the second time harvest took place, there was
                    ####no passing of the TSUMshfhv = 2298 Cd. Subtraction of a small number
                    ####(Frac=0.01 Cd) was sufficient to "repair" this.
                    ####This was due to the sequence in which the events were handled.
                    ####Since the TSUM2=TSUMshfhv is always biologically coinciding with
                    ####TSUM1=TSUMHARV, setting the switch SWFSU2 to 1.0 is now put under the
                    ####harvest event at the end of the program. The technical problem is now also
                    ####solved, because there is only one event defined.
                    ####Because the simulation is started in the middle of the development The
                    ####value of SWFSU2 should be set to 1.0 in the initial: after all it is after a
                    ####harvest.
                    #ENDEVENT
                    
                    EVENT
                    ZEROCONDITION TSUM1-TSUMfloinit
                    NEWVALUE SWFSU2     = 0.0
                    ###Switch SWFSU2 is put to 0 at flower initiation of the (new) motherplant at
                    ###TSUMfloinit (at TSUM1=TSUMfloinit=2423 Cd). At this physiological time the
                    ###support of the motherplant to sucker 1 is completely stopped.
                    ENDEVENT
                    
                    ###PAL:
                      ### Net rates of change of the weights of the dry matter due to the death of the
                    ### leaves and roots. To calculate this the death rate of the leaf weights are directly
                    ### used. Is this OK with respect to the pruning THINK!!!!!!!!
                      #PAL thinks YES, because this is the continuous part of the dying leaves. Kenneth,
                    #what do you think?
                      NDM1            = GDM1 - DWDM1
                    NDM2            = GDM2 - DWDM2
                    NDM3            = GDM3 - DWDM3
                    #DWDM1           = DWleaf1
                    #DWDM2           = DWleaf2
                    #DWDM3           = DWleaf3
                    DWDM1           = DWleaf1   +  DWrt1
                    DWDM2           = DWleaf2   +  DWrt2
                    DWDM3           = DWleaf3   +  DWrt3
                    
                    #DWDMi is the sum of the death rates of leaves - kg ha-1 d-1(DWleafi) and roots (DWrti)
                    #for plant i = 1-3
                    ######################################################################################(ADDED:KN)
                    ### Mother plant
                    GDM1HLP         = PARINT1 # LUE1
                    # Total production of plant 1 (kg DM ha-1 d-1), from intercepted PAR(MJ ha-1 d-1)
                    # and a constant LUE (kg MJ-1 PAR), of which a part, indicated by the result of
                    # function-subroutine FSU2 (that is substituted in the variable FSU2RED2_2),
                    # goes to sucker 1. The remainder (1.0 - FSU2RED2_2) goes to the motherplant
                    # via GDM1.
                    # PAL22Jun2012: apart from the normal photosynthesis, there is also the reallocation of
                    # Wcorm1 to the DM1 growth. Therefore the rate dCorm1_ReAll_After_Harv is added here.
                    GDM1            = ( ( 1.0 - FSU2RED2_2 ) # GDM1HLP ) + dCorm1_ReAll_After_Harv
                    
                    ### Sucker 1
                    GDM2HLP         = PARINT2 # LUE2
                    #Since sucker 1 fully feeds sucker 2 during a "period" from 0 - 360 Cd (360
                                                                                            #refers to TSUM3 and corresponds with TSUM2=1662 Cd), the total DM produced by
                    #sucker 1 (defined in the help-variable GDM2HLP) is divided over sucker 1 and
                    #sucker 2 during that "period". This period starts, however, when emergence of
                    #sucker 2 takes place, indicated by SWemerg3 that becomes 1.0
                    #if TSUM2 > TSUMSUC(=1302 Cd).
                    #The part of GMD2HLP that goes to sucker 2 is ( SWemerg3#(1.0-SWstdm3) # FSU2max )
                    #and the remainder goes to GDM2 itself: {1-( SWemerg3#(1.0-SWstdm3) # FSU2max )}.
                    #Since, however, either G2from2_1 or G2from2_2 should be active in rate GDM2,
                    # G2from2_2 must be SWitched on with SWstdm3, whereas G2from2_1 must then be
                    #SWitched off by (1.0-SWstdm3). That makes it subsequently superfluous to
                    #also multiply by (1.0-SWstdm3) in the expression
                    #{1-( SWemerg3#(1.0-SWstdm3) # FSU2max )}, and therefore ithis has been removed
                    #For memory's sake it is outcommented instead of physically removed.
#G2from2_1       = (1.0-SWstdm3)#( 1.0 - (SWemerg3#(1.0-SWstdm3) # FSU2max) ) ...
#                                                                       # GDM2HLP
G2from2_1       = (1.0-SWstdm3)#( 1.0 - SWemerg3#FSU2max  ) # GDM2HLP

G2from2_2       =      SWstdm3 #( 1.0 -        FSU2RED2_1 ) # GDM2HLP
G2from1         =                              FSU2RED2_2   # GDM1HLP
GDM2            = G2from2_1 + G2from2_2 + G2from1
# Total rate of increase in weight of plant 2 (kg DM ha-1 d-1)

### Sucker 2
G3from2_1       = ( SWemerg3 #(1.0-SWstdm3) # FSU2max ) # GDM2HLP
G3from2_2       =                            FSU2RED2_1 # GDM2HLP
G3Photos        =    SWstdm3 # (PARINT3 # LUE3)
GDM3            =  G3from2_1 + G3from2_2 + G3Photos

#18Jun2012. Check whether sum is 1.00. Is Perfect
CheckOne = ( G2from2_1 + G2from2_2 + G3from2_1 + G3from2_2 ) / GDM2HLP

# Total rate of increase in weight of plant 3 (kg DM ha-1 d-1) due to
# (i) DM partitioning from sucker 1 to sucker 2 (G3from2_1) in the period of
# "no functional leaves" (0.0 < TSUM3 < 360),
# (ii) due to a decreasing amount of supply from sucker 1 to 2 in the period that
# sucker 2 has photosynthesis but is still supported (G3from2_2), and
# (iii) due to photosynthesis of sucker 2 itself. Since this should start only
# after TSUM3 > 360 Cd, it is put on with SWstdm3. Note that GLAI3 will have a value
# from emergence onwards, and that LAI3 also increases and will intercept light,
# because it is supported by sucker 1, but it is functionally not working.

FSU2RED2_1      = SWstdm3 # FSU2(FSU2max,TSUMfloinit,TSUMendfulldepe,TSUM2)
FSU2RED2_2      = SWFSU2  # FSU2(FSU2max,TSUMfloinit,TSUMendfulldepe,TSUM1)
#FSU2RED2_1 is the function that takes care of the linear decrease of the amount
#of newly produced DM2, so given by GDM2HLP, that is supplied by sucker 1 to
#sucker 2 in the period that sucker 1 is not yet shifted to the status of mother
#plant. The function is than read by TSUM2 and it is a fraction between FSU2max
#and 0.0.
#After the shift, at TSUM2=TSUMshfhv=2298 Cd (coinciding with TSUM1=TSUMHARV=
#3600 Cd), the function is read by TSUM1, because sucker 1 has become motherplant,
#and will continue to supply dry matter up to its flower initiation at 2423 Cd.
#However, the dry matter now goes to sucker 1 (previous sucker 2) and comes from
#the mother plant (previous sucker 1). Thus, the rates have to be differently
#formulated, and that has been done in the above programming part.
#During the period TSUM2=1662 to 2298 Cd SWstdm3 is 1, otherwise, it is 0;
#During the period TSUM1=2298 to 2423 Cd SWFSU2 is 1, otherwise, it is 0.
#SWemerg3= 1 after TSUM2=1302, up to the harvest.

#############################################################################################

### Crop total dry matter produced is divided over root and shoot

### Crop root growth or dry matter production of roots
Wrt1            = INTGRL (Wrt1I, NWrt1)
#Dry matter of roots of plant 1 (kg ha-1)
Wrt2            = INTGRL (Wrt2I, NWrt2)
#Dry matter of roots of plant 2 (kg ha-1)
Wrt3            = INTGRL (Wrt3I, NWrt3)
#Dry matter of roots of plant 3 (kg ha-1)
WrtTot          = Wrt1 + Wrt2 + Wrt3
#Total dry matter of roots of the whole of the banana plant (kg ha-1)
##############################################################################(NWrti ADDED K:N)

### Crop shoot growth or dry matter production of shoots
Wsh1            = INTGRL (Wsh1I, NWsh1)
#Dry matter of shoots of plant 1 (kg ha-1)
Wsh2            = INTGRL (Wsh2I, NWsh2)
#Dry matter of shoots of plant 2 (kg ha-1)
Wsh3            = INTGRL (Wsh3I, NWsh3)
#Dry matter of shoots of plant 3 (kg ha-1)
WshTot          = Wsh1 + Wsh2 + Wsh3
#Total dry matter of shoots of the whole of the banana plant (kg ha-1)

#Some root:shoot ratio's derived from the modelprogram. Note that the
                    #root:shoot ratio for the motherplant (RtShratio1) after the first harvest
                    #continues to increase (certainly at this very moment where no death of
                                            #roots is included (date 24 June 2009)), because only the shoot, but not
                    #the root, is harvested. The total-banana-plant root:shoot ratio is
                    #therefore also not yet very informative. If death rates are included,
                    #however, these values will be informative.
                    RtShratio1      = Wrt1  /NOTNUL(Wsh1)
                    RtShratio2      = Wrt2  /NOTNUL(Wsh2)
                    RtShratio3      = Wrt3  /NOTNUL(Wsh3)
                    RtShratioAll    = WrtTot/NOTNUL(WshTot)
                    
                    DMTotRtSh       = WrtTot + WshTot
                    
                    EVENT
                    ZEROCONDITION TSUM1-TSUMflower
                    NEWVALUE SWFrt1    = 0.0
                    ###Switch SWFrt is put to 0 at flowering of the (new) motherplant at TSUMflower
                    ###(at TSUM1=TSUMflower=2663 Cd). At this physiological time root
                    ###formation of the motherplant is assumed to completely stop.
                    ###For the motherplant, that is initialized after 360 Cd (in fact between
                                                                              ###2262 and 2423 Cd) and ends at 3600, this switch is the only important one.
                    ###For Sucker 1, that is initialized after 360 Cd (in fact between
                                                                       ###960 and 1121 Cd) and ends at 2298 (and then continues to be motherplant),
                    ###this switch is also the only important one.
                    ENDEVENT
                    
                    ###PAL#
                      ### Net rates of change of the weights of the shoot due to the death of the
                    ### leaves. To calculate this the death rate of the leaf weights are directly
                    ### used. Is this OK with respect to the pruning THINK!!!!!!!!
                      ###PAL: Same as previous comment. Below Part of corm1 goes to Wsh1 after harvest
                    ###   (PAL22June2012: the dCorm1_to_Wsh1 should go to the DM1 that exists directly after the harvest
                           ###    and not anymore to the shoot. This change means that the growth rate of the DM1 is a bit enlarged)
                    #NWsh1           = GWsh1 - DWsh1 + dCorm1_to_Wsh1
                    NWsh1           = GWsh1 - DWsh1
                    NWsh2           = GWsh2 - DWsh2
                    NWsh3           = GWsh3 - DWsh3
                    DWsh1           = DWleaf1
                    DWsh2           = DWleaf2
                    DWsh3           = DWleaf3
                    #dCorm1_to_Wsh1  = pcorm1_to_Wsh1 # dCorm1_After_Harv
                    
                    ###PAL:
                      #A help variable
                    
                    #Corm1_to_Wsh1   = INTGRL( Corm1_to_Wsh1I, dCorm1_to_Wsh1)
                    
                    #Integral to keep track of weight of the corm of plant 1 at harvest
                    Wcorm1H              = INTGRL(Wcorm1H_I, RWcorm1H)
                    RWcorm1H             = 0.0
                    
                    ### Net rates of change of the weights of the living roots due to death of the
                    ### roots (kg ha-1 d-1).
                    NWrt1           = GWrt1 - DWrt1
                    NWrt2           = GWrt2 - DWrt2
                    NWrt3           = GWrt3 - DWrt3
                    
                    ### Growth rates of the different plants 1, 2 and 3.
                    ### Mother plant
                    GWrt1           =         FrtRED1   # GDM1
                    GWsh1           = ( 1.0 - FrtRED1 ) # GDM1
                    FrtRED1         = SWFrt1#prt2(prt_wish,psh_wish,TSUM3I,STGRLV,TSUM1,TSUMflower)
                    
                    ### Sucker 1
                    GWrt2           =         FrtRED2   # GDM2
                    GWsh2           = ( 1.0 - FrtRED2 ) # GDM2
                    FrtRED2         = SWFrt1#prt2(prt_wish,psh_wish,TSUM3I,STGRLV,TSUM2,TSUMflower)
                    
                    ### Sucker 2
                    GWrt3           =         (FrtGR + FrtRED3)   # GDM3
                    GWsh3           = ( 1.0 - (FrtGR + FrtRED3) ) # GDM3
                    # The dry matter that is either given to sucker 2 or produced by sucker 2 is
                    # also distributed over the roots and shoot. This is done by 2 functions: FrtGR
                    # and FrtRED3.
                    # Function FrtGR is either increasing or constant between 0 < TSUM3 < 360 Cd
                    # (STGRLV).
                    # Function FrtRED3 decreases from where function FrtGR ends (so at STGRLV),
                    # down to zero at TSUMflower (2663 Cd). For sucker 2 function FrtRED3 is read
                    # up to TSUM3 = 996, so up to the shift. Function FrtGR is only used by sucker 2,
                    # because of the initialization of this sucker 2 as zero.
                    FrtGR           = ( SWemerg3 # (1.0-SWstdm3) ) # ...
                    prt1(TIME,STTIME,prt_wish,psh_wish,Frtmax,STGRLV,TSUM3I,TSUM3)
                    FrtRED3         =   SWstdm3 # ...
                    prt2(prt_wish,psh_wish,TSUM3I,STGRLV,TSUM3,TSUMflower)
                    
                    #####################################################################################(ADDED : KN)
                    #Death of roots
                    
                    #Based on experimentation by Moreau et al.1963. Among 110 roots produced during the first
                    #3 months on a parent banana plant (Gros Michel), 102 roots had dissappeared/died at 7 months,
                    #therefore maximum lifespan is about 4 months
                    #After about 4 months (120 days), 7.3% of the original root dry matter will be left
                    #RDRrt = 1n(0.073)/(-120) =  0.0218 d-1
                    #RDRrt2 = 0.0218 d-1  (plant 2)
                    #RDRrt3 = 0.0218 d-1  (plant 3)  - start after 360 Cd
                    
                    #The roots present at flowering remain alive during the reproductive phase, with a moderate
                    #decrease of about 8% (Blomme, 2000).
                    #Duration from flowering to harvest at Kawanda about (110 days) and Ntungamo (150 days)
                    #RDRrt1 = 1n(0.92)/(-110)  = 0.000758
                    #Kawanda
                    #RDRrt1 = 1n(0.92)/(-150)  = 0.000556
                    #Ntungamo
                    #Other part (TSUM1 2262-2663) before flowering, RDRrt1=0.0218
                    
                    DWrt1 = RDRrt1 # Wrt1
                    DWrt2 = RDRrt2 # Wrt2
                    DWrt3 = RDRrt3 # Wrt3
                    
                    #Total weight of dead roots for plant 1 (kg ha-1)
                    WrtD1_Tot       = INTGRL(Wrt1DI, dWrt1_plant1)
                    dWrt1_plant1    = DWrt1
                    
                    #Total weight of dead roots for plant 2 (kg ha-1)
                    WrtD2_Tot       = INTGRL(Wrt2DI, dWrt2_plant2)
                    dWrt2_plant2    = DWrt2
                    
                    #To calculate the total weight of dead roots for plant 3 (kg ha-1)
                    WrtD3_Tot       = INTGRL(Wrt3DI, dWrt3_plant3)
                    dWrt3_plant3    = DWrt3
                    
                    TotWrtD         = WrtD1_Tot + WrtD2_Tot + WrtD3_Tot
                    #Total weight of roots dead for plant 1, 2 and 3 (kg ha-1)
                    
                    RDRrt1 = AFGEN (RDRrt1TB, TSUM1)
                    
                    FUNCTION RDRrt1TB =  2262.,0.0218, 2663.,0.0218, 2700.,5.56E-4, 3600.,5.56E-4
                    #Function to cater for reduced root senescence after flowering
                    
                    ###PAL:
                      Wrt1_After_Harv  = INTGRL(Wrt1_After_HarvI , dWrt1_After_Harv)
                    #Weight of roots after harvest of plant 1 (kg ha-1)
                    #The amount in this state "Wrt1_After_Harv" is in fact SOM that decays (PAL22Jun2012: this
                                                                                            #material is considered directly dead after the harvest of the mother plant and thus starts
                                                                                            #as fresh organic matter, so the relative decomposition rate should be rather high. The
                                                                                            #decomposed material is in fact really SOM (that can decompose, etc.)).
                    #We have a positive flow of material from the Corm1 that is going to the SOM as well,
                    # but that can perhaps not be added here directly, because of other decay rates.
                    
                    dWrt1_After_Harv = - RDRrt1_After_Harv # Wrt1_After_Harv
                    #in HARVEST-event: Wrt1_After_Harv = Wrt1_After_Harv + Wrt1
                    #Death rate of roots after harvest of plant 1 (kg ha-1 d-1)
                    
                    #The rate "dCorm1_Lost_After_Harv" can perhaps be added to the rate by which
                    #Wrt1_After_Harv decreases, if the decomposition rates are similar.
                    #If we add it it would look as follows:
                      #dWrt1_After_Harv = - RDRrt1_After_Harv # Wrt1_After_Harv + dCorm1_Lost_After_Harv
                    ###PAL-end
                    
                    ######################################################################################(ADDED : KN)
                    
                    ### Crop above ground dry matter produced is divided over corm, pseudostem,
                    ### green leaves and bunch.
                    ### The bunch appears onto the motherplant, so Pbunch2 and Pbunch3 are not
                    ### meaningful; they are therefore named DummyPbunch2 and DummyPbunch3.
                    ### They are calculated to maintain the same calculational structure,
                    ### but they not used.
                    ### Explanation about the dead leaves:
                      ### Weights of dead leaves of the individual plants (WleafDi) increase with the dying
                    ### of green leaves and decrease by pruning (time-event) and harvesting (state-event).
                    
                    Wcorm1          = INTGRL (Wcorm1I , GWcorm1 )
                    #Dry matter of corms of plant 1 (kg ha-1)
                    Wpsstm1         = INTGRL (Wpsstm1I, GWpsstm1)
                    #Dry matter of pseudostem of plant 1 (kg ha-1)
                    Wleaf1          = INTGRL (Wleaf1I , NWleaf1 )
                    #Dry matter of leaves of plant 1 (kg ha-1)
                    WleafD1         = INTGRL (WleafD1I, DWleaf1 )
                    #Weight of dry leaves for plant 1 (kg ha-1)
                    Wbunch1         = INTGRL (Wbunch1I, GWbunch1)
                    #Dry matter of the bunch of plant 1 (kg ha-1)
                    
                    ###PAL:
                      #  (PAL22Jun2012: different point of view: the Wcorm1 is split into two states in
                          #   the harvest event. Then one state is degraded (Corm1_Lost_After_Harv) and the other
                          #   state is reallocated dynamically (Corm1_After_Harv) to DM1.)
                    
                    #Further Corm1 dynamics. After harvest "Corm1" decays and partially feeds Wsh1.
                    #The remainder is going to a state "Corm1_Lost_After_Harv", which is an
                    #accumulator only (for balances for example (at the moment similar to Wcorm1H)).
                    #The "Corm1_Lost_After_Harv" is in fact SOM and, if the decomposition rates of
                    #"Wrt1_After_Harv" and "Corm1_Lost_After_Harv" are assumed similar, we should
                    #add the rate "dCorm1_Lost_After_Harv" to "dWrt1_After_Harv".
                    Corm1_After_Harv          = INTGRL( Corm1_After_HarvI , dCorm1_After_Harv )
                    dCorm1_After_Harv         = - RALR_Corm1_To_DM1 # Corm1_After_Harv
                    dCorm1_ReAll_After_Harv   = dCorm1_After_Harv
                    
                    #Degradation of Wcorm1 after harvest: (PAL22jun2012)
                    Corm1_Lost_After_Harv       = INTGRL( Corm1_Lost_After_HarvI , dCorm1_Lost_After_Harv)
                    dCorm1_Lost_After_Harv      = - RDRCorm1_After_Harv # Corm1_Lost_After_Harv
                    
                    #dCorm1_Lost_After_Harv      = (1.0 - pcorm1_to_Wsh1) # dCorm1_After_Harv
                    
                    ###PAL:
                      #Added dead leaves to the total shoots "i".
                    ###PAL-end
                    WshPlnt1Tot     = Wcorm1 + Wpsstm1 + Wleaf1 + WleafD1 + Wbunch1
                    #Total dry matter of the above ground motherplant (kg ha-1)
                    LWR1            = Wleaf1/DM1
                    #LeafWeightRatio as calculated from the program
                    
                    ###Harvests
                    HarvestWpsstm   = INTGRL(HarvestWpsstm_I, RHarvestWpsstm)
                    #HarvestWpsstm - "harvested" pseudostems
                    RHarvestWpsstm  = 0.0
                    
                    HarvestWLeaf    = INTGRL(HarvestWLeaf_I, RHarvestWLeaf)
                    #New comment: HarvestWLeaf - "harvested" green leaves in terms of weight.
                    #These are done only at harvest (and only for the mother plant), not by the
                    #prunings, because than we only take away the dead leaves (but than for mother
                                                                               #plant, sucker 1 and sucker 2). since the accumulation is shockwise, the
                    #day-to-day continuous inflowrate is taken zero.
                    RHarvestWLeaf   = 0.0
                    
                    HarvestWLeafD   = INTGRL(HarvestWLeafD_I, RHarvestWLeafD)
                    #HarvestWLeafD - "harvested" dead leaves in terms of weight. These are done by
                    #prunings, so not by a daily rate, and by the harvests.
                    RHarvestWLeafD  = 0.0
                    #The "RHarvestWLeafD" could represent the decomposition rate of the dead leaves
                    #that have just been harvested. It obviously NOT only concerns the dead leaves
                    #of the harvested mother plant, but also the decomposition of the pruned leaves
                    #of all three plants (mother, sucker 1 and 2).
                    #At the moment (28 July 2009) we have have decided not to consider decomposition
                    #of leaves and pseudostems separately, but to combine this in the state variable
                    #DMmulch. So, THAT state variable will decompose and will affect the rain
                    #interception and runoff when the model is combined with the water balance.
                    #This "HarvestWLeafD" state variable therefore only accumulates dead leaf weight.
                    
                    HarvestWbunch   = INTGRL(HarvestWbunch_I, RHarvestWbunch)
                    #HarvestWbunch - harvested bunches.
                    RHarvestWbunch  = 0.0
                    #The "RHarvestWbunch" will always be 0.0, because there is no decomposition
                    #of the economic yield in the model, and the bunches will via the harvest
                    #come into the state variable. In fact, we implicitly assume that there are
                    #no harvest losses of the bunches.
                    
                    Wcorm2          = INTGRL (Wcorm2I , GWcorm2 )
                    #Dry matter of corms of plant 2, sucker 1 (kg ha-1)
                    Wpsstm2         = INTGRL (Wpsstm2I, GWpsstm2)
                    #Dry matter of pseudostem of plant 2 (kg ha-1)
                    Wleaf2          = INTGRL (Wleaf2I , NWleaf2 )
                    #Dry matter of leaves of plant 2 (kg ha-1)
                    WleafD2         = INTGRL (WleafD2I, DWleaf2 )
                    #Weight of dry leaves for plant 2 (kg ha-1)
                    WshPlnt2Tot     = Wcorm2 + Wpsstm2 + Wleaf2 + WleafD2
                    #Total dry matter of the above ground sucker 1 (kg ha-1)
                    LWR2            = Wleaf2/DM2
                    #LeafWeightRatio as calculated from the program
                    
                    Wcorm3          = INTGRL (Wcorm3I , GWcorm3 )
                    #Dry matter of corms of plant 3, sucker 2 (kg ha-1)
                    Wpsstm3         = INTGRL (Wpsstm3I, GWpsstm3)
                    #Dry matter of pseudostem of plant 3 (kg ha-1)
                    Wleaf3          = INTGRL (Wleaf3I , NWleaf3 )
                    #Dry matter of leaves of plant 3 (kg ha-1)
                    WleafD3         = INTGRL (WleafD3I, DWleaf3 )
                    #Weight of dry leaves for plant 3 (kg ha-1)
                    WshPlnt3Tot     = Wcorm3 + Wpsstm3 + Wleaf3 + WleafD3
                    #Total dry matter of the above ground sucker 2 (kg ha-1)
                    
                    LWR3            = Wleaf3/NOTNUL(DM3)
                    #LeafWeightRatio as calculated from the program
                    
                    ### The sum of the three plants should equal WshTot, and is used in the
                    ### Balance
                    WshTotPlants    = WshPlnt1Tot + WshPlnt2Tot + WshPlnt3Tot
                    
                    #######################################################################################(ADDED : KN)
                    ###PAL:
                      #I slightly modified the equations with respect to their lay-out.
                    ###PAL-end
                    #To calculate the approximate number of green leaves per plant to have an idea whether
                    #the number is reasonable.
                    
                    Leafnum_plant1  = (LAI1/MLAH1) / ((100./Plant_distance)##2)
                    #Number of green leaves for plant 1
                    Leafnum_plant2  = (LAI2/MLAH2) / ((100./Plant_distance)##2)
                    #Number of green leaves for plant 2
                    # #/plant       =  (ha leaf/ha soil) / (ha middle leaf/middle leaf) / (#plant/ha)
                      #Groundarea Plantl    = 9. (3#3 spacing = Plant_distance # Plant_distance)
                    
                    #Leafnum_plant1  = ( (Wleaf1 # SLA1) # (Plant_distance)##2 ) / MLA1
                    #Number of green leaves plant 1 (##My formulation, both equations give same result)
                    #MLA1 = AFGEN (MLATB, TSUM1)
                    
                    MLAH1 = AFGEN (MLAHTB, TSUM1)
                    MLAH2 = AFGEN (MLAHTB, TSUM2)
                    
                    FUNCTION MLAHTB =  0.,0.        , 360.,0.      , 960.,0.4864E-4, 1500.,0.7652E-4, ...
                    2262.,1.14E-4, 2663.,1.45E-4, 3600.,1.42E-4
                    #Middle leaf area (MLA) was measured as a function of the temperature sum
                    ########################################################################################(ADDED : KN)
                    
                    ##Total weight of dead leaves of all plants is dynamically calculated (by integrating
                                                                                           ##rate DWleafi) and, moreover, this state variable is harvested regularly by pruning
                    ##(below).
                    ##The states WleafDi and TotWleafD at pruning is then set back to zero,
                    ##while the harvest state for dead leaves, HarvestWleafD, is incremented by the WleafDi
                    ##with i = 1,2,3.
                    TotWleafD       = INTGRL (TotWleafD_I, DWleafTot)
                    #Total weight of dry leaves of plant 1, 2 and 3 (kg ha-1)
                    DWleafTot       = DWleaf1 + DWleaf2 + DWleaf3
                    
                    ### Net rates of change of the weights of leaves
                    NWleaf1         = GWleaf1 - DWleaf1
                    NWleaf2         = GWleaf2 - DWleaf2
                    NWleaf3         = GWleaf3 - DWleaf3
                    
                    ### Growth rates of the different plants 1, 2 and 3.
                    ### Mother plant
                    GWcorm1         = Pcorm1  # GWsh1
                    GWpsstm1        = Ppsstm1 # GWsh1
                    GWleaf1         = Pleaf1  # GWsh1
                    GWbunch1        = Pbunch1 # GWsh1
                    CALL INTERPOL('PCOTB','PSTTB','PLVTB','PBUTB',...
                                  PCOTB ,PSTTB  ,PLVTB  ,PBUTB  ,...
                                  K,TSUM1,  Pcorm1,Ppsstm1,Pleaf1,Pbunch1)
                    
                    ### Sucker 1
                    GWcorm2         = Pcorm2  # GWsh2
                    GWpsstm2        = Ppsstm2 # GWsh2
                    GWleaf2         = Pleaf2  # GWsh2
                    CALL INTERPOL('PCOTB','PSTTB','PLVTB','PBUTB',...
                                  PCOTB ,PSTTB  ,PLVTB  ,PBUTB  ,...
                                  K,TSUM2,  Pcorm2,Ppsstm2,Pleaf2,DummyPbunch2)
                    
                    ### Sucker 2
                    GWcorm3         = Pcorm3  # GWsh3
                    GWpsstm3        = Ppsstm3 # GWsh3
                    GWleaf3         = Pleaf3  # GWsh3
                    CALL INTERPOL('PCOTB','PSTTB','PLVTB','PBUTB',...
                                  PCOTB ,PSTTB  ,PLVTB  ,PBUTB  ,...
                                  K,TSUM3,  Pcorm3,Ppsstm3,Pleaf3,DummyPbunch3)
                    
                    ### The summations of the fractions can be taken out as soon as the check on
                    ### the sum of fractions of the partitioning functions has been implemented in
                    ### the INITIAL.
                    Psum1           = Pcorm1 + Ppsstm1 + Pleaf1 + Pbunch1
                    Psum2           = Pcorm2 + Ppsstm2 + Pleaf2 + DummyPbunch2
                    Psum3           = Pcorm3 + Ppsstm3 + Pleaf3 + DummyPbunch3
                    
                    ### Death rates of the leaves of the different plants 1, 2 and 3.
                    DWleaf1         = Wleaf1 # RDR1
                    #Death of leaves of the plant 1 (kg ha-1 d-1)
                    DWleaf2         = Wleaf2 # RDR2
                    #Death of leaves of the plant 2 (kg ha-1 d-1)
                    DWleaf3         = Wleaf3 # RDR3 # SWstdm3
                    #Death of leaves of the plant 3 (kg ha-1 d-1)
                    #Death rate of the weights of leaves of plant 3 (ha leaf ha-1 soil d-1) is assumed to
                    #only start after 360 Cd, and is therefore triggered by SWstdm3.
                    #No death of the weight of leaves for period TSUM3 0-360 Cd, where (non-functional)
                    #LAI3 (via the weight) is formed by dry matter from sucker1.
                    
                    EVENT
                    ###Pruning TIME event
                    ###PAL
                    #
                      #The first pruning should take place at 30 days after the onset of the simulation.
                    #That is already correct. It should not be changed, however, because of the assumption
                    #that we start just after pruning with only green leaves (Fleaf_greeni = 1.0),
                    #implying that there are no dead leaves to start with and DMmulch must have a certain
                    #value.
                    #
                      ###PAL-end
                    ###To take care of the pruning of dead leaves
                    ###The dead leaves of plant 1, 2 and 3 (WleafDi, and TotWleafD) are pruned at
                    ###"Days_between_prunings", usually taken as 30 day intervals and is applied
                    ###as mulch. Technical remark: because StTime is at least the first day of the
                    ###year, so StTime=1, the first pruning is put to StTime-1. + Days_between_prunings,
                    ###so that we arrive at prunings at 30, 60, 90, etc. days, instead of 31, 61, 91, etc.
                    FIRSTTIME StTime-1. + Days_between_prunings
                    ###Days_between_prunings affects the pruning of leaves and is a management parameter
                    ###here set at 30 days.
                    NEXTTIME Time + Days_between_prunings
                    
                    #  In principle Dead Leaf Area for the three separate plants is pruned. However, we
                    #  did not include a state variable for Dead Leaf Area.
                    #  The reason that there are no state variables for the Dead Leaf Area's, is that it
#  is assumed that the dead leaves hang along the stem and thus do not take away light.

#  Dead leaf weight for the three separate plants is pruned. So, WleafDi must be
#  reset to 0.0, and TotWleafD ( = SUM(WleafDi) ) is added to the mulch at prunings.
#  Technical remark 1: the "i"'s in the variables HELPWleafD1i are introduced to
                    #  distinguish these variables from the ones in the Harvest section, so just to
                    #  make them unique.
                    HELPWleafD1i            = WleafD1
                    HELPWleafD2i            = WleafD2
                    HELPWleafD3i            = WleafD3
                    HELPTotWleafDi          = TotWleafD
                    HELPDMmulchi            = DMmulch
                    
                    HELPHarvestDM1_Prui     =  HarvestDM1_Pru
                    HELPHarvestDM2_Prui     =  HarvestDM2_Pru
                    HELPHarvestDM3_Prui     =  HarvestDM3_Pru
                    
                    NEWVALUE WleafD1        = 0.0
                    NEWVALUE WleafD2        = 0.0
                    NEWVALUE WleafD3        = 0.0
                    NEWVALUE TotWleafD      = 0.0
                    NEWVALUE DMmulch        = HELPDMmulchi   + HELPTotWleafDi
                    
                    NEWVALUE HarvestDM1_Pru =  HELPHarvestDM1_Prui   +   HELPWleafD1i
                    NEWVALUE HarvestDM2_Pru =  HELPHarvestDM2_Prui   +   HELPWleafD2i
                    NEWVALUE HarvestDM3_Pru =  HELPHarvestDM3_Prui   +   HELPWleafD3i
                    
                    #  The Dry Matter of the plants is affected by the pruned dead leaf weight for the
                    #  three separate plants. So, WleafDi must be subtracted from the DM states as well.
                    HELPDM1i                = DM1
                    HELPDM2i                = DM2
                    HELPDM3i                = DM3
                    NEWVALUE DM1            = HELPDM1i       - HELPWleafD1i
                    NEWVALUE DM2            = HELPDM2i       - HELPWleafD2i
                    NEWVALUE DM3            = HELPDM3i       - HELPWleafD3i
                    
                    #  The Shoot Dry Matter of the plants is also affected by the pruned dead leaf weight
                    #  for the three separate plants. So, WleafDi must be subtracted from the Wsh states
                    #  as well.
                    HELPWsh1i               = Wsh1
                    HELPWsh2i               = Wsh2
                    HELPWsh3i               = Wsh3
                    NEWVALUE Wsh1           = HELPWsh1i      - HELPWleafD1i
                    NEWVALUE Wsh2           = HELPWsh2i      - HELPWleafD2i
                    NEWVALUE Wsh3           = HELPWsh3i      - HELPWleafD3i
                    
                    HELPHarvestDMi          = HarvestDM
                    NEWVALUE HarvestDM      = HELPHarvestDMi + HELPWleafD1i + HELPWleafD2i + ...
                    HELPWleafD3i
                    #  Here in HarvestDM the dead leaves from the prunings are collected; in the harvest
                    #  part all harvested parts are collected (pseudostem, green leaves, dead
                                                               #  leaves and bunch of plant 1 (motherplant)). (This is done in the state-event.)
                    
                    HELPHarvestWLeafDi      = HarvestWLeafD
                    NEWVALUE HarvestWLeafD  = HELPHarvestWLeafDi + HELPWleafD1i + ...
                    HELPWleafD2i + HELPWleafD3i
                    #  In HarvestWLeafD the dead leaves from the prunings are collected for the 3 plants,
                    #  and at harvest the dead leaves from the motherplant.
                    
                    ###END Pruning TIME event
                    ENDEVENT
                    
                    ################################################################################################
                      
                      ### If a certain temperature sum has been reached for the mother plant, the
                    ### harvest of bunches takes place, sucker 1 becomes the new mother plant and
                    ### sucker 2 becomes the new first sucker. Furthermore, harvested leaves, in
                    ### terms of weight, and the dry matter of the stems and (PAL22Jun2012: part of the corm1) is accumulated in
                    ### a state variable to be used as mulch.
                    ### Technically,
                    ### the parameters TSUMi, LAIi and DMi, with i 1 to 3, have to be reinitialized
                    ### again. TSUM(i) = TSUM(i+1); LAI(i) = LAI(i+1); (i) = DM1(i+1) and i=1 is
                    ### harvested and i=3 is reset to its initial values, namely 0. Finally the
                    ### SWitches (SWemerg3 and SWstdm3) should be reset to 0.0.
                    
                    ### TSUMHARV is the temperature sum at harvest
                    EVENT
                    ZEROCONDITION TSUM1 - TSUMHARV
                    PARAMETER TSUMHARV      = 3600.0
                    
                    ###Balance section
                    ###PAL
                    #   HELPBalDmHlp3           = BalDmHlp3
                    #   NEWVALUE BalDmHlp3      = HELPBalDmHlp3       - (HELPDM1 - HELPWrt1)
                    HELPBalDmHlp3           = BalDmHlp3
                    NEWVALUE BalDmHlp3      = HELPBalDmHlp3       - (HELPDM1 - (HELPWrt1+HELPWcorm1))
                    ###PAL-end
                    
                    ###Development section  (Temperature sum)
                    HELPTSUM2               = TSUM2
                    HELPTSUM3               = TSUM3
                    NEWVALUE TSUM1          = HELPTSUM2
                    NEWVALUE TSUM2          = HELPTSUM3
                    NEWVALUE TSUM3          = 0.0
                    NEWVALUE SWemerg3       = 0.0
                    
                    ###State variables section (Leaf area index, dry matter, roots and shoot,
                                                ###shoot parts, corms, pseudostems, leaves, bunches, ...)
                    #  Leaf area for the three separate plants
                    HELPLAI1                = LAI1
                    HELPLAI2                = LAI2
                    HELPLAI3                = LAI3
                    NEWVALUE LAI1           = HELPLAI2
                    NEWVALUE LAI2           = HELPLAI3
                    NEWVALUE LAI3           = LAI3I
                    
                    #  Total dry matter for the three separate plants: the corm and the roots
                    #  of the plants are not harvested, but stay in the soil. Therefore, the
                    #  new value of DM1 is not only taking over the DM of plant 2 (sucker 1)
                    #  but keeps its own roots and corm.
                    HELPDM1                 = DM1
                    HELPDM2                 = DM2
                    HELPDM3                 = DM3
                    ###PAL: Perhaps DM! must be adapted now we have Wrt1 and Wcorm1 apart??
                      #   NEWVALUE DM1            = HELPDM2 + HELPWrt1  +  HELPWcorm1
                    #18jun2012: Since DM1 must also reflect what goes out or not, the above statement
                    #should be good, and not the statement NEWVALUE DM1 = HELPDM2
                    #############################################################################split (KN)
                    NEWVALUE DM1            = HELPDM2
                    NEWVALUE DM2            = HELPDM3
                    NEWVALUE DM3            = DM3I
                    
                    #  Total dry matter for the three separate plants is split into roots and
                    #  shoot
                    #  Root
                    HELPWrt1                = Wrt1
                    HELPWrt2                = Wrt2
                    HELPWrt3                = Wrt3
                    HELPWrt1_After_Harv     = Wrt1_After_Harv
                    ###PAL:
                      #At harvest The plants, including their roots are shifted. The roots of plant 1 were
                    #assumed to stay on the plant and the Wrt2 was added: NEWVALUE Wrt1 = HELPWrt2 + HELPWrt1.
                    #But now, (5Aug2009) we decided that the roots of plant 1 would decay after the harvest
                    #of the motherplant. So, we should write NEWVALUE Wrt1 = HELPWrt2, and HELPWrt1 should
                    #go to a new state, which you called: "Wrt1_Harv". I would like to call it
                    #"Wrt1_after_Harvest", and in fact it is (in a later phase of model development) to be
                    #named Soil Organic Matter SOM.
                    ###PAL-end
                    #   NEWVALUE Wrt1           = HELPWrt2            + HELPWrt1
                    NEWVALUE Wrt1           = HELPWrt2
                    NEWVALUE Wrt2           = HELPWrt3
                    NEWVALUE Wrt3           = Wrt3I
                    NEWVALUE Wrt1_After_Harv= HELPWrt1_After_Harv + HELPWrt1
                    
                    #  Shoot
                    HELPWsh1                = Wsh1
                    HELPWsh2                = Wsh2
                    HELPWsh3                = Wsh3
                    #  In the situation before splitting up the plant parts in corm, pseudostem, leaves
                    #  and bunch, the balances BalRtShHlp1 and BalRtShHlp4 were correct if
                    #  "NEWVALUE Wsh1 = HELPWsh2" was defined (see also text near the definition
                                                               #  of these balances). Because not the whole of the shoot is harvested, however,
                    #  but instead the corm is remaining on the plant, the new shoot is shoot for plant 2
                    #  plus corm of plant 1 (corm1).
                    #   NEWVALUE Wsh1           = HELPWsh2 Outcommented after splitting up in plant parts.
                    ###PAL:
                      #The corm1 is now gradually going towards Wsh1, and not anymore at once.
                    #   (PAL22Jun2012: the corm is partly reallocated to the growth rate of the DM1 that has just
                         #    become motherplant.)
                    #   NEWVALUE Wsh1           = HELPWsh2            + HELPWcorm1
                    
                    NEWVALUE Wsh1           = HELPWsh2
                    ###PAL-end
                    NEWVALUE Wsh2           = HELPWsh3
                    NEWVALUE Wsh3           = Wsh3I
                    
                    #  Total SHOOT dry matter for the three separate plants is split into corms,
                    #  pseudostems, green leaves and bunches. There is also dead leaves, but these come
                    #  from the green leaves that died. The bunch, of course, only applies to the motherplant.
                    #  First all HELP variables are defined
                    HELPWcorm1              = Wcorm1
                    HELPWcorm2              = Wcorm2
                    HELPWcorm3              = Wcorm3
                    HELPWcorm1H             = Wcorm1
                    HELPCorm1_After_Harv    = Corm1_After_Harv
                    HELPCorm1_Lost_After_Harv    = Corm1_Lost_After_Harv
                    
                    #   HELPCorm1_to_Wsh1       = Corm1_to_Wsh1
                    #   (PAL22Jun2012 line HELPCorm1_Lost_After_Harv    = Corm1_Lost_After_Harv introduced.)
                    ################################################################################(ADDED : KN)
                    
                    HELPWpsstm1             = Wpsstm1
                    HELPWpsstm2             = Wpsstm2
                    HELPWpsstm3             = Wpsstm3
                    
                    HELPWleaf1              = Wleaf1
                    HELPWleaf2              = Wleaf2
                    HELPWleaf3              = Wleaf3
                    
                    HELPWleafD1             = WleafD1
                    HELPWleafD2             = WleafD2
                    HELPWleafD3             = WleafD3
                    HELPTotWleafD           = TotWleafD
                    
                    HELPWbunch1             = Wbunch1
                    
                    HELPWrtD1_Tot           = WrtD1_Tot
                    HELPWrtD2_Tot           = WrtD2_Tot
                    HELPWrtD3_Tot           = WrtD3_Tot
                    
                    
                    #   NEWVALUE Corm1_to_Wsh1 =  0.0
                    NEWVALUE WrtD1_Tot      = HELPWrtD2_Tot
                    NEWVALUE WrtD2_Tot      = HELPWrtD3_Tot
                    NEWVALUE WrtD3_Tot      = 0.0
                    ############################################################( added KN )
                    
                    #  Then all resettings are arranged. Corm is not harvested.
                    #   NEWVALUE Wcorm1         = HELPWcorm2 + HELPWcorm1
                    NEWVALUE Wcorm1         = HELPWcorm2
                    
                    ###PAL:
                      ##Corm1_After_Harv = Corm1_After_Harv + Wcorm1, via the HELP construction
                    #This should be treated similarly to the Wrt1, I guess.
                    #Further "NEWVALUE Wcorm1 = HELPWcorm2 + HELPWcorm1" must become
                    # "NEWVALUE Wcorm1 = HELPWcorm2" only.
                    #   NEWVALUE Corm1_After_Harv = HELPCorm1_After_Harv + HELPWcorm1
                    #   NEWVALUE Corm1_After_Harv =  HELPWcorm1
                    
                    NEWVALUE Corm1_After_Harv = HELPCorm1_After_Harv + ( pcorm1_To_ReAllocation # HELPWCorm1 )
                    NEWVALUE Corm1_Lost_After_Harv = HELPCorm1_Lost_After_Harv + ( (1.0 - pcorm1_To_ReAllocation) # HELPWCorm1 )
                    
                    NEWVALUE Wcorm1         = 0.0
                    NEWVALUE Wcorm2         = HELPWcorm3
                    NEWVALUE Wcorm3         = Wcorm3I
                    
                    NEWVALUE Wpsstm1        = HELPWpsstm2
                    NEWVALUE Wpsstm2        = HELPWpsstm3
                    NEWVALUE Wpsstm3        = Wpsstm3I
                    
                    NEWVALUE Wleaf1         = HELPWleaf2
                    NEWVALUE Wleaf2         = HELPWleaf3
                    NEWVALUE Wleaf3         = Wleaf3I
                    
                    NEWVALUE WleafD1        = HELPWleafD2
                    NEWVALUE WleafD2        = HELPWleafD3
                    NEWVALUE WleafD3        = WleafD3I
                    
                    NEWVALUE Wcorm1H        = HELPWcorm1H
                    
                    ###PAL
                    #28 Jul 2009 discovered that TotWleafD is just reset to itself, but it should be
                    #set at harvest to what is still on the plant, which is WleafD2 + WleafD3 + WleafD3I
                    #that was formed just up to the harvest. This also means that at harvest the leaves
                    #of plant 2 and plant 3 are not pruned at harvest.
                    NEWVALUE TotWleafD      = HELPWleafD2 + HELPWleafD3 + WleafD3I
                    ###PAL-end
                    NEWVALUE Wbunch1        = Wbunch1I
                    
                    #  Calculating the Harvest Index at the moment of harvesting
                    HI1                     = HELPWbunch1/HELPWsh1
                    ###The harvest index as calculated from only the motherplant
                    HIoverall               = HELPWbunch1/WshTot
                    ###The harvest index as calculated from the whole of the plant, so mother +
                      ###sucker 1 + 2.
                    
                    #  Harvesting section (for later use as mulch). Also DM1 and Wsh1 will be involved.
                    #  The
                    #  part of DM1 that is no doubt involved is the shoot part (Wsh1). This amount
                    #  will have to be corrected for the the bunch weight.
                    #  The HarvestLAI will also contain the dead leaves that fall onto the soil.
                    #  This "harvest LAI" and "harvest DM" is not worked out yet.
                    #   HELPHarvestLAI         = HarvestLAI
                    #   NEWVALUE HarvestLAI    = HELPHarvestLAI       + HELPLAI1
                    
                    HELPHarvestDM           = HarvestDM
                    HELPDMmulch             = DMmulch
                    ###PAL Reconsider these harvests
                    ###PAL #####DID I FORGET CORM???? No because it is not harvested.
                    HELPHarvestWpsstm       = HarvestWpsstm
                    HELPHarvestWLeaf        = HarvestWLeaf
                    HELPHarvestWLeafD       = HarvestWLeafD
                    HELPHarvestWbunch       = HarvestWbunch
                    
                    #   NEWVALUE HarvestDM      = HELPHarvestDM       + (HELPWsh1 - HELPWcorm1 + HELPWleafD1)
                    ###PAL
                    #New formulation that is easier to understand:
                      NEWVALUE HarvestDM      = HELPHarvestDM       + (HELPWpsstm1 + HELPWleaf1 + ...
                                                                       HELPWleafD1 + HELPWbunch1)
                    
                    #####################################################################################(ADDED : KN)
                    ###PAL:
                      #Prunings shifted to allow proper balancing of individual plant shoots
                    ###PAL-end
                    HELPHarvestDM1_Pru      =  HarvestDM1_Pru
                    HELPHarvestDM2_Pru      =  HarvestDM2_Pru
                    HELPHarvestDM3_Pru      =  HarvestDM3_Pru
                    
                    NEWVALUE HarvestDM1_Pru =  HELPHarvestDM2_Pru
                    NEWVALUE HarvestDM2_Pru =  HELPHarvestDM3_Pru
                    NEWVALUE HarvestDM3_Pru =  HarvestDM_pru3I
                    ######################################################################################(ADDED : KN)
                    
                    ###PAL-end
                    #  In HarvestDM all harvested parts are contained, so pseudostem, green leaves, dead
                    #  leaves and bunch of plant 1 (motherplant) and the dead leaves from the prunings.
                    
                    NEWVALUE HarvestWLeaf   = HELPHarvestWLeaf    + HELPWleaf1
                    #  Also HELPWleafD2 and HELPWleafD3 must be added to the HarvestWLeaf, but that
                    #  should be done during the dynamic part of the simulation
                    NEWVALUE HarvestWpsstm  = HELPHarvestWpsstm   + HELPWpsstm1
                    NEWVALUE HarvestWbunch  = HELPHarvestWbunch   + HELPWbunch1
                    #  Having available these three separate harvested components (psstm, green leaves,
                                                                                   #  dead leaves) enables us to introduce their own decomposition rates (if desired).
                    ###PAL-end
                    
                    NEWVALUE HarvestWLeafD  = HELPHarvestWLeafD   + HELPWleafD1
                    
                    
                    NEWVALUE DMmulch        = HELPDMmulch         + ...
                    (HELPWpsstm1 + HELPWleaf1 + HELPWleafD1)
                    #  In DMmulch all harvested parts of plant 1 accumulate, so the (chopped)
                    #  pseudostem1, green leaves1 and dead leaves1.
                    #  Note that at harvest to the DMmulch is added the Wpsstm1, Wleaf1 and WleafD1,
                    #  but not the WleafD2 and WleafD3. These are only added (together with WleafD1)
                    #  at the prunings.
                    
                    #  The Mulch Area Index (MAI) can be calculated for instance according to the
                    #  paper of Marc Corbeels and will be used in retarding the evaporation of
                    #  water from the soil. Also a certain amount of water will be intercepted by
                    #  this mulch, thus not reaching the soil surface.
                    #  IS THIS STILL TRUE? Check: the (DMmulch + HarvestWbunch) should equal (HarvestDM).
                    
                    #Resetting the switches
                    NEWVALUE SWstdm3        = 0.0
                    NEWVALUE SWFSU2         = 1.0
                    NEWVALUE SWFrt1         = 1.0
                    
                    ENDEVENT
                    
                    ###PAL:
                      #If you would do what is stated below, there are two decompositions of the roots:
                      #One of root 1 during growth of the motherplant and one of the motherplant +
                      #sucker 1 that has just become motherplant. It should be such that the HELPWrt1
                    #is put into this new state Wrt1_Harv (let's call it Wrt1_After_Harv), while the
#sucker 1 that has just become motherplant takes with it its own roots.
#In fact "Wrt1_After_Harv" is SOM (Soil Organic Matter), that decays = connection
#with nutrients later.
#at harvest
###PAL-end

END

#WEATHER ISTN=1; ...
#      IYEAR=2007

#END

#WEATHER ISTN=2; ...
#      IYEAR=2006

#END

#WEATHER ISTN=2; ...
#      IYEAR=2007

#PARAMETER IRRIGF = 1.
END
#PARAM DELTMP = 3.0
PARAM K1 = 0.35; K2 = 0.35; K3 = 0.35
#END
###NOTE: If IRRIGF = 1.0, the WCI must be taken as WCFC, to correctly start the calculations.
#PARAM DELTMP = 0.0; IRRIGF = 1. ; WCI = 0.33

#END
#PARAM DELTMP = 3.0
#END
STOP

####ADDED: GT

# ---------------------------------------------------------------------#
#  SUBROUTINE PENMAN                                                   #
#  Purpose: Computation of the PENMAN EQUATION                         #
# ---------------------------------------------------------------------#
#23456789012345678901234567890123456789012345678901234567890123456789012
      SUBROUTINE PENMAN(DAVTMP,VP,DTRPEN,WN,ECCOFC,LAI1,LAI2,LAI3,
     $                  ECCOFM,MAI,ALBS,ALBC,RINTC1,RINTC2,RINTC3,
     $                  PEVAPS,PEVAPM,PTRAN1,PTRAN2,PTRAN3,RLWN)
      IMPLICIT REAL (A-Z)

      DTRJM2 = DTRPEN # 1.E6  # 1.E4
      BOLTZM = 5.668E-8       # 1.E4
      LHVAP  = 2.4E6
      PSYCH  = 0.067

      BBRAD  = BOLTZM # (DAVTMP+273.)##4 # 86400.
      SVP    = 0.611 # EXP(17.4 # DAVTMP / (DAVTMP + 239.))
      SLOPE  = 4158.6 # SVP / (DAVTMP + 239.)##2
      RLWN   = BBRAD # MAX(0.,0.55#(1.-VP/SVP))

      NRADS  = DTRJM2 # (1.-ALBS) - RLWN
      NRADC  = DTRJM2 # (1.-ALBC) - RLWN

      PENMRS = NRADS # SLOPE/(SLOPE+PSYCH)
      PENMRC = NRADC # SLOPE/(SLOPE+PSYCH)
      WDF    = 2.63 # (1.0 + 0.54 # WN)     # 1.E4
      PENMD  = LHVAP # WDF # (SVP-VP) # PSYCH/(SLOPE+PSYCH)

      PEVAPS = EXP(-ECCOFC # LAI1) # EXP(-ECCOFC # LAI2) #
     $         EXP(-ECCOFC # LAI3) # EXP (-ECCOFM # MAI) #
     $         (PENMRS + PENMD) / LHVAP

      PEVAPS = MAX(0., PEVAPS)

      PEVAPM = EXP(-ECCOFC # LAI1) # EXP(-ECCOFC # LAI2) #
     $         EXP(-ECCOFC # LAI3) # (1. - EXP(-ECCOFM # MAI)) #
     $        (PENMRS + PENMD) / LHVAP

      PEVAPM = MAX(0., PEVAPM)

      PTRAN1  = ( 1.-EXP(-ECCOFC#LAI1) ) # (PENMRC + PENMD) / LHVAP
      PTRAN1  = MAX(0., PTRAN1-0.5 # RINTC1)

      PTRAN2  = EXP(-ECCOFC # LAI1) # ( 1. - EXP(-ECCOFC # LAI2) ) #
     $          (PENMRC + PENMD) / LHVAP
      PTRAN2  = MAX(0., PTRAN2-0.5 # RINTC2)

      PTRAN3  = EXP(-ECCOFC#LAI1)# EXP(-ECCOFC#LAI2) #
     $          ( 1. - EXP(-ECCOFC#LAI3) ) # (PENMRC + PENMD) / LHVAP
      PTRAN3  = MAX(0., PTRAN3-0.5 # RINTC3)

      RETURN
      END
# ---------------------------------------------------------------------#
#  SUBROUTINE EVAPTR                                                   #
#  Purpose: To compute actual rates of evaporation and transpiration   #
# ---------------------------------------------------------------------#
#234567
      SUBROUTINE EVAPTR(PEVAPS,PTRAN1,PTRAN2,PTRAN3,
     $                  ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST,
     $                  TRANCO,DELT,
     $                  WCCR,EVAPS,TRAN1,TRAN2,TRAN3,FR)
      IMPLICIT REAL (A-Z)

      WC   = 1.E-4 # 0.001 # WA   / ROOTD
      WAAD = 1.E4  # 1000. # WCAD # ROOTD
###Peter 8Okt2014: WAFC is not used in this routine, so taken out
#      WAFC = 1000. # WCFC # ROOTD

#BEGIN GT 24 Sept 2014: PTRANT was not transferred to the subroutine.

      PTRANT = PTRAN1 + PTRAN2 + PTRAN3

#END GT

      EVAPS  = PEVAPS # LIMIT( 0., 1., (WC-WCAD)/(WCFC-WCAD) )

      WCCR = WCWP + MAX( 0.01, PTRANT/(PTRANT+TRANCO) # (WCFC-WCWP) )
        IF (WC.GT.WCCR) THEN
             FR = LIMIT( 0., 1., (WCST-WC)/(WCST-WCWET) )
        ELSE
             FR = LIMIT( 0., 1., (WC-WCWP)/(WCCR-WCWP)  )
        ENDIF
      TRAN1 = PTRAN1 # FR

      TRAN2 = PTRAN2 # FR

      TRAN3 = PTRAN3 # FR

# BEGIN Peter22Sept2014:TRANT was not transferred to this routine. I calculate it here locally

      TRANT  = TRAN1 + TRAN2 + TRAN3
      AVAILF = MIN( 1., ((WA-WAAD)/DELT)/NOTNUL(EVAPS+TRANT) )

# END Peter22Sept2014:

      EVAPS = EVAPS # AVAILF
      TRAN1 = TRAN1 # AVAILF
      TRAN2 = TRAN2 # AVAILF
      TRAN3 = TRAN3 # AVAILF

      RETURN
      END
# ---------------------------------------------------------------------#
#  SUBROUTINE DRUNIR                                                   #
#  Purpose: To compute rates of drainage, runoff and irrigation        #
# ---------------------------------------------------------------------#
#234567
      SUBROUTINE DRUNIR(INFILT,EVAPS,TRANT,IRRIGF,
     $                  DRATE,DELT,WA,ROOTD,WCFC,WCST,
     $                  DRAIN,RUNOFF,IRRIG)
      IMPLICIT REAL (A-Z)

      WAFC = 1.E4 # 1000. # WCFC # ROOTD
      WAST = 1.E4 # 1000. # WCST # ROOTD

      DRAIN  = LIMIT( 0., DRATE, (WA-WAFC)/DELT +
     $               (INFILT - EVAPS - TRANT)  )

      RUNOFF = MAX( 0., (WA-WAST)/DELT +
     $               (INFILT - EVAPS - TRANT - DRAIN)  )

      IRRIG  = IRRIGF # MAX( 0., (WAFC-WA)/DELT -
     $               (INFILT - EVAPS - TRANT - DRAIN - RUNOFF) )

      RETURN
      END


# ---------------------------------------------------------------------#

####STOP: GT
####PAL24Feb2015
# -----------------------------------------------------------------------------#
#  FUNCTION TRARFfu                                                            #
#  Purpose: This Function computes the TRANFi and prevents the zero devision   #
# -----------------------------------------------------------------------------#

      FUNCTION TRARFfu (CHOICE_Trans_or_Photo, FR, TRAN, PTRAN)
      IMPLICIT REAL (A-Z)
      SAVE

      IF (PTRAN.GT.0.0) THEN
###      This is the normal situation where no zero division can take place due to
###      PTRAN. The FR will be: FR.GE.0.0 .AND. FR.LE.1.0. If FR = 0.0 TRAN will
###      be 0.0 as well and growth is stopped.
         TRARF = TRAN/PTRAN
      ELSE
###      Here PTRAN = 0.0, and that would lead to a zero division in the
###      original formulation. This situation is comparable with FR=0.0,
###      because TRAN = FR # PTRAN, so if either FR or PTRAN equals zero, the
###      outcome TRAN would be zero. So should the TRARF yield zero? Does no
###      transpiration mean no growth? One can also reason: If there
###      is enough water in the soil, but the PENMAN calculation yields 'no
                                                           ###      potential transpiration', there can still be some photosynthesis
###      because at least there is light. Then it is better to not set TRARF to
###      zero, but, for example, to the fraction FR. Situation: enough water in
###      the soil, no PTRAN and thus no TRAN, but some photosynthesis because of
###      light. In principle (and in reality) some photosynthesis could also be due to
###      reserves. Reserves are, however, not incorporated in the model.
###         I have made this choice between "no transpiration -> no growth" and
###         "no transpiration but still some growth" possible, because it is a
###         different hypothesis, that needs support by literature to make a final
###         choice.
###         If the parameter CHOICE_Trans_or_Photo = 0.0: "no transpiration -> no growth"
###         If the parameter CHOICE_Trans_or_Photo = 1.0: "no transpiration -> still some growth"
###         (in fact CHOICE_Trans_or_Photo may be any other value than 0.0 to obtain
###         the second option)
            IF ( NINT(CHOICE_Trans_or_Photo) .EQ. 0) THEN
               TRARF = 0.0
            ELSE
               TRARF = FR
            ENDIF
      ENDIF

      TRARFfu    = TRARF

      Return
      END FUNCTION TRARFfu

# -----------------------------------------------------------------------------#
#  SUBROUTINE GLA                                                              #
#  Purpose: This subroutine computes daily increase of leaf area index         #
#           ( ha leaf ha-1 ground d-1 )                                        #
# -----------------------------------------------------------------------------#

      SUBROUTINE GLA(SLA,Pleaf,GWsh,SWstdm3,TSUM,TSUM3stop_exp,LAI,
     $               LAIstop_exp,RGRL,DTEFF,                  GLAI)
      IMPLICIT REAL (A-Z)
      INTEGER  ISWstdm3
      SAVE

#---- Growth during pre-juvenile stage and during maturation stage is given by:
#---- GLAI=SLA#Pleaf#GWsh (formerly: GLAI=SLA#LWR#GDM). Note that (only) GDM3
#---- can be calculated by two different processes (DM coming from sucker 1
#---- (pre-juvenile stage) or due to its own photosynthesis). The photosynthesis
#---- part (in fact only DM production) is taken care of in this subroutine, the
#---- support from sucker 1 is described in the main program.
#---- Below, GLAI is always first calculated from shoot dry matter, and if
#---- juvenile conditions are present this value is over written by the GLAI
#---- calculated in the IF condition.

      GLAI     = SLA # Pleaf # GWsh

      ISWstdm3 = NINT(SWstdm3)
#---- ISWstdm3 is introduced to prevent numerical problems in the .EQ.
#---- comparison in the below IF-statement.

#---- Growth during the juvenile stage (at the moment, the value of which is
#---- TSUM3stop_exp = 960.).
#---- Juvenile growth is exponential: dLAI/dt=Rgr x LAI, with Rgr=RGRL x DTEFF.
#---- Technical remark: the differential equation form is chosen instead of the
#---- analytical rate equation, because DELT occurs in the analytical formulation.
#---- This might give problems at the TSUM-events where the time step necessarily
#---- is adapted. If this adaptation would not be correctly implemented in the
#---- analytical rate equation, this would give erroneous results.
#---- The time coefficient, 1/Rgr = 1/(RGRL # DTEFF), also allows to use the
#---- differential form: it is around 1/(0.0077 Cd x 20 C)= 6.5 days, so the
#---- time step should be less or equal than 6.5/10=0.65 days: it is, however,
#---- 1 day, so a little worse that the rule of thumb, but still acceptable.
#---- A closer investigation, however, yielded that this time step, in
#---- combination with Euler, did not give converging results and it was decided to
#---- use the Runge-Kutta method to integrate the rates, with a time step of 0.25.

      IF ( (ISWstdm3 .EQ. 1) .AND. (TSUM .LT. TSUM3stop_exp) .AND.
     $     (LAI      .LT. LAIstop_exp) )
     $   GLAI  = (RGRL # DTEFF) # LAI

      RETURN
      END

##########################################################################################

      FUNCTION Fsu2(FSU2max,TSUMfloinit,TSUMendfulldepe,TSUM)
      IMPLICIT REAL (A-Z)
      SAVE

#---- This function represents a straight line defining the linear decrease of
#---- the support of sucker 2 by sucker 1 during the physiological time period
#---- of sucker 1 from TSUM2 = 1662 (corresponds with TSUM3=360 Cd) to
#---- TSUM2 = 2298, where harvesting and shifting takes place.
#---- After that physiological moment of 2298 Cd, sucker 1 is motherplant and
#---- sucker 2 has become sucker 1, and the support of (now) sucker 1 continues
#---- up to flower initiation of (now) the motherplant at 2423 Cd.

      Fsu2     = -FSU2max/(TSUMfloinit-TSUMendfulldepe) # (TSUM - TSUMfloinit)

      Return
      END FUNCTION Fsu2

#########################################################################################

      FUNCTION prt1(TIME,STTIME,prt_wish,psh_wish,Frtmax,STGRLV,TSUM3I,TSUM3)
      IMPLICIT REAL (A-Z)
      COMMON /SLOPE/ Afixed
      SAVE

      prt_cal  = prt_wish / (psh_wish + prt_wish)
#---- prt_cal is the average amount of the newly grown dry matter that goes to
#---- the roots. It is calculated from the rt:sh ratio that is given in the main
#---- program from prt_wish and psh_wish. Since the total crop is the sum of the
#---- root and shoot, prt_cal is calculated as shown.

      IF ( Frtmax .GT. (1.81#prt_cal) ) CALL FATALERR
     &   ('prt1','Frtmax > prt_cal')
#---- The below function, prt1 = A # TSUM3 + B, is a straight line that is
#---- calculated "around" the average value prt_cal. This average value is the
#---- root:shoot ratio that is wished to be reached at the end of the 360 Cd
#---- period and coincides with measured data. It was found, however, that
#---- during the 0-360 Cd period the root:shoot ratio of sucker 2 of a banana
#---- plant increases. So, if a known average need to be reached at the end of
#---- the 360 Cd period, the function should start with a lower value and should
#---- end with a higher value than the average. Therefore, the reference values
#---- of the function were taken in the middle of the trajectory 0-360 Cd, so at
#---- 180 Cd, because in the middle the coordinates (x,y) are known, and at the
#---- end of the period of 360 Cd, where the value is called Frtmax.
#---- Obviously, Frtmax should not be larger than 2#prt_cal, because in that
#---- case the the start of the function would be lower than 0.0. Therefore, the
#---- FATALERR was introduced in this routine and works under the condition:
#---- Frtmax > 2.0#prt_cal. It may be clear that Frtmax may be:
#---- prt_cal <= Frtmax <= (2 # prt_cal).
#---- The value of 2.0# appears to give (very small) negative values (10^-9),
#---- due to rounding of.
#---- Therefore, the more practical value of 1.81 is taken, instead of 2.0, as
#---- maximum that is allowed to be reached by prt_cal. The 0.01 in 1.81 was
#---- apparently needed for rounding purposes, because a Factor of 1.8 gave
#---- problems.

      IF ( NINT(TIME) .EQ. NINT(STTIME) ) THEN
         Afixed  = (Frtmax-prt_cal) / (STGRLV-(STGRLV-TSUM3I)/2.0)
      ENDIF
#---- Afixed defines slope A of the line prt1 = A # TSUM3 + B. It is called
#---- Afixed because it should be possible to shift the line parallelly to its
#---- starting position up and down (but not beyond its starting position) as a
#---- function of water stress. The dynamics of the parallel shift is
#---- effectuated via intercept B, that is a linear function of prt_cal:

      B        = prt_cal - (Afixed # (STGRLV-TSUM3I)/2.0)

      prt1     = Afixed # TSUM3 + B

      Return
      END FUNCTION prt1

############################################################################################

      FUNCTION prt2 (prt_wish,psh_wish,TSUM3I,STGRLV,TSUM,TSUMflower)
      IMPLICIT REAL (A-Z)
      COMMON /SLOPE/ Afixed
      SAVE

      prt_cal  = prt_wish / (psh_wish + prt_wish)

      B        = prt_cal - (Afixed # (STGRLV-TSUM3I)/2.0)
#---- The function prt2 should start at the same value as function prt1 ends.
#---- Function prt1 ends at the intercept value B of prt1 plus the slope of prt1
#---- (Afixed) times the distance between the start of function prt2 (is the
#---- same as the end of function prt1) and the beginning of prt1, so
#---- (STGRLV-TSUM3I).
#---- In prt1 the slope was fixed by calculating Afixed once at time = 0. In
#---- function prt2, the end point must be fixed (at TSUMflower partitioning to
#---- roots is zero), and the slope must be dynamically adapted by making Frtmax
#---- at the end of prt1 equal to { B + Afixed#(STGRLV-TSUM3I) }, where B is
#---- dynamic through prt_cal. The labelled COMMON/SLOPE/ transfers variable
#---- Afixed from function prt1 to prt2.

      prt2     = -1.0#(B + Afixed#(STGRLV-TSUM3I))/(TSUMflower-STGRLV)#
     $                                           (TSUM-TSUMflower)

      Return
      END FUNCTION prt2

############################################################################################

      SUBROUTINE INTERPOL(Name1,Name2,Name3,Name4,
     $                    TABLE1,TABLE2,TABLE3,TABLE4,
     $                    N,TIME,         Y1,Y2,Y3,Y4)
      IMPLICIT NONE
      CHARACTER(LEN=#)    :: Name1,Name2,Name3,Name4
      INTEGER             :: N
      REAL, DIMENSION(N)  :: TABLE1,TABLE2,TABLE3,TABLE4
      REAL                :: TIME, Y1, Y2, Y3, Y4

#     local
      REAL :: LINT2

      Y1 = LINT2 (Name1,TABLE1,N, TIME)
      Y2 = LINT2 (Name2,TABLE2,N, TIME)
      Y3 = LINT2 (Name3,TABLE3,N, TIME)
      Y4 = LINT2 (Name4,TABLE4,N, TIME)

      RETURN
      END

#########################################################################################
ENDJOB

##PARAM pcorm1_to_Wsh1 = 0.5 ?????

#done
##Corm1_After_Harv = INTGRL( Corm1_After_HarvI , dCorm1_After_Harv )
##dCorm1_After_Harv= - RDRCorm1_After_Harv # Corm1_After_Harv
##INCON Corm1_After_HarvI = 0.0
##PARAM RDRCorm1_After_Harv = 0.038
#end done

#done
#In the Harvest section:
##Corm1_After_Harv = Corm1_After_Harv + Wcorm1, via the HELP construction
#This should be treated similarly to the Wrt1, I guess.
#end done

#done
#Near the Corm1 section:
##Corm1_Lost_After_Harv = INTGRL( Corm1_Lost_After_HarvI , dCorm1_Lost_After_Harv)
##dCorm1_Lost_After_Harv = (1.0 - pcorm1_to_Wsh1) # dCorm1_After_Harv
##INCON Corm1_Lost_After_HarvI = 0.0
#The rate dCorm1_Lost_After_Harv can perhaps be added to the rate by which
#Wrt1_After_Harv decreases, if the decomposition rates are similar.
#end done