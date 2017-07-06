PROGRAM geff
!--------------------------------------------------------- 
! GEFF: ECMWF Fire Forecast model .
!
! Di Giuseppe F. 2014, ECMF
! nen@ecmwf.int
!
!---------------------------------------------------------
! --- order of code ---

  ! 0- set-up conditions 
  !---------------------------------------------------------------------------
     ! 0.1 weather type for the pixel 
     ! 0.2 fuel model for the pixel based on the JRC climatological maps
     ! 0.3 climate class for the pixel
     ! 0.4 vegetation stage  
     ! 0.5 mean slope

!!A)  NFDRS 
  ! 1. calculate moisture/loading content for the various 
  !       fuel models (prognostic part)
  !-----------------------------------------------------------------------------
     ! 1.1 --- 1hr fuel
     ! 1.2 --- 10hr fuel
     ! 1.3 --- 100hr fuel
     ! 1.4 --- 1000hr fuel
     ! 1.5 --- herbaceous and wood     
  ! 2- Fire Model indeces (diagnostic part)
  !---------------------------------------------------------------------------     
     !2.1 Fire characteristics /properties (fire_prop)
       !2.1.1 Spread component
       !2.1.2 Energy Release
       !2.1.3 Burning Index
    !2.2 Fire occurrence probability (fire_prob) 
       !2.2.1 ignition probability
       !2.2.2 Human caused fire occurrence index
       !2.2.3 Lightning-caused fire occurrence index
       !2.2.4 Fire load index
    !2.3 Mask outputs for wet/snow/dew  conditions 
       !--------------------

!!B)  MARK-5

!!C)  FWI
  ! 1 The fine fuel moisture code
  ! 2 The duff moisture /content code
  ! 3 The Drought Code 
  !---------------------------------------------------------------------------     
  ! 4 Initial Spread Index
  ! 5 Buildup Index 
  !----------------------------------------------------------------------------
!! D- Netcdf output
  !----------------------------------------------------   
! --- release history --- 
! v1.0 MARCH 2014 - First release of the model 
! v1.2 AUGUST 2014 - Added the FWI calculation
! v1.3 FEB 2017 - Corrected bug in the keetch byram calculation
!      MAY 2017          Chnaged the way the land see mask is handled to allow for the small islands to be included 
!============================================================================================
!
  USE netcdf
  USE mo_constants
  USE mo_control
  USE mo_fire
  USE mo_nfdrs
  USE mo_mark5
  USE mo_fwi
  USE mo_ncdf_tools
  USE mo_fuelmodel
  USE mo_vegstage
  IMPLICIT NONE

! Local type 
  TYPE (fuelmodel_type):: fuelmodel
  TYPE (vegstage_type) :: vegstage
  ! local integers 
  INTEGER :: istep,icheck,idate,iday,ix,iy,ii
  INTEGER ::  iweather,iclima,ilightning,jslope
  INTEGER(4) :: actualdate 
  REAL:: daylit

  ! local real scalars for prognostic calculations
  REAL :: zrain, ztemp, zmaxtemp,  zrainclim &
&           ,zmintemp,  zcc, zwspeed, zdp&
&           ,zrh, zminrh, zmaxrh, zsnow, zemc &
&           ,zbndryh,zbdybar,diff,zlat
  REAL :: &
       & zemcpr,zminem,zmaxem,zemcbar,mcwoodp,mcherbp,&
       & wherbp,fctcur=0
  REAL :: &
       tempa,rhb

  REAL:: gren
  
  !local real scalar for diagnostic calculations 

  REAL :: wtotd,wtotl,w1p,wtot,w1n,w10n,w100n,w1000n,& 
       &  wherbn,wwoodn, rhobed,rhobar,betbar,etasd,etasl,&
       &  hn1,hn10,hn100,hn1000,hnherb,hnwood, wrat,sa1,sa10,sa100,saherb,&
       &  sawood,sadead,salive,f1,f10,f100,fherb,fwood,fdead,flive,wdeadn,wliven,&
       &  sgbrd,sgbrl,sgbrt,betop,gmaop,ad,zeta,mclfe,mxl,wtmcd,wtmcl,etamd,etaml,&
       &  dedrt,livrt,b,c,e,ufact,phiwnd,slpfct,phislp,ir,htsink
  REAL :: f1e,f10e,f100e,f1000e,fherbe,fwoode,fdeade,flivee,wdedne,wlivne,&
       &  sgbrde,sgbrle,sgbrte,betope,gmamx,gmamxe,gmaope,ade,wtmcde,wtmcle,etamde,&
       &  etamle,dedrte,livrte,ire,tau,qign,chi,p_i,scn,p_fi,mrisk,lgtdur,&
       &  finsid,fotsid,raidur,fmf,icbar,lrisk,ws
  REAL :: netrainfall, kb_drought_factor
  REAL:: a1,a2,a3,a4,a5
  REAL :: lrsf !needs to be put in
  REAL:: rkwet,rktmp
  REAL:: KBDI_temp, Ep

  REAL::	mo, rf, kd, kw, &
       &	mr, mr0, mr1,&
       &	ed, ed0, ed1, ed2,&
       &	ko, ko0, ko1, ko2,&
       &	ew, ew0, ew1, ew2,&
       &	kl, kl0, kl1, kl2,&
       &         vv,rd, qo, qr,dr,&
       &	re, moo, mrr, bb, pr, k,&
       &	m, fw, fd, fwiB,ff, ff0, ff1,&
       &        dc0,dl,lf,fwind,uu,mm
 

 !
  ! local integer  scalars
  INTEGER :: jfueltype,jvs
  INTEGER :: jyear,jmonth,jday

! fortran timer functions
  REAL :: time1=0.0,time2=0.0
  LOGICAL :: lspinup, &          ! spin up period - turn off output
&            ltimer=.false.      ! turn the cpu timer on for the first timestep

 
  ! --------------------
  ! START OF ECFIRE CODE
  ! --------------------

  print *,'setup'

  CALL setup ! initial conditions for arrays

  DO istep=1,nrun

     IF (ltimer) CALL timer('go  ',icheck,time1,time2) !set up timer

     idate=ndate(istep)
     iday=istep*dt

     CALL ADD_DAY (date1,idate,actualdate)

     jyear=INT(actualdate/10000)
     jmonth=INT((actualdate-jyear*10000)/100.)
     jday=INT((actualdate-jyear*10000)-jmonth*100)

     PRINT*,'step ',istep, "actualdate", actualdate
  
     IF (MOD(iday,ndaydiag)==0) WRITE(iounit,*) 'date ',idate,"actualdate", actualdate

     ! read in met data timeslice
     !---------------------------
     CALL getdata(istep)
   
     !-----------------
     ! GRIDDED LOOP !!!
     !-----------------

     DO ix=1,nlon
        DO iy=1,nlat
           fctcur=0
           zrain=MAX(rrain(ix,iy),0.0) ! 1 day rainfall 
           ztemp=MAX(rtemp(ix,iy),0.0) ! temperature on a daily timestep 
           zmaxtemp=MAX(rmaxtemp(ix,iy),0.0) ! max daily temperature
           zmintemp=MAX(rmintemp(ix,iy),0.0) ! min daily temperature 
           zrh=MAX(rrh(ix,iy),0.0) ! relative humidity 
           zmaxrh=MAX(rmaxrh(ix,iy),0.0) ! max daily relative humidity 
           zminrh=MAX(rminrh(ix,iy),0.0) ! min daily relative humidity
           zsnow=rsnow(ix,iy) ! snow mask 
           zcc=MAX(rcc(ix,iy),0.0) ! cloud cover
           zwspeed=MAX(rwspeed(ix,iy),0.0) ! wind speed 
           zdp=MAX(rdp(ix,iy),0.0) !duration of precipiatation in hours
           jvs=MAX(ivs(ix,iy),0)   !vegetation stage 
           jslope=islope(ix,iy)
           zrainclim=MAX(rrainclim(ix,iy),0.0) ! for the Keetch-byram 
           zlat=lats(iy)                                                ! index the ammount of annual 
                                                           ! precipitation is limited to 2,032mm/year
           ilightning=0!MAX(MIN(ilal(ix,iy),6),0) ! lightining class limited to 6
          
           
           !TEST 1
           !
           !===============================NFDRS=======================
    
        !   actualdate=20150423
        !   ztemp=288.094444
        !   zrh=24.5
        !   zmaxtemp=288.316667
        !   zmintemp=276.094444
        !   zmaxrh=85.6
        !   zminrh=21.6
        !   zdp=0
        !   zsnow=0
        !   zrain=0
        !   ifm(ix,iy)=7
        !   zcc=0.38
        !   mc(ix,iy)%r100hr=14
        !   mc(ix,iy)%r1000hr=16
        !   zlat=45
        !   !Answer
           !mc(ix,iy)%r100hr=10.3
           !mc(ix,iy)%r1000hr=14.8
 
    !TEST 2
           !
           !===============================NFDRS=======================
    
           !actualdate=20021023
           !ztemp=289.816667
           !zrh=82
           !zmaxtemp=294.816667 
           !zmintemp=286.483333 
           !zmaxrh=97
           !zminrh=61
           !zdp=0
           !zsnow=0
           !ifm(ix,iy)=7
           !zcc=0.8
           !zwspeed=0.89
           !zrain=0.0
!
           !mc(ix,iy)%r100hr=10
           !mc(ix,iy)%r1000hr=13
           !Answer
           !mc(ix,iy)%r100hr=12
           !mc(ix,iy)%r1000hr=15.3
 

          
           !---------------------------------------
           ! ONLY  MODEL POINT IF 
           ! NOT A 100%  LAKE/SEA POINT 
           ! YES ALSO  ARTIC CLIMATE .AND. icr(ix,iy) .gt.  0
           ! fuel model IS DEFINED (i.e. one of the  20 valid fuel models)
           ! vegetation stage is defined  (this has been removed )
           !---------------------------------------

         !  bounding necessary since the land sea mask in the climatic zone
          !and the IFS land-sea mask are not the same! 
           IF (rlsm(ix,iy) .gt. 0.0000001  .AND. icr(ix,iy) .gt. 0 )  THEN
           
            ! 0- set-up conditions 
   !---------------------------------------------------------------------------
     ! 0.1 weather type for the pixel 

              IF (zcc .lt. 0.1)                        iweather=0
              IF (zcc .ge. 0.1 .and. zcc .lt. 0.5 )    iweather=1
              IF (zcc .ge. 0.5 .and. zcc .lt. 0.7 )    iweather=2
              IF (zcc .ge. 0.7                    )    iweather=3

     ! 0.2 fuel model for the pixel based on the JRC climatological maps
              !accordingly to the pixel fuel model the specific characteristic are loaded
      
              CALL define_fuelmodel(ifm(ix,iy) , fuelmodel )

 
              !All fuel loadings are converted to pounds per squarefoot by
              !multiplying the tons per acre value by 0.0459137=rtopoundsft2
              ! Only for the NFDRS
              
              fuelmodel%weight%r1hr      = fuelmodel%weight%r1hr   *  rtopoundsft2 
              fuelmodel%weight%r10hr     = fuelmodel%weight%r10hr  *  rtopoundsft2   
              fuelmodel%weight%r100hr    = fuelmodel%weight%r100hr *  rtopoundsft2 
              fuelmodel%weight%r1000hr   = fuelmodel%weight%r1000hr*  rtopoundsft2 
              fuelmodel%weight%rwood     = fuelmodel%weight%rwood  *  rtopoundsft2  
              fuelmodel%weight%rherb     = fuelmodel%weight%rherb  *  rtopoundsft2 

   
     ! 0.3 climate class for the pixel
              ! we wanto to run also on the artic /climate 
              !bounding necessary since the land sea mask in the climatic zone
              !and the IFS land-sea mask are not the same! 
   
              iclima=icr(ix,iy)
     ! 0.4 vegetation stage 
       
             CALL  define_vegstage(jvs,vegstage)
        
     ! 0.5 mean slope

            IF (jslope .EQ. 0) jslope =1 
           
!A)  NFDRS 
!=======================================================================================================
       
  ! 1- Moisture content calculation for the dead  live fuel 
  !---------------------------------------------------------------------------     
 
       tempa= (ztemp  *9./5.  -459.69)+rweathercorrection_temp(iweather)
       rhb=  zrh*rweathercorrection_rh(iweather)   
       CALL  emc(tempa,rhb,zemcpr)

       tempa= (zmaxtemp  *9./5.  -459.69)+rweathercorrection_temp(iweather)
       rhb=  zmaxrh*rweathercorrection_rh(iweather)  
       CALL  emc(tempa,rhb  ,zmaxem)
   
       tempa= (zmintemp  *9./5.  -459.69)+rweathercorrection_temp(iweather)
       rhb=  zminrh *rweathercorrection_rh(iweather)  
       CALL  emc(tempa,rhb  ,zminem)

     
          !1.1 --- 1hr fuel

       mc(ix,iy)%r1hr=1.03*zemcpr
     !1.2 --- 10hr fuel
       mc(ix,iy)%r10hr=1.28*zemcpr

       !Snow on the ground or  precipitation 
      IF ( zrain .gt. 1.5 .or. zsnow .eq. 1) THEN 
          ! take relative humidity to saturation
 
         tempa= (zmintemp  *9./5.  -459.69)+rweathercorrection_temp(iweather)
         rhb=  100.0
         CALL  emc(tempa,rhb,zminem)
 
         tempa= (zmaxtemp  *9./5.  -459.69)+rweathercorrection_temp(iweather)
         rhb=  100.0
         CALL  emc(tempa,rhb,zmaxem)
         ! and reset the values of 1hr and 10 hr to 35 %
         mc(ix,iy)%r1hr=35.0
         mc(ix,iy)%r10hr=35.0

      END IF

 
       !duration of daylight
       CALL CAL_DAYLIT (zlat,actualdate,daylit)
    
       ! weighted 24-hour average EMC
     
       zemcbar=(daylit*zminem +(24.0-daylit)*zmaxem)/24.0
       zbndryh=((24-zdp)*zemcbar+zdp*(0.5*zdp+41.0))/24.0
    
       !update value for MC100
  
   !1.3 --- 100hr fuel
       mc(ix,iy)%r100hr=MAX(mc(ix,iy)%r100hr,0.0)
       mc(ix,iy)%r100hr=mc(ix,iy)%r100hr+(zbndryh-mc(ix,iy)%r100hr)*(1.0-0.87*EXP(-0.24))
      
       
     !1.4 --- 1000hr fuel
       !
   
    
       mc(ix,iy)%rbndryt=(((24-zdp)*zemcbar+zdp*(2.5*zdp+76.0))/24.0 +6*mc(ix,iy)%rbndryt)/7.0 
   
         
       mc(ix,iy)%r1000hr= mc(ix,iy)%r1000hr + ( mc(ix,iy)%rbndryt -  mc(ix,iy)%r1000hr)*(1.00-0.82*EXP(-0.168))
   
       mc(ix,iy)%rx1000=MAX( mc(ix,iy)%rx1000,0.0)

 
    !  write(11,*) vegstage%green_up,vegstage%green, vegstage%transition, vegstage%cured
       !1.5 --- herbaceous and wood 

        IF (vegstage%green_up .GE. 1 ) THEN
           
          !=============================
         
           gren=MIN(MAX(REAL(vegstage%green_up)/(7.0*REAL(iclima)),0.0),1.0)
           IF (iclima .EQ. 5) THEN
              gren=MIN(MAX(REAL(vegstage%green_up)/7.0,0.0),1.0)
           END IF
         
          ! potential wood moisture content - only depends on the mc-1000hr
 
          mcwoodp =MIN(MAX(woodga(iclima)+woodgb(iclima) *mc(ix,iy)%r1000hr,pregrn(iclima)),300.)
         
     
          !Instead to calculate the potential moisture content for the herbaceous 
          ! we need the tendency in the mc-1000hr which is stored in the variable diff
          ! Since this is the green up I assume that the difference has to be >0

          diff=(zbdybar -  mc(ix,iy)%r1000hr)*(1.00-0.82*EXP(-0.168))
         
   
          CALL kwet(mc(ix,iy)%r1000hr,diff, rkwet)
          CALL ktmp(zmintemp,zmaxtemp,rktmp)
       
         
          ! diagnostic variable which depends on mc-1000hr increments and
          ! is used to take into account for the slow moisture recovery
          ! in herbaceaous plants compared to dead fuel
         ! print*,"green-up",mc(ix,iy)%rx1000
       
          mc(ix,iy)%rx1000=MAX(mc(ix,iy)%rx1000 + (diff *rkwet *rktmp),0.0)
        
          
          !therefore the potential herbaceous humidity content is not only a linear 
          ! function of   mc-1000hr but of a function of it 

          mcherbp =MIN(MAX(rherbga(iclima)+rherbgb(iclima) *mc(ix,iy)%rx1000,30.),300.)
       
          ! there is a thricky bit here for the herbaceaous part.
          ! The model causes fuel to be transfered back and forth between 
          ! the herbaceaous and the 1-hr class as   mc(ix,iy)%rherb 
          ! fluctuates between 30 and 120 %
          ! In the IF-statement below  mc(ix,iy)%rherb  is calculated accordingly
          ! to the various phases of the vegetation
          ! Once  mc(ix,iy)%rherb is given than the fuel loaded 
          ! is given by 
          !    fctcur=MAX(MIN(1.33 - 0.0111 * mc(ix,iy)%rherb,1),0)
          !    w1p= fuelmodel%weight%r1hr +  fctcur * fuelmodel%weight%rherb
          !    wherbp=(1-fctcur)*fuelmodel%weight%rherb
          ! These equations are calculated at the end of the IF-statement
                    
          mc(ix,iy)%rherb=30.0 + (mcherbp-30)*gren
          mc(ix,iy)%rwood=pregrn(iclima)+(mcwoodp-pregrn(iclima))*gren
          
          fctcur=MAX(MIN(1.33 - 0.0111 * mc(ix,iy)%rherb,1.0),0.0)
  
        ELSE IF (vegstage%green .GE.1 ) THEN
          !=============================   
          gren=1
        !  print*,"green",mc(ix,iy)%rx1000
         mcherbp =rherbga(iclima)+rherbgb(iclima) *mc(ix,iy)%rx1000
         
          IF (mcherbp .lt. 120.0 ) THEN
             ! bypass the green stage and go into transition
             SELECT CASE (fuelmodel%herb_type)
             CASE ("annual")
                mc(ix,iy)%rherb= MAX(MIN(annta(iclima) + anntb(iclima)* mc(ix,iy)%rx1000, mc(ix,iy)%rherb),30.0)
             CASE ("perennial")
                mc(ix,iy)%rherb= MIN(MAX(perta(iclima) + pertb(iclima)* mc(ix,iy)%rx1000,30.0),150.0)
             END SELECT
             
          ELSE
             mc(ix,iy)%rherb= MAX(MIN(rherbga(iclima)+ rherbgb(iclima) *mc(ix,iy)%rx1000 ,250.0),30.0)
             
          END IF
          mc(ix,iy)%rwood= MAX(MIN(woodga(iclima) + woodgb (iclima) *mc(ix,iy)%r1000hr,200.0),pregrn(iclima))
         

       ELSE IF (vegstage%transition .GE.1 ) THEN
          !=============================
          mc(ix,iy)%rx1000=mc(ix,iy)%r1000hr
       !   print*,"transition",mc(ix,iy)%rx1000
          SELECT CASE (fuelmodel%herb_type)
          CASE ("annual")
             mc(ix,iy)%rherb= MAX(MIN(annta(iclima) + anntb(iclima)* mc(ix,iy)%rx1000, mc(ix,iy)%rherb),30.0)
          CASE ("perennial")
             mc(ix,iy)%rherb= MIN(MAX(perta(iclima) + pertb(iclima)* mc(ix,iy)%rx1000,30.0),150.0)
          
          END SELECT
          mc(ix,iy)%rwood=pregrn(iclima)
       

       ELSE IF (vegstage%cured .GE.1 ) THEN
          
          mc(ix,iy)%rx1000=mc(ix,iy)%r1000hr
       
          mc(ix,iy)%rwood=pregrn(iclima)
      !      print*,"cured",mc(ix,iy)%rx1000
          SELECT CASE (fuelmodel%herb_type)
             
          CASE ("annual")
             mc(ix,iy)%rherb=MAX(MIN(mc(ix,iy)%r1hr,30.0),0.0)
          CASE ("perennial")
             mc(ix,iy)%rherb= MIN(MAX(perta(iclima) + pertb(iclima)* mc(ix,iy)%rx1000,30.0),150.0)
             
          END SELECT

       
       END IF
       ! limit values of rwood and rherb
      ! mc(ix,iy)%rwood=MAX(MIN(mc(ix,iy)%rwood,200),1)
      ! mc(ix,iy)%rherb=MAX(MIN(mc(ix,iy)%rherb,200),1)
     !  IF (mc(ix,iy)%rherb< 0) then 
     !     write(11,*) zbdybar , mc(ix,iy)%r1000hr,mc(ix,iy)%rx1000 , mc(ix,iy)%rherb,vegstage%green_up,vegstage%green, vegstage%transition, vegstage%cured,  mc(ix,iy)%r1hr
    
      ! end if
   
       w1p= MAX(fuelmodel%weight%r1hr,0.0) +  fctcur * MAX(fuelmodel%weight%rherb,0.0)
       wherbp=(1-fctcur)*MAX(fuelmodel%weight%rherb,0.0)


       ! 2- Fire Model
       !---------------------------------------------------------------------------     
 
       !2.1 Fire characteristics
       !--------------------
       
       !Total dead and live fuel loadings
       
       wtotd=(w1p+fuelmodel%weight%r10hr+fuelmodel%weight%r100hr+fuelmodel%weight%r1000hr )
       wtotl=(wherbp+fuelmodel%weight%rwood)
       wtot=wtotd+wtotl
       ! Net fuel Loading for each fuel class

       w1n     =                   w1p  *(1.0-std)
       w10n    =fuelmodel%weight%r10hr  *(1.0-std)
       w100n   =fuelmodel%weight%r100hr *(1.0-std)
       w1000n  =fuelmodel%weight%r1000hr*(1.0-std)
       wherbn  =fuelmodel%weight%rherb  *(1.0-stl)
       wwoodn  =fuelmodel%weight%rwood  *(1.0-stl)
      ! Bulk Density of the fuel bed
       
       rhobed=(wtot - fuelmodel%weight%r1000hr)/MAX(fuelmodel%rdepth,reps) 
       
       !Weighted Fuel Density
       rhobar=((wtotl*rhol)+(wtotd*rhod))/MAX(wtot,reps)
    
       ! Packing Ratio
       betbar=rhobed/MAX(rhobar,reps)
     
       !Mineral damping coefficients for live and dead fuel
       etasd=0.174*sd**(-0.19)
       etasl=0.174*sl**(-0.19)

       ! heating numbers for each fuel class
       hn1=0
       hn10=0
       hn100=0
       hn1000=0
       hnherb=0
       hnwood=0
       IF(fuelmodel%s2v%r1hr > 0)     hn1     =w1n   *EXP(-138.0/fuelmodel%s2v%r1hr)
       IF(fuelmodel%s2v%r10hr > 0)    hn10    =w10n  *EXP(-138.0/fuelmodel%s2v%r10hr)
       IF(fuelmodel%s2v%r100hr > 0)   hn100   =w100n *EXP(-138.0/fuelmodel%s2v%r100hr)
       IF(fuelmodel%s2v%r1000hr > 0)  hn1000  =w1000n*EXP(-138.0/fuelmodel%s2v%r1000hr)

       IF(fuelmodel%s2v%rherb > 0)    hnherb  =wherbn *EXP(-500.0/fuelmodel%s2v%rherb)
       IF(fuelmodel%s2v%r1000hr > 0)  hnwood  =wwoodn *EXP(-500.0/fuelmodel%s2v%rwood)

       ! ratio of dead-to-live fuel heating numbers
       wrat=(hn1+hn10+hn100)/MAX((wherbn+wwoodn),reps)
      

       !2.1.1 Spread component
 
       !surface area of each fuel class
       sa1=0.0   
       sa10=0.0    
       sa100=0.0   
       saherb=0.0  
       sawood=0.0  

       sa1   =                     (w1p  /MAX(rhod,reps))*fuelmodel%s2v%r1hr
       sa10  =  (fuelmodel%weight%r10hr  /MAX(rhod,reps))*fuelmodel%s2v%r10hr
       sa100 =  (fuelmodel%weight%r100hr /MAX(rhod,reps))*fuelmodel%s2v%r100hr
       saherb=  (fuelmodel%weight%rherb  /MAX(rhol,reps))*fuelmodel%s2v%rherb
       sawood=  (fuelmodel%weight%rwood  /MAX(rhol,reps))*fuelmodel%s2v%rwood


       !total surface area of dead and live fuel
       sadead=MAX(sa1+sa10+sa100,reps)
       salive=MAX(saherb+sawood,reps)
       !weighting factors for each fuel class
       f1    =sa1   /sadead
       f10   =sa10  /sadead
       f100  =sa100 /sadead

       fherb =saherb/salive
       fwood =sawood/salive

       ! weighting factor of dead and live fuel
       fdead=sadead/(sadead+salive)
       flive=salive/(sadead+salive)

       ! weighted net loading of dead and live fuels
       wdeadn=(f1*w1n)+(f10*w10n)+(f100*w100n)
       wliven=(fherb*wherbn)+(fwood*wwoodn)
     
       !Dead and live fuel characteristic surface -area to volume ratio
       sgbrd=(f1*fuelmodel%s2v%r1hr)+(f10*fuelmodel%s2v%r10hr)+(f100*fuelmodel%s2v%r100hr)
       sgbrl=(fherb*fuelmodel%s2v%rherb)+(fwood*fuelmodel%s2v%rwood)

       !Characteristic surface are to volume ratio
       sgbrt=(fdead*sgbrd)+(flive*sgbrl)

           !optimum packing ratio
       betop=3.348*sgbrt**(-0.8189)
       
       !Maximum reaction Velocity 
       gmamx=(sgbrt**(1.5))/(495+0.0594*sgbrt**(1.5))

       !Optimum Reaction velocity
       ad=133.0*sgbrt**(-0.7913)
       gmaop=gmamx*(betbar/MAX(betop,reps))**(ad)*EXP(ad*(1-betbar/betop))

       !No wind propagating flux ratio 
       zeta=EXP((0.792+0.681*sgbrt**0.5)*(betbar+0.1))/(192.0+0.2595*sgbrt)

       !weighted dead-fuel moisture content for live extinsion moisture:
       mclfe=(( mc(ix,iy)%r1hr*hn1)+(mc(ix,iy)%r10hr*hn10)+(mc(ix,iy)%r100hr*hn100))/(hn1+hn10+hn100)
       
       !moisture of extinsion of live fuel
       mxl=MAX((2.9*wrat*(1.0-mclfe/fuelmodel%rmxd)-0.226)*100.0,fuelmodel%rmxd)

       !weighted moisture content of dead and live fuels
       wtmcd=(f1*mc(ix,iy)%r1hr)+(f10*mc(ix,iy)%r10hr)+(f100*mc(ix,iy)%r100hr)
       wtmcl=(fherb*mc(ix,iy)%rherb)+(fwood*mc(ix,iy)%rwood)
       
       !moisture damping coefficients of dead and live fuel
       dedrt=wtmcd/fuelmodel%rmxd
       livrt=wtmcl/mxl
       etamd=MIN(MAX(1.0-2.59*dedrt+5.11*dedrt**2-3.52*dedrt**3,0.0),1.0)
       etaml=MIN(MAX(1.0-2.59*livrt+5.11*livrt**2-3.52*livrt**3,0.0),1.0)

       !wind effect coefficients
       b=0.02526*sgbrt**0.54
       c=7.47*EXP(-0.133*sgbrt**0.55)
       e=0.715*EXP(-3.59*10.0**(-4.0)*sgbrt)
       ufact=c*(betbar/betop)**(-e)
       ws=zwspeed*2.23693629 ! wind speed in miles/hr

       !wind effect multiplier
       IF ((ws*88.0*fuelmodel%rwndfc) .GT. (0.9*ir)) THEN 
           phiwnd=ufact*(0.9*ir)**b
       ELSE
          phiwnd=ufact*(ws*88.0*fuelmodel%rwndfc)**b
       END IF
         !
       ! slope effect multiplier
       phislp=rslpfct(jslope)*MAX(betbar,reps)**(-0.3)
    
    ! reaction intensity 
       ir=gmaop*((wdeadn* fuelmodel%rhd *etasd *etamd)+(wliven*fuelmodel%rhl *etasl*etaml))
      
       !heat sink 
       a1=0;a2=0;a3=0;a4=0;a5=0
       IF (fuelmodel%s2v%r1hr > 0)       a1=EXP(-138.0/fuelmodel%s2v%r1hr)
       IF (fuelmodel%s2v%r10hr > 0)      a2=EXP(-138.0/fuelmodel%s2v%r10hr)
       IF (fuelmodel%s2v%r100hr > 0)     a3=EXP(-138.0/fuelmodel%s2v%r100hr)
       IF (fuelmodel%s2v%rherb > 0)      a4=EXP(-138.0/fuelmodel%s2v%rherb)
       IF (fuelmodel%s2v%rwood > 0)      a5=EXP(-138.0/fuelmodel%s2v%rwood)

       htsink =rhobed*(fdead*&
            &(f1 *a1 *(250.0+11.16*mc(ix,iy)%r1hr)+&
            &f10 *a2 *(250.0+11.16*mc(ix,iy)%r10hr)+&
            &f100*a3 *(250.0+11.16*mc(ix,iy)%r100hr)))+&
            &(flive*&
            &(fherb*a4*(250.0+11.16*mc(ix,iy)%rherb)+&
            &fwood*a5*(250.0+11.16*mc(ix,iy)%rwood)))
   
       !rate of spread
    
       fire_prop(ix,iy)%ros=ir *zeta *(1+phislp+phiwnd)/MAX(htsink ,reps) !ft/min 
    !   if (ISNAN(fire_prop(ix,iy)%ros) .OR.fire_prop(ix,iy)%ros .GT. 200. )  print*,mc(ix,iy)%rherb,mc(ix,iy)%rwood,ir, zeta,mc(ix,iy)%r1hr,fuelmodel%herb_type
       ! spread component 
       fire_prop(ix,iy)%sc=NINT(fire_prop(ix,iy)%ros)
       
       !2.1.2 Energy Release
       
       ! weighting factor of each fuel class:
 
       

       f1e    =                    w1p   /MAX(wtotd,reps)
       f10e   = fuelmodel%weight%r10hr   /MAX(wtotd,reps)
       f100e  = fuelmodel%weight%r100hr  /MAX(wtotd,reps)
       f1000e = fuelmodel%weight%r1000hr /MAX(wtotd,reps)
       fherbe = fuelmodel%weight%rherb   /MAX(wtotl,reps)
       fwoode = fuelmodel%weight%rwood   /MAX(wtotl,reps)
       
       ! weighting factor of dead and live fuel
       fdeade=wtotd/wtot
       flivee=wtotl/wtot
       
       !net loadings of dead and live fuel
       wdedne=wtotd*(1-std)
       wlivne=wtotl*(1.0-stl)

       !dead and live fuel characteristic surface-area-to-volume ratios
       sgbrde=(f1e*fuelmodel%s2v%r1hr)      +&
            & (f10e*fuelmodel%s2v%r10hr)    +&
            & (f100e*fuelmodel%s2v%r100hr)  +&
            & (f1000e*fuelmodel%s2v%r1000hr)
       sgbrle=(fwoode*fuelmodel%s2v%rwood)  +&
            & (fherbe*fuelmodel%s2v%rherb)

     
       !characteristic surface-area to volume ratio
       sgbrte=(fdead*sgbrde)+(flivee*sgbrle) 
     !  print*,fdead,sgbrde,flivee,sgbrle,sgbrte
       ! optimum packing ratio
       betope=3.348*sgbrte**(-0.8189)
       
       !maximum reaction velocity
       gmamxe=sgbrte**1.5 /(495.0+0.0594*sgbrte**1.5)
       
       !optimum reaction velocity
       ade=133.0*sgbrte**(-0.7913)
       gmaope=gmamxe*(betbar/betope)**ade* EXP(ade*(1.0-betbar/betope))

       !weighted moisture contents of dead and live fuels
       wtmcde=(f1e* mc(ix,iy)%r1hr)        +&
             &(f10e* mc(ix,iy)%r10hr)      +&
             &(f100e* mc(ix,iy)%r100hr)    +&
             &(f1000e* mc(ix,iy)%r1000hr)  

       wtmcle=(fwoode* mc(ix,iy)%rwood)    +&
             &(fherbe* mc(ix,iy)%rherb)

       !moisture dumping coefficients of dead and live fuel
       dedrte=wtmcde/fuelmodel%rmxd
       livrte=wtmcle/fuelmodel%rmxd
       
       etamde=MIN(MAX(1.0 - 2.0*dedrte + 1.5*dedrte**2 - 0.5*dedrte**3.0,0.0),1.0)
       etamle=MIN(MAX(1.0 - 2.0*livrte + 1.5*livrte**2 - 0.5*livrte**3.0,0.0),1.0)
       
       !reaction intensity
       ire=gmaope*((fdeade*wdedne*fuelmodel%rhd*etasd*etamde)+&
                  &(flivee*wlivne*fuelmodel%rhl*etasl*etamle))

       !resident time of the flaming front
      
       tau=384.0/MAX(sgbrt,reps)
       !energy release component (1= 25 Btu/ft**2) 
       fire_prop(ix,iy)%erc= NINT(0.04*ire*tau) 
       !2.1.3 Burning Index
       fire_prop(ix,iy)%bi= NINT(3.01*(fire_prop(ix,iy)%sc*fire_prop(ix,iy)%erc)**0.46)

     !2.2  Fire occurrence probability
     !------------------------------------

        !2.2.1 Ignition probability

        !heat of ignition
  
        qign=144.5-(0.266*(ztemp*9./5.-459.69))               -&
             &     (0.00058*(ztemp*9./5.-459.69)**2)           -&
             &     (0.01*(ztemp*9./5.-459.69)*mc(ix,iy)%r1hr) +&
             &  (18.54*(1.0-EXP(-0.151*mc(ix,iy)%r1hr))+6.4*mc(ix,iy)%r1hr)
       
        chi=(344.0-qign)/10.0
        IF ((chi**(3.6)*pnorm3-pnorm1) <= 0.0 ) THEN 
          !probability of ignition 
           p_i=0.0
           fire_prob(ix,iy)%ic=0.0
        ELSE
           p_i=MIN(MAX((chi**(3.6)*pnorm3-pnorm1)*100.0/pnorm2,0.0),100.0)
           scn=100.0*MIN(fire_prop(ix,iy)%sc/fuelmodel%rscm,1.0)
           p_fi=scn**0.5
           fire_prob(ix,iy)%ic=NINT(0.10*p_i*p_fi) !FDG Hacked for now we need to save both variables 
        END IF
          !2.2.2 Human caused fire occurrence index
        !mrisk???? = 10
        mrisk=10
        fire_prob(ix,iy)%mcoi=NINT(0.01*mrisk*fire_prob(ix,iy)%ic)
        
        !2.2.3 Lightning-caused fire occurrence index

        !duration of lightning at a point within the affected area:
        lgtdur=-86.83 +153.41*cgrate(ilightning)**(0.1437)

        !fraction of area occupied by the lightning -rain and lightning only corridors
        finsid=((stmdia(ilightning)*stmspd*lgtdur)+&
               &(0.7854*stmdia(ilightning)**2))    /&
               & ((stmdia(ilightning)*stmspd*totwid(ilightning))+&
               & (0.7854*totwid(ilightning)**2))
        fotsid=(1-finsid)

        !rain duration
        raidur =stmdia(ilightning)/stmspd

        !moisture content of the 1-hr fuel within the rain area
        fmf=mc(ix,iy)%r1hr+((76.0+2.7*raidur)-mc(ix,iy)%r1hr)*(1.0-EXP(-raidur))

        !Area weighted ignition component
        icbar=((finsid*fire_prob(ix,iy)%ic)+(fotsid*fire_prob(ix,iy)%ic))/100.
        
        !lightning risk
        lrsf=1
        lrisk=MIN(MAX(cgrate(ilightning)*lrsf,0.0),100.0)
        IF (zrain > 1.0 .OR. ilightning .EQ. 1 ) THEN
           fire_prob(ix,iy)%loi=MIN(MAX(NINT(0.25 *fire_prob(ix,iy)%loi),0),100)
        ELSE
           fire_prob(ix,iy)%loi=MIN(MAX(NINT(10.0*(lrisk*icbar)+0.25*fire_prob(ix,iy)%loi),0),100)
        END IF
       
        IF (ilightning .EQ. 6) THEN 
           fire_prob(ix,iy)%loi=100
           lrisk=100
           
        END IF

        !2.2.4 Fire load index

        fire_prob(ix,iy)%fli=0.71*SQRT(REAL(MIN(fire_prop(ix,iy)%bi**2.0,100.0)&
             &+MIN(MAX((fire_prob(ix,iy)%loi+fire_prob(ix,iy)%mcoi)**2.0,0.0),100.0)))
       

  !   2.3 Mask outputs for wet/snow/dew  conditions 
        
        ! Raining but no now /ice on the ground
        IF (zsnow .EQ. 1.0 .OR. zrain .GT. 1.5  ) THEN
           fire_prop(ix,iy)%ros=0.0
           fire_prop(ix,iy)%sc=0
           fire_prop(ix,iy)%erc=0
           fire_prop(ix,iy)%bi=0
           fire_prob(ix,iy)%ic=0
           fire_prob(ix,iy)%mcoi=0         
           fire_prob(ix,iy)%loi=0
           fire_prob(ix,iy)%fli=0.0

           mc(ix,iy)%r1hr=35
           mc(ix,iy)%r10hr=35
        END IF


       ! IF (fire_prop(ix,iy)%ros .GT. 200 ) THEN 
       !    PRINT*,fire_prop(ix,iy)%ros,zrain,lats(ix),lons(iy),ifm(ix,iy),ivs(ix,iy)
       ! END IF
           
!!B) MARK-5   
!-----------
        !here we assume that the curing is the one calculated for the nfdrs 
        mark5_fuel(ix,iy)%curing=fctcur*100.

        ! Net-rainfall 

   

       !Keetch-Byram drough factor in SI unit from Crane (1982) [0,203]
        ! it represents the drying (i.e. the increase in mm deficency) due to temperature (evapotranspiration)
        ! for a given location. The assumprion is that the mean annual total precipitation  is used as a proxy
        ! for the amount of vegetation present. 
        ! please note that when the temperature is below 6.8 C these term will become negative 
        ! (0.968*EXP(0.0875*(zmaxtemp-r0CtoK)+1.5552)-8.3 in Keetch-Byram world this only means 
        ! that the drought_factor will come to a stand still and the 
        ! drought index will be persistent despite the precipitation
        ! I have taken this into account by limiting the term above to zero
        
! TEST  

!Yesterday's maximum screen temp 27 (°C)
! 	Site's average annual rainfall 2000 (mm)
! 	Gross 24hr rain to 9.00am 9 (mm) 
!	KBDI yesterday 200

!THEN 
! KBDI today should be  196

! zrain=9 
! zmaxtemp=27+r0CtoK
! zrainclim=2000
! mark5_fuel(ix,iy)%kb_drought_index =200

     IF (mark5_fuel(ix,iy)%timesincerain .EQ. 0) THEN 
           ! we had rain yestrday so the net-rainfall is
           !  
           netrainfall=MAX(zrain,0.0)
        ELSE IF (mark5_fuel(ix,iy)%timesincerain .GE. 1) THEN
           ! we come from a dry period the net precipitation needs to be reducend by 5 mm

           netrainfall=MAX(zrain -5.0,0.0)
        END IF

!Potential Evapotranspiration

Ep= (0.968*EXP(0.0875*(zmaxtemp-r0CtoK)+1.5552)-8.3)/&
     &  (1+10.88*EXP(-0.001736*zrainclim))

 !yesterday KBDI less effective precipitation
  KBDI_temp=MAX(mark5_fuel(ix,iy)%kb_drought_index-netrainfall,0.0)

  kb_drought_factor=((203.2-KBDI_temp) /1000.0)*Ep

   

    !    kb_drought_factor=(203.2-MAX(mark5_fuel(ix,iy)%kb_drought_index-netrainfall,0.0))&
    !         &                          *MAX((0.968*EXP(0.0875*(zmaxtemp-r0CtoK)+1.5552)-8.3),0.0)/&
    !         &                           (1+10.88*EXP(-0.001736*zrainclim))/1000.0 *dt
     
   
        ! the Keetch-Byram drough index of today is the Keetch-Byram drough index  of yestrday  PLUS
        ! the drought factor calculated above
    !    WRITE(9,*) 'Keetch-Byram drough index ',mark5_fuel(ix,iy)%kb_drought_index,'drought factor',kb_drought_factor
        mark5_fuel(ix,iy)%kb_drought_index=MIN(MAX(KBDI_temp + kb_drought_factor,0.0),203.2)
       
      
        ! Once the Keetch-Byram index has been calculated use it to
        ! calculatethe mark-5 drought factor Drought factor [scaled
        ! from 0 to 10], which estimates the proportion of fine fuels
        ! available for the forward spread of a fire.
 
        mark5_fuel(ix,iy)%drought_factor=MIN(0.191*(mark5_fuel(ix,iy)%kb_drought_index+104.0)* &
             & ((mark5_fuel(ix,iy)%timesincerain + 1.0)**(1.5))/ &
             & (3.52*((mark5_fuel(ix,iy)%timesincerain + 1.0)**(1.5))+zrain-1.0),10.0)

 

        !Forest Fire Danger Index [open-ended scale, generally less than
        !100]. It measures both: the flammability of fuels, and thus the fire
        !danger; and the potential behaviour of a fire.
        mark5_prob(ix,iy)%fire_danger_index=MIN(2.0 * EXP(-0.450 + &
          &                                           0.987*LOG(mark5_fuel(ix,iy)%drought_factor+0.001)-&
          &                                           0.0345*zrh+0.0338*(ztemp-r0CtoK)+0.0234*zwspeed*tokmhr),100.0)

        mark5_fuel(ix,iy)%moist=MAX((97.7+4.06*zrh)/ &
             & MAX((ztemp-r0CtoK+6.0),reps)-0.00854*zrh+3000.0/MAX(mark5_fuel(ix,iy)%curing,reps )-30.0,0.0)
        
        mark5_fuel(ix,iy)%weight=wtot/ rtopoundsft2  !in units of ton/acre
      
        mark5_prop (ix,iy)%ros_theta0= 0.0012*mark5_prob(ix,iy)%fire_danger_index*mark5_fuel(ix,iy)%weight
        mark5_prop (ix,iy)%ros_theta=phislp*mark5_prop (ix,iy)%ros_theta0                                 !km/hr
        mark5_prop (ix,iy)%flame_height=13.0* mark5_prop (ix,iy)%ros_theta0*mark5_fuel(ix,iy)%weight      !m
        mark5_prop (ix,iy)%flame_distance=mark5_prop (ix,iy)%ros_theta0*(4.17-0.033*mark5_fuel(ix,iy)%weight)-0.36 !km

        !time since rain (The threshold for rain is set at 5 mm/day for consistency with the 
        ! Keetch-Byram drough index)
         IF (zrain .LT. 5.) THEN
            mark5_fuel(ix,iy)%timesincerain=mark5_fuel(ix,iy)%timesincerain + 1.
         ELSE
       
           mark5_fuel(ix,iy)%timesincerain=0
        ENDIF

        
        IF ((zsnow .EQ. 1 .OR. zrain .GT. 1.5)) THEN
        
           mark5_prob(ix,iy)%fire_danger_index=0.0

        END IF


!C)  FWI
!=======================================================================================================

!1 The fine fuel moisture code 
!==========================================================================

!Parameters for the calculation:
!    TEMP is the 12:00 LST temperature in degrees celsius
!    RH is the 12:00 LST relative humidity in %
!    WIND is the 12:00 LST wind speed in kph
!    RAIN is the 24-hour accumulated rainfall in mm, calculated at 12:00 LST
!    FFMCPrev is the previous day's FFMC

!TEST ***************************
!test ffmc should be  87.692980092774448 if
!        ztemp=17.+r0CtoK
!        zrh=42.
!        zwspeed=6.944
!        zrain=0
!        fwi_risk(ix,iy)%ffmc=85 
!TEST ***************************

	mo = 147.2 * (101.0 - fwi_risk(ix,iy)%ffmc)/(MAX((59.5 + fwi_risk(ix,iy)%ffmc),reps)) 
        IF ( zrain .GT. 0.5) THEN
           rf = zrain-0.5
           mr0 = EXP(-100.0 /(251.0 - mo))
           mr1 = (1 - EXP(-6.93 / rf))
           mr = mo + 42.5 * rf * mr0 * mr1
    
           IF (mo .GT. 150.0)  mr = mr+0.0015*(mo-150.0)**(2.0)*(rf)**(0.5)

           IF (mr .GT. 250.0)  mr = 250.0
           mo = mr
        ENDIF

        ed0 = zrh**( 0.679)
        ed1 = exp((zrh-100.0)/10.0)
        ed2 = 1.0-EXP(-0.115*zrh)
        ed = 0.942*ed0+11.0*ed1+0.18*(21.1-(ztemp-r0CtoK))*ed2

        IF (mo .GT. ed) THEN
           ko0 = 1-(zrh/100.0)**(1.7)
           ko1 = (zwspeed*tokmhr)**(0.5)
           ko2 = 1.0-(zrh/100.0)**(8.0)
           ko = 0.424*ko0 + 0.0694*ko1*ko2
           kd = ko*0.581*EXP(0.0365*(ztemp-r0CtoK))
           mm = ed+(mo-ed)*(10.0)**(-kd)
        ELSE
           ew0 = (zrh)**(0.753)
           ew1 = EXP((zrh-100.0)/10.0)
           ew2 = 1.0-EXP(-0.115*zrh)
           ew = 0.618*ew0 + 10*ew1 + 0.18*(21.1-(ztemp-r0CtoK))*ew2

           IF (mo .LT.  ew) THEN 
              kl0 = 1-((100-zrh)/100)**(1.7)
              kl1 = (zrh)**(0.5)
              kl2 = 1-((100-zrh)/100)**(8)
              kl = 0.424 * kl0 + 0.0694 * kl1 * kl2
              kw = kl*0.581*EXP(0.0365*(ztemp-r0CtoK))
              mm = ew-(ew-mo)*(10.0)**(-kw)
           ELSE
              mm = mo
           ENDIF
        ENDIF
! rgw ffmc is scaled between 2% and 101%
        fwi_risk(ix,iy)%ffmc = MIN(MAX(59.5*(250.0-mm)/(147.2+mm),2.0),101.0)

!        WRITE (9,*) 'fwi_risk(ix,iy)%ffmc',fwi_risk(ix,iy)%ffmc,87.692980092774448
   !     m=MAX(m,0.0)

! 2  The Duff Moisture Code
!==========================================================================

!Parameters for the calculations and units:
!    TEMP is the 12:00 LST temperature in degrees celsius
!    RH is the 12:00 LST relative humidity in %
!    RAIN is the 24-hour accumulated rainfall in mm, calculated at 12:00 LST
!    DMC is the prevvious day's DMC
!    Lat is the latitude in decimal degrees of the location for which calculations are being made
!    Month is the month of Year (1..12) for the current day's calculations.'''
!
!TEST ***************************
!test dnc should be  8.5450511359999997  if
!        ztemp=17.+r0CtoK
!        zrh=42.
!        zrain=0
!        fwi_risk(ix,iy)%dmc= 6
!        zlat=45.98
!        jmonth=4
!TEST *************************** 
   IF (zrain .GT. 1.5) THEN
   !   re = 0.92 * zrain - 1.27
      re = 0.92 * zrain - 1.5
      moo = 20.0 + EXP(5.6348 -fwi_risk(ix,iy)%dmc / 43.43)

      IF  (fwi_risk(ix,iy)%dmc .LE. 33.0) THEN
         bb = 100.0 / (0.5 + 0.3 * fwi_risk(ix,iy)%dmc)
       ELSE IF (fwi_risk(ix,iy)%dmc .GT. 33.0 .AND. fwi_risk(ix,iy)%dmc .LE. 65.0) THEN
         bb = 14.0 - 1.3 * LOG(fwi_risk(ix,iy)%dmc)
       ELSE
         bb = 6.2 * LOG(fwi_risk(ix,iy)%dmc) - 17.2
       ENDIF
       mrr = moo + 1000.0 * re / (48.77 + bb * re)

       pr = 244.72 - 43.43 * LOG(mrr - 20.0)

       IF ( pr .GT. 0.0) THEN
         fwi_risk(ix,iy)%dmc = pr
       ELSE
         fwi_risk(ix,iy)%dmc = 0.0
       ENDIF
     ENDIF
  
     IF ((ztemp-r0CtoK) .GT. -1.1) THEN
       
       CALL DayLength(zlat,jmonth,dl)
       
       k = 1.894 * ((ztemp-r0CtoK) + 1.1) * (100.0 - zrh) * dl * 0.000001
     ELSE
       k = 0.0
     ENDIF

  ! increments

     fwi_risk(ix,iy)%dmc=MAX(fwi_risk(ix,iy)%dmc,0.0)+ 100.0 * k
!     WRITE (9,*) 'fwi_risk(ix,iy)%dmc',fwi_risk(ix,iy)%dmc,' 8.5450511359999997'
! 3  The Drought Code 
!==========================================================================

!Parameters for the calculations and units:
!    TEMP is the 12:00 LST temperature in degrees celsius
!    RAIN is the 24-hour accumulated rainfall in mm, calculated at 12:00 LST
!    DMC is the prevvious day's DMC
!    Lat is the latitude in decimal degrees of the location for which calculations are being made
!    Month is the month of Year (1..12) for the current day's calculations.'''
!TEST ***************************
!test dc should be  19.013999999999999  if
!        ztemp=17.+r0CtoK
!        zrain=0
!        fwi_risk(ix,iy)%dc= 15
!        zlat=45.98
!        jmonth=4
!TEST ***************************

    IF (zrain .GT. 2.8) THEN 
      !  rd = 0.83 * zrain - 1.27
      rd = 0.83 * zrain -  2.8 
      Qo = MIN(800.0 * EXP(-fwi_risk(ix,iy)%dc / 400.0),800.0)
        Qr = Qo + 3.937 * rd
        Dr = 400.0 * LOG(800.0 / Qr)
        
        IF (Dr .GT. 0.0) THEN
            fwi_risk(ix,iy)%dc = Dr
         ELSE
            fwi_risk(ix,iy)%dc = 0.0
         ENDIF
       ENDIF

      CALL DryingFactor(zlat,jmonth,Lf)
      IF ((ztemp-r0CtoK) .GT. -2.8)THEN 
        vv = 0.36 * ((ztemp-r0CtoK)+2.8) + Lf
      ELSE
        vv = 0.0
      ENDIF

      fwi_risk(ix,iy)%dc=MAX(fwi_risk(ix,iy)%dc + 0.5 * vv,0.0)

!       WRITE (9,*) 'fwi_risk(ix,iy)%dc',fwi_risk(ix,iy)%dc,'19.013999999999999 '
! 4   Initial Spread Index
!==========================================================================
!
!    '''Calculates today's Initial Spread Index
!Parameters:
!    WIND is the 12:00 LST wind speed in kph
!    FFMC is the current day's FFMC'''

!TEST ***************************
! test ISI should be  10.853661073655068   if
!      
!        zwspeed=6.944
!        fwi_risk(ix,iy)%ffmc=87.692980092774448
!TEST ***************************     

    fWIND = EXP(0.05039*zwspeed*tokmhr)
   
    m = 147.2 * (101.0 - fwi_risk(ix,iy)%ffmc) /(59.5 + fwi_risk(ix,iy)%ffmc)

    ff = 91.9 *EXP(-0.1386 * m) * (1.0 + m**(5.31) / 49300000.0)

    fwi_risk(ix,iy)%isi= 0.208 * fWIND * ff


!     WRITE (9,*) 'fwi_risk(ix,iy)%isi',fwi_risk(ix,iy)%isi,'10.85366107365 '

! 5    Buildup Index 
!==========================================================================
!    '''Calculates today's Buidup Index
!Parameters:
!    DMC is the current day's Duff Moisture Code
!    DC is the current day's Drought Code'''
!TEST ***************************
! test BUI should be  8.4904265358371838   if
!      fwi_risk(ix,iy)%dmc=8.5450511359999997
!      fwi_risk(ix,iy)%dc=19.013999999999999
!TEST ***************************      

    IF ( fwi_risk(ix,iy)%dmc .LE.  0.4 * fwi_risk(ix,iy)%dc ) THEN 
       uu =( 0.8 * fwi_risk(ix,iy)%dmc * fwi_risk(ix,iy)%dc ) / (fwi_risk(ix,iy)%dmc + 0.4 * fwi_risk(ix,iy)%dc)
    ELSE
       uu = fwi_risk(ix,iy)%dmc - (1.0 - 0.8 * fwi_risk(ix,iy)%dc / (fwi_risk(ix,iy)%dmc + 0.4 * fwi_risk(ix,iy)%dc )) * &
            &  (0.92 + (0.0114 * fwi_risk(ix,iy)%dmc)**(1.7))
    ENDIF
    fwi_risk(ix,iy)%bui=MAX(uu,0.0)
!    WRITE (9,*) 'fwi_risk(ix,iy)%bui',fwi_risk(ix,iy)%bui,'8.4904265358371838 '
! 6  Fire Weather Index   
!==========================================================================
!
!
!    '''Calculates today's Fire Weather Index
!Paramteres:
!    ISI is the current day's ISI
!    BUI is the current day's BUI'''
!test:
!TEST ***************************
! FWI should be  10.096371392382368 if
!    fwi_risk(ix,iy)%isi=10.853661073655068 
!    fwi_risk(ix,iy)%bui=8.4904265358371838
!TEST *************************** 
    IF (fwi_risk(ix,iy)%bui .LE. 80.0)THEN
       fD = 0.626 * fwi_risk(ix,iy)%bui**(0.809) + 2.0
    ELSE
       fD = 1000.0 / (25.0 + 108.64 *EXP(-0.023 *fwi_risk(ix,iy)%bui))
    ENDIF
    fwiB = 0.1 * fwi_risk(ix,iy)%isi * fD

    IF (fwiB .GT. 1.0) THEN 
       fwi_risk(ix,iy)%fwi = MAX(EXP(2.72 *(0.434 * LOG(fwiB))**(0.647)),0.0)
    ELSE
       fwi_risk(ix,iy)%fwi = MAX(fwiB,0.0)
    ENDIF
!    WRITE (9,*) 'fwi_risk(ix,iy)%fwi',fwi_risk(ix,iy)%fwi,'10.096371392382368 '

! mask snow points (do we have to mask for heavy precipitation ?)
       IF (zsnow .EQ. 1 ) THEN
           fwi_risk(ix,iy)%bui=0.0
           fwi_risk(ix,iy)%isi=0.0
           fwi_risk(ix,iy)%fwi=0.0
        END IF



    IF (fwi_risk(ix,iy)%fwi .LT. 5.2                                    ) fwi_risk(ix,iy)%danger_risk=1.0
    IF (fwi_risk(ix,iy)%fwi .GE. 5.2 .AND. fwi_risk(ix,iy)%fwi .LT. 11.2) fwi_risk(ix,iy)%danger_risk=2.0
    IF (fwi_risk(ix,iy)%fwi .GE. 11.2.AND. fwi_risk(ix,iy)%fwi .LT. 21.3) fwi_risk(ix,iy)%danger_risk=3.0
    IF (fwi_risk(ix,iy)%fwi .GE. 21.3.AND. fwi_risk(ix,iy)%fwi .LT. 38.0) fwi_risk(ix,iy)%danger_risk=4.0
    IF (fwi_risk(ix,iy)%fwi .GE. 38.0.AND. fwi_risk(ix,iy)%fwi .LT. 50.0) fwi_risk(ix,iy)%danger_risk=5.0
    IF (fwi_risk(ix,iy)%fwi .GT. 50.0                                   ) fwi_risk(ix,iy)%danger_risk=6.0

    fwi_risk(ix,iy)%dsr=0.0272*(fwi_risk(ix,iy)%fwi**(1.77))







 ELSE  ! not a valid point for calculation 

       fire_prop(ix,iy)%ros=rfillvalue
       fire_prop(ix,iy)%sc=ifillvalue
       fire_prop(ix,iy)%erc=ifillvalue
       fire_prop(ix,iy)%bi=ifillvalue

       fire_prob(ix,iy)%ic=ifillvalue
       fire_prob(ix,iy)%mcoi=ifillvalue         
       fire_prob(ix,iy)%loi=ifillvalue
       fire_prob(ix,iy)%fli=rfillvalue
  
       mc(ix,iy)%r1hr=rfillvalue
       mc(ix,iy)%r10hr=rfillvalue
       mc(ix,iy)%r100hr=rfillvalue
       mc(ix,iy)%r1000hr=rfillvalue

       mc(ix,iy)%rherb=rfillvalue
       mc(ix,iy)%rwood=rfillvalue
       mc(ix,iy)%rx1000=rfillvalue
       mc(ix,iy)%rbndryt=rfillvalue

       rdiag2d(ix,iy,:)=rfillvalue

       fwi_risk(ix,iy)%fwi=rfillvalue        
       fwi_risk(ix,iy)%ffmc=rfillvalue       
       fwi_risk(ix,iy)%dmc=rfillvalue        
       fwi_risk(ix,iy)%dc=rfillvalue         
       fwi_risk(ix,iy)%isi=rfillvalue        
       fwi_risk(ix,iy)%bui=rfillvalue        
       fwi_risk(ix,iy)%dsr=rfillvalue        
       fwi_risk(ix,iy)%danger_risk=rfillvalue


    ENDIF !non-lake or sea point
    !--------------------
    ! END OF SPATIAL LOOP
    !--------------------

ENDDO !nlon
ENDDO !nlat

!!D)    NCDF OUTPUT
!------------

IF (lnc_rain) CALL check( nf90_put_var(ncidout, ncvar_rain(1), rrain, start=(/ 1, 1, iday /) ))
IF (lnc_temp) CALL check( nf90_put_var(ncidout, ncvar_temp(1), rtemp, start=(/ 1, 1, iday /) ))
IF (lnc_maxtemp) CALL check( nf90_put_var(ncidout, ncvar_maxtemp(1), rmaxtemp, start=(/ 1, 1, iday /) ))
IF (lnc_mintemp) CALL check( nf90_put_var(ncidout, ncvar_mintemp(1), rmintemp, start=(/ 1, 1, iday /) ))
IF (lnc_rh) CALL check( nf90_put_var(ncidout, ncvar_rh(1), rrh, start=(/ 1, 1, iday /) ))
IF (lnc_maxrh) CALL check( nf90_put_var(ncidout, ncvar_maxrh(1), rmaxrh, start=(/ 1, 1, iday /) ))
IF (lnc_minrh) CALL check( nf90_put_var(ncidout, ncvar_minrh(1), rminrh, start=(/ 1, 1, iday /) ))
IF (lnc_cc) CALL check( nf90_put_var(ncidout, ncvar_cc(1), rcc, start=(/ 1, 1, iday /) ))
IF (lnc_snow) CALL check( nf90_put_var(ncidout, ncvar_snow(1), rsnow, start=(/ 1, 1, iday /) ))
IF (lnc_wspeed) CALL check( nf90_put_var(ncidout, ncvar_wspeed(1), rwspeed, start=(/ 1, 1, iday /) ))
IF (lnc_dp) CALL check( nf90_put_var(ncidout, ncvar_dp(1), rdp, start=(/ 1, 1, iday /) ))
IF (lnc_vs   ) CALL check(nf90_put_var(ncidout, ncvar_vs (1) ,ivs,start =(/1 ,1,iday/)))
!IF (lnc_lal   ) CALL check(nf90_put_var(ncidout, ncvar_lal(1) ,ilal,start =(/1 ,1,iday/)))

IF (lnc_nfdrs) THEN 
 
      CALL check( nf90_put_var(ncidout, ncvar_mc1(1),  mc%r1hr, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mc10(1), mc%r10hr , start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mc100(1),mc%r100hr , start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mc1000(1),mc%r1000hr , start=(/ 1, 1, iday /) ))
 
      CALL check( nf90_put_var(ncidout, ncvar_x1000(1),  mc%rx1000, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mcherb(1), mc%rherb, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mcwood(1), mc%rwood , start=(/ 1, 1, iday /) ))
 
      CALL check( nf90_put_var(ncidout, ncvar_ros(1),  fire_prop%ros, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_sc(1) ,  fire_prop%sc , start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_erc(1),  fire_prop%erc, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_bi(1),   fire_prop%bi, start=(/ 1, 1, iday /) ))
 
      CALL check( nf90_put_var(ncidout, ncvar_ic(1),   fire_prob%ic, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mcoi(1), fire_prob%mcoi , start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_loi(1),  fire_prob%loi, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fli(1),  fire_prob%fli, start=(/ 1, 1, iday /) ))
 
ENDIF 

IF (lnc_mark5) THEN 
 
      CALL check( nf90_put_var(ncidout, ncvar_mark5_kb(1),  mark5_fuel%kb_drought_index, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_df(1),  mark5_fuel%drought_factor , start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_mc(1),  mark5_fuel%moist, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_w(1),   mark5_fuel%weight, start=(/ 1, 1, iday /) ))
   
      CALL check( nf90_put_var(ncidout, ncvar_mark5_ros0(1),    mark5_prop%ros_theta0,   start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_ros(1),     mark5_prop%ros_theta,    start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_height(1),  mark5_prop%flame_height, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_heightd(1), mark5_prop%flame_distance, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_mark5_fdi(1),     mark5_prob%fire_danger_index, start=(/ 1, 1, iday /) ))

   END IF

IF (lnc_fwi) THEN 


 
      CALL check( nf90_put_var(ncidout, ncvar_fwi_fwi(1),  fwi_risk%fwi, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_ffmc(1), fwi_risk%ffmc , start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_dmc(1),  fwi_risk%dmc, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_dc(1),   fwi_risk%dc, start=(/ 1, 1, iday /) ))
   
      CALL check( nf90_put_var(ncidout, ncvar_fwi_isi(1),     fwi_risk%isi,   start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_bui(1),     fwi_risk%bui,    start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_dsr(1),     fwi_risk%dsr, start=(/ 1, 1, iday /) ))
      CALL check( nf90_put_var(ncidout, ncvar_fwi_danger_risk(1), fwi_risk%danger_risk, start=(/ 1, 1, iday /) ))


   END IF



ENDDO ! date loop

  CALL setdown

  WRITE(iounit,*) 'integration finished'

!---------------------------------------
CONTAINS
  SUBROUTINE timer(str,icheck,time1,time2)

  IMPLICIT NONE

  REAL :: time1,time2
  INTEGER :: icheck
  CHARACTER*(*) :: str

  CALL cpu_time(time2)
  PRINT *,'Check point ',icheck,str,1000*time2-time1
  time1=time2
  icheck=icheck+1

  RETURN
  END SUBROUTINE timer
 
  LOGICAL FUNCTION ISNAN(R)
     REAL :: R
  IF (R .GT. 0.0) THEN
     ISNAN = .FALSE.
  ELSEIF (R .LE. 0.0) THEN
     ISNAN = .FALSE.
  ELSE
     ISNAN = .TRUE.
  END IF
END FUNCTION ISNAN




END PROGRAM geff
