#!/bin/ksh
#
# nomenclature:
#   p  - pressure levels (hPa)    ; must be a coordinate variable
#   t  - temperature (k)          ; must have a pressure coordinate variable
#   td - dew point temperature (k); must have a pressure coordinate variable
#  opt - not used; set to False
#  
# http://www.nwas.org/digest/papers/1988/Vol13-Issue2-May1988/Pg23-Haines.pdf
# The Haines_Index is technically the "Low Atmosphere Stability Index" ==> LASI
#
# It consists of two parts: 
#    (i) Stability Term: (Tp1 - Tp2) 
#   (ii) Moisture Term : (Tp1 - TDp1)
#
# The Haines Index can range between 2 and 6. 
# The drier and more unstable the lower atmosphere the higher the index.
# ROS means 'Risk Of Spread'
# A Haines Index of 2-3= a very low ROS, 4= low ROS, 5=moderate ROS, and 6= high ROS.
# 
# The function calculates a "low" (0), "mid" (1) and "high" (2) elevation index
# -----------------------------------------
# The following classifications are based upon: 
#    http://www.erh.noaa.gov/cae/haines.htm
#
# The HI boundaries are those used for the above web site.
# The low-level Haines boundary values for the Coastal and Midlands zones, and 
# the mid-level Haines bounadries for the Upstate zones.
#
#mars retrievals

module load cdo/1.9.2
today=20180130
#
mars <<EOF
retrieve,
   class=od,
   date=$today,
   expver=1,
   levelist=950/850/700/500,
   levtype=pl,
   param=157.128,
   step=AV,
   stream=oper,
   time=00:00:00,
   type=fc,
   grid=O1280,
   fieldset=rh_neg
compute,
   formula ="max(min(rh_neg,100.0),1.0)",
   fieldset=rh
retrieve,
   param=130.128,
   fieldset=t_neg
compute,
   formula ="max(t_neg,0.0)",
   fieldset=t
compute,
    fieldset=part1,
    formula="log(rh/100.0)"
compute,
    fieldset=part2,
    formula="17.625*(t-273.15)/(243.04+t-273.15)"
compute,
    fieldset=part3,
    formula="17.625-log(rh/100.0)"
compute,
    fieldset=part4,
    formula="(17.625*(t-273.15))/(243.04+(t-273.15))"
compute,
    fieldset=td_neg,
    formula="243.04*((part1+part2)/(part3-part4))+273.15"
compute,
    fieldset=td,
    formula="max(td_neg,0)"
write, 
    fieldset=t,
    target="/scratch/rd/nen/tmp/t.grb"
write, 
    fieldset=td,
    target="/scratch/rd/nen/tmp/td.grb"


EOF

exit

mars <<EOF

retrieve,
      source="/scratch/rd/nen/tmp/t.grb",
      class=od,
      date=$today,
      expver=1,
      levelist=950/850/700/500,
      levtype=pl,
      grid=O1280, 
      param=130.128,
      step=AV,
      stream=oper,
      time=00:00:00,
      type=fc,
      target="/scratch/rd/nen/tmp/t_[levelist].grb"
retrieve,
      source="/scratch/rd/nen/tmp/td.grb",
      class=od,
      date=$today,
      expver=1,
      levelist=950/850/700/500,
      levtype=pl,
      param=157.128,
      step=AV,
      grid=O1280,
      stream=oper,
      time=00:00:00,
      type=fc,
      target="/scratch/rd/nen/tmp/td_[levelist].grb"
EOF


d /scratch/rd/nen/tmp/

for lev in 950 850 700 500;do 
    echo $lev
    for iday in {0..9};do
	day=`add_dd $today +$iday`
	echo $day
	metview -b /home/ma/ma9/bin/field_at_solar_time $day 12 t_$lev.grb   t12_${day}_$lev.grb 
	metview -b /home/ma/ma9/bin/field_at_solar_time $day 12 td_$lev.grb   td12_${day}_$lev.grb 
  
    cdo -R -f nc -remapbil,n1280  -settime,12:00:00 -setname,tt t12_${day}_$lev.grb t12_${day}_$lev.nc 
    cdo -R -f nc -remapbil,n1280  -settime,12:00:00 -setname,td td12_${day}_$lev.grb td12_${day}_$lev.nc 
done
done

# haines calculation 
#=============================================
#stability terms 
for iday in {0..9};do
    day=`add_dd $today +$iday`

    echo $day
    #stability terms 
    cdo -sub t12_${day}_950.nc  t12_${day}_850.nc st_low.nc
    cdo -sub t12_${day}_850.nc  t12_${day}_700.nc st_med.nc
    cdo -sub t12_${day}_700.nc  t12_${day}_500.nc st_hig.nc
    #moisture terms 
    cdo -sub t12_${day}_850.nc  td12_${day}_850.nc mt_low.nc
    cdo -sub t12_${day}_850.nc  td12_${day}_850.nc mt_med.nc
    cdo -sub t12_${day}_700.nc  td12_${day}_500.nc mt_hig.nc

#; Low (index 0) Cycle over the three atmospheric levels 

    for lev in low med hig ;do
	case $lev in 
	    low) 
		stl1=3.5
		stl2=7.5
		mtl1=5.5
		mtl2=9.5
		;;
	    med)
		stl1=5.5
		stl2=9.5
		mtl1=5.5
		mtl2=11.5
		;;
	    hig)
		stl1=17.0
		stl2=20.5
		mtl1=14.0
		mtl2=20.0
		;;
	esac

	cdo -setrtoc,$stl2,10000,3  -setrtoc,$stl1,$stl2,2  -setrtoc,-10000,$stl1,1 st_$lev.nc hi_$lev.nc
	cdo -mulc,1 -ltc,$mtl1            mt_$lev.nc mt${lev}_mask1.nc
	cdo -mulc,2 -gtc,$mtl1 -ltc,$mtl2 mt_$lev.nc mt${lev}_mask2.nc
	cdo -mulc,3 -gtc,$mtl2            mt_$lev.nc mt${lev}_mask3.nc

	cdo -setvar,hi_$lev  -add mt${lev}_mask1.nc -add mt${lev}_mask2.nc -add mt${lev}_mask3.nc hi_$lev.nc hi_${day}_$lev.nc

	rm -f mt*_mask*.nc hi_$lev.nc
    done

    cdo merge hi_${day}_*.nc hi_${day}.nc 
done
cdo mergetime hi_????????.nc Haines_Index_3lev_${today}.nc

#
#  HI@long_name = "Haines Index"
#  HI@reference = "http://www.nwas.org/digest/papers/1988/Vol13-Issue2-May1988/Pg23-Haines.pdf" 
#  HI!0         = "elevation"
#  HI@info      = "Low Atmosphere Stability Index (LASI)" 
#  HI@elevation = (/"Low", "Medium", "High"/)
#  HI@details   = "http://www.erh.noaa.gov/cae/haines.htm"
