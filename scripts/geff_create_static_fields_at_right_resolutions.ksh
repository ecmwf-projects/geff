#!/bin/ksh
module load cdo/1.9.2
#original fix fields

fuel_model=GlobalFuelMap_v1_modif01.nc
climate_class=Koeppen_CRU_GPCCVASClimO_Late_d_reduced.nc
vegetation_stage=MODIS_lai_TS_CLIM_1279_vegstage_y.nc
precip_climate=era_tpclimatology_19790101_20131231.nc


## extract land sea mask of IFS at the right resolution 
##see for when those grids were active
##https://software.ecmwf.int/wiki/display/USS/Evolution+of+IFS
## 128
mars <<EOF
retrieve,
class=ei,
date=1979-01-01,
expver=1,
levtype=sfc,
param=172.128,
stream=oper,
time=00:00:00,
type=an,
target="lm_N128_res.grib"
EOF
#256
mars<<EOF
retrieve,
class=od,
date=2001-01-01,
expver=1,
levtype=sfc,
param=172.128,
step=3,
stream=oper,
time=00:00:00,
type=fc,
target="lm_N256_res.grib"
EOF
#640
mars<<EOF
retrieve,
class=od,
date=2011-01-01,
expver=1,
levtype=sfc,
param=172.128,
step=3,
stream=oper,
time=00:00:00,
type=fc,
target="lm_N640_res.grib"
EOF
#O640
mars<<EOF
retrieve,
class=od,
date=2017-01-01,
expver=1,
levtype=sfc,
param=172.128,
step=3,
stream=oper,
time=00:00:00,
type=fc,
target="lm_O1280_res.grib"
EOF
#O1280
mars<<EOF
retrieve,
class=od,
date=2017-01-01,
expver=1,
levtype=sfc,
param=172.128,
step=0,
stream=enfo,
time=00:00:00,
type=cf,
target="lm_O640_res.grib"
EOF
#
##interpolate to a regular gaussian and trasfoms the land sea-mask in a 0/1 masks
#
cdo -R -f nc -remapnn,n128 lm_N128_res.grib lm_N128_res.nc
cdo -R -f nc -remapnn,n256 lm_N256_res.grib lm_N256_res.nc
cdo -R -f nc -remapnn,n640 lm_O640_res.grib lm_N640_res.nc
cdo -R -f nc -remapnn,n1280 lm_O1280_res.grib lm_N1280_res.nc
#
#
cdo -setmissval,-9999 -setctomiss,0 -gtc,0 lm_N128_res.nc  /tmp/n128_mask.nc
cdo -setmissval,-9999 -setctomiss,0 -gtc,0 lm_N256_res.nc  /tmp/n256_mask.nc
cdo -setmissval,-9999 -setctomiss,0 -gtc,0 lm_N640_res.nc  /tmp/n640_mask.nc
cdo -setmissval,-9999 -setctomiss,0 -gtc,0 lm_N1280_res.nc /tmp/n1280_mask.nc
#
##interpolate the fixed fields to make sure thay are on  the right grid and matching mask of IFS 

for dest_res in 128 256 640 1280; do
#climate cumulative precip- from era interim use remap-conservative
cdo -R -f nc -setmissval,-9999 -mul /tmp/n${dest_res}_mask.nc -remapcon,n$dest_res \
    $precip_climate  prclim_n${dest_res}.nc
# fuel model- original grid is very high resolution, nearest neighbours is ok
cdo -R -f nc -setmissval,-9999 -mul /tmp/n${dest_res}_mask.nc -remapnn,n$dest_res \
    $fuel_model   fm_n${dest_res}.nc
# grid on climate class resolution is 8 km use nn
cdo -R -f nc -setmissval,-9999 -mul /tmp/n${dest_res}_mask.nc -remapnn,n$dest_res \
    $climate_class   climclass_n${dest_res}.nc
cdo -R -f nc -setmissval,-9999 -mul /tmp/n${dest_res}_mask.nc -remapnn,n$dest_res \
    $vegetation_stage   vs_n${dest_res}.nc

done




