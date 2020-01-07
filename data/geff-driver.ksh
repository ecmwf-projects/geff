#!/bin/ksh -v
#--------------------------------------------------
# This is an example scripts to run geff using ecmwf forecast inputs
# files needs to be available in the input directory
#
#--------------------------------------------------

# control
rundir="./" # directory where geff runs
# data needs to be copied into the directory
# $rundir/input. The results will be stored in $rundir and some diagnostic outputs will be dumped in $rundir/output.
geff_exe="../src/geff_exe"
geff_data_dir="../data/"
# forcings files

#tempfile     2m temperature 12 local time (Kelvin)
#maxtempfile  2m max temperature in 24 h before (Kelvin)
#mintempfile  2m min temperature in 24 h before (Kelvin)
#rhfile       2m relative humidity at 12 local time (%)
#maxrhfile    2m max relative humidity in the 24 h before (%)
#minrhfile    2m min relative humidity in the 24 h before (%)
#rainfile     accumulated precipitation in the 24 h before  (mm/day)
#ccfile       Cloud cover file (fraction)
#wspeedfile   wind speed intensity  (m/s)
#snowfile     snow cover mask (0/1)
#dpfile       duration of precipitation in the previous 24 hours (hours)
#vsfile       vegetation stage file (1 = cured,2=pre-green, 3= green,4=transition )

# constant files

#lsmfile       land sea mask file
#crfile        climatic regions  4 =  wet,  3 = humid, 2 =  snow, 1= dry
#fmfile        fuel model file code.
#slopefile     slope is divided in 5 classes (0,1,2,3,4)
#cvfile        vegetation cover (fraction [0,1])
#rainclimfile   #climatological precipitation accumulated over an year mm/year

# some global constant fields can be found in the directory
#$rundir/constant

#
cd $rundir/
mkdir -p $rundir/input  #this two directories need to exhist
mkdir -p $rundir/output #this two directories need to exhist

cp $geff_data_dir/* $rundir/input

namelist=ecfire.namelist

cat <<EOF > ./input/$namelist
  !
  ! this is the namelist file for the geff  model
  !
  ! F. Di GIUSEPPE 2014, ECMWF
  !
&CONTROL
output_file='geff.nc',
rundir='./',
date1=20170404,
date2=20170413
nday=10,
time=12,
init_file='restart_EU.nc',
lstick=.FALSE.,
now='20170404',
version='1.4'
/
&CLIMATE
tempfile='tt_EU.nc',
maxtempfile='ttmax_EU.nc',
mintempfile='ttmin_EU.nc',
rhfile='rh_EU.nc',
maxrhfile='rhmax_EU.nc',
minrhfile='rhmin_EU.nc',
rainfile='pr_EU.nc',
ccfile='cc_EU.nc',
wspeedfile='ws_EU.nc',
snowfile='sc_EU.nc',
dpfile='prdur_EU.nc',
vsfile='vs_EU.nc',
/
&CONSTDATA
lsmfile='lm_EU.nc',
crfile='cr_EU.nc',
fmfile='fm_EU.nc',
slopefile='slope_EU.nc',
cvfile='vc_EU.nc',
rainclimfile='prclim_EU.nc'
/
EOF

banner " run geff"
#==================================================================

$geff_exe

