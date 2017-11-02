%includeonce <suite.h>

sim_ymd=%YMD%
sim_prev_ymd=$(newdate -D $sim_ymd -1)
sim_dir=$suite_workdir/sim/$sim_ymd
sim_prev_dir=$suite_workdir/sim/$sim_prev_ymd
sim_resol=128
sim_numdays=1

# needed by ecfire.sms
sim_inidir=$sim_prev_dir
sim_inifile=restart.nc
sim_workdir=$sim_dir
sim_staticdir=$sim_dir

sim_cold_start=%COLD_START:0%
