# include file for tasks which do something
# with the single ensemble member simulation


%includeonce <suite.h>
%includeonce <ens.h>


# Simulation ID; this is eiter 'fc', 'cf' or 'pfXX'
sim_id=%ENS_MEMBER:hr%


# resolution of the grib
sim_grib_resol=$ens_grib_resol


# Resolution of the model
sim_resol=$ens_resol


# nominal date of the simulation
sim_ymd=$ens_ymd
sim_prev_ymd=$ens_prev_ymd


# number of days in the simulation
sim_numdays=$ens_numdays
# list of days of the simulation
sim_days=$ens_days


# directory with current and previous simulation
sim_dir=$ens_dir/$sim_id
sim_prev_dir=$ens_prev_dir/$sim_id


# directory with initial conditions
sim_ic_dir=$ens_ic_dir


# Directories where the atmospheric forcing fields
# retrieved from Mars are kept.
sim_grib_prefix=$ens_grib_dir/$sim_id.
sim_prev_grib_prefix=$ens_grib_prev_dir/$sim_id.


# Parameters for mars retrievals of forcing fields
sim_mars_steps=$ens_mars_steps
sim_maxhour=$ens_maxhour
sim_mars_first_day_steps=$ens_mars_first_day_steps
sim_mars_first_24h_steps=$ens_mars_first_24h_steps
sim_mars_expver=$ens_mars_expver
sim_mars_class=$ens_mars_class
case $sim_id in
hr ) sim_mars_type="type=fc"; sim_mars_stream='stream=oper' ;;
00 ) sim_mars_type="type=cf"; sim_mars_stream='stream=enfo' ;;
*  ) sim_mars_type="type=pf"; sim_mars_stream='stream=enfo' ;;
esac
sim_mars_grid=$ens_mars_grid
sim_mars_date=$ens_mars_date
sim_mars_prev_date=$ens_mars_prev_date
sim_mars_prev_day_steps=$ens_mars_prev_day_steps
sim_mars_prev_24h_steps=$ens_mars_prev_24h_steps
sim_prev_24h_last_step=$ens_prev_24h_last_step


sim_cold_start=%COLD_START:0%
