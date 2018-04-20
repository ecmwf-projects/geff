# include file for tasks which do something
# with geff model run in the fillup context


%includeonce <suite.h>
%includeonce <fillup.h>


# Resolution of the gribs
sim_grib_resol=$fillup_grib_resol


# Resolution of the model
sim_resol=$fillup_resol


# nominal date of the simulation
sim_ymd=$fillup_ymd
sim_prev_ymd=$fillup_prev_ymd


# number of days in the simulation
sim_numdays=1
# list of days of the simulation
sim_days=$(seq 0 $sim_numdays)


# directory with current and previous simulation
sim_dir=$fillup_dir
sim_prev_dir=$fillup_prev_dir


# directory with initial conditions for this simulation
sim_ic_dir=$sim_dir


# Directories where the atmospheric forcing fields
# retrieved from Mars are kept.
sim_grib_prefix=$fillup_grib_dir/
sim_prev_grib_prefix=$fillup_grib_prev_dir/


# params for fillup forcings mars requests
sim_mars_class='class=od'
sim_mars_expver='expver=1'
sim_mars_stream='stream=oper'


sim_cold_start=%COLD_START:0%


sim_id=fillup
