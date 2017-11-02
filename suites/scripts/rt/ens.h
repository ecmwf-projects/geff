# Header file for tasks which do
# something with the ensemble forecast
# (i.e. at the level of an ensemble,
# not individual ensemble members).

%includeonce <suite.h>

# nominal date of the ensemble run
ens_ymd=%YMD%
ens_prev_ymd=$(newdate -D $ens_ymd -1)


# resolution of the grib
ens_grib_resol=%ens_resol%


# Resolution of the model
ens_resol=$ens_grib_resol


# list of days of the ensemble
ens_numdays=10
ens_maxday=$((ens_numdays-1))
ens_days=$(seq 0 $ens_maxday)


# Directory with ensemble runs
ens_dirs=$suite_dir/ens/runs


# directory with current and previous ensemble run
ens_dir=$ens_dirs/$ens_ymd
ens_prev_dir=$ens_dirs/$ens_prev_ymd


# directory with NetCDF initial conditions
ens_ic_dir=$ens_dir


# directory where grib files retrieved from MARS are stored
ens_grib_dirs=$suite_dir/ens/grib
ens_grib_dir=$ens_grib_dirs/$ens_ymd
ens_grib_prev_dir=$ens_grib_dirs/$ens_prev_ymd


# Parameters of Mars request for retrieving an ensemble
ens_mars_expver='expver=<?config.get('forecast_forcings_expver', '0001')?>'
ens_mars_class='class=od'
ens_mars_steps='step=0/3/6/9/12/15/18/21/24/27/30/33/36/39/42/45/48/51/54/57/60/63/66/69/72/75/78/81/84/87/90/93/96/99/102/105/108/111/114/117/120/123/126/129/132/135/138/141/144/150/156/162/168/174/180/186/192/198/204/210/216/222/228/234/240'
ens_maxhour=240
#ens_mars_steps='step=0/3/6/9/12/15/18/21/24/27/30/33/36/39/42/45/48'
#ens_maxhour=48
ens_mars_first_day_steps='step=3/6/9/12/15/18/21'
ens_mars_first_24h_steps='step=0/3/6/9/12/15/18/21/24'
ens_mars_prev_day_steps='step=3/6/9/12/15/18/21'
ens_mars_prev_24h_steps='step=0/3/6/9/12/15/18/21/24'
ens_mars_grid="grid=av"
ens_mars_date="date=$ens_ymd, time=0"
ens_mars_prev_date="date=$ens_prev_ymd, time=0"


# this is simple number in case of geff ensemble
# but for example must be calculated in case of
# hindcast (see hc.h in the hindcast scripts),
ens_prev_24h_last_step=24
