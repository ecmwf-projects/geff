%includeonce <suite.h>
%includeonce <newdate.h>

# YMD corresponds to the end of 24h fillup simulation
# The nominal date of YMD is YMD minus 24h.
# TODO: maybe change it when date arithmetic can be
# done in the trigger expression.

fillup_ymd=$(newdate -D %YMD% -1)
fillup_prev_ymd=$(newdate -D $fillup_ymd -1)

fillup_grib_resol=%hres_resol%
fillup_resol=$fillup_grib_resol
fillup_expver=1

fillup_dirs=$suite_dir/ic/ic
fillup_dir=$fillup_dirs/$fillup_ymd
fillup_prev_dir=$fillup_dirs/$fillup_prev_ymd

fillup_grib_dirs=$suite_dir/ic/grib
fillup_grib_dir=$fillup_grib_dirs/$fillup_ymd
fillup_grib_prev_dir=$fillup_grib_dirs/$fillup_prev_ymd
