# header file for tasks which do something
# with ERA Interim or fillup forcings.

%includeonce <suite.h>

fo_dir=$suite_workdir/ei
#fo_expver=2426
#fo_expver=1931
#fo_expver=0001
fo_expver=<?config.get('ei_expver')?>
fo_class=ei
