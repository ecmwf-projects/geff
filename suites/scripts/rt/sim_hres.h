# the same as sim_ens.h except:
# - resolution
# - directory with initial conditions

%includeonce <sim_ens.h>

# resolution of the forcing gribs
sim_grib_resol=%hres_resol%

# resolution of the model
sim_resol=$sim_grib_resol

sim_ic_dir=$sim_dir
