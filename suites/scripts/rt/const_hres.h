# same as const_ens.h except:
# - resolution
# - directory where files are stored

%includeonce <const_ens.h>
%includeonce <sim_hres.h>

const_resol=$sim_resol
const_grib_resol=$sim_grib_resol
const_dir=$sim_dir
