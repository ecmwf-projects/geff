# Global ECMWF Fire Forecasting (GEFF) model

> Fire danger indices from the FWI, NFDRS and MARK5 systems
> Maintainer: [Francesca Di Giuseppe](https://www.ecmwf.int/en/about/who-we-are/staff-profiles/francesca-di-giuseppe) francesca.digiuseppe@ecmwf.in

The Global ECMWF Fire Forecasting (GEFF) model is a Fortran-95 program to calculate fire danger indices from atmospheric inputs. 
It implements the Fire Weather Index, the National Fire Danger Rating System and the Mc-Arthur ratings in one single infrastructure. 
While it was principally designed for gridded data, it can operate with any kind of inputs.
GEFF is available under an APACHE-2 license.

## Content:
-----------
docs/ documentation
src/ source code
tests/ script for launching an example run 
data/ sample input data (please note, ERA-Interim and other inputs will need to be locally available)

## Meta:
--------

-   This software and functions herein are part of an experimental open-source project. They are provided as is, without any guarantee.
-   Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
-   Please report any issues or bugs to francesca.digiuseppe@ecmwf.int.
-   License: [APACHE-2](LICENSE)
-   If you use this software, please cite the following:
    Di Giuseppe, F., Pappenberger, F., Wetterhall, F., Krzeminski, B., Camia, A., Libertá, G. and San Miguel, J., 2016. 
    The potential predictability of fire danger provided by numerical weather prediction. Journal of Applied Meteorology and Climatology, 55(11), pp.2469-2491.
    https://journals.ametsoc.org/doi/abs/10.1175/JAMC-D-15-0297.1
