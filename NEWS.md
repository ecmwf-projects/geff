# GEFF 3.0

* 01/18 Alligned version with JRC provided parameters 
 
*  implemented  5 zones and not 4 for "le " (DMC calculation).

    - {6.5,7.5,9.0,12.8,13.9,13.9,12.4,10.9,9.4,8.0,7.0,6.0},  //          lat >= 30
    - {7.9,8.4,8.9,9.5,9.9,10.2,10.1,9.7,9.1,8.6,8.1,7.8},         // 30 > lat >= 10
    - {9,9,9,9,9,9,9,9,9,9,9,9},                                                         // 10 > lat >= -10
    - {10.1,9.6,9.1,8.5,8.1,7.8,7.9,8.3,8.9,9.4,9.9,10.2},        // -10 > lat >= -30
    - {11.5,10.5,9.2,7.9,6.8,6.2,6.5,7.4,8.7,10.0,11.2,11.8}  // -30 > lat

* Changed  day-length adjustment "Lf"  equatorial belt between  10/-10 instead than  15/-15 to be coherent with the  "Le".

* Removed any stopping rule, snow, temp, etc …

 
# GEFF 2.2

* 04/2018 Reanalysis release documented in Vitolo et al (Scientific Data, under review) 

# GEFF 2.0

* 11/2015 First full release documented in Di Giuseppe et al (JAMC 2016)

# GEFF 1.0

* 06/2014 This is the first release of the GEFF model.
