### ITHIM: Integrated Transport and Health Impacts Model

Welcome to the repository for the R package ITHIM.

#### Quick Start

Install ITHIM from GitHub and create the default ITHIM model.

```r
library("devtools")
install_github("syounkin/ITHIM", ref="master")
library("ITHIM")
ITHIM <- createITHIM()
```

The default ITHIM object can be inspected as follows.

```r
> ITHIM
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~ Physical Activity ~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Walking:
  Mean = 13 min./week
  Relative Means = 0.77, 0.69, 0.62, 0.92, 1.15, 1, ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Cycling:
  Mean = 13 min./week
  Relative Means = 7.85, 8.08, 0.77, 0.77, 80.38, 8.08, ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Physical Activity (non-travel):
  Mean = 2 MET-hrs./week
  Relative Means = 0, 0, 0.97, 0, 0, 1, ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Coefficients of Variation:
  Active Transport: 3.03
  Physical Activity (non-travel): 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Means given above are for the referent class 
(women aged 15-30 yrs.).  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~ Road Injuries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Not fully implemented yet.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Road Injury Count: Use getRoadInjuries() to display.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Safety in Numbers: Use getSIN() to display.
(Not implemented yet.)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~ Air Pollution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Not yet implemented
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Parameter Names: Rwt, Rct, muwt, muct, cv, cvNonTravel, nAgeClass,
muNonTravel, muNonTravelMatrix, GBD, meanType, quantiles,
roadInjuries, distRoadType, safetyInNumbers,
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
```