
# nparACT

<!-- badges: start -->
<!-- badges: end -->

Computes interdaily stability (IS), intradaily variability (IV) & the relative amplitude (RA) from
actigraphy data as described in Blume et al. (2016) <doi: 10.1016/j.mex.2016.05.006> and 
van Someren et al. (1999) <doi: 10.3109/07420529908998724>. Additionally, it also computes L5
(i.e. the 5 hours with lowest average actigraphy amplitude) and M10 (the 10 hours with highest average amplitude)
as well as the respective start times. The flex versions will also compute the L-value for a user-defined
number of minutes. IS describes the strength of coupling of a rhythm to supposedly stable zeitgebers.
It varies between 0 (Gaussian Noise) and 1 for perfect IS. IV describes the fragmentation of a rhythm, i.e.
the frequency and extent of transitions between rest and activity. It is near 0 for a perfect sine wave,
about 2 for Gaussian noise and may be even higher when a definite ultradian period of about 2 hrs is present.
RA is the relative amplitude of a rhythm. Note that to obtain reliable results, actigraphy data should cover a reasonable number of days.

## Installation

You can install the CRAN version of nparACT like so:

``` r
install.packages("nparACT")
```

You can install the development version of nparACT like so:

``` r
remotes::install_github("ChristineBlume/nparACT")
```