# Erfcx
Erfcx(x): A multiple precision (single, double and quadruple) elemental fortran function to accurately and efficiently calculate the scaled
complementary error function, exp(x*x)*erfc(x), of a real argument

To install it:
------------
Save all accompanied files in one directory with a name acceptable by your compiler


To compile the package
---------------------------------------------------------------
1) Use the accompanying makefile_erfcx 
make -f makefile_erfcx

or on the run terminal type (for gfortran compiler)
2)  gfortran -O3 set_rk.f90  elemental_erfcx_mod_rk.f90 erfcx_driver.f90 -o erfcx_driver.exe
or
 for Intel Fortran compiler, replace "gfortran" by "ifort"


To run the code
---------------
Type 
erfcx_driver

The code will run automatically and write self-explainatory results on the screen

the user can change the precision by choosing the parameter "rk" in the file 
"set_rk.f90" and recompile and rerun



List of Files
---------------
makefile_erfcx           : a Makefile for compilation 

.f90 files 
------------
set_rk.f90                          : to select the precision by selecting the value of the integer "rk" 
constants.f90                      : an auxiliary file that containes repeatedly used numerical constants
cheb_t_erfcx_parameters_sdq.f90              : file that contains Chebyshev coefficients in addition to other parameters
elemental_erfcx_mod_rk.f90                       : A fortran module that contains the elemental function erfcx(x) 
erfcx_driver.f90          : A fortran driver code or main program 


.txt files
-------------
erfcx_ref_values        : file including externally generated data for accuracy check 
