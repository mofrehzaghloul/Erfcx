  ! -----------------------------------------------------------------------
  ! Erfcx_Driver is a driving code for the elementary function Erfcx_rk(x)
  ! found in the module elemental_erfcx_mod_rk found in the file
  ! elemental_erfcx_mod_rk.f90.
  !
  ! --
  ! For compilation
  ! gfortran -O3 set_rk.f90 elemental_erfcx_mod_rk.f90
  ! Erfcx_Driver.f90 -o Erfcx_Driver.exe
  ! !  --warn-all -Wall -fcheck=all -std=f2008
  ! ! or
  ! ifort -O3 set_rk.f90 elemental_erfcx_mod_rk.f90
  ! Erfcx_Driver.f90 -o Erfcx_Driver.exe
  ! ! or
  ! Use the accompanying makefile
  ! -----------------------------------------------------------------------

  Program erfcx_driver
    Use set_rk, Only: rk, qp
    Use elemental_erfcx_mod_rk, Only: erfcx_rk

    Implicit None

    Integer, Parameter :: mmax = 10000000
    Real (rk), Dimension (mmax) :: xref, yref, yp
    Real (rk) :: a, b, time_begin, time_end
    Integer :: i
    Character (Len=45) :: filename

    ! :-
    ! Accuracy check using externally generated data
    ! :-
    filename = 'erfcx_ref_values.txt'
    Open (Unit=13, File=filename, Status='old')

    Do i = 1, 40001
      Read (13, *) xref(i), yref(i)
    End Do

    Write (*, *) ' '
    Write (*, *) 'PRECISION =', rk
    Write (*, *) '**********************************************************'
    Write (*, '(A38,ES15.7E3,A3,ES15.7E3,A8)') ' !!**** Accuracy check using data in [', xref(1), &
      ' , ', xref(40001), '] ****!!'
    Write (*, *) '**********************************************************'

    ! :- Accuracy check of the built-in function against externally
    ! generated reference data

    yp(1:40001) = erfc_scaled(xref(1:40001))
    Write (*, *) 'Built-in   : Erfc_Scaled'
    Write (*, '(A37,ES15.7E3)') 'Max Re. Error |y-yref|/|yref| = ', &
      maxval(abs(yp(1:40001)-yref(1:40001))/abs(yref(1:40001)))
    Write (*, *) ' '

    ! :-Accuracy check of the present function against externally generated
    ! reference data
    yp(1:40001) = erfcx_rk(xref(1:40001))
    Write (*, *) 'Present    : Erfcx_rk'
    Write (*, '(A37,ES15.7E3)') 'Max Re. Error |y-yref|/|yref| = ', &
      maxval(abs(yp(1:40001)-yref(1:40001))/abs(yref(1:40001)))
    Write (*, *) ' '

    ! :-
    ! Efficiency Comparison and Timing Experiments
    ! :-

    a = 1.0E-6_rk                  ! x_min
    b = 1.0E+6_rk                  ! x_max
    Call check_erfcx(a, b)

  Contains
    ! :-
    Subroutine check_erfcx(xmin, xmax)
      ! This subroutine receives the smallest(xmin) and largest(xmax) values
      ! of the real input and returns the average CPU time consumed in
      ! evaluating the scaled complementary error function for an array of
      ! mmax values of the input "xref" using the present algorithm and other
      ! competetive algorithms available in the literature including the built-in
      ! function "Erfc_Scaled".
      ! The integer "mmax" is defined in the main program.
      ! The values of the input variable are equally spaced on the
      ! logarithmic scale if both of xmin and xmax are positive.
      ! When both of xmin and xmax are negative, the values generated
      ! will be the negative of the values generated using |xmin|
      ! and |xmax| equally spaced on the logarithmic scale. Otherwise, the
      ! values of input variable generated will be equally spaced
      ! on the linear scale

      Real (rk), Intent (In) :: xmin, xmax
      Real (rk) :: lg10_xmin, lg10_xmax
      Integer :: k

      Write (*, *) ' '
      Write (*, *) ' '
      Write (*, *) ' '
      Write (*, *) '*********************************************************'
      Write (*, '(A34,ES15.7E3,A3,ES15.7E3,A8)') ' !!**** Efficiency Comparison in [', xmin, ' , ', &
        xmax, '] ****!!'
      Write (*, *) '*********************************************************'

      ! :- Generate a number of mmax input values, uniformly spaced on the
      ! logarithmic scale, between xmin & xmax
      If (xmin>0 .And. xmax>0) Then
        lg10_xmin = log10((xmin))
        lg10_xmax = log10((xmax))

        Do k = 1, mmax
          xref(k) = (1.0E1_rk**(lg10_xmin+real((k-1),kind=rk)*(lg10_xmax-lg10_xmin)/real((mmax-1), &
            kind=rk)))
        End Do

      Else If (xmin<0 .And. xmax<0) Then
        lg10_xmin = log10(-xmin)
        lg10_xmax = log10(-xmax)

        Do k = 1, mmax
          xref(k) = -(1.0E1_rk**(lg10_xmin+real((k-1),kind=rk)*(lg10_xmax-lg10_xmin)/real((mmax-1), &
            kind=rk)))
        End Do

      Else

        Do k = 1, mmax
          xref(k) = xmin + real((k-1), kind=rk)*(xmax-xmin)/real((mmax-1), kind=rk)
        End Do

      End If

      ! :- Timing of the built-in function
      ! Write (*, *) 'nrep=', nrep

      Call cpu_time(time_begin)
      yp = erfc_scaled(xref)
      Call cpu_time(time_end)

      Write (*, '(A41,ES15.7E3,A10)') 'Elapsed time: ERFC_SCALED             = ', &
        (time_end-time_begin), '  seconds'

      ! :- Timing of present function
      Write (*, *) ' '

      Call cpu_time(time_begin)
      yp = erfcx_rk(xref)
      Call cpu_time(time_end)

      Write (*, '(A41,ES15.7E3,A10)') 'Elapsed time: Present Erfcx_rk        = ', &
        (time_end-time_begin), '  seconds'

      ! :- For sp & dp compare with Cody's function from Algorithm 715
      If (rk/=qp) Then
        Write (*, *) ' '

      End If

    End Subroutine
  End Program
