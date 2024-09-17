    Module elemental_erfcx_mod_rk
      ! :----------------
      ! This module provides generic interfaces for the scaled complementary
      ! error function of a real argument, exp(x*x)*erfc(x), in any of the
      ! standard precision arithmetic (single, double or quad) based on
      ! the choice of an integer "rk" in the subsidiary module "set_rk" in the
      ! file "set_rk.f90".
      ! :------------------

      Use set_rk
      Implicit None

      Include 'cheb_t_erfcx_parameters_sdq.f90'

      Public :: erfcx_rk
    Contains
      ! -------------------------
      Elemental Function erfcx_rk(x)
        ! :----------------
        ! Erfcx_rk is an elemental function that evaluates the
        ! scaled complementary error function of a real argument x,
        ! i.e. "exp(x*x) * erfc(x)".

        ! The function Erfcx_rk(x) receives "x" as an input and returns
        ! "Erfcx_rk' for the scaled complementary error function
        ! using the precision arithmetic determined by the integer "rk"
        ! set in the subsidiary module "set_rk".
        ! :-
        ! :-
        ! COPYRIGHT (C) [2023]
        ! AUTHOR: MOFREH R. ZAGHLOUL
        ! UNITED ARAB EMIRATES UNIVERSITY, Dec 03, 2023
        ! ALL RIGHTS RESERVED.
        ! SOFTWARE NAME: Erfcx_RK
        ! VERSION: 1
        ! THIS SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
        ! INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
        ! PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHOR OR COPYRIGHT HOLDER BE LIABLE
        ! FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
        ! OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
        ! OTHER DEALINGS IN THE SOFTWARE.
        ! REDISTRIBUTION AND USE IN SOURCE AND BINARY FORMS, WITH OR WITHOUT MODIFICATION, ARE
        ! PERMITTED PROVIDED THAT THE FOLLOWING CONDITIONS ARE MET:
        ! 1. REDISTRIBUTIONS OF SOURCE CODE MUST RETAIN THE ABOVE COPYRIGHT NOTICE, THIS LIST OF
        ! CONDITIONS AND THE FOLLOWING DISCLAIMER.
        ! 2. REDISTRIBUTIONS IN BINARY FORM MUST REPRODUCE THE ABOVE COPYRIGHT NOTICE, THIS LIST OF
        ! CONDITIONS AND THE FOLLOWING DISCLAIMER IN THE DOCUMENTATION AND/OR OTHER MATERIALS
        ! PROVIDED WITH THE DISTRIBUTION.
        ! 3. THIS SOFTWARE CANNOT BE USED FOR COMMERCIAL PURPOSES WITHOUT SPECIFIC PRIOR WRITTEN
        ! PERMISSION.
        ! 4. PROPER CITATION TO THE ORIGINAL PAPER MUST BE MADE IN ANY PUBLICATIONS OR PRODUCTS USING
        ! THIS SOFTWARE: ZAGHLOUL, M. R., “Efficient  multi-precision computation of thr scaled
        ! complementary error function and the Dawson integral”
        ! Numerical Algorithms Volume 95, pages 1291–1308,(2024) https://doi.org/10.1007/s11075-023-01608-8  
        ! !---------------

        Implicit None
        Real (rk), Intent (In) :: x
        Real (rk) :: erfcx_rk, ax, t, dumy
        Integer :: jmin, j, i

        ax = abs(x)

        If (x<=zeps(7)) Then
          If (x<-9.0_rk) Then
            t = x*x
            erfcx_rk = two*exp(t)

          Else If (ax<=zeps(1)) Then
            erfcx_rk = one - two_by_sqrt_pi*x

          Else
            t = x*x
            erfcx_rk = zero

            Do j = 2, 7
              If (ax<=zeps(j)) Then
                Do i = j, 2, -1
                  erfcx_rk = t*(erfcx_rk+c_even(i)+c_odd(i)*x)
                End Do
                erfcx_rk = erfcx_rk + c_even(1) + x*c_odd(1)
                Exit
              End If
            End Do

            Return
          End If

          ! :- Chebyshev subinterval polynomial approximation
          !
        Else If (ax<=cheb_ax) Then
          t = two_p_one/(ax+two_p_one)
          jmin = n_dvs*t
          jmin = jmin*np_pls_1 + 1
          erfcx_rk = zero

          Do j = 1, n_loop_max
            erfcx_rk = t*(t*(t*(t*(erfcx_rk+cffs(jmin))+cffs(jmin+1))+cffs(jmin+2))+cffs(jmin+3))
            jmin = jmin + 4
          End Do
          erfcx_rk = erfcx_rk + cffs(jmin)

        Else
        
          
          ! Laplace continued fraction in a rational function form
          ! 
          If (x_vbig<=ax) Then
            erfcx_rk = one_by_sqrt_pi/ax

          Else If (x_big<=ax) Then
            erfcx_rk = one_by_sqrt_pi/(ax+half/x)

          Else
            t = ax*ax
            erfcx_rk = one
            dumy = one

outer:      Do j = 1, 4
              i = (j*(1+j))/2
              If (xbig_border(j)<=ax) Then
                Do jmin = i, i + j
                  erfcx_rk = t*erfcx_rk + cf_num(jmin)
                  dumy = t*dumy + cf_dnm(jmin)
                End Do
                erfcx_rk = (one_by_sqrt_pi/ax)*erfcx_rk/dumy
                Exit outer
              End If
            End Do outer

          End If

        End If

        If (x<zero) Then
          t = x*x
          erfcx_rk = two*exp(t) - erfcx_rk
        End If

        Return

      End Function
      
      
    End Module
