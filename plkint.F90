module plkint_mod
    
  use params_mod, only: double, i_kind, dk, unit_out

  implicit none

contains

  real(double) function plkint ( x )

    ! ======================================================        
    !        return the integral of the planck function:
    !   --------------------------------------------------
    !                     _x
    !                   _/    dy  y**3 / (exp(y)-1)    
    !                  0
    !   --------------------------------------------------
    !  the table values are the logarithms of the integrals             
    !           forequally spaced values of  log(x).    
    ! ======================================================                                   

    ! ----------------------
    !  initialize variables
    !  (sorted by function)
    ! ----------------------

    real(double),intent(in)  ::  x

    real(double)     ::  a
    real(double)     ::  b
    real(double)     ::  c
    real(double)     ::  d

    ! ==============================
    !  Calculate the Plank Integral
    ! ==============================

    ! -------------------------------------------
    !    approximate small values of x using a 
    !  perturbed first order expansion of exp(x)     
    ! -------------------------------------------

    if ( x .lt. 1.0_dk ) then

        a  =  3.000000_dk   !  diff(x^4,x) = 3 * x^3
        b  =  0.617292_dk   !  involved in power
        c  =  0.225043_dk   !  involved in exp (rate)
        d  =  0.084099_dk   !  involved in exp (offset)
        
        plkint = x**3 / ( 3 + b*x ) * exp( -c * ( x - d ) )

        return

    endif

    ! --------------------------------
    !  approximate larger values of x 
    !    using a sigmoidal function
    ! --------------------------------

    if ( x .gt. 5.0_dk ) then

        a  =  0.343919_dk   !  vertical offset
        b  =  6.151070_dk   !  involved in numerator
        c  =  0.696762_dk   !  involved in exp (rate)
        d  =  3.488050_dk   !  involved in exp (offset)
        
        plkint = a + b / ( 1.0_dk + exp( -c * ( x - d ) ) ) 

        return     

    endif

    ! ---------------------------------------
    !  use two 4th degree polynomials to fit 
    !  the region between the two asymptotes
    ! ---------------------------------------

    if ( x .lt. 3 ) then 

        ! ------------------------
        !  region between 1 and 3
        ! ------------------------

        a  =   0.1933770_dk
        b  =  -0.5914600_dk
        c  =   0.7049410_dk
        d  =  -0.0818982_dk

    else

        ! ------------------------
        !  region between 3 and 5
        ! ------------------------

        a  =  -1.4712400_dk
        b  =   1.0059400_dk
        c  =   0.1985780_dk
        d  =  -0.0289467_dk

    endif

    plkint = a + x * ( b + x * ( c + x * d ) )

    return                                                                                

  end function plkint

end module plkint_mod
