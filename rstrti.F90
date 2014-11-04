module rstrti_mod

    ! ------------------
    !   == CENSORED == 
    ! ------------------
    
  use   params_mod
  use    cntrl_mod 
  use  options_mod 

  implicit none


CONTAINS


  function rstrti ( mssave , ncsave ) result ( testValue )


    ! =====================================================================
    !  rstrti reads all data from all common blocks from a binary file for 
    !  restarts. It searches through the binary data until NCYCLE = ncsave.
    !
    !    **********   updated as of 7/23/14  -  version 682   **********
    ! =====================================================================

    integer(i_kind) , intent(in)   ::   mssave   ,   ncsave

    integer(i_kind)                ::   i  ,  j  ,  k  ,  l

    integer(i_kind)                ::   autoRestartFile
    integer(i_kind)                ::   testValue
    integer(i_kind)                ::   mstmp

    integer(i_kind)                ::   prevCycle  =  -1
    !integer(i_kind)                ::    tmpCycle  =  -1

    CHARACTER( LEN = 35 )          ::   curFile

    !logical                        ::   restartExists


    ! ********************************************************************
    !
    !   mssave == {  -1 , -2 , -3  }  :  used for  auto  - mode loading
    !   mssave == {       -5       }  :  used for normal - mode loading
    !
    ! ********************************************************************


    ! ============================
    !  get start mode information
    ! ============================

    if (  mssave .gt. 0  .or.  mssave .eq. -5  )   then

        mstmp            =  mssave
        autoRestartFile  =  1

    else

        mstmp            =  2
        autoRestartFile  =  mod( -mssave , 3 )

    end if


    ! ====================
    !  get curFile's name
    ! ====================

    select case ( mstmp )

        case ( +2 )

            ! -------------------------------------------------------------
            !  bucky.restart_2 is only used if bucky.restart_1 is unusable 
            !  bucky.restart_0 is only used if bucky.restart_2 is unusable
            ! -------------------------------------------------------------

            WRITE( curFile , "(i3)" ) autoRestartFile

            curFile  =  trim(baseFile)//'.restart_'//trim(adjustl(curFile))

        case ( -5 )

            curFile  =  trim(baseFile)//'.new_restart'

        case default

            curFile  =  trim(baseFile)//'.cur_restart'

    end select


    ! ===================================
    !  open restart file for valid input
    ! ===================================

    close  ( unit = unit_rescur )

    open   ( unit = unit_rescur , file = curFile , status = 'old' , form = 'unformatted' )

    rewind ( unit = unit_rescur )


    ! ===================================================================================
    !                loop over all the restart points in the restart file
    !                         until the desired cycle is found
    ! ===================================================================================

    DO

        ! ----------------------------
        !  testValue should be -12345
        ! ----------------------------

        read(unit_rescur,end=608,err=608) testValue 


        ! ===================================================================================
        !                      END  OF  CURRENT  RESTART  CYCLE  READ
        ! ===================================================================================


        ! =======================================
        !  investigate the current restart cycle
        ! =======================================

        if (  testValue .ne. -12345  )   then

            ! ---------------------
            !  corrupted testValue
            ! ---------------------

            goto 608 

        else if ( mstmp .ne. 2  .and.  ncycle .lt. ncsave )   then

            ! ---------------------------------------------
            !  mark cur cycle as a valid one to start from
            ! ---------------------------------------------

            prevCycle  =  ncycle

            CYCLE

        else

            ! ----------------------------------------------
            !  there's only one entry per auto mode restart
            ! ----------------------------------------------

            EXIT       

        end if


    END DO  ! *******************************************************************************


    ! ============================
    !  close current restart file
    ! ============================

    close ( unit_rescur )


    ! ======================================
    !  complete current auto - restart load
    ! ======================================

    if (  mstmp .eq. 2  )    return


    ! ===========================================
    !  standard not-found restart cycle protocol
    !   ( i.e. ncycle not exactly in the logs )
    ! ===========================================

    if ( ncycle .gt. ncsave ) then

        ! ---------------------------------------------
        !  if ncycle is closer to prevCycle than ncsave
        !        read from prevCycle instead
        ! ---------------------------------------------

        if ( prevCycle .eq. -1 )    goto 608 

        testValue    =   +12345

        if (  abs(    ncycle - ncsave )  .lt. &
              abs( prevCycle - ncsave )  ) then

            ncycrs   =      ncycle

        else

            ncycrs   =   prevCycle
        
        end if

        return

    end if


    ! ============
    !  good data!
    ! ============

    return


! ***************************************************
!  608 :  corrupted file  - AND / OR -  a file where
!  ncycrs is later than any cycle in the restart log
! ***************************************************


608 close(  unit_rescur  )


    ! =================================================
    !  handle special case of non-auto mode corruption
    ! =================================================

    if (  mstmp .ne. 2  .and.  prevCycle .ne. -1  )  then

        testValue   =   +12345
        ncycrs      =   prevCycle

        return

    end if
 

    ! ============================
    !  handle standard corruption 
    ! ============================

    testValue   =   0

    return


  end function rstrti

end module rstrti_mod