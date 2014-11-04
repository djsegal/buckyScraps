module initia_auto_mod


  use  init1_mod, only  :     init1

  use rstrti_mod, only  :     rstrti

  use params_mod, only  :     i_kind     ,  baseFile

  use  cntrl_mod, only  :     ncycle     ,  isAlreadyDoneRun  & 
                        ,  isInitialRun  ,  isSemiInitialRun    


  implicit none


contains      


  subroutine initia_auto


  	! ============================
  	!  start initia for auto mode
  	! ============================

    logical           ::   restartExists

    integer(i_kind)   ::   testValue
    integer(i_kind)   ::   tmpCyc
    integer(i_kind)   ::   maxCyc


    ! ===============================================
    !   set initial values for testValue and maxCyc
    !  note : ncycrs is unused by users in this mode
    ! ===============================================

    testValue   =   0
    maxCyc      =   huge( maxCyc )


    ! ==========================
    !  start corrupted run over
    ! ==========================

    INQUIRE(  FILE = trim(baseFile)//".restart_0"  ,  EXIST = restartExists  )

    if (      restartExists      )    testValue  =  rstrti( -3 , maxCyc )    

    if (  testValue .ne. -12345  )    then

      call init1 

      isInitialRun  =  .true. 

      return

    end if


    ! ==========================================
    !  check if the current run is already done
    !   i.e. if it is in a file with mult inps
    !              ----  OR  ----
    !  check if the current run is semi-initial
    !   i.e. if it starts from bucky.restart_0
    ! ==========================================

    INQUIRE(   FILE = trim(baseFile)//".restart_1" , EXIST = restartExists )

    if ( .not. restartExists )  then

      INQUIRE( FILE = trim(baseFile)//".restart_2" , EXIST = restartExists )

      if (     restartExists )  then

        isAlreadyDoneRun  =  .true.

      else

        isSemiInitialRun  =  .true.

      end if
        
      return

    end if


    ! ================================
    !  make initial call to rstrti to 
    !     find out what to do next
    ! ================================

    testValue  =  rstrti( -1 , maxCyc )


    ! =====================================
    !  fix runs with a bad bucky.restart_1
    ! =====================================

    INQUIRE( FILE = trim(baseFile)//".restart_2" , EXIST = restartExists )

    if (   testValue .ne. -12345  .and.  restartExists  )  then

      testValue  =  rstrti( -2 , maxCyc )

      if ( testValue .eq. -12345 )  return

    end if

    ! --------------------------------------------
    !  when bucky.restart_1 .and. bucky.restart_2
    !  are corrupted , start from bucky.restart_0
    ! --------------------------------------------

    if (   testValue .ne. -12345 )  then

      testValue  =  rstrti( -3 , maxCyc )    

      isSemiInitialRun  =  .true.

      return

    end if
 

    ! ===================================
    !  make sure to load from the latest 
    !       uncorrupted restart log
    ! ===================================

    INQUIRE( FILE = trim(baseFile)//".restart_2" , EXIST = restartExists )

    if (  .not.  restartExists  )   return

    tmpCyc        =   ncycle

    testValue     =   rstrti( -2 , maxCyc )

    if (  testValue .ne. -12345  .or.  ncycle .lt. tmpCyc  )  then

      testValue   =   rstrti( -1 , maxCyc )

    end if

    return


  end subroutine initia_auto


end module initia_auto_mod
