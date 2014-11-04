module initia_restart_mod


  use  cntrl_mod , only :  mstart    ,   ncycrs      ,  &
                           curInput  ,  inputCount   ,  isAlreadyDoneRun

  use rstrti_mod , only :  rstrti

  use params_mod , only :  i_kind    ,  unit_rescur  ,  &
                           baseFile  ,  unit_resnew  


  implicit none


contains  


  subroutine initia_restart


    ! ===========================
    !  start initia for restarts
    ! ===========================

    logical           ::   restartExists
    logical           ::   useMaxCycle

    integer(i_kind)   ::   testValue
    integer(i_kind)   ::   mssave
    integer(i_kind)   ::   ncsave

    integer(i_kind)   ::   tmpCycle


    ! =====================
    !  save initial values
    ! =====================

    mssave        =   mstart
    ncsave        =   ncycrs


    ! ===========================================
    !  treat cycle numbers under one as infinite
    ! ===========================================

    useMaxCycle   =   .false.

    if ( ncycrs .le. 0 )   useMaxCycle   =   .true.
    
    if (  useMaxCycle  )   ncsave        =   huge( ncycrs )


    ! ========================
    !  check new restart file
    ! ========================

    tmpCycle      =   0
    testValue     =   0

    INQUIRE( FILE = trim(baseFile)//'.new_restart' , EXIST = restartExists )

    if ( restartExists )  then

      testValue   =   rstrti(  -5   ,  ncsave  )

    end if

    if ( abs(testValue) .eq. +12345 )  tmpCycle   =   ncycrs


    ! =====================================
    !  short circuit inifinite cycle calls
    ! =====================================

    if (  useMaxCycle  .and.  tmpCycle .gt. 0  )  then

      goto 369

    end if


    ! ========================
    !  check cur restart file
    ! ========================

    testValue     =   0

    INQUIRE( FILE = trim(baseFile)//'.cur_restart' , EXIST = restartExists )

    if ( restartExists )  then

      testValue   =   rstrti( mssave , ncsave )

    end if

    if ( testValue .eq. -12345 )  return


    ! =====================================
    !  special case for when no files work
    ! =====================================

    if ( tmpCycle .eq. 0  .and.  testValue .ne. +12345 )  then

      if ( curInput .lt. inputCount )  then

        isAlreadyDoneRun = .true.

        return

      end if

      stop   &  
      'the restart file is corrupted or ncycrs may be earlier than any cycle in the current restart log.'

    end if


    ! ==============================
    !  load the better of two files 
    !   or the only one that works   
    ! ==============================

    if (       tmpCycle  .eq.  0     .or. &
          abs(   ncycrs  - ncsave )  .lt. &
          abs( tmpCycle  - ncsave )  ) then

      testValue   =   rstrti(  mssave  ,   ncycrs   )

      return

    else

      ! --------------------------------------
      !  load tmpCycle from bucky.new_restart
      ! --------------------------------------

      goto 369

    end if


    ! ==================================
    !  no loads should reach this point
    ! ==================================

    STOP ': error in initia_restart.F90.'


! ===================
!  load new restarts
! ===================

369 continue

  ! --------------------------
  !  close both restart files
  ! --------------------------

  close( unit  =  unit_resnew )   
  close( unit  =  unit_rescur ) 

  ! -------------------------------------
  !   change cur restart's name so that
  !  the file can be used later by users
  ! -------------------------------------

  INQUIRE( FILE = trim(baseFile)//'.cur_restart' , EXIST = restartExists )

  if ( restartExists )  then

    call  rename( trim(baseFile)//'.cur_restart' , trim(baseFile)//'.tmp_restart' )

  end if

  ! ----------------------------------------------
  !  move new_restart to the cur_restart position
  ! ----------------------------------------------

  call rename( trim(baseFile)//'.new_restart' , trim(baseFile)//'.cur_restart' )

  open(  unit  =  unit_resnew      ,   file   =  trim(baseFile)//'.new_restart'  &
      ,  form  =  'unformatted'    ,  status  =  'replace'            )

  close( unit  =  unit_resnew ) 

  ! -------------------------
  !  load data from tmpCycle
  ! -------------------------

  testValue    =  rstrti(  mssave  ,  tmpCycle  )

  return


  end subroutine initia_restart


end module initia_restart_mod
