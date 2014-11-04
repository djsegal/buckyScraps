program bucky

    ! ------------------
    !   == CENSORED == 
    ! ------------------

  ! ======================================================================
  !        BUCKY is a 1-D radiation-hydrodynamics code developed at             
  !        the University of Wisconsin Fusion Technology Institute 
  !                          to study ICF plasmas.
  ! ======================================================================


  ! ======================================================================
  !
  !               Subroutines Called by the MAIN Program :
  !
  ! ----------------------------------------------------------------------
  !
  !    initia - sets up default variables and reads input data
  !
  !    quitb  - wraps up the calculation
  !
  !    quitz  - wraps up the current input file
  !
  ! ======================================================================

  use  initia_mod  ,  only  :  initia
  use   quitb_mod  ,  only  :  quitb
  use   quitz_mod  ,  only  :  quitz

  use parseInputArguments_mod  &
                   ,  only  :  parseInputArguments


  ! ==================
  !  variable modules
  ! ==================

  use  params_mod
  use   cntrl_mod

  implicit none

  real(double)       ::  dummy


  ! ====================================
  !  get filenames for input and output
  ! ====================================

  if ( iargc() > 0 )    call parseInputArguments


  ! ==========================
  !  open files used by BUCKY
  ! ==========================

  open(  unit_dat      , file = trim(baseFile)//'.dat'                , status = 'unknown' , form = 'formatted'   )
  open(  unit_inp      , file = trim( inpFile)//'.inp' ,action='read' , status = 'old'     , form = 'formatted'   )  
  !open( unit_out      , file = trim(baseFile)//'.out_filename        , status = 'replace' , form = 'formatted'   )
  !open( unit_resnew   , file = trim(baseFile)//'.new_restart'        , status = 'replace' , form = 'unformatted' )

  write( unit_surtemp  ,  '(a,a18,a)') "# " ,  "Time (s)"  ,  "  Surface Temperature (C)" 


  ! ============================================
  !  loop over every input section in bucky.inp
  ! ============================================

  do

    ! ----------------------------------
    !  initiate the current calculation
    ! ----------------------------------

    if (       bucky_stop   )  exit

    call initia

    if (    isCompletedRun  )  exit
    if (  isAlreadyDoneRun  )  cycle

    ! ------------------------
    !  start restart up again
    ! ------------------------

    if ( .not. isInitialRun )  then 

      if ( timing(dummy) )     cycle

      call shiftt

    end if

    ! ===================
    !  main program loop 
    ! ===================

    ! ------------------
    !   == CENSORED == 
    ! ------------------

    do

      call loop

      if ( something )  stop ''

    end do

    ! ------------------
    !   == CENSORED == 
    ! ------------------

    ! -------------------------------------------
    !      *** end of current simulation ***
    !  make final output and terminate execution
    ! -------------------------------------------

    call quita

  end do


  ! ==================================
  !  terminate successful program run
  ! ==================================

  call quitz


  stop 'OK DONE'


end program bucky
