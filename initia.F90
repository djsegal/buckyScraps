module initia_mod

    ! ------------------
    !   == CENSORED == 
    ! ------------------

  use initia_auto_mod    , only :  initia_auto
  use initia_restart_mod , only :  initia_restart
  use initia_reinit_mod  , only :  initia_reinit
  
  implicit none


contains      


  subroutine initia


    ! ================================
    !    -initia- is the input and 
    !  storage initialization routine
    ! ================================

    integer(i_kind)        ::  mssave
    integer(i_kind)        ::  ncsave
    integer(i_kind)        ::  ierr
    integer(i_kind)        ::  i
    integer(i_kind)        ::  timdatarray(8)
    integer(i_kind)        ::  curFileIndex
    integer(i_kind)        ::  dummy
    integer(i_kind)        ::  prevMode

    character(len=i_kind)  ::  curFile
    character(len=120)     ::  cstrng
    character(len=16)      ::  chdate
    character(len=16)      ::  chtime
    character(len=16)      ::  filler
    character(len=35)      ::  out_filename
    character(len=35)      ::  rstrt_filename

    logical                ::  restartExists
    logical                ::  isTransitionRun
    logical                ::  fileExists
    logical                ::  prevFirstCall

    character(len=35) &
      ,  dimension(4)      ::  cur_filenames

    ! -------------------------------------------------------------------------
    !                             namelist input                          
    ! -------------------------------------------------------------------------

    namelist / rstart / mstart, ncycrs

    namelist / input /  &  !  censored

    ! -------------------------------------------------------------------------


    ! *****************
    !  begin execution
    ! *****************
 

    ! ===============================
    !  check if first call to initia
    ! ===============================

    prevFirstCall  =  isFirstCall

    if ( mstart .eq. -1 )  then

      isFirstCall  =  .true.
      mstart       =  0
      curInput     =  1

    else

      isFirstCall  =  .false.
      prevMode     =  mstart
      curInput     =  curInput + 1

    end if


    ! ======================================
    !  check if input file read is complete 
    !  i.e. if at the last log of the file
    !   ( the one with the original data )
    ! ======================================

    if ( .not. isFirstCall ) then

      if ( curInput .gt. inputCount )  then

        isCompletedRun  =  .true.

        if ( prevFirstCall )   isFirstCall = .true.

        return

      end if

    end if 


    ! ========================================
    !  get the total number of input sections 
    !      in the current bucky.inp deck
    ! ========================================

    read(  unit_inp  ,  rstart  ,  iostat = ierr  )  

    if (  isFirstCall  )      inputCount  =  1

    if (  isFirstCall  .and.  ierr .eq. 0  ) then

      do

        read(  unit_inp  ,  rstart  ,  iostat = ierr  )

        if ( ierr .ne. 0 )  exit

        inputCount = inputCount + 1
        
      end do

      ierr  =  0

    end if


    ! ===================================
    !  read in the correct input section
    ! ===================================

    if ( ierr .ne. 0 )  then

      rewind( unit_inp )

    else if( isFirstCall )  then

      rewind( unit_inp )

      if ( mstart.eq.0 .or. mstart.eq.2 )  then

        ! ---------------------------------------------------
        !  for runs that start with an initial or auto run ,
        !     start reading from the bottommost log     
        ! ---------------------------------------------------

        do i = 1 , inputCount

          read(  unit_inp  ,  rstart  ,  iostat = ierr  )

        end do   

      else

        read(    unit_inp  ,  rstart  ,  iostat = ierr  )

      end if

    end if


    ! =================================
    !  transition runs started in auto 
    !    to a deck of standard loads
    ! =================================

    isTransitionRun    =  .false.

    INQUIRE(  FILE = trim(baseFile)//'.restart_1'    ,  EXIST = restartExists  )
    INQUIRE(  FILE = trim(baseFile)//'.new_restart'  ,  EXIST =    fileExists  )

    write(*,*) restartExists
    write(*,*)    fileExists

    if ( .not. isFirstCall .and. restartExists )  then

      if (  mstart .eq. 1  .and.  prevMode .eq. 2  )  then

        ! -------------------------------------------------------------
        !  bucky.restart_1 gets deleted to show that the current input
        !         is past the auto mode part of the input file
        ! -------------------------------------------------------------

        if (  .not. fileExists  .and.  restartExists  )  then

          isTransitionRun  =  .true.

          dummy            =  mstart
          mstart           =  prevMode
          prevMode         =  dummy

          call merge 
          call rstrto

        end if 

      end if 

    end if


    ! ===========================
    !  find the given start mode
    ! ===========================

    isInitialRun       =   (  mstart .eq.  0  )
    ! isRstrtRun       =   (  mstart .eq.  1  )
    isAutoRun          =   (  mstart .eq.  2  )
    isMergeRun         =   (  mstart .eq. 10  )

    isCompletedRun     =   .false.
    isSemiInitialRun   =   .false.
    isAlreadyDoneRun   =   .false.
    
    if ( mstart.ne.0 .and. mstart.ne.1     .and. &
         mstart.ne.2 .and. mstart.ne.10 )   then
      stop 'incorrect start mode (mstart)'
    end if


    ! ======================
    !  start initialization
    ! ======================

    mssave   =   mstart
    ncsave   =   ncycrs

    if ( isInitialRun ) then    

      ! ----------------------------------------------
      !    if an initial run ( i.e. not a restart )
      !  initialize variables to their default values
      ! ----------------------------------------------

      call init1

    else if ( isAutoRun ) then

      ! ------------------------------
      !  handle auto runs differently
      ! ------------------------------

      call initia_auto

    else 

      ! --------------------------------
      !  read in data from restart file      
      ! -------------------------------- 

      call initia_restart

    endif

    ! ------------------------------------------
    !  read back in mstart and corrected ncycrs
    ! ------------------------------------------

    mstart   =   mssave
    ncycrs   =   ncsave

    if (  ncycrs .le. 0  )   ncycrs = ncycle


    ! =======================================
    !  finish up a run that was already done
    ! =======================================

    if ( isAlreadyDoneRun )  then

      if ( isInitialRun .or. isAutoRun )  then

        if ( isFirstCall )   rewind( unit_inp )

      end if

      return

    end if


    ! ===============================================
    !  return transition runs to their correct state
    ! ===============================================

    if ( isTransitionRun )     then

      mstart     =  prevMode
      isAutoRun  =  .false.

      ! ------------------------------------------
      !  move used restart to tmp_restart's place 
      !      for further renaming next step
      ! ------------------------------------------

      call rename(  trim(baseFile)//".restart_1"  ,  trim(baseFile)//".tmp_restart"   )
  
      open(      unit_ioscrach      ,      file   =  trim(baseFile)//".cur_restart"   )
      close(     unit_ioscrach      ,     status  =                  "delete"         )

      ! -----------------------------------------
      !  open a new restart file and write to it
      ! -----------------------------------------

      open(      unit_resnew        ,      file   =  trim(baseFile)//".new_restart"   &
          ,  form = "unformatted"   ,     status  =                  "replace"        )

      close(     unit_resnew        )

      write(*,*) 'woop'

    end if


    ! ===========================================
    !  handle tmp restart files made from rstrti
    ! ===========================================

    INQUIRE( FILE = trim(baseFile)//'.tmp_restart' , EXIST = restartExists )

    if ( restartExists )  then

      if ( isInitialRun .or. isAutoRun .or. isMergeRun )  then

        open(  unit = unit_ioscrach ,  file  = trim(baseFile)//'.tmp_restart' )
        close( unit = unit_ioscrach , status = 'delete'            )

      else

        curFileIndex  =  nrestarts - ( lastmerge + 1 )

        if ( isTransitionRun )      curFileIndex  =  curFileIndex + 1

        WRITE ( curFile , "(i3)" )  curFileIndex

        rstrt_filename  =  trim(baseFile)//"_"//trim(adjustl(curFile))//".restart"

        CALL rename( trim(baseFile)//'.tmp_restart' , rstrt_filename )

      end if

    end if


    ! ==================================================
    !    if selected , merge the restart output files 
    !  into 'bucky.h5' and 'bucky.out' and stop program
    ! ==================================================

    if ( isMergeRun )   then

      call merge 
      call rstrto

      isAlreadyDoneRun  =  .true.
      return

    end if


    ! ===============================
    !  setup the restart information 
    ! ===============================

    if ( isInitialRun ) then    

        ! ------------------------------------------
        !  set initial values for restart variables
        ! ------------------------------------------

        lastmerge     =  0
        nrestarts     =  0
        chkPtCount    =  0

        ! ----------------------------------------
        !  set names for the hdf and output files
        ! ----------------------------------------

        curFile       =  "0"
        hdf_filename  =  trim(baseFile)//".h5"
        out_filename  =  trim(baseFile)//".out"

    else

        ! ------------------------------
        !      get index number for 
        !  the hdf and the output files
        ! ------------------------------

        curFileIndex  =  ( nrestarts + 1 ) - lastmerge
        WRITE( curFile , "(i3)" ) curFileIndex

        ! ------------------------------
        !         set names for 
        !  the hdf and the output files
        ! ------------------------------

        hdf_filename  =  trim(baseFile)//"_"//trim(adjustl(curFile))//".h5"
        out_filename  =  trim(baseFile)//"_"//trim(adjustl(curFile))//".out"

    endif 


    ! ==============================================
    !  read the namelist input file to scratch file
    ! ==============================================
    
    ! -------------------------------------------------
    !  copy the namelist input file to a scratch file, 
    !          neglecting the comment records 
    !    this procedure was added so that comments 
    !        can be included in the input deck
    ! -------------------------------------------------

    open(  unit = unit_ioscrach  ,  &
           form = 'formatted'    ,  status = 'scratch'  )

    write(unit_out,905)

    do 

      ! -----------------------------
      !  remove preceding whitespace 
      !   and write input to output
      ! -----------------------------

      read(unit_inp,801,end=51)  cstrng

      cstrng  =  adjustl(        cstrng ) 

      write(unit_out,901)        cstrng

      ! ----------------------------------------
      !  check for comments and end of sections
      ! ----------------------------------------

      if ( cstrng(1:1) .ne.  '!'   ) &
        write (unit_ioscrach,901)        cstrng

      if ( cstrng(1:4) .eq. '$end' )     exit

    enddo 

51  continue


    ! =============================================
    !  short-circuit loading semi-initial run here
    ! =============================================

    if ( isSemiInitialRun )  then

      isInitialRun = .true.

      nrestarts     =  nrestarts + 1

      open(   unit_out       ,   file  = out_filename  &
                             ,   form  = 'formatted'   )

      open(   unit_ioscrach  ,   file  = hdf_filename  )
      close(  unit_ioscrach  ,  status =   'delete'    )

      call hdfinit 

      return

    end if


    ! ===========================================
    !  read in namelist data from a scratch file 
    ! ===========================================

    rewind ( unit_ioscrach )
    read   ( unit_ioscrach , input )
    close  ( unit_ioscrach )


    ! =================================
    !  rewind the input file if needed
    ! =================================

    if ( isInitialRun .or. isAutoRun )  then

      if ( isFirstCall )   rewind( unit_inp )

    end if


    ! ==================================
    !  do some checks on the input data
    ! ==================================

    if ( nfg .gt. n ) stop 'nfg is larger than n'

    if ( isInitialRun ) then

       do i=1,jmax

          if (    temp(i) .le. 0._dk ) stop 'ck input  temp.  array: < 0'
          if ( density(i) .le. 0._dk ) stop 'ck input density array: < 0'

       enddo

    endif

    if (  ncycle .ge. nmax  .or.  ta .ge. tmax  )   return


    ! =================================================
    !  update nrestarts now that everything is checked
    ! =================================================

    if ( .not. isInitialRun )    nrestarts = nrestarts + 1


    ! =====================
    !  make initial output
    ! =====================

    ! --------------------------
    !  write header information
    ! --------------------------

    close( unit_out )
    open(  unit_out , file   = out_filename ,      &
                      status = 'replace'    , form = 'formatted' )

    if ( isInitialRun ) then   
      write(unit_out , 900)
    else
      write(unit_out , 991) nrestarts , ncycrs
    end if

    ! -------------------------------------
    !  print out the time and date of calc
    ! -------------------------------------

    call date_and_time( chdate , chtime , filler , TimDatArray )

    write( unit_out,911 )   TimDatArray(2) , TimDatArray(3) , TimDatArray(1) , &
          TimDatArray(5)  , TimDatArray(6) , TimDatArray(7)

    write(unit_bin  ) chdate
    write(unit_bin  ) chtime

    write(unit_dat,*) chdate
    write(unit_dat,*) chtime


    ! =================================
    !  do further initialization steps 
    ! =================================

    if ( .not. isInitialRun )  then

      call initia_reinit       !  reinitialize some vars for restarts

    else

      call init{1,...,N}       !  censored

    end if


    ! =========================
    !  initialize the hdf file 
    ! =========================

    open(   unit_ioscrach  ,  file   = hdf_filename  )
    close(  unit_ioscrach  ,  status =   'delete'    )

    call hdfinit 


    ! =====================================
    !  now that everything is initialized, 
    !      set up the new restart file
    ! =====================================

    if ( isAutoRun )  then

      if ( isInitialRun )  then

        open(     unit_rescur      ,   file  = trim(baseFile)//'.restart_1'    )
        open(     unit_resnew      ,   file  = trim(baseFile)//'.restart_2'    )

        close(    unit_rescur      ,  status = 'delete'             )
        close(    unit_resnew      ,  status = 'delete'             )

      end if

    else

      if ( isInitialRun )  then

        open(     unit_rescur      ,  file   = trim(baseFile)//'.cur_restart'  )
        close(    unit_rescur      ,  status = 'delete'             )

      end if

      open(       unit_resnew      ,  file   = trim(baseFile)//'.new_restart'  &
          ,  status  =  'replace'  ,  form   = 'unformatted'        )  
      close(      unit_resnew      )    
 
    end if 


    ! =======================================
    !  delete old new_restart for first call
    ! =======================================

    if ( isFirstCall .and. mstart .ne. 1 )  then

      open(   unit_ioscrach  ,   file  =  trim(baseFile)//".new_restart"  )
      close(  unit_ioscrach  ,  status =      "delete"         )      

    end if 


    ! =========================================
    !   delete files from old, discarded runs
    !  using units that are known to be unused
    ! =========================================

    if ( isInitialRun )  then

      curFileIndex    =  0

      do

        curFileIndex  =  curFileIndex + 1

        WRITE( curFile , "(i3)" ) curFileIndex

        cur_filenames(1)  =  trim(baseFile)//"_"//trim(adjustl(curFile))//".h5"
        cur_filenames(2)  =  trim(baseFile)//"_"//trim(adjustl(curFile))//".out"
        cur_filenames(3)  =  trim(baseFile)//"_"//trim(adjustl(curFile))//".restart"

        INQUIRE( FILE = cur_filenames(1) , EXIST = fileExists )

        if ( .not. fileExists )  exit

        do i = 1 , 3

          open  (  unit_ioscrach  ,   file  = cur_filenames(i)  )
          close (  unit_ioscrach  ,  status =     'delete'      )        

        end do
        
      end do

    end if


    ! ================================
    !   make initial restart log for 
    !  auto runs and  transition runs
    ! ================================

    if ( isInitialRun .and. isAutoRun )    call rstrto

    if (       isTransitionRun        )    call rstrto


    ! ===========================================
    !  return to MAIN with variables initialized
    ! ===========================================

    return


    ! *************************************************************************
    !                             format statements
    ! *************************************************************************


  end subroutine initia

end module initia_mod
