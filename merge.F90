module merge_mod

  use  cntrl_mod , only : isAutoRun , ncycrs   , ncycle , &
                          nrestarts         , lastmerge   , chkPtCount
  use params_mod , only : i_kind , unit_out , unit_oldout , &       
                          unit_resnew       , baseFile
  use    hdf_mod , only : hdf_filename      , groupname1  , groupname2  , &
                          hdfinit           , hdfclose    , &
                          hdferr            , file_id 
                        
  use hdf5       , only : h5open_f  , h5fopen_f  , h5gopen_f   , h5gn_members_f , &
                          h5ocopy_f , h5fclose_f , h5gclose_f  , h5close_f      , &
                          h5gget_obj_info_idx_f  , h5ldelete_f , h5lexists_f    , &
                          H5F_ACC_RDWR_F

  use rstrto_mod , only : rstrto                          

  implicit none


contains


  subroutine merge


  ! ===========================================================
  !  merge combines h5 and out files accumulated from restarts
  ! ===========================================================


  integer(i_kind) , dimension(:) , allocatable   ::   file_id
  integer(i_kind) , dimension(:) , allocatable   ::   group_id
  logical         , dimension(:) , allocatable   ::   earlyRestart

  integer(i_kind)     ::  i , j , k
  integer(i_kind)     ::  nfiles , nmembers
  integer(i_kind)     ::  ios ! ios catches advance errors (i.e. [I]nput/[O]utput [S]tatus errors)
  integer(i_kind)     ::  tmpCycle , oldCycle , newCycle , minCycle , lastCycle
  integer(i_kind)     ::  obj_type

  character(LEN=11)   ::  tmpFile  ! File name (set in initia)
  character(LEN=15)   ::  hdf_filename_holder
  character(LEN=20)   ::  curFilename
  character(LEN=25)   ::  obj_name
  character(LEN=333)  ::  curString 

  logical             ::  earlyMerge    
  logical             ::  final_exists  
  logical             ::  cycle_exists  


  ! ===================================
  !  get data ready for call to rstrto
  ! ===================================

  nfiles        =   ( nrestarts - lastmerge )
  lastmerge     =     nrestarts
  chkPtCount    =         1
 
  if ( nfiles .eq. 0 )   return


  ! ================================
  !  initialize file and group id's
  !      as well as earlyRestart
  ! ================================

  allocate(       file_id( 0 : nfiles+1 )  )
  allocate(      group_id( 0 : nfiles+1 )  )
  allocate(  earlyRestart( 1 : nfiles   )  )

  earlyRestart  =  .false.


  ! ================================
  !  main .h5 copying part of merge
  ! ================================

  call h5open_f (hdferr)  

  do i = 1 , nfiles

    ! ------------------------------------------------------
    !  open current h5 and reopen main file for consistency
    ! ------------------------------------------------------

    write( tmpFile , '(i3)' ) i
    tmpFile  = trim(baseFile)//"_"//trim(adjustl(tmpFile))

    call h5fopen_f ( trim(baseFile)//".h5" , H5F_ACC_RDWR_F, file_id(0) , hdferr )
    call h5fopen_f ( trim( tmpFile)//".h5" , H5F_ACC_RDWR_F, file_id(i) , hdferr )

    ! ----------------------------------------
    !  make sure cycles exist in current file
    ! ----------------------------------------

    call h5lexists_f( file_id(i) , groupname1 , cycle_exists, hdferr )

    if ( .not. cycle_exists )    then

      call h5fclose_f (  file_id(0) , hdferr )
      call h5fclose_f (  file_id(i) , hdferr )

      cycle

    end if

    ! ------------------------------------------------
    !  get lastCycle and nmembers from relevant files
    ! ------------------------------------------------

    call h5gopen_f (     file_id(0) , groupname1 , group_id(0) , hdferr )
    call h5gopen_f (     file_id(i) , groupname1 , group_id(i) , hdferr )

    call h5gn_members_f( file_id(0) , groupname1 ,  lastCycle  , hdferr )
    call h5gn_members_f( file_id(i) , groupname1 ,   nmembers  , hdferr )

    ! --------------------------------
    !  find minimum cycle in cur file
    ! --------------------------------

    minCycle = huge( tmpCycle )

    do j = 0 , (  nmembers - 1 )

      call h5gget_obj_info_idx_f(  file_id(i)   , groupname1 ,   &
                                   j , obj_name ,  obj_type  , hdferr )  

      read( obj_name(3:) , '(i8)' , iostat = ios )  tmpCycle

      if ( ios .ne. 0 )  exit

      if ( minCycle .gt. tmpCycle )   minCycle   =  tmpCycle

    end do

    if ( ios .ne. 0 )  cycle

    ! ----------------------------
    !  check for an early restart
    ! ----------------------------

    do j = (lastCycle-1) , 0 , -1

      call h5gget_obj_info_idx_f (file_id(0) , groupname1 , j , obj_name , obj_type , hdferr)  

      read( obj_name(3:) , '(i8)' ) oldCycle

      if ( oldCycle .ge. minCycle ) then

        call h5ldelete_f ( group_id(0) , trim(obj_name) , hdferr )
        earlyRestart(i) = .true.

      end if

    end do

    ! --------------------------
    !  copy cycles to main file
    ! --------------------------

    do k = 0 , (nmembers - 1)

      call h5gget_obj_info_idx_f (    file_id(i) , &
                       groupname1  ,k, obj_name  , obj_type , hdferr )

      call h5ocopy_f(  group_id(i)  ,  obj_name  ,    &
                       group_id(0)  ,  obj_name  ,  hdferr  )

    end do

    ! ---------------------------------------------------
    !  close cur h5 file and main hfiles for consistency 
    ! ---------------------------------------------------
    
    call h5gclose_f ( group_id(0) , hdferr )
    call h5gclose_f ( group_id(i) , hdferr )

    call h5fclose_f (  file_id(0) , hdferr )
    call h5fclose_f (  file_id(i) , hdferr )

  end do


  ! ==========================
  !  check for an early merge
  ! ==========================

  earlyMerge  =  .false. 
  newCycle    =   ncycrs

  if ( .not. isAutoRun ) then

    call h5fopen_f( trim(baseFile)//".h5" , H5F_ACC_RDWR_F , file_id(0) , hdferr )    
    call h5gopen_f( file_id(0) ,   groupname1  , group_id(0) , hdferr )    

    call h5gn_members_f( file_id(0) , groupname1 , lastCycle , hdferr )

    do j = (lastCycle-1) , 0 , -1

      call h5gget_obj_info_idx_f ( file_id(0) , &
                        groupname1 , j , obj_name , obj_type , hdferr )  

      read( obj_name(3:) , '(i8)' ) oldCycle

      if ( oldCycle .gt. newCycle ) then

        call h5ldelete_f ( group_id(0) , trim(obj_name) , hdferr )
        earlyMerge  =  .true.

      end if

    end do

    call h5gclose_f ( group_id(0) , hdferr )
    call h5fclose_f(   file_id(0) , hdferr )

  end if


  ! =============================
  !  identify new finals section
  ! =============================

  i  =  nfiles

  if ( earlyMerge )  i = i + 1
  
  write( tmpFile , '(i3)' )  i
  tmpFile  = trim(baseFile)//"_"//trim(adjustl(tmpFile))//".h5"

  if ( .not. earlyMerge )  then

    hdf_filename_holder  =  tmpFile

  else

    ! -----------------------------------
    !  make new section for early merges
    !   ( whether it's a crash or not )
    ! -----------------------------------

    hdf_filename_holder  =  hdf_filename
    hdf_filename         =  tmpFile

    call hdfinit
    call hdfclose  
    call h5open_f(hdferr)   ! gets closed in hdfclose

    hdf_filename         =  hdf_filename_holder
    hdf_filename_holder  =  tmpFile

  endif


  ! =========================
  !  copy new finals section
  ! =========================

  call h5fopen_f (  hdf_filename_holder  , H5F_ACC_RDWR_F , file_id(i+1) , hdferr )
  call h5fopen_f ( trim(baseFile)//".h5" , H5F_ACC_RDWR_F , file_id(  0) , hdferr )

  call h5lexists_f(    file_id(  0) , groupname2 , final_exists , hdferr )
  if ( final_exists )                                                  &
    call h5ldelete_f ( file_id(  0) , groupname2 ,                hdferr )

  call h5ocopy_f  (    file_id(i+1) , groupname2 ,                     &
                       file_id(  0) , groupname2 ,                hdferr )

  call h5fclose_f (    file_id(i+1) , hdferr )
  call h5fclose_f (    file_id(  0) , hdferr )

  call  h5close_f (                   hdferr )


  ! ==============================
  !  .out part of merging process
  ! ==============================

  open( unit_out , file = trim(baseFile)//".out" , status   =  'old'   ,  &
                   form = 'formatted'            , position = 'append' )

  do i = 1 , nfiles

    write( tmpFile , '(i3)' ) i
    tmpFile  = trim(baseFile)//"_"//trim(adjustl(tmpFile))

    open ( unit_oldout , file =     trim(tmpFile)//".out"     ,  &
                         form =         'formatted'           )

    ! ---------------------------
    !  create filler white-space
    ! ---------------------------

    write ( unit_out , '(3/)' )

    if ( earlyRestart(i) ) then

      write ( unit_out , '(4(3x,a,/),2/)' )     &
          '-----------------------------------------------------' ,   &
          ' The next restart started at conditions that were    ' ,   &
          ' different than the previous run''s final conditions.' ,   &
          '-----------------------------------------------------'

    end if

    ! ---------------------------------------------------
    !  copy text from an old output file to the main one
    ! ---------------------------------------------------

    ios = 0

    do while( .not. is_iostat_end(ios) )

      read  ( unit_oldout , '(a  )' , advance = 'no' , iostat = ios ) curString
      write ( unit_out    , '(a,/)' , advance = 'no' )          trim( curString )

    end do

    close (   unit_oldout )

  end do

  ! --------------------
  !  wrap up .out write
  ! --------------------

  if ( earlyMerge ) then

    write ( unit_out , '(3/,3(3x,a,/),(3x,a))' )     &
        '-----------------------------------------------------' ,   &
        ' The next restart started at conditions that were    ' ,   &
        ' different than the previous run''s final conditions.' ,   &
        '-----------------------------------------------------'

  end if

  close ( unit_out )


  ! ======================================
  !  delete old restarts and output files
  !  unit_oldout is re-used here for ease
  ! ======================================

  ! ---------------------------
  !  restart new restart files
  ! ---------------------------

  if ( isAutoRun )  then
    call   rstrto
  else
    open(   unit   =  unit_resnew  ,   file   =  trim(baseFile)//'.new_restart'  &
        ,  status  =   'replace'   ,   form   =  'unformatted'        )
    close(  unit   =  unit_resnew  )
  end if

  ! --------------------------
  !  delete old restart files
  ! --------------------------

  if ( isAutoRun )  then
    curFilename =  trim(baseFile)//'.restart_2'
  else
    curFilename =  trim(baseFile)//'.cur_restart'
  end if

  open(   unit  =  unit_oldout  ,   file   =  curFilename  )
  close(  unit  =  unit_oldout  ,  status  =    'delete'   )

  ! -----------------------------
  !  delete old out and h5 files
  ! -----------------------------

  do i = 0 , nfiles+1

    write( tmpFile , '(i3)' ) i
    tmpFile  =  trim(baseFile)//'_'//trim( adjustl(tmpFile) )

    open ( unit_out    ,  file  =  trim(tmpFile)//'.out'     )  
    open ( unit_oldout ,  file  =  trim(tmpFile)//'.h5'      ) 
    open ( unit_resnew ,  file  =  trim(tmpFile)//'.restart' ) 

    close( unit_out    , status =          'delete'           ) 
    close( unit_oldout , status =          'delete'           ) 
    close( unit_resnew , status =          'delete'           ) 

  end do


  return


  end subroutine merge


end module merge_mod
