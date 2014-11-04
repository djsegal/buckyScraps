module quitz_mod

  use  cntrl_mod  , only :  isAutoRun , isFirstCall   ,  &
                            nrestarts , lastmerge

  use  merge_mod  , only :  merge

  use rstrto_mod  , only :  rstrto

  use params_mod  , only :  i_kind    , unit_ioscrach ,  &
                            baseFile

  implicit none


contains


  subroutine quitz


    ! =====================================================
    !  -quitz- terminates the last input file in bucky.inp
    ! =====================================================

    integer(i_kind)        ::  curFileIndex

    logical                ::  restartExists1  =  .false.
    logical                ::  restartExists2  =  .false.

    character(len=i_kind)  ::  curFile1
    character(len=i_kind)  ::  curFile2

    character(len=35)      ::  rstrt_filename1
    character(len=35)      ::  rstrt_filename2


    ! =======================
    !  delete unneeded files
    ! =======================

    open(   unit_ioscrach  ,   file  = trim(baseFile)//'.restart_0'  )
    close(  unit_ioscrach  ,  status =      'delete'      )

    open(   unit_ioscrach  ,   file  = trim(baseFile)//'.restart_2'  )
    close(  unit_ioscrach  ,  status =      'delete'      )


    ! ===================
    !  wrap up auto runs
    ! ===================

    if ( isAutoRun )  then

      call merge
      call rstrto

      return

    end if


    ! =========================================
    !  check if the cur and new restarts exist
    ! =========================================

    if ( .not. isFirstCall )  then

      INQUIRE( FILE = trim(baseFile)//'.cur_restart' , EXIST = restartExists1 )
      INQUIRE( FILE = trim(baseFile)//'.new_restart' , EXIST = restartExists2 )

    end if


    ! ===================================
    !  rename both files when they exist
    ! ===================================

    if (  restartExists1  .and.  restartExists2  )  then

      curFileIndex  =  nrestarts - lastmerge

      WRITE( curFile1 , "(i3)" ) curFileIndex
      WRITE( curFile2 , "(i3)" ) curFileIndex + 1

      rstrt_filename1  =  trim(baseFile)//trim(adjustl(curFile1))//".restart"
      rstrt_filename2  =  trim(baseFile)//trim(adjustl(curFile2))//".restart"

      call rename( trim(baseFile)//'.cur_restart' , rstrt_filename1 )
      call rename( trim(baseFile)//'.new_restart' , rstrt_filename2 )

      return

    end if


    ! ================================
    !  rename a restart if one exists
    ! ================================

    if (  restartExists1  .or.  restartExists2  )  then

      curFileIndex  =  nrestarts - lastmerge

      WRITE( curFile1 , "(i3)" ) curFileIndex + 1

      rstrt_filename1  =  trim(baseFile)//trim(adjustl(curFile1))//".restart"

      if ( restartExists1 )  call rename( trim(baseFile)//'.cur_restart' , rstrt_filename1 )
      if ( restartExists2 )  call rename( trim(baseFile)//'.new_restart' , rstrt_filename1 )

      return

    end if


    ! =============================
    !  return for every other case
    ! =============================

    return


  end subroutine quitz


end module quitz_mod
