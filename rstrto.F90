module rstrto_mod

    ! ------------------
    !   == CENSORED == 
    ! ------------------
    
  use params_mod
  use cntrl_mod 
  use options_mod

  implicit none


contains


  subroutine rstrto


    ! ====================================================
    !  rstrto dumps all the data from every common blocks 
    !           into a binary file for restarts
    !
    !    ***  updated as of 7/23/14 -- version 682  ***
    ! ====================================================

    integer(i_kind)      ::  i , j , k , l

    integer(i_kind)      ::  curVal

    CHARACTER(LEN=35)    ::  curFile

    ! -------------------------------------------
    !  testValue used for verifying restart data
    ! -------------------------------------------

    integer(i_kind)      ::  testValue  =  -12345


    ! ==========================================
    !  auto restart mode opens new restarts now
    ! ==========================================

    if ( mstart.eq.2 )  then

        curVal   =  mod( chkPtCount-1 , 2 ) + 1    !  0 1 2 1 2 1 2 ...

        WRITE( curFile , "(i3)" ) curVal

        curFile  =  trim(baseFile)//'.restart_'//trim( adjustl(curFile) )

        open(  unit_resnew        ,  file = curFile              &
            ,  status = 'replace' ,  form = 'unformatted'        )

        chkPtCount  =  chkPtCount + 1

    else

        open(  unit_resnew        ,  file = trim(baseFile)//'.new_restart'   &
            ,  status = 'old'     ,  form = 'unformatted'        ,  position = 'append'  )

    endif


    ! ===================================================================================
    !                         Write out Restart Information
    ! ===================================================================================

    ! ----------------------------
    !  testValue should be -12345
    ! ----------------------------

    write(unit_resnew) testValue


    ! ===================================================================================
    !                              END  OF  RESTART  WRITE
    ! ===================================================================================


    ! ============================
    !  close the new restart file
    ! ============================

    close ( unit_resnew )


    ! =========================
    !  restart write completed
    ! =========================

    return


  end subroutine rstrto

end module rstrto_mod