module parseInputArguments_mod


  use   params_mod  ,  only  :  inpFile , baseFile  

  implicit none

  contains


    subroutine parseInputArguments


      character(LEN=20)  ::  curStr
      integer(KIND=4)    ::  curIndex
      logical            ::  outputExists
      integer(KIND=4)    ::  nArgs

      ! ==============================
      !  validate number of arguments
      ! ==============================

      nArgs = iargc()
 
      call  getarg( iargc() , curStr )

      if ( curStr(1:1).eq.' ' )  nArgs = nArgs - 1

      if ( nArgs > 3 )  stop 'Error: Too many input arguments.'

     
      ! ======================= 
      !  get input file's name
      ! =======================

      call getarg(1, inpFile )


      ! ===============================
      !  get the base output file name
      !   for when a flag isn't given
      ! ===============================

      if ( nArgs < 3 )  then

        call  getarg( nArgs , baseFile )

        return

      end if


      ! ================================
      !     use flag to determine if
      !  overwriting prevention is used
      ! ================================

      call  getarg( 2 , baseFile )
      call  getarg( 3 ,  curStr  )

      ! ------------------------------------
      !  if false flag , overwrite baseFile
      ! ------------------------------------

      if (  curStr(1:1).eq.'0'  .or.  &
            curStr(1:1).eq.'F'  .or.  &
            curStr(1:1).eq.'f'  .or.  &
            curStr(1:1).eq.'N'  .or.  &
            curStr(1:1).eq.'n'        )  return

      ! ---------------------------------
      !  otherwise , prevent overwriting
      ! ---------------------------------

      INQUIRE( FILE = trim(baseFile)//'.out' , EXIST = outputExists )

      if ( .not. outputExists )  return

      curIndex = 0

      do

        curIndex = curIndex + 1

        write( curStr , '(i5)' )  curIndex

        curStr = trim(baseFile)//trim(adjustl(curStr))

        INQUIRE( FILE = trim(curStr)//'.out' , EXIST = outputExists )

        if ( .not. outputExists )  exit

      end do

      baseFile = curStr

  
    end subroutine parseInputArguments

end module parseInputArguments_mod
