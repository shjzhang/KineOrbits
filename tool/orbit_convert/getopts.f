c
c   subroutine getopts
c
      subroutine getopts(nopts, opts, stat)
*******************************************************************************
c
c   Purpose
c   =======
c   
c   Read command-line arguments and check for valid options. This
c   function is a mimic of the shell funtion: getopts
c      
c
c   Notes
c   =====
c
c   Input/Ouput
c   ===========   
c
c   Name         Type    I/O     Description
c   ------------+-------+-------+-----------------------------------------------
c   nopts        I       I       input options number
c   opts         C       O       output options character array
c   stat         I       O       output status:
c                                if options is not equal with nopts, return 1, 
c                                operate errorly
c                                if options is equal with nopts, return 0, 
c                                operate normally
c
c   Version
c   =======
c
c   1.0
c
c   Time         Author          Description
c   ------------+---------------+-----------------------------------------------
c   2008/04/08   S.J. Zhang      build this subroutine
c
c
c   ------------+---------------+-----------------------------------------------
c
********************************************************************************
c
      implicit none
c
c     external function
c
      integer       iargc
c
c     parameters
c
      integer       MAX_OPTS
      parameter    (MAX_OPTS = 20)
c
c     input/output variables
c
      integer       stat
      integer       nopts
      character*100 opts(MAX_OPTS)
c
c     local variables
c
      integer       i, j, k
      integer       nargc
      character*100 argv
c
c     get the number of arguments passed on the command line
c
      nargc = iargc()
c
c     check if the number of arguments from command line is equal with
c     the nopts, which is the number of arguments needed.
c
      stat = 0
c
      if(nargc.ne.nopts)then
c
c       if nargc is not equal with nopts, return 1, means error
c       if nargc is     equal with nopts, return 0, means normally
c
        stat = 1
c
        return
c
      endif
c
      if(nargc.eq.nopts)then
        i = 1
        do while(i.le.nargc)
c
c       read arguments from command-line 
c
        call getarg(i, argv)
c
        opts(i) = trim(argv)
c
        i = i+1
        enddo
      endif
c
      return
c
      end
