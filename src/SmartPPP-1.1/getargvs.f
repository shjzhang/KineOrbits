c
c   subroutine getargvs
c
      subroutine getargvs(nargv, argvs, stat)
c
c=======================================================================
c     ****f* qualicontr/getargvs
c
c   FUNCTION   
c   
c     Read command-line arguments and check for valid options. This
c     function is a mimic of the shell funtion: getargvs
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     nargv  integer    (o) options number
c     argvs  character  (o) options character array
c     stat   integer    (o) status:
c
c                           1 - if the argument number from the command
c                               line is greater than variable "nargv',
c                               then return 1;
c                           0 - if the argument number from the command
c                               line is less and equal than variable 
c                               "nargv', then return 0.
c
c   COPYRIGHT
c
c     Copyright(c) 2006-    Shoujian Zhang
c                           School of Geodesy and Geomatics
c                           Wuhan University.
c     ***
c
C     $Id: getargvs.f,v 1.0 2009/06/28 $
c=======================================================================
c
      implicit none
c
c     External function
c
      integer       iargc
c
c     Parameters
c
      integer       MAX_OPTS
      parameter    (MAX_OPTS = 20)
c
c     Input/output variables
c
      integer       stat
      integer       nargv
      character*200 argvs(MAX_OPTS)
c
c     Local variables
c
      integer       i, j, k
      character*200 argv
c
      stat = 0
c
c     Get the number of arguments from the command line
c     iargc is a library function of gfortran
c
      nargv = iargc()
c
c     check if the number of arguments from command line.
c     if the number is 0. set stat=1
c     if the number is Not a zero. set stat=0
c
      if(nargv == 0)then
c
        stat = 1
c
        return
c
      endif
c
c     parse argumetns from command-line
c
      i = 1
      do while(i <= nargv)
c
c        read arguments from command-line 
c
         call getarg(i, argv)
c
         if(i.gt.MAX_OPTS)then
            write(*,*) 'qualicontr/getargvs.f'
            write(*,*) 'Too many arguments from command line'
            write(*,*) 'Argument number should be less than', MAX_OPTS
            stop
         endif
c
         if(len_trim(argv) >= 200)then
            write(*,*) 'qualicontr/getargvs.f'
            write(*,*) 'the length of the Arguments name is too long'
            write(*,*) 'which should be less than', 200
            stop
         endif
c
         argvs(i) = trim(argv)
c
         i = i+1
c
      enddo
c
      return
c
      end
