*
*  program orbit_compare
*
      program orbit_compare
*      
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*  
*    compare different position file.  
*  
*  Notes
*  =====
*
*    Details of the orbit elements , please referece the book, Satellite
*    ORBITS- Theory and Model. by Oliver and Gill
*
*  History
*  =======
*
*    Vesion 1.0
*    ----------
*
*********1*********2*********3*********4*********5*********6*********7**
c
      implicit none
c
      integer       MAX_EPO
      parameter(    MAX_EPO = 100000)
c
c     variables in getopts
c
      integer       MAX_OPTS
      parameter    (MAX_OPTS = 20)
      integer       nopts
      parameter    (nopts    =  6)
      integer       stat
      character*100 opts(MAX_OPTS)
c
      real*8        epo1(MAX_EPO)
      real*8        x1(MAX_EPO)
      real*8        y1(MAX_EPO)
      real*8        z1(MAX_EPO)
      real*8        dt1(MAX_EPO)
      real*8        epo2(MAX_EPO)
      real*8        x2(MAX_EPO)
      real*8        y2(MAX_EPO)
      real*8        z2(MAX_EPO)
      real*8        dt2(MAX_EPO)
c
c
      integer       idx_t1
c
      real*8        tt1
      real*8        tx1,ty1,tz1,tx2,ty2,tz2
      real*8        dx,dy,dz
c
      character*200 ifile(2)
      character*200 ofile
c
      character*200 line
c
c     local variable
c
      integer       i, j, k, ios
      integer       irec, nrec1, nrec2
      logical       find
c
      do i=1, MAX_EPO
         epo1(i) = 0.0d0
         epo2(i) = 0.0d0
      enddo
c
      do i=1, MAX_EPO
        x1(i) = 0.0d0
        y1(i) = 0.0d0
        z1(i) = 0.0d0
        x2(i) = 0.0d0
        y2(i) = 0.0d0
        z2(i) = 0.0d0
      enddo
c
c     read file names from command line
c
      call getopts(nopts, opts, stat)
c
      if(stat.ne.0)then
        write(*,*)'orbit_compare 2008/08/02 by Soujian Zhang'
        write(*,*)
        write(*,*)
     +  ' Usages:','orbit_compare',
     +  ' -iorb [position file] ',
     +  ' -iorb [position file] ',
     +  ' -odif [position difference file]'
        stop
      endif
c
      ifile(1) = opts(2)
      ifile(2) = opts(4)
      ofile    = opts(6)
c
      open(101, file=ifile(1), status="old",     iostat=ios)
      open(102, file=ifile(2), status="old",     iostat=ios)
      open(201, file=ofile   , status="replace", iostat=ios)
c
      if(ios.ne.0)then
        write(*,*) 'open files error in orbit_compare!'
        stop
      endif
c
c     read position from ifile(1) 
c        
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
      irec = 0
      line = ' '
c
100   continue
c
      read(101,'(A200)',end=200)  line
c
      irec = irec+1
c
      read(line,*)  epo1(irec), x1(irec),y1(irec),z1(irec),dt1(irec)
c
      goto 100
c
200   continue
c
      nrec1 = irec 
c
      if(nrec1.eq.0)then
        write(*,*) 'There are no record?'
        stop
      endif
c
c     read position from ifile(2) 
c        
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
      write(*,*) 'a'
     
      line = ' '
      irec = 0
c
300   continue
c
      read(102,'(A200)',end=400)  line
c
      irec = irec + 1
c
      read(line,*) epo2(irec),
     &             x2(irec),y2(irec),z2(irec),dt2(irec)
c
      goto 300
c
400   continue
c     
      nrec2 = irec 
c
c     difference 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
c
      k = 0
c
      do irec=1, nrec1
c
         tt1 = epo1(irec)  
         tx1 = x1(irec)
         ty1 = y1(irec)
         tz1 = z1(irec)

         write(*,*) tt1, tx1, ty1, tz1
c
         call lagrange(epo2,x2,nrec2,tt1,tx2)
         call lagrange(epo2,y2,nrec2,tt1,ty2)
         call lagrange(epo2,z2,nrec2,tt1,tz2)

         dx = tx1 - tx2
         dy = ty1 - ty2
         dz = tz1 - tz2
         write(*,*) tt1, tx2, ty2, tz2
c
         write(201,'(F20.8,3F14.3)') tt1, dx, dy, dz
c
      enddo
c
      close(101)
      close(102)
      close(201)
c
      write(*,*) 'Stop Normally'
c
      stop
c
      end
