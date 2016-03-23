*
*  Procedure orbcvt
*
      program orbcvt
*      
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*  
*  1) convert the LEO's state vector and difference from ITRF to ICRF, 
*  2) calculate the orbit elements from the state vector 
*  3) transform the difference between JPL and SGG from the ICRF
*     to the orbit Radial Along-track and Cross-track coordinate  
*  
*  Notes
*  =====
*
*  Details of the orbit elements , please referece the book, Satellite
*  ORBITS- Theory and Model. by Oliver and Gill
*
*  History
*  =======
*
*  Vesion 1.0
*  ----------
*    
*  Time         Author      Description
*  ----         ------      --------------------------------------------
*  07/06/22     S.J.Zhang   build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
c
      implicit none
c
      include      'rinex.params'
c
c     variables in getopts
c
      integer       MAX_OPTS
      parameter    (MAX_OPTS = 20)
      integer       nopts
      parameter    (nopts    = 6)
      integer       stat
      character*100 opts(MAX_OPTS)
c
c     local variable
c
      integer       i, j, k, ios
      integer       irec, nrec
c
      real*8        tepo, rcv_pos(3), rcv_vel(3), dt
c
      real*8        epo(MAX_EPO)
      real*8        VEC_ECF(MAX_EPO,6)
      real*8        VEC_ECI(MAX_EPO,6)
      real*8        DIF_ECF(MAX_EPO,6)
      real*8        DIF_ECI(MAX_EPO,6)
c
      real*8        TMP_VEC_ECF(6)
      real*8        TMP_VEC_ECI(6)
      real*8        TMP_DIF_ECF(6)
      real*8        TMP_DIF_ECI(6)
c
      real*8        TMP_DIF_POS(3)
      real*8        TMP_DIF_RAC(3)
c
      character*200 rcv_rst_file
      character*200 diforb1_file
      character*200 diforb2_file
c
c     read file names from command line
c
      call getopts(nopts, opts, stat)
c
      if(stat.ne.0)then
        write(*,*)'knmpod  2008/08/02 by Soujian Zhang'
        write(*,*)
        write(*,*)
     +  ' Usages:','knmpod',
     +  ' -rcv_rst [rcv result file from SGG] ',
     +  ' -diforb1 [orbit difference from GFZ in ECF] ',
     +  ' -diforb2 [orbit difference from GFZ in RAC]'
        stop
      endif
c
      rcv_rst_file = opts(2)
      diforb1_file = opts(4)
      diforb2_file = opts(6)
c
      write(*,*) rcv_rst_file
      write(*,*) diforb1_file
      write(*,*) diforb2_file
c
      open(102, file=rcv_rst_file, status="old",     iostat=ios)
      open(103, file=diforb1_file, status="old",     iostat=ios)
      open(104, file=diforb2_file, status="replace", iostat=ios)
c
      if(ios.ne.0)then
        write(*,*) 'open files error in orbcvt!'
        stop
      endif
c
c     read LEO's state vector from rcv_rst_file in 
c     ECF(Earth Center Fixed)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
c
      irec = 1
c
100   continue
c
c     read(102, *, end=200) epo(irec), ( rcv_pos(i), i=1,3 ), dt, 
c    +                                 ( rcv_vel(i), i=1,3 )
      read(102, *, end=200) epo(irec), ( rcv_pos(i), i=1,3 ), dt
c
      VEC_ECF(irec, 1) = rcv_pos(1)
      VEC_ECF(irec, 2) = rcv_pos(2)
      VEC_ECF(irec, 3) = rcv_pos(3)
      VEC_ECF(irec, 4) = rcv_vel(1)
      VEC_ECF(irec, 5) = rcv_vel(2)
      VEC_ECF(irec, 6) = rcv_vel(3)
c
      irec = irec + 1
c
      goto 100
c
200   continue
c
      nrec = irec - 1
c
c     judge the number of orbit and difference
c
      if(nrec.eq.0)then
        write(*,*) 'There are no record?'
        stop
      endif
c
c     read orbit difference from diforb1_file  
c        
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
c        
      irec = 1
c
300   continue
c
      read(103, *, end=400) epo(irec), ( DIF_ECF(irec,i), i=1,3 ), dt
c
      irec = irec + 1
c
      goto 300
c
400   continue
c     
      nrec = irec - 1
c
c     Transform ?
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
c
      do irec=1, nrec
c
         tepo = epo(irec)
c
         do k=1, 6
           TMP_VEC_ECF(k) = VEC_ECF(irec, k)
           TMP_DIF_ECF(k) = DIF_ECF(irec, k)
         enddo
c
c        write(*,*)  tepo
c        write(*,*) (TMP_VEC_ECF(k),k=1,6)
c        write(*,*) (TMP_VEC_ECI(k),k=1,6)
c
c        convert state from ECF to ECI
c
         call ITRS2ICRS(tepo, TMP_VEC_ECF, TMP_VEC_ECI)
c
         call ITRS2ICRS(tepo, TMP_DIF_ECF, TMP_DIF_ECI)
c
         do k=1, 3
            TMP_DIF_POS(k) = TMP_DIF_ECI(k)
         enddo
c
c        convert difference from ECI to RAC
c
         call eci2rac(TMP_VEC_ECI, TMP_DIF_POS, TMP_DIF_RAC)
c
         write(104,'(f14.3, 3f7.3)')  tepo, (TMP_DIF_RAC(k),k=1,3)
         write(*  ,'(f14.3, 3f7.3)')  tepo, (TMP_DIF_RAC(k),k=1,3)
c
      enddo
c
      close(102)
      close(103)
      close(104)
c
      stop
c
      end
