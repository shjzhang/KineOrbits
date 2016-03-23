c
c   subroutine rhpxhdr
c
      subroutine rhpxhdr(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ)
c
c=======================================================================
c     ****f* SmartPPP/rhpxhdr
c
c   FUNCTION   
c   
c     read information from hprnx header file, and RETURN the following
c     information:
c
c     a) satellite system,  satellite system number
c     b) observation types, observation types number
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     csat_sys(*)   characeter     returned satellite systems in rinex 
c                                  file
c     nobs_typ(*)   integer        observation types number for existed 
c                                  satellite systems.
c     cobs_typ(*,*) character      observation types for existed 
c                                  satellite systems.
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: rhpxhdr.f,v 1.0 2009/07/26 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
c     input/output
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
      character*60  MARKER_NAME
c     Center Of Mass offset      
      real*8        COM_OFF_NEU(3)
c
c     local variable
c
c     RINEX header
c
      character*80  line
      character*20  clbl
      character*80  cmmt
c
      integer       ihdr
      integer       i, j, k, ios
c
      common /hpx/  MARKER_NAME
c
      real*8        ANT_OFF_NEU(3)
c
      common /ANT_OFF/ ANT_OFF_NEU
c
      character*20  ANT_NUM, RCV_ANT_TYP
c
      common /RCV_ANT_TYP/ RCV_ANT_TYP
c
      do i=1,3
         ANT_OFF_NEU(i) = 0.0d0
      enddo
c
c     reading rinex header information
c     ================================
c
      ihdr = 0
c
 100  continue             
c
c     read the header of the RINEX file
c     =================================
c
      read( 101, '(A80)', end=200, iostat=ios) line
c
      if(ios.NE.0)then
         write(*,*) 'SmartPPP/rhpxhdr.f'
         write(*,*) 'read rinex header file error'
         stop
      endif
c
      clbl = line(61:80)
      write(*,*) clbl
c
      ihdr = ihdr + 1
c
      if(ihdr.gt.40)then
         write(*,*) 'SmartPPP/rhpxhdr.f'
         write(*,*) 'rinex header record is more than:', ihdr
         write(*,*) 'change the record line in rhpxhdr.f'
         stop
      endif
c
c++   RETURN rinex information
c
c     RINEX VERION / TYPE
c
      if(   clbl.EQ.'RINEX VERSION / TYPE')then
c
         read(line( 1: 9),*) rnxvsn
c
         if(rnxvsn.NE.3.0)then
            read(line(41:41),*) csat_sys(1)
         endif
c
         if(rnxvsn.lt.2.00)then
            write(*,*) 'SmartPPP/rhpxhdr.f'
            write(*,*) '  this program does not process observation ',
     +                 '  file with rinex version before 2.10'
            stop
         endif
c
         goto 100
c
c     ANT # / TYPE
      elseif(clbl.EQ.'ANT # / TYPE        ')then
c
         read(line( 1:40),'(2A20)') ANT_NUM, RCV_ANT_TYP
c
         goto 100
c
c     ANTENNA OFFSET
      elseif(clbl.EQ.'ANTENNA: DELTA H/E/N')then  ! for ground receiver
c
         read(line( 1:60),'(3F14.4)') ANT_OFF_NEU(3),
     &                                ANT_OFF_NEU(2),ANT_OFF_NEU(1)
c
         goto 100

      elseif( clbl.EQ.'ANTENNA: DELTA X/Y/Z')then  ! for SPACEBORNE satellite
c
         read(line( 1:60),'(3F14.4)') ANT_OFF_NEU(1),
     &                                ANT_OFF_NEU(2),ANT_OFF_NEU(3)
c     !!! 
c     if MARKER_TYPE.EQ.SPACEBORNE, the antenna's reference point is 
c     corresponding to the satellite-fixed coordinate.
c     !!!
c
         goto 100

c     Center Of Mass OFFSET
      elseif(clbl.EQ.'CENTER OF MASS: XYZ  ')then  ! for SPACEBORNE satellite
c
         read(line( 1:60),'(3F14.4)') COM_OFF_NEU(1),
     &                                COM_OFF_NEU(2),COM_OFF_NEU(3)

         write(*,*) COM_OFF_NEU(1), COM_OFF_NEU(2), COM_OFF_NEU(3)
c     !!! 
c     Center of Mass is corresponding to the satellite-fixed coordinate.
c     !!!
c
         goto 100
c
c     MARKER NAME
      elseif(clbl.EQ.'MARKER NAME         ')then
c
         read(line( 1:60),'(A60)') MARKER_NAME
c
         goto 100
c
c     # / TYPES OF OBSERV 
      elseif(clbl.EQ.'# / TYPES OF OBSERV ')then
c
c        RINEX version .NE. 3.00, set satellite systems = 1
         nsat_sys = 1
c
c        read NUMBER OF TYPES OF OBSERV
         read(line(1:6), '(I6)') nobs_typ(1)
c
c        types number ?
         if(    nobs_typ(1) .le. 9)then
c
c        read the first line of TYPES of OBSERV
         read(line(7:60),'(9(4X,A2))')(cobs_typ(1,i),i=1,nobs_typ(1))
c
         elseif(nobs_typ(1) .gt. 9 .and. nobs_typ(1) .le. 18)then
c
c        read the first line of TYPES of OBSERV
         read(line(7:60),'(9(4X,A2))')(cobs_typ(1,i),i=1,9)
c
         read( 101,'(A80)') line
c
c        read the second line of TYPES of OBSERV
         read(line(7:60),'(9(4X,A2))')(cobs_typ(1,i),i=10,nobs_typ(1))
c
         else
c
         write(*,*) 'SmartPPP/rhpxhdr.f'
         write(*,*) 'this program can not process observation types'
         write(*,*) 'more than 18'
         stop
c
         endif
c
         goto 100
c
c     SYS / # / OBS TYPES : rinex 3.00
c
      elseif(clbl.EQ.'SYS / # / OBS TYPES ')then
c
         write(*,*) 'SmartPPP/rhpxhdr.f'
         write(*,*) '  Extended for RINEX 3.00'
         stop
c
         goto 100
c
c     End of header
      elseif(clbl.EQ.'END OF HEADER       ')then
c
         goto 200
c
      endif
c
      goto 100
c
c     End of header processing
c
 200  continue

c
c     Transform the ANT_OFF_NEU relative to origin of SF(satellite frame) to 
c     relative to the Center of Mass
      ANT_OFF_NEU(1) = ANT_OFF_NEU(1) - COM_OFF_NEU(1)
      ANT_OFF_NEU(2) = ANT_OFF_NEU(2) - COM_OFF_NEU(2)
      ANT_OFF_NEU(3) = ANT_OFF_NEU(3) - COM_OFF_NEU(3)
c
      write(*,*) (ANT_OFF_NEU(i),i=1,3)
c
      return
c
      end
