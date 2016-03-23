c
c   subroutine rnxhdr
c
      subroutine rnxhdr(rnxvsn,nsat_sys,csat_sys,
     &                         nobs_typ,cobs_typ)
c
c=======================================================================
c     ****f* fmttrs/rnxhdr
c
c   FUNCTION   
c   
c     read information from RINEX header file, and complete the
c     following works:
c
c     a) REURN   rinex version,satellite system,satellite system number
c                observation types,observation types number
c
c     b) MODIFY 'PRM / RUN BY / DATE' with program name, user, 
c                and date inside
c
c     c) INSERT  Marker Type, antenna reference point, into the rinex
c                header if the rinex version is less than 3.00. the
c                Marker Type, antenna reference point, are set in the 
c                configure parameter in /etc/rinex.conf. 
c
c     d) WRITE   The MODIFIED and INSERTED information into hprnx file.
c
c   NOTES
c     
c     if the Maker Type is 'GEODETIC' or 'NON_GEODETIC', the receiver 
c     is static, usually, the antenna reference point is measured from 
c     the marker to the ARP and reported in the "ANTENNA: DELTA H/E/N" 
c     header record. for the kinematic receiver, the ARP is usually 
c     measured in the body-fixed coordinate system, and can be reported
c     in the "ANTENNA: DELTA X/Y/Z", but the RINEX version 2.10, 2.11
c     and 2.20, usually don't report this information because the 
c     kinematic reciver such as airplane, spacecrft(such as GRACE,CHAMP)
c     is not managed by IGS. and this information must be given by the
c     user himself. 
c
c     For the spacecraft such as LEO, there are three 'center' to be 
c     paid more attention. That is 'Center of MASS'. 'Center of ARP', 
c     'Center of PHASE'. Ususaly in kinematic platform, the Center of
c     PHASE is defined in body-fixed system, NOT relative the Center of
c     ARP. So the Center of ARP is not important and not used,
c     which is different from the satic ground-based receiver's.
c
c     The Center of MASS is must be given, for that the orbit
c     integration  expressed in inertial coordiate corresponds to the 
c     center of Mass.  
c
c     The Center of Phase is usually provied in another separate file. 
c     which is adopted by IGS nowadays. So, IN hpxrnx header, it is not
c     included.
c
c     The phase center variation is usually can be omitted, so the zero
c     direction of the antenna and the bore-sight direction can also
c     be omitted.
c
c     The attitude of the vehicle has to be provied by separate 
c     attitude files in the same body-fixed coordinate system. 
c
c     Details see RINEX 3.00
c
C   ARGUMENTS
C   
C     rnxvsn        real       (o) returned rinex version from rinex
C                                  header
c     csat_sys(*)   characeter (o) returned satellite systems in rinex 
c                                  file
c     nobs_typ(*)   integer    (o) observation types number for existed 
c                                  satellite systems.
c     cobs_typ(*,*) character  (o) observation types for existed 
c                                  satellite systems.
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: rnxhdr.f,v 1.0 2009/06/28 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
c
c     external funtion declaration
c
      real*8        cal2sec
c
c     variable for rinex header
c
c     RINEX VERSION / TYPE
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
      character*80  line
      character*20  clbl
c
      character*20  prog
      character*20  user
      character*20  date
c
      character*20  Marker_Number
c
c     local variable
c
      integer       i, j, k,ios
      integer       ihdr, nhdr
      integer       ihpx_hdr, nhpx_hdr
c     header label
      character*20  chpx_hdrlbl(80)
      character*20  crnx_hdrlbl(80)
c     header section
      character*60  chpx_hdrsec(80)
      character*60  crnx_hdrsec(80)
c
c     returned record number, header label, header section for special
c     input header label
c
      integer       rrec
      character*20  rrnx_hdrlbl(80)
      character*60  rrnx_hdrsec(80)
c
c     reading rinex header information
c     ================================
c
      do i=1, MAX_SAT_SYS
         csat_sys(i) = ' '
      enddo
c
      Marker_Number = ' '
c
      ihdr = 0
c
 100  continue             
c
c     read the header of the RINEX file
c
      read( 101, '(A80)', end=200, iostat=ios) line
c
      if(ios.NE.0)then
         write(*,*) 'fmttrs/rnxhdr.f'
         write(*,*) 'read rinex header file error'
         stop
      endif
c
      clbl = line(61:80)
c
      ihdr = ihdr + 1
c
      if(ihdr.gt.80)then
        write(*,*) 'fmttrs/rnxhdr.f'
        write(*,*) 'rinex header record is more than:', ihdr
        write(*,*) 'change the record line in rnxhdr.f'
        stop
      endif
c
      crnx_hdrsec(ihdr) = line( 1:60)
      crnx_hdrlbl(ihdr) = line(61:80)
c
c     RINEX VERSION / TYPE LINE
c
      if(clbl.EQ.'RINEX VERSION / TYPE')then
c
        read(line( 1: 9),*) rnxvsn
c
        if(rnxvsn.NE.3.0)then
          read(line(41:41),'(A1)') csat_sys(1)
        endif
c
        if(rnxvsn.lt.2.00)then
          write(*,*) 'fmttrs/rnxhdr.f'
          write(*,*) 'this program does not process observation ',
     +               'file with rinex version before 2.00'
          stop
        endif
      endif
c
c     End of header
      if(clbl.EQ.'END OF HEADER       ')then
c
        goto 200
c
      endif
c
      goto 100
c
 200  continue
c
c     rinex header record number
c
      nhdr = ihdr
c
c     Determine HPRNX header label array
c     ==================================
c
c     According to the RINEX version, "MARKER TYPE" and "ANTENNA REFERENCE POINT"
c     from the etc/rinex.conf, determining the least hprnx label descriptor array.
c
      nhpx_hdr       = 0
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'RINEX VERSION / TYPE'   ! common 
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'PGM / RUN BY / DATE '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'MARKER NAME         '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'MARKER NUMBER       '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'MARKER TYPE         '   ! RINEX 2.20, 3.00
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'OBSERVER / AGENCY   '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'REC # / TYPE / VERS '   ! common  
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'ANT # / TYPE        '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'APPROX POSITION XYZ '   ! common
c
c     GEODETIC          Earth-fixed, high-precision monumentation
c     NON_GEODETIC      Earth-fixed,  low-precision monumentation
c
      if(trim(MARKER_TYPE).EQ.'GEODETIC' .or.
     &   trim(MARKER_TYPE).EQ.'NON_GEODETIC')then
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'ANTENNA: DELTA H/E/N'   ! common
c
      else
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'ANTENNA: DELTA X/Y/Z'   ! RINEX 2.20, 3.00
c
      endif
c
      if(trim(MARKER_TYPE).EQ.'SPACEBORNE')then
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'CENTER OF MASS: XYZ '   ! RINEX 2.20, 3.00
c
      endif
c
cc    if(rnxvsn.EQ.3.0)then
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'ANTENNA: PHASECENTER'   ! RINEX 2.20, 3.00
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'ANTENNA: B.SIGHT XYZ'   ! RINEX 2.20, 3.00
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'ANTENNA: ZERODIR AZI'   ! RINEX 2.20, 3.00
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'ANTENNA: ZERODIR XYZ'   ! RINEX 2.20, 3.00
c
cc    endif
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'WAVELENGTH FACT L1/2'   ! RINEX 2.11
c
      if(rnxvsn.NE.3.00)then
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = '# / TYPES OF OBSERV '   ! common
c
      else
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'SYS / # / OBS TYPES '   ! RINEX 3.00 OBS TYPES
c
      endif
c
      if(rnxvsn.EQ.3.0)then
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'SIGNAL STRENGTH UNIT'   ! RINEX 3.00 OBS TYPES
c
      endif
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'INTERVAL            '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'TIME OF FIRST OBS   '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'TIME OF LAST OBS    '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'RCV CLOCK OFFS APPL '   ! common
c
      if(rnxvsn.EQ.3.0)then
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'SYS / DCBS APPLIED  '   ! RINEX 3.00 OBS TYPES
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'SYS / PCVS APPLIED  '   ! RINEX 3.00 OBS TYPES
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'SYS / SCALE FACTOR  '   ! RINEX 3.00 OBS TYPES
c
      endif
c
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'LEAP SECONDS        '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = '# OF SATELLITES     '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'PRN / # OF OBS      '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'COMMENT             '   ! common
      nhpx_hdr              =  nhpx_hdr + 1
      chpx_hdrlbl(nhpx_hdr) = 'END OF HEADER       '   ! common
c
c    "RETRUN","MODIFY","INSERT" information into the hprnx header
c     ===========================================================
c
      do ihpx_hdr = 1, nhpx_hdr
c
c       find the hpxrnx header label in the rinex header array
c       if find=.true., then "MODIFY" the correponding
c          information
c       if find=.false.,then "INSERT" the information from rinex
c          configure parameters
c       The Modified, Inserted information along others' will be 
c       written in the hprnx header 
c
c       Meanwhile, RETURN some useful information to "rnxtrs" to
c       help process the observation data.
c       
c       rrec : returned record number 
c       rrnx_hdrlbl : returned rinex header label
c       rrnx_hdrsec : returned rinex header section
c
        clbl = chpx_hdrlbl(ihpx_hdr)
c
c       initialization
c
        line = ''
        do i = 1, 10
           rrnx_hdrlbl(i) = ''
           rrnx_hdrsec(i) = ''
        enddo
c
c       find the record in rinex header which match the input hprnx
c       header
c
        call find_rnxhdr(clbl, nhdr, crnx_hdrlbl, crnx_hdrsec, 
     &                         rrec, rrnx_hdrlbl, rrnx_hdrsec)
c
cccc    RETURN rinex information
c
c       RINEX VERION / TYPE
        if(   clbl.EQ.'RINEX VERSION / TYPE')then
c
          if(rrec.NE.1)then
             write(*,*) 'fmttrs/rnxhdr'
             write(*,*) ' "RINEX VERSION / TYPE" line abnormal'
             write(*,*) '  Not a rinex file? or More than 1 line'
             stop
          endif
c
          line = rrnx_hdrsec(1)
c
          read(line( 1: 9),*) rnxvsn
c
          if(rnxvsn.NE.3.0)then
             read(line(41:41),'(A1)') csat_sys(1)
          endif
c
          if(rnxvsn.lt.2.00)then
             write(*,*) 'fmttrs/rnxhdr.f'
             write(*,*) '  this program does not process observation ',
     +                  '  file with rinex version before 2.00'
             stop
          endif
c
          if(csat_sys(1).EQ.' ')then
             line(41:41)='G'
          endif        
c
          write(102,'(A60, A20)') line, clbl
c
c       # / TYPES OF OBSERV 
        elseif(clbl.EQ.'# / TYPES OF OBSERV ')then
c         RINEX version .NE. 3.00, set satellite systems = 1
          nsat_sys = 1
c         read the first line
c
          if(rrec.lt.1)then
            write(*,*) 'fmttrs/rnxhdr'
            write(*,*) '  "# / TYPES OF OBSERV" line is less than 1'
            stop
          endif
c
          line = rrnx_hdrsec(1)
c
c         read NUMBER OF TYPES OF OBSERV
          read(line(1:6), '(I6)') nobs_typ(1)
c
c         types number ?
          if(    nobs_typ(1) .le. 9)then
c
c         read the first line of TYPES of OBSERV
          read(line(7:60),'(9(4X,A2))')(cobs_typ(1,i),i=1,nobs_typ(1))
c
          write(102,'(A60,A20)') line, clbl
c
          elseif(nobs_typ(1) .gt. 9 .and. nobs_typ(1) .le. 18)then
c
          if(rrec.lt.2)then
             write(*,*) 'fmttrs/rnxhdr'
             write(*,*) '  returned "# / TYPES OF OBSERV" line is less '
             write(*,*) '  than 2, but observation type is', nobs_typ(1)
          endif
c
c         read the first line of TYPES of OBSERV
          read(line(7:60), '(9(4X,A2))')(cobs_typ(1,i),i=1,9)
c
          write(102,'(A60,A20)') line, clbl
c
c         read a new line which is continuation line of "# / TYPES OF OBSERV"
          line = rrnx_hdrsec(2)
c
c         read the second line of TYPES of OBSERV
          read(line(7:60),'(9(4X,A2))')(cobs_typ(1,i),i=10,nobs_typ(1))
c
          write(102,'(A60,A20)') line, clbl
c
          else
c
          write(*,*) 'fmttrs/rnxhdr.f'
          write(*,*) 'this program can not process observation types'
          write(*,*) 'more than 18'
          stop
c
          endif
c
c       MARKER NUMBER : rinex 2.10, 2.11, 2.20, 3.00.
c
        elseif(clbl.EQ.'MARKER NUMBER       ')then
c
          if(rrec.EQ.1)then
c
          line = rrnx_hdrsec(1)
c
          read(line(1:20),'(A20)') Marker_Number
c
C$        if(Marker_Number.gt.1)then
C$          write(*,*) 'fmttrs/rnxhdr.f'
C$          write(*,*) '  This program does not process multiple'
C$          write(*,*) '  antenna marker'
C$          stop
C$        endif
c
          write(102,'(A60,A20)') line, clbl
c
          endif
c
c       SYS / # / OBS TYPES : rinex 3.00
c
        elseif(clbl.EQ.'SYS / # / OBS TYPES ')then
c
          write(*,*) 'fmttrs/rnxhdr.f'
          write(*,*) 'modify for RINEX 3.00'
          stop
c
ccccc   MODIFY information rinex 2.10, 2.11, 2.20, 3.00
c
        elseif(clbl.EQ.'PGM / RUN BY / DATE ')then
c
c         PGM
c         replace raw information from rinex file with new parameters 
c
          prog = 'rnxtrs'
          call date_and_time(date) 
          call getenv("LOGNAME",user)
c
          write(line( 1:20),'(A20)') prog
          write(line(21:40),'(A20)') user
          write(line(41:60),'(A08)') date
c
          write(102,'(A60,A20)') line, clbl
c
ccccc   INSERT information for rinex 2.10, 2.11, 2.20, 3.00
c
        elseif(clbl.EQ.'MARKER TYPE         ')then
c
          write(line(1:20),'(A20)') MARKER_TYPE
c
          write(102,'(A60,A20)') line, clbl
c
c
c       CENTER OF ARP (Antenna Reference Point)
        elseif(clbl.EQ.'ANTENNA: DELTA X/Y/Z')then
c
          write(line(1:60),'(3F14.4)') ARP_DX, ARP_DY, ARP_DZ
c         
c         line = rrnx_hdrsec(1)
c
          write(102,'(A60,A20)')       line, clbl
c
c         CENTER OF MASS           
        elseif(clbl.EQ.'CENTER OF MASS: XYZ ')then
c
          write(line(1:60),'(3F14.4)') MAS_DX, MAS_DY, MAS_DZ
c
          write(102,'(A60,A20)')       line, clbl
c
ccccc   OUTPUT other useful information from RINEX 
c
        elseif(rrec.gt.0)then
c
          do i = 1,rrec
c
            write(102,'(A60,A20)') rrnx_hdrsec(i), rrnx_hdrlbl(i)
c
          enddo
c
        endif
c
      enddo
c
      return
c
      end
