*
*  subroutine read_atx
*      
      subroutine read_atx()
c
c=======================================================================
c     ****f* SmartPPP/read_atx
c
c   FUNCTION   
c   
c     read IGS antex file, which is used to correct:
c     1)  the receiver's PCO and PCV
c     2)  the GNSS's PCO and PCV
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     NONE
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: read_atx.f,v 1.0 2009/07/31 $
c=======================================================================
*         
      implicit none
c
      include       '../../include/igs.h'
      include       '../../include/atx.h'
      include       '../../include/rinex.h'
      include       '../../include/rinex.conf.h'
      include       '../../include/SmartPPP.h'
      include       '../../include/SmartPPP.conf.h'
c
      real*8        cal2sec
c         
c     local         
c         
      character*200  antex_table
      character*200  HOPES_HOME
      character*800 line
      character*20   flag
c
      integer       year, month, day, hour, min
      real*8        sec
      real*8        sec_int
      real*8        sec_dec
      character*5   NOAZI
c
      integer       i,j,k,l,m,n
      integer       ifreq, iazi
      integer       ios
c     current cpu time
      character*8   date
      character*10  time
c
c**   antex file 
c
      integer       NREC_ATX
c
c     ANTEX VERSION / SYST
      real*8        atxvsn
      character*1   satsys
c
c     PCV TYPE / REFANT
      character*1   PCV_TYP
c
c     TYPE / SERIAL NO
c     antenna type
      character*20  ANT_TYP (MAX_REC_ATX)
c     serial number 1
      character*20  SN1     (MAX_REC_ATX)
c     serial number 2
      character*10  SN2     (MAX_REC_ATX)
c     cospar ID 
      character*10  COSPAR  (MAX_REC_ATX)
c
c     DAZI
      real*8        DAZI    (MAX_REC_ATX)
      integer       NAZI    (MAX_REC_ATX) ! added internal
c
c     ZEN1 / ZEN2 / DZEN
      real*8        ZEN     (MAX_REC_ATX,3)
      real*8        DZEN
      integer       NZEN    (MAX_REC_ATX) ! added internal
c
c     # OF FREQUENCIES
      integer       NFREQ   (MAX_REC_ATX)
c
c     VALID FROM
      real*8        Valid1  (MAX_REC_ATX)
c     VALID UNTIL
      real*8        Valid2  (MAX_REC_ATX)
c
c     START OF FREQUENCY
      character*1   sys
      integer       Freq    (MAX_REC_ATX,MAX_FREQ)
c
c     NORTH / EAST / UP
      real*8        ANT_ECC (MAX_REC_ATX,MAX_FREQ,3)
c
c     Antenna Eccentricities non-azimuth dependent
      real*8        ANT_ECC1(MAX_REC_ATX,MAX_FREQ,MAX_REC_ZEN)
c
c     Antenna Eccentricities     azimuth dependent
      real*8        ANT_ECC2(MAX_REC_ATX,MAX_FREQ,MAX_REC_AZI,
     &                                            MAX_REC_ZEN)
c
c     common
c     ******
c
      real*8        rcv_pco  (MAX_FREQ,3)
      real*8        dpco     (3)
      real*8        xZEN     (MAX_REC_ZEN)
      real*8        yant_pcv1(MAX_FREQ,MAX_REC_ZEN)
      real*8        yant_pcv2(MAX_FREQ,MAX_REC_AZI,MAX_REC_ZEN)
      integer       nrec_zen
c
      real*8        trs_pco  (MAX_PRN,3)
c*
      common /atx/  rcv_pco,dpco,xzen,yant_pcv1,yant_pcv2,nrec_zen,
     &              trs_pco
c
c     receiver realted common
c
      character*60  MARKER_NAME
c*
      common /hpx/  MARKER_NAME
c
c**   receiver antenna type from HPRNX file.
c
      character*20  RCV_ANT_TYP
c
      common /RCV_ANT_TYP/ RCV_ANT_TYP
c
      integer       NSAT, NEPO
      integer       NAMB, NREC
      integer       iSAT_iPRN(MAX_PRN), iPRN_iSAT(MAX_PRN)
      integer       NAMB_iPRN(MAX_PRN), NAMB_iSAT(MAX_PRN)
      integer       NREC_iPRN(MAX_PRN), NREC_iSAT(MAX_PRN)
c
      character*3   cPRN_iSAT(MAX_PRN)
c
      real*8        TIME_SPAN(2)
c*
      common /obs/  NSAT,      NEPO,     
     &              NAMB,      NREC, 
     &              iPRN_iSAT, iSAT_iPRN,
     &              NREC_iPRN, NREC_iSAT,
     &              NAMB_iPRN, NAMB_iSAT, 
     &              cPRN_iSAT, TIME_SPAN
c
c     antex table path
c     ================
c
c     relative path
      antex_table = 'share/tables/igs05_1542.atx'
c
c     software system install path
      call getenv('HOPES_HOME',HOPES_HOME)
c
c     absolute path
      antex_table = trim(HOPES_HOME)//trim(antex_table)
c
c     INITIAL
c     =======
c
      do i=1, MAX_REC_ATX
c
         ANT_TYP (i)     = ''         
         SN1     (i)     = ''
         SN2     (i)     = ''
         COSPAR  (i)     = ''
c
         DAZI    (i)     = 0.0d0
         NAZI    (i)     = 0
c
         do j=1,3
         ZEN     (i,j)   = 0.0d0
         NZEN    (i)     = 0
         enddo
c
         NFREQ   (i)     = 0
c
         Valid1  (i)     = 0.0d0
         Valid2  (i)     = 0.0d0
c
         do j=1, MAX_FREQ
         Freq    (i,j)   = 0
         enddo
c
         do j=1, MAX_FREQ
         do k=1, 3
         ANT_ECC (i,j,k) = 0.0d0
         enddo
         enddo
c
         do j=1, MAX_FREQ
         do k=1, MAX_REC_ZEN
         ANT_ECC1(i,j,k)  = 0.0d0
         enddo
         enddo
c
         do j=1, MAX_FREQ
         do k=1, MAX_REC_AZI
         do l=1, MAX_REC_ZEN
         ANT_ECC2(i,j,k,l)= 0.0d0
         enddo
         enddo
         enddo
c
      enddo
c
c     open antex file
c     ===============
c
      NREC_ATX = 0
c
      open(unit=666,file=antex_table,  status='old',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) ' SmartPPP/read_atx'
         write(*,*) ' open atx file error!'
         stop
      endif
c
 100  continue
c
      line = ' '
c
      read(666,'(A800)',end=200) line
c
      flag = line(61:80)
c
c     ANTEX HEADER
c     ============
c
c     ANTEX VERSION / SYST
c
      if(flag.EQ.'ANTEX VERSION / SYST')then
c
         read(line( 1: 8),*) atxvsn
         read(line(21:21),*) satsys
c
      endif
c
c     PCV TYPE / REFANT
c
      if(flag.EQ.'PCV TYPE / REFANT   ')then
c
         read(line( 1: 1),*) PCV_TYP
c
         if(PCV_TYP.NE.'A') then
c
            write(*,*) 'SmartPPP/read_atx'
            write(*,*) 'Not absolute values,not suitable for Spacecraft'
            stop
c
         endif
c
      endif
c
c     ANTEX DATA
c     ==========
c
c     NEW ANTENNA 
c
      if(     flag.EQ.'START OF ANTENNA    ')then
         ifreq    = 0
         NREC_ATX = NREC_ATX + 1
      endif
c
c     TYPE / SERIAL NO
c
      if(     flag.EQ.'TYPE / SERIAL NO    ')then
c
c        antenna type
         read(line( 1:20),'(A20)') ANT_TYP(NREC_ATX)
c
c        serial number 1
         read(line(21:40),'(A20)') SN1    (NREC_ATX)
c
c        serial number 2
         read(line(41:50),'(A10)') SN2    (NREC_ATX)
c
c        cospar ID 
         read(line(51:60),'(A10)') COSPAR (NREC_ATX)
c
      elseif(flag.EQ.'DAZI                ')then
c
c        DAZI
         read(line( 3: 8),'(F6.1)') DAZI(NREC_ATX)
c
c        calculate number of DAZI
c
         if(DAZI(NREC_ATX).EQ.0)then
            NAZI(NREC_ATX) = 0
         else
            NAZI(NREC_ATX) = INT(360.0/DAZI(NREC_ATX)) +1
         endif
c
      elseif(flag.EQ.'ZEN1 / ZEN2 / DZEN  ')then
c
c        ZEN1 / ZEN2 / DZEN
         read(line( 1:20),'(2X,3F6.1)') (ZEN(NREC_ATX,k),k=1,3)
c
c        calculate number of DZEN
         DZEN = ZEN(NREC_ATX,3)
c
         NZEN(NREC_ATX) = INT((ZEN(NREC_ATX,2)-ZEN(NREC_ATX,1))/DZEN) +1
c
      elseif(flag.EQ.'# OF FREQUENCIES    ')then
c
c        # OF FREQUENCIES
         read(line( 1: 6),'(I6)') NFREQ(NREC_ATX)
c
      elseif(flag.EQ.'VALID FROM          ')then
c
c        VALID FROM
         read(line( 1:43),'(5I6,F13.7)') year,month,day,hour,min,sec
c
         sec_int  = DINT(sec)
         sec_dec  = sec - DINT(sec)
c
         valid1(NREC_ATX) = cal2sec(year,month,day,hour,min,sec_int,
     &                                                      sec_dec)
c
      elseif(flag.EQ.'VALID UNTIL         ')then
c
c        VALID UNTIL
         read(line( 1:43),'(5I6,F13.7)') year,month,day,hour,min,sec
c
         sec_int  = DINT(sec)
         sec_dec  = sec - DINT(sec)
c
         valid2(NREC_ATX) = cal2sec(year,month,day,hour,min,sec_int,
     &                                                      sec_dec)
c
c     START OF FREQUENCY
      elseif(flag.EQ.'START OF FREQUENCY  ')then
c
         ifreq = ifreq + 1
c
         read(line( 1: 6),'(3X,A1,I2)') sys,Freq(NREC_ATX,ifreq)
c
c     NORTH / EAST / UP
      elseif(flag.EQ.'NORTH / EAST / UP   ')then
c
         read(line( 1:30),'(3F10.2)'  )(ANT_ECC(NREC_ATX,ifreq,k),k=1,3)

c        write(*,*) line(1:30)
c
c        *******************************
c
         iazi = 0
c
  300    continue ! read antenna phase center variations
c
         read(666,'(A800)',end=400) line

c        write(*,*) 'line'
c        write(*,*)  line
         
c
         flag = line(61:80)
         NOAZI= line( 4:8 )
c
         if(    flag.EQ.'END OF FREQUENCY    ')then
c
            goto 100
c
         else
c           Antenna Eccentricities non-azimuth dependent
            if(  NOAZI.EQ.'NOAZI')then
c
            read(line( 9:800),*) 
     &          (ANT_ECC1(NREC_ATX,ifreq,k),
     &                                   k=1,NZEN(NREC_ATX))
c           Antenna Eccentricities     azimuth dependent
            else
c
            iazi = iazi + 1
c
            read(line( 9:800),*) 
     &          (ANT_ECC2(NREC_ATX,ifreq,iazi,k),
     &                                   k=1,NZEN(NREC_ATX))
c
            endif
c
            goto 300
c
         endif
c
c        ********
c
  400    continue
c
      endif
c
      goto 100
c
 200  continue
c
c     receiver's PCO and PCV   
c     ======================
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
         k = 0
c
         RCV_ANT_TYP = MARKER_NAME
c
         do i=1, NREC_ATX
            if(trim(RCV_ANT_TYP).EQ.trim(ANT_TYP(i)))then
               k = i
            endif
         enddo
c
         if(k.EQ.0)then
            write(*,*) 'SmartPPP/read_atx'
            write(*,*) 'Antenna type not included in igs antex file'
            write(*,*) 'please insert it into the antex file'
            stop
         endif
c
c        ANT PCO ! JUST for two frequencies 
         rcv_pco(1,1) = ANT_ECC(k,1,1)/1000.0d0
         rcv_pco(1,2) = ANT_ECC(k,1,2)/1000.0d0
         rcv_pco(1,3) = ANT_ECC(k,1,3)/1000.0d0
         rcv_pco(2,1) = ANT_ECC(k,2,1)/1000.0d0
         rcv_pco(2,2) = ANT_ECC(k,2,2)/1000.0d0
         rcv_pco(2,3) = ANT_ECC(k,2,3)/1000.0d0
c
c        Delta PCO
         dpco(1) = rcv_pco(2,1) - rcv_pco(1,1)
         dpco(2) = rcv_pco(2,2) - rcv_pco(1,2)
         dpco(3) = rcv_pco(2,3) - rcv_pco(1,3)
c
c        ANT PCV
         do i=1, NZEN(k)
            xZEN     (  i) = ZEN(k,1) + (i-1)*DZEN
            yANT_PCV1(1,i) = ANT_ECC1(k,1,i )/1000.0d0
            yANT_PCV1(2,i) = ANT_ECC1(k,2,i )/1000.0d0
         enddo
c
         NREC_ZEN = NZEN(k)
c
c        !!! NOT Consider azimuth-dependent variations
c
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c
         k = 0
c
         do i=1, NREC_ATX
            if(trim(RCV_ANT_TYP).EQ.trim(ANT_TYP(i)))then
               k = i
            endif
         enddo
c
         if(k.EQ.0)then
            write(*,*) 'SmartPPP/read_atx'
            write(*,*) 'Antenna type not included in igs antex file'
            write(*,*) 'please insert it into the antex file'
            stop
         endif
c
c        ANT PCO
         rcv_pco(1,1) = ANT_ECC(k,1,1)/1000.0d0
         rcv_pco(1,2) = ANT_ECC(k,1,2)/1000.0d0
         rcv_pco(1,3) = ANT_ECC(k,1,3)/1000.0d0
         rcv_pco(2,1) = ANT_ECC(k,2,1)/1000.0d0
         rcv_pco(2,2) = ANT_ECC(k,2,2)/1000.0d0
         rcv_pco(2,3) = ANT_ECC(k,2,3)/1000.0d0
c
c        Delta PCO
         dpco(1) = rcv_pco(2,1) - rcv_pco(1,1)
         dpco(2) = rcv_pco(2,2) - rcv_pco(1,2)
         dpco(3) = rcv_pco(2,3) - rcv_pco(1,3)
c
c        ANT PCV
         do i=1, NZEN(k)
            xZEN     (  i) = ZEN(k,1) + (i-1)*DZEN
            yANT_PCV1(1,i) = ANT_ECC1(k,1,i )/1000.0d0
            yANT_PCV1(2,i) = ANT_ECC1(k,2,i )/1000.0d0
         enddo
c
         NREC_ZEN = NZEN(k)
c
c        !!! NOT Consider azimuth-dependent variations
c
      elseif(MARKER_TYPE.EQ.'NON_GEODETIC')then
c
         write(*,*) 'SmartPPP/read_atx' 
         write(*,*) 'Make modification'
         stop
c
      endif    
c
c     GNSS satellite's PCO and PCV
c     ============================
c
      do i=1, NSAT
      do k=1, 3
         trs_pco(i,k) = 0.0d0
      enddo
      enddo
c
      do i=1, NREC_ATX
c
         if(valid2(i).EQ.0.0d0)then
c
c           current cpu time
            call date_and_time(date,time)
c
c           year moth, day
            read(date,'(i4,i2,i2)') year, month, day
c
            valid2(i) = cal2sec(year,month,day+1, 0, 0, 0, 0.0d0)
c
         endif
c
         do k=1,NSAT
c
            if(     trim(SN1(i)).EQ.cPRN_iSAT(k)
     &         .and.TIME_SPAN(1).ge.valid1(i)
     &         .and.TIME_SPAN(2).lt.valid2(i))then
c
               trs_pco(iPRN_iSAT(k),1) = ANT_ECC(i,1,1)/1000.0d0
               trs_pco(iPRN_iSAT(k),2) = ANT_ECC(i,1,2)/1000.0d0       
               trs_pco(iPRN_iSAT(k),3) = ANT_ECC(i,1,3)/1000.0d0
c
            endif
c
         enddo
c
      enddo
c
c     do i=1, NSAT
c        write(*,*) iPRN_iSAT(i),(trs_pco(iPRN_iSAT(i),k),k=1,3)
c     enddo
c
      return
c
      end
