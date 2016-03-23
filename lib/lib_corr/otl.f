c
c   subroutine otl
c
      subroutine otl(GPST,site_pos,otidexyz)
c
c=======================================================================
c     ****f* lib_corr/otl
c
c   FUNCTION   
c   
c     Compute the ocean-loading component for special station
c   
c   Reference
c
c     The code is modified from GAMIT software. 
c     ALL the model corrections are followed from the IERS Tech. Notes.
c
c   INPUTS
c    
c     GPST          real*8      input time [GPST]
c     site_pos      real*8      site position [m]
c
c   OUTPUT
c
c     otidexyz(3)   real*8      ocean tide loading in xyz direction [m]
C
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
C     $Id: otl.f,v 1.0 2010/03/17 $
c=======================================================================
c       
      implicit none
c
      include '../include/grdtab.h'
      include '../include/model.h'
c
c     external function
c
      real*8    gps2utc
c
c     input
      real*8    GPST
      real*8    site_pos(3)
c
c     output
      real*8    otidexyz(3)
c     
c     local variables     
c
      integer   i,j,k
c
      real*8    utc
      real*8    jd_frac,jd_int,mjd
      real*8    sec_jd_frac
      real*8    oangle(54)
      real*8    otideneu(3)
      real*8    otidecmc(3)
      real*8    site_geod(3)
      real*4    slat,slon,sht
      real*8    pi
c
      logical   debug
c
      debug = .false.
c
      pi = 4.0d0*datan(1.0d0)
c
      call xyz2geod(site_pos,site_geod)
c
      slat = site_geod(1)*180.0d0/pi
      slon = site_geod(2)*180.0d0/pi
      sht  = site_geod(3)
c
c     Read Tidal amplitudes and phases for 11 frequencies from the ocean
c     loading tides model 
c       
      call otlgrdtab(slat,slon,sht)
c
c     Time for ocean-tide arguments should be UT1, but use UTC instead 
c    (max error 1 sec = 10 ppm)
c
      utc = gps2utc(GPST)
c
c     notl = 11, otide_source = OSO 
      if(notl.eq.11 ) then 
c
c        Scherneck routine expects true Julian date 
c
         call sec2jdd(utc,0.0,jd_int,jd_frac)
c   
c********NOTES
c        Modifiy for the subroutine ocearg       
c
         jd_int      =  jd_int  - 0.5 + FLOOR(jd_frac+0.5)
         sec_jd_frac =( jd_frac + 0.5 -(FLOOR(jd_frac+0.5)))*86400.0D0 
c
c        angular arguments
         call ocearg (sec_jd_frac,jd_int,oangle ) 
c
c     notl = 54, otide_source = NAO 
      elseif(notl.eq.54 ) then
c
c        Matsumoto routine expects Modified Julian date 
         call sec2jdd(utc,0.0d0,jd_int,jd_frac)
c
         mjd = jd_frac+jd_int-2400000.5d0
c        angular arguments
         call ocearg2 (mjd, oangle )  
      endif
c
c     compute NEU from all components 
      do i=1,3
         otideneu(i) = 0.0d0
      enddo       
c
c     NOTES !!!!
c     the standard table gives up, west, south, so reverse order and signs to get N, E, U 
c     units of table are meters and degrees, convert to km and rad for use, mm for debug 
c
      do i=1,notl         
c        north (1) is negative of south (3)
         otideneu(1) = otideneu(1)    - otides(i,3)*
     $               ( dcos(oangle(i) - otides(i,6)*pi/180.d0) )
c        east (2) is negative of west (2) 
         otideneu(2) = otideneu(2)    - otides(i,2)* 
     $               ( dcos(oangle(i) - otides(i,5)*pi/180.d0) )
c        up (3) is positive of up (1)
         otideneu(3) = otideneu(3)    + otides(i,1) * 
     $               ( dcos(oangle(i) - otides(i,4)*pi/180.d0) ) 
      enddo
c
c     site position
c
      otidexyz(1) = 0.0d0
      otidexyz(2) = 0.0d0
      otidexyz(3) = 0.0d0
c
      call neu2xyz(site_pos,otideneu,otidexyz)
c
c     correct from CE to CM frame if necessary
c
      if(otlgmod(8:8).eq.'E') then 
c
         call otlcmc(GPST,otlgmod,notl,otidecmc )
c
         if( debug ) then  
            write(*,'(a,9x,3f8.1)') 'Otide CE dXYZ :  '
     $          , (otidexyz(i)*1.d3,i=1,3)
            write(*,'(a,9x,3f8.1)') 'Ocean tide CMC:  '
     $          , (otidecmc(i)*1.d3,i=1,3)
         endif      
c
c        this corrects from CE to CM, but the sign that works (+)  seems
c        to be the opposite of what Scherneck says on his web page. 
c        Gerd Gendt (GFZ) confirms this.
c
         do i=1,3
            otidexyz(i) = otidexyz(i) + otidecmc(i)
         enddo
c
      endif
c
c ====Debug for ocean-tide component====
c                                       
      if(debug)then
         write(*,'(a,3f8.1)') 'E-fixed ocean tide dXYZ : ',
     &                        (otidexyz(i)*1.d3,i=1,3)
      endif
c
c=====End debug
c
      return
c
      end
