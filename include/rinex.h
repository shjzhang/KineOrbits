c
ccc   physical constants
c
      real*8    c_light
      parameter(c_light=0.299792458D+09)
c
ccc   frequencies. c.f. RINEX 3.00 
c
c     GPS system
c
      real*8    f1_GPS, f2_GPS, f5_GPS
      parameter(f1_GPS=1575.42D+06)
      parameter(f2_GPS=1227.60D+06)
      parameter(f5_GPS=1176.45D+06)
c
c     GLONASS system: to be added by the channel difference
c
      real*8    f1_GLO, f2_GLO
      parameter(f1_GLO=1602D+06)
      parameter(f2_GLO=1602D+06)
c
c     GALIEO system
c
      real*8    f1_GAL, f5_GAL, f7_GAL, f8_GAL, f6_GAL
      parameter(f1_GAL=1575.420D+06)
      parameter(f5_GAL=1176.450D+06)
      parameter(f7_GAL=1207.140D+06)
      parameter(f8_GAL=1191.795D+06)
      parameter(f6_GAL=1278.750D+06)
c
c     SBAS system
c
      real*8    f1_SBA, f5_SBA
      parameter(f1_SBA=1575.42D+06)
      parameter(f5_SBA=1176.45D+06)
c   
ccc   wavelength
c   
c     GPS system   
c   
      real*8 lam_L1_GPS, lam_L2_GPS, lam_Nw_GPS
      parameter(lam_L1_GPS=c_light/ f1_GPS )
      parameter(lam_L2_GPS=c_light/ f2_GPS )
      parameter(lam_Nw_GPS=c_light/(f1_GPS-f2_GPS) )
c
c     other combination wavelength ...???
c
c
c     GLONASS system
c
c
c     GALIEO system
c
c
c**	  sigma
c
	  real*8	    sig_P3,sig_L3
      parameter(	sig_P3=10.0)
      parameter(	sig_L3=0.006)
c
ccc   parameters in processing RINEX DATA BLOCK
c
      integer       MAX_SAT_SYS
      integer       MAX_OBS_TYP
      parameter(    MAX_SAT_SYS     = 5)
      parameter(    MAX_OBS_TYP     =15)
c
      integer       MAX_EPO
      integer       MAX_OBS_REC
      integer       MAX_PRN
      integer       MAX_SAT_NUM
      parameter(    MAX_EPO         = 10000)
      parameter(    MAX_OBS_REC     = 1000000)
      parameter(    MAX_PRN         = 50)
      parameter(    MAX_SAT_NUM     = 100)
c
cc    max satellite number observed at one epoch
c
      integer       MAX_SAT_EPO
      parameter    (MAX_SAT_EPO   = 12)
c
c     END of RINEX.h
c
