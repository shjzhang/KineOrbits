      program test_otl
c
      integer   year,month,day,hour,minute,sec_int
      real*8    sec_frac
c
      real*8    GPST
      real*8    site_pos(3) ! xyz   
      real*8    otidexyz(3)
c
c     External Function
c
      real*8    cal2sec
      real*8    jd1,jd2
c
      year      = 2009 
      month     = 5
      day       = 4
      hour      = 0
      minute    = 0
      sec_int   = 0
      sec_frac  = 0.0
c
      GPST = cal2sec(year,month,day,hour,minute,sec_int,sec_frac)
c
      site_pos(1) = 4696989.7140
      site_pos(2) = 723994.2030
      site_pos(3) = 4239678.3310
c
      call otl(GPST,site_pos,otidexyz)
c
      write(*,*) otidexyz(1),otidexyz(2),otidexyz(3)
c
      site_pos(1) = 4696989.7140
      site_pos(2) = 723994.2030
      site_pos(3) = 4339678.3310
c
      call otl(GPST,site_pos,otidexyz)
c
      write(*,*) otidexyz(1),otidexyz(2),otidexyz(3)
c
      stop
c
      end
