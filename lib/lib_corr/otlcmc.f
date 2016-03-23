c
c   subroutine otlcmc
c
      subroutine otlcmc(GPST, otidemod, notl, dx )
c
c   Function
c
c     Calculate corrections for ocean tidal loading to transform between a terrestrial, 
c     solid Earth (CE) frame and a joint (E + oceans) center-of-mass (CM) frame.
c                               
c   Input                  
c
c     otidemod :    Ocean tide model 
c     notl     :    number of tidal components (54 for NAO, 11 for others so far)
c
c   Output
c     dx       :    Cartesian offset of the CE frame with respect to the CM frame
c
c   Reference
c
c     modified from GAMTI otlcmc.f
c
      implicit none              
c
      real*8        gps2utc     
c
      integer*4     GPST,notl,icall
      real*8        dx(3)
      character*8   otidemod
      character*100 cmcfile
c    
      real*8        utc
      real*8        jd_frac,jd_int, mjd, oangle(54), cmccof(54,6)   
      real*8        sec_jd_frac
c     for otlcmc.dat
      logical       unit_ok,found
      integer*4     len,rcpar,ioerr,i,j
      character*80  prog_name     
      character*120 line                        
      character*256 message
c
      save cmccof 
c
c     Read the CMC correction coefficients      
c
c     make sure the unit number is not taken
      cmcfile = '/Volumes/Home/Develop/hopes_dev/tables/otlcmc.dat'
      inquire(unit=46,exist=unit_ok,iostat=ioerr) 
      if( unit_ok ) then
          open(file=cmcfile,unit=46,iostat=ioerr,status='old')
          if( ioerr.ne.0 ) then
              write(*,*) 
     &       'Error opening file for COM ocean loading correction'
          endif
      else
          write(*,*)
     &   'Unit 46 not available to open otlcmc.dat'
      endif 
      found = .false.
      do while( .not.found ) 
         read(46,'(a)',iostat=ioerr) line 
         if( ioerr.eq.-1 ) then    
             write(*,'(a,a7,a)') 
     &         'EOF on otlcmc.dat, model ',otidemod(1:7),' not found'
         elseif( ioerr.ne.0 ) then
             write(*,*) 'Error reading OTL model name'
         else   
             if( line(1:5).eq.'MODEL' ) then     
                 if(line(7:13).eq.otidemod(1:7)) then      
                    found = .true.
                    do i=1,notl
                       read(46,'(41x,3(2x,2e12.4))',iostat=ioerr) 
     .                (cmccof(i,j),j=1,6)   
                       if( ioerr.ne.0)  then
                            write(*,'(a,a8,a,i2,a)') 
     .          'Error reading OTL CMC values for model ',otidemod
     .          ,' (notl=',notl,')'
                       endif
                    enddo 
                 else
                    continue
                 endif  
             else
                 continue
             endif
         endif
      enddo      
      close( unit=46 )         
c
      utc = gps2utc(GPST)
c       
c  Get the angular arguments
c
c     notl=11,OSO                            
      if( notl.eq.11 ) then 
c
c         Scherneck routine expects true Julian date
          call sec2jdd(utc,0.0d0,jd_int,jd_frac)
c
c******** NOTES
c         Modifiy for the subroutine ocearg       
          jd_int      =  jd_int  - 0.5 + FLOOR(jd_frac+0.5)
          sec_jd_frac =( jd_frac + 0.5 -(FLOOR(jd_frac+0.5)))*86400.0D0 
c
c         arguments
          call ocearg (sec_jd_frac,jd_int,oangle )
c
c     notl=54,NAO                            
      elseif (notl.eq.54 ) then

c         Matsumoto routine expects Modified Julian date 
          call sec2jd(utc,0.0d0,jd_frac,jd_int)
          mjd = jd_frac+jd_int-2400000.5d0
c         angular arguments
          call ocearg2 (mjd, oangle )  
      endif
c
c  Compute the corrections
c     
      dx(1) = 0.d0
      dx(2) = 0.d0
      dx(3) = 0.d0
      do i=1,notl       
c        Note: Scherneck tabulates coefficients in order Z X Y 
         dx(1) = dx(1) + cmccof(i,3)*dcos(oangle(i)) 
     &                 + cmccof(i,4)*dsin(oangle(i))
         dx(2) = dx(2) + cmccof(i,5)*dcos(oangle(i))
     &                 + cmccof(i,6)*dsin(oangle(i))
         dx(3) = dx(3) + cmccof(i,1)*dcos(oangle(i)) 
     &                 + cmccof(i,2)*dsin(oangle(i))
      enddo
c              
c     print *,'OTLCMC xjd, oangle ',xjd,(oangle(i),i=1,notl) 
c     endif on icall
c                
      return
      end
