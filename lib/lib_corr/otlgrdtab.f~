c
c   Subroutine otlgrdtab
c
      subroutine otlgrdtab(slat,slon,ell_ht) 
c
c=======================================================================
c     ****f* lib_corr/otlgrdtab
c
c   FUNCTION   
c   
c     Program to read a station list file or interpolate a global grid 
c     files for ocean, given the site position  
c
c   Reference
c
c     The code is modified from GAMIT subroutine grdtab.f. 
c     ALL the model corrections are followed from the IERS Tech. Notes.
c
c   INPUTS
c    
c     slat          real*8      latitude  of station [decmimal degrees]    
c     slon          real*8      longitude of station [decimal degrees]
c     ell_ht        real*8      ellipsoidal height of station [m]
c
c   OUTPUT
c
c     otides(54,6)  real*8      ocean tide components for station
c                               3 amplitudes and 3 phases
c                               11 constituents for Scherneck
c                               54 constituents for Matsumoto
C
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c
c     2010/03/18                Modified from GAMIT grdtab.f
c
c     ***
C     $Id: otlgrdtab.f,v 1.0 2010/03/17 $
c=======================================================================
c
c     where the first column (# here) non-blank denotes a comment, which may be anywhere
c     in the file.  The keyword STATION denotes the beginning of values for each station.
c     The keyword MODEL is followed by an 8-character string, and integers indicating the
c     number of columns and lines are to be read.  The file must terminate with the 
c     string ENDFILE.
c
c     otl.list   ocean tidal loading station files, OSO/IGS format   
c     otl.grid   ocean tidal loading grid file (converted from OSO da files)
c
      implicit none     
c                                  
      include '../include/grdtab.h'   
      include '../include/model.h'
c            
c     Local variables
c     Session epoch and duration (days)
      integer*4     syear,sdoy
      real*8        sdur       
      character*3   daynum                            
c
c     File names (list, grid, and print file names are hardwired)
      character*10  fname,files(10),dfile,coordfile,ufile
c
c     Flags set true if the file is present 
      logical       otll,otlg 
c
      data          otll/.false./,otlg/.false./               
c
c     Station information
c     sitcods       4-character station ids      
c     slat          latitudes of stations (decimal degrees)
c     slon          longitudes of stations(decimal degrees)
      real*4        slat,slon,ell_ht
      character*4   sitcods                           
c
      integer*4     iyear,imonth,iday,ihr,imn,isec,ihnsec    

c     Double precision arguments for geoc_to_geod
      real*8        pi,site_epoch,pos(3),latr,lonr,clatd,radius,glatd,
     &              ht,semi,finv
c                     
      logical       eol 
      character*1   latflag
      character*8   arg
      character*256 message  
      integer*4     ioerr,nsessn,nfiles,i,j,k,m
      real*8        alat
      character*200 otl_grid_file
c
c     External functions
      integer*4     nblen,iarg,iclarg
      real*8        decyrs   
      logical       fcheck      
c
      data          lud/1/,luc/2/,luprnt/3/,luu/4/,luotll/11/,luotlg/12/
c
      pi = 4.d0*datan(1.d0)
      otlg = .true.
c
      otl_grid_file
     & ='/Volumes/Home/Develop/hopes_dev/tables/otl_FES2004.grid'
c
      open(luotlg,file=otl_grid_file,status='old',access='direct',
     &            form='unformatted',recl=176    ,iostat=ioerr)
c    &            form='unformatted',recl=44     ,iostat=ioerr)
c
      if(ioerr.ne.0) then
         write(*,*) 'Error opening direct-access file for ocean loading'
      endif    
c
c     Set and write the program identifiers and open the print file
c
      open (unit=luprnt,file='grdtab.out',form='formatted',
     &      status='unknown',iostat=ioerr)
      if(ioerr .ne. 0 ) then
         write(*,*) 'Error opening grdtab print file'
      endif   
c
      ufile = 'otides.txt' 
      open (unit=luu,file=ufile,form='formatted', status='unknown'
     &     , iostat=ioerr )
      if(ioerr .ne. 0 ) then
         write(*,*) 'Error opening grdtab output file '
      endif
c                      
c     read the header information
c
      call read_otl_grid()
c
c     reading the values from the available files for input sit
c
c     write the station line of the u-file 
      if( slat.ge.0.d0 ) then
         latflag='N'
         alat = slat
      else
         latflag='S'
         alat= -slat
      endif
      write(luu,'(a,a4,1x,f8.4,a1,1x,f8.4,a1)') 
     .     'STATION ',sitcods,slon,'E',alat,latflag    
      write(luprnt,'(/,a4,1x,f8.4,a1,1x,f8.4,a1)') 
     .      sitcods,slon,'E',alat,latflag 
c
c     Ocean tidal loading 
c
      if( otlg ) then      
         call get_otl_grid( slat,slon) 
         notl = notlg
         write(luu,'(a,a8,i3,a3)') 'OCEANLOD MODEL ',
     &                              otlgmod,notl,'  6' 
      else   
         write(*,*) 'No site match in otl.list and otl.grid missing'
      endif
      if( notl.eq.11 ) then
          write(luu,'(2a)')'#          '
     .   ,' 11 constituents   amp (up west south) and phase (deg)'
          write(luu,'(2a)')'#             M2     S2     N2     K2'
     .        ,'     K1     O1     P1     Q1     Mf     Mm    Ssa'
c         for now, omit comment for NAO 54-component model
      endif   
      do j=1,3      
c        convert from m to mm for new u-file   
         write(luu,'(a,54f7.2)')'OCEANLOD',
     &                          (otides(m,j)*1.e3,m=1,notl)
      enddo
      do j=4,6
         write(luu,'(a,54f7.1)')'OCEANLOD',
     &                          (otides(m,j),m=1,notl)
      enddo   
c
c    C'est tout
c
      write(luu,'(a)') ' ENDFILE'
c
      close(luotlg)
      close(luprnt)
      close(luu)
c
      return
c
      end
