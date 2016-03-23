c
c   subroutine read_otl_grid
c
      subroutine read_otl_grid()
c                             
c     Read the header of a Scherneck-style grid file for ocean 
c     tidal loading created by (MIT program) daf2da. 

c     Written by R. King from old utils/octtab.   25 July 2004   
c     Revised to add 'N-S order flag' 060911
c
      implicit none     
c
      include '../include/grdtab.h'  
c
c Input unit numbers from grdtab.h                                        
c
c     luprnt  print file  
c     luotlg  OTL grid file
c
c Output to grdtab.h      
c
c     otlgver   r*4     version number of grid file
c     otlgswap  logical true if the grid file needs to be byte-swapped
c     otlgmod   c*8     model name (last 2 char are cntr-of-figure CE or cntr-ofmass CM)
c     otlgsrc   c*3     institutional source of tides (OSO or NAO)
c     otlglat   i*4     number of latitude values on grid
c     otlglon   i*4     number of longitude values on grid 
c     notlg     i*4     number of tidal components available by interpolating
c                       the grid.  For OSO, =11; for NAO, 21 are read from the
c                       grid but admittance is used to compute 33 minor 
c                       constituents, giving a total of 54.
c     otlgns    c*1     latitude order for grid ('N' for N->S, 'S' for S->N)

c     Local variables                               
c
c     grecl     i*4     record length of direct-access file
      integer*4         grecl
c     ngtid     i*4     number of values per lat/lon on grid (=4 x # components)
c                       OSO = 4 x 11 =44     NAO = 4 x 21 = 84
      integer*4         ngtid
      integer*4         ioerr            
      character*200     otl_grid_file
c
c     Set byte-swapped status to false (will be checked below)
      otlgswap = .false.
c
CZ    otl_grid_file
CZ   & ='/Volumes/Home/Develop/hopes_dev/tables/otl_FES2004.grid'
CZc
CZ    close(luotlg)
CZ    open(luotlg,file=otl_grid_file,status='old',access='direct',
CZ   &            form='unformatted',recl=176    ,iostat=ioerr)
CZc  &            form='unformatted',recl=44     ,iostat=ioerr)
CZc
CZ    if(ioerr.ne.0) then
CZ       write(*,*) 'Error opening direct-access file for ocean loading'
CZ    endif    
c
c     Read the first record to get array limits and record size
      read(luotlg,rec=1,iostat=ioerr) 
     &     ngtid,otlglon,otlglat,otlgmod,otlgns 
c
c     print *,'DEBUG Rec 1 ngtid,otlglon,otlglat,otlgmod otlgns ',
c    &                     ngtid,otlglon,otlglat,otlgmod,otlgns 
c
c     Swap bytes?
      if(ngtid.ne.44 .and. ngtid.ne.84 ) then
c        doesn't match either OSO or NAO--must be byte-swapped    
         otlgswap = .true.    
         call swap_bytes(4,ngtid,1)
         call swap_bytes(4,otlglon,1)
         call swap_bytes(4,otlglat,1) 
      endif
c
      if(ngtid.eq.44 ) then
         otlgsrc ='OSO'
         notlg   = 11
         grecl   = 176
      elseif(ngtid.eq.84 ) then
         otlgsrc ='NAO'
         notlg   = 54
         grecl   = 336 
c        need to close and reopen the NAO file with the right record length
         close(luotlg)
         open(luotlg,file=otl_grid_file,status='old',access='direct'
     &              ,form='unformatted',recl=grecl  ,iostat=ioerr)
         if(ioerr.ne.0) then
            write(*,*) 
     &     'Error opening direct-access file for ocean loading'
         endif
         read(luotlg,rec=1,iostat=ioerr) ngtid,otlglon,otlglat,otlgmod 
         if(ioerr.ne.0 ) then
            write(*,*) 
     &     'Error opening direct-access file for ocean loading'
         endif
         if(otlgswap) then     
            call swap_bytes(4,ngtid,1)
            call swap_bytes(4,otlglon,1)
            call swap_bytes(4,otlglat,1)
         endif
      else
c        Still a problem so kill.
         write(*,*) 
     &  '# components on grid file record not equal 44 or 84'
      endif  
c
c     Set defaults for old grids missing model name
      if(otlgmod.eq.'       ') then
         if( ngtid.eq.44 ) otlgmod = 'CSR4   E'
         if( ngtid.eq.84 ) otlgmod = 'NAO99b E'
      endif  
c
c     Need to translate some names to match the OSO names in otlcmc.dat
c     So far, all grids are CE 
      if( otlgmod(8:8).ne.'M' ) otlgmod(8:8)='E'
      if( otlgmod(1:5).eq.'FES04') otlgmod(1:7) = 'FES2004'
c
c     Set If order not on header, set this from the model name 
c    (all grids are N->S now except the original CSR3/4 and NAO99)
      if(otlgns.eq.' ') then
         if(otlgmod(1:3).eq.'CSR' .or. otlgmod(1:3).eq.'NAO' ) then
            otlgns = 'S'
         elseif (otlgmod(1:3).eq.'TPX' .or. otlgmod(1:3).eq.'FES'. or.
     &           otlgmod(1:3).eq.'GOT' ) then
            otlgns = 'N'
         else
            write(*,*)
     &          'Cannot determine latitude order for o-tide grid'
         endif
      endif 
c
      return
c
      end
