c     Common for GRDTAB.  Last changed by R. King 060812
     
c Unit numbers -- values set in grdtab_bd.f
      integer*4 lud,luc,luotll,luotlg,luatmll,luatmlg,luatll,luatlg
     .        , lumetl,lumetg,lumapl,lumapg,luprnt,luu
      common /files/ lud,luc,luotll,luotlg,luatmll,luatmlg
     .     ,luatll,luatlg,lumetl,lumetg,lumapl,lumapg,luprnt,luu  

c Version numbers for input files 
      real*4 otlgver,otllver,atmlgver,atmllver,atllver,atlgver
     .     , metlver,metgver,maplver,mapgver
      common/versions/ otlgver,otllver,atmlgver,atmllver,atllver,atlgver
     .     , metlver,metgver,maplver,mapgver  

c Model id for each list or grid file
      character*8 otllmod,otlgmod,atmllmod,atmlgmod,atllmod,atlgmod
     .          , metlmod,metgmod,maplmod,mapgmod
      common/models/ otllmod,otlgmod,atmllmod,atmlgmod,atllmod,atlgmod
     .          , metlmod,metgmod,maplmod,mapgmod
 
c Number of components available for each file
c     (For NAO ocean loading grid, this is the number returned by the
c      program, expanded using admittance from the number on the file)
      integer*4 notll,notlg,natmll,natmlg,natll,natlg,nmetl,nmetg
     .        , nmapl,nmapg
      common/filecomp/ notll,notlg,natmll,natmlg,natll,natlg,nmetl,nmetg
     .        , nmapl,nmapg
                   
c Names, coordinates of sites available on list files
      integer*4 maxlsit,nsitotl,nsitatlm,nsitatl,nsitmet,nsitmap
      parameter(maxlsit=1000)   
      real*4 crds_otl,crds_atml,crds_atl,crds_met,crds_map   
      character*4 sites_otl,sites_atml,sites_atl,sites_met,sites_map
      common/sites/ nsitotl,nsitatlm,nsitatl,nsitmet,nsitmap
     .     , crds_otl(3,maxlsit),crds_atml(3,maxlsit)
     .     , crds_atl(3,maxlsit),crds_met(3,maxlsit),crds_map(3,maxlsit) 
     .     , sites_otl(maxlsit),sites_atml(maxlsit)
     .     , sites_atl(maxlsit),sites_met(maxlsit),sites_map(maxlsit)

c Start,end (year/doy) and interval (days) for time-dependent list and grid files
      integer*4 atmll_start,atmlg_start,metl_start,metg_start
     .        , mapl_start,mapg_start, atmll_end,atmlg_end,metl_end
     .        , metg_end,mapl_end,mapg_end     
      real*4 atmll_int,atmlg_int,metl_int,metg_intm,mapl_int,mapg_int  
      common/times/ atmll_start(2),atmlg_start(2)
     .            , metl_start(2),metg_start(2)
     .            , mapl_start(2),mapg_start(2)
     .            , atmll_end(2),atmlg_end(2)
     .            , metl_end(2),metg_end(2)
     .            , mapl_end(2),mapg_end(2)
     .            , atmll_int,atmlg_int,metl_int,metg_intm
     .            , mapl_int,mapg_int

c Geographic array sizes and byte order for grid files  
      integer*4 otlglat,otlglon, atmlglat,atmlglon
     .        , atlglat,atlglon, metglat,metglon
     .        , mapglat,mapglon    
      logical otlgswap,atmlgswap,atlgswap,metgswap,mapgswap        
      common/grids/ otlglat,otlglon,atmlglat,atmlglon
     .            , atlglat,atlglon,metglat,metglon
     .            , mapglat,mapglon    
     .            , otlgswap,atmlgswap,atlgswap,metgswap,mapgswap


c Insititutional source of ocean loading files (OSO or NAO) - determines tides 
c present and order (may be redundant with number of components, but we 
c can't be sure)
c        source of values (OSO or NAO, determines format)         
      character*3 otllsrc,otlgsrc
c        order of OSO grid (N->S or S->N)
      character*1 otlgns
      common/otlsource/otllsrc,otlgsrc,otlgns

