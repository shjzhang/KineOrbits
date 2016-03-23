c     Common for MODEL.  Last changed by R. King 060721
c
c This common stores values read from the u-file for ocean tidal loading,
c atmopheric loading (non-tidal and tidal), hydrological loading (not yet 
c implemented), and c meteorological values and mapping function coefficients 
c for the delay c calculation; met values may also come from a RINEX met file.  
c Variables c are described in subroutine gamit/model/readu.f.   rwk 060721
                 
c     max number of ocean tidal constituents from grid
      integer*4 	maxotl
      PARAMETER (	maxotl=54)
c     max number of values per session for atmospheric loading 
      integer*4 	maxatml 
      PARAMETER (	maxatml=20) 
c     max number of atmospheric tide constituents 
      integer*4 	maxatl
      PARAMETER (	maxatl=2)
c     max number of values per session for met values
      integer*4 	maxmet 
      PARAMETER (	maxmet=3000)
c     max number of values per session for mapping function coefficients 
      integer*4 	maxmap 
      PARAMETER (	maxmap=20)
                            
      character*2 	map_name                    
      character*8 	otidemod,atmlmod,atidemod,metmod,mapmod
      integer*4 	notl,natml,natl,nmet,nmap  
     .        		, ntatml,ntmet,ntmap
      real*4 		otides,atml_time,atml_val,atides,met_time,met_val
     .      		,map_time,map_val
      logical 		lotl,latml,latl,lmet,lmap

      common /ufcom/  
     . 				  otidemod,atmlmod,atidemod,metmod,mapmod
     . 				, otides(maxotl,6)
     . 				, atml_time(maxatml), atml_val(maxatml,3)  
     . 				, atides(maxatl,6)    
     . 				, met_time(maxmet), met_val(maxmet,3)
     . 				, map_time(maxmap), map_val(maxmap,9)
     . 				, notl,natml,natl,nmet,nmap    
     . 				, ntatml,ntmet,ntmap
     . 				, lotl,latml,latl,lmet,lmap 
     . 				, map_name(9)

