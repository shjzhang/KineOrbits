c      
c     Marker Type      
c     
      character*20 MARKER_TYPE
c     
c     Antenna reference point    
c     
      real*8 ARP_DX, ARP_DY, ARP_DZ
c     
c     Mass center coordinate
c     
      real*8 MAS_DX, MAS_DY, MAS_DZ
c
c     common
c
      common /rinex_conf/ ARP_DX, ARP_DY, ARP_DZ,
     &                    MAS_DX, MAS_DY, MAS_DZ,
     &                    MARKER_TYPE
