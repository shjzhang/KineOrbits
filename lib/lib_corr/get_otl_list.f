      Subroutine get_otl_list( slat,slon,lstn )

c     Read a Scherneck-style station file to get values for ocean tidal 
c     loading for a particular station.  Renamed and changed slightly
c     from sb get_statin in old old utils/octtab. 
c     Written by R. King  8 August 2006
 
      implicit none    

      include '../includes/grdtab.h'
      include '../includes/model.h'

c Input calling arguments
c   slat     r*4  latitude of site (decimal deg)
c   slon     r*4  longitude of site (decimal deg)
      real*4 slat,slon

            
c Input from grdtab.h   
c   luprnt    i*4 unit number of print file ('grdtab.out')
c   luotll    i*4 unit number for station list file ('olt.list')   
c   otllsrc   c*3  institutional source of tides (OSO or NAO),sets order and #
c   notll     i*4  number of tidal components read from list file 
c                  (11 or OSO, 54 for NAO)
     

c  Output to model.h common /ufcom/   
c   otides(54,6) r*4  tidal components for site
       
c   1st subscript (col) gives the tidal constituent
c     Scherneck (11): M2 S2 N2 K2 K1 O1 P1 Q1 MF MM SSA  
c     Matumoto (54) :  M2 S2 K1 O1 N2 P1 K2 Q1 M1 
c                      J1 OO1 2N2 Mu2 Nu2 L2 T2 2Q1
c                      Sigma1 Q1' Rho1 O1' Tau1 M1' Kai1
c                      Pi1 P1' K1'' K1' Psi1 Phi1 Theta1
c                      J1' OO1' Eps.2 Mu2' N2' Nu2' Gamma2
c                      Alpha2 M2' Beta2 Delta2 Lambda2 S2' R2
c                      K2' Zeta2 Eta2 Eta2' Mtm Mf Mm Ssa Sa
c   2nd subscript (row) gives the component: Up Amp   West Amp   South Amp  (meters)
c                                            Up Phase West Phase South Phase (degrees)


c Local

c   tolerance in meters for a site match; set to 10km
      real*4 tol    

      data tol/10000.e0/


c   lat, lon from station file
      real*4 tlat,tlon
       
      integer*4 ioerr,i,j
      real*4  latrange,lonrange,pi
      character*433 line
      logical found_stnhd,endhead,found_tide,lstn

c External


                  
c   File format : Lines begin with $$ or two blanks   

c  For Scherneck file:

c  $$ Ocean loading displacements
c  $$  Comments: variable number of lines beginning with $$
c  $$  
c  $$ COLUMN ORDER:  M2  S2  N2  K2  K1  O1  P1  Q1  MF  MM SSA
c  $$ ROW ORDER:
c  $$ AMPLITUDES (m)
c  $$   RADIAL
c  $$   TANGENTL    EW
c  $$   TANGENTL    NS
c  $$ PHASES (deg)
c  $$   RADIAL
c  $$   TANGENTL    EW
c  $$   TANGENTL    NS
c  $$
c  $$ N.B.: Tangential displacements towards west / south
c  $$ END HEADER
c  $$
c    ALGONQUI  7282
c  $$ Complete CSR4 PTM
c  $$ Computed by OLFG, H.-G. Scherneck, Onsala Space Observatory 16-Jun-99
c  $$ ALGONQUIN       40104S001 VLBI       RADI TANG lon/lat:  -78.0727   45.9555
c    .00540 .00183 .00105 .00048 .00301 .00189 .00098 .00038 .00017 .00005 .00008
c    .00238 .00059 .00054 .00015 .00043 .00022 .00014 .00005 .00003 .00003 .00002
c    .00046 .00021 .00011 .00006 .00019 .00009 .00005 .00005 .00002 .00001 .00000
c     139.7  168.3  125.6  171.2   -5.3   -1.0   -6.2   13.3   12.8  -18.0 -180.0
c    -165.1 -156.0  173.9 -159.5  -95.3 -157.2  -98.5  155.3   -9.1 -139.3 -180.0
c      26.1   70.4   33.8   74.7  121.4 -167.7  119.2 -176.0  -33.6  -89.0  180.0
c  $$   

c  For Matsumoto file:

c  $$ COLUMN ORDER:  M2 S2 K1 O1 N2 P1 K2 Q1 M1 J1 OO1 2N2 Mu2 Nu2 L2 T2
c  $$                2Q1 Sigma1 Q1' Rho1 O1' Tau1 M1' Kai1 Pi1 P1' K1' K1'
c  $$                Psi1 Phi1 Theta1 J1' OO1' Eps.2 Mu2' N2' Nu2' Gamma2 
c  $$                Alpha2 M2' Beta2 Delta2 LAmbda2 S2' R2 K2' Zeta2 Eta2
c  $$                Eta2' Mtm Mf Mm Ssa Sa 
c  $$ ROW ORDER:
c  $$ AMPLITUDES (m)
c  $$   RADIAL
c  $$   TANGENTL    EW
c  $$   TANGENTL    NS
c  $$ PHASES (deg)
c  $$   RADIAL
c  $$   TANGENTL    EW
c  $$   TANGENTL    NS
c  $$
c  $$ N.B.: Tangential displacements towards west / south
c  $$ END HEADER
c  $$
c  $$
c        GRAS 
c  $$ Processed by the program GOTIC2_seq
c  $$ Computed by K. Matsumoto, NAO, Nov., 2001
c  $$     GRAS           lon/lat   6.9206000000  43.7547000000
c    .006974 .002256 .002352 .000979 .001469 .000738 .000614 .000012 .000112 .000127 .000095 .000224 .000265 .000286 .000176 .000138 .000043 .000043 .000002 .000005 .000185 .000004 .000022 .000021 .000042 .000008 .000047 .000320 .000018 .000033 .000024 .000025 .000062 .000072 .000010 .000055 .000010 .000021 .000024 .000260 .000021 .000008 .000047 .000005 .000019 .000183 .000030 .000194 .000085 .000082 .000440 .000242 .000225 .000027
c    .002851 .000792 .000274 .000215 .000632 .000094 .000204 .000055 .000019 .000016 .000023 .000098 .000116 .000117 .000066 .000047 .000017 .000019 .000011 .000009 .000040 .000003 .000004 .000004 .000006 .000001 .000005 .000037 .000002 .000004 .000003 .000003 .000015 .000035 .000004 .000024 .000004 .000009 .000010 .000106 .000009 .000003 .000018 .000002 .000006 .000061 .000006 .000036 .000016 .000004 .000016 .000013 .000013 .000003
c    .001099 .000414 .000157 .000095 .000223 .000041 .000111 .000052 .000004 .000023 .000020 .000031 .000038 .000047 .000027 .000023 .000014 .000016 .000010 .000009 .000018 .000003 .000001 .000000 .000002 .000000 .000003 .000021 .000001 .000003 .000004 .000005 .000016 .000010 .000001 .000008 .000002 .000003 .000004 .000041 .000003 .000001 .000007 .000001 .000003 .000033 .000002 .000017 .000007 .000005 .000025 .000020 .000023 .000002
c    288.409 311.301 302.850 272.735 271.451 298.781 305.473  19.206 284.576 338.388  10.802 250.462 253.531 273.127 300.801 311.131  76.747  76.898  26.231 283.935 272.482 258.385 284.502 285.293 296.788 298.684 302.704 302.913 304.948 307.163 332.574 338.163  16.181 234.731 254.771 271.346 273.591 286.264 287.158 288.360 289.312 290.081 299.306 311.090 309.378 305.336 264.771 263.399 263.360   6.309   6.235   5.597   1.068 359.916
c     76.847 105.808  86.026   6.248  57.302  77.356 100.808 297.320  45.440 175.298 212.841  36.466  39.053  58.825  92.940 106.100 264.022 266.608 297.171 305.539   6.038 309.633  45.678  49.578  73.590  77.834  85.929  86.316  91.058  96.251 165.127 175.711 211.824  20.862  40.124  57.266  60.275  74.540  75.579  76.773  78.247  79.676  90.946 106.169 104.168 100.636  24.470  20.025  19.945 142.994 171.367 189.742 181.637 181.737
c    294.422 337.685  85.570 138.155 280.683  80.505 336.426 119.892 175.329  87.490  89.702 265.088 265.705 281.969 314.695 337.486 114.919 115.313 119.843 121.103 137.767 100.600 175.504 157.876  78.291  79.055  85.372  85.678  85.198  85.610  87.359  87.601  90.034 312.726 273.079 280.789 285.239 293.045 293.798 294.393 296.198 298.320 311.636 338.982 337.654 336.331 191.406 187.032 187.021 165.574 183.442 197.721 183.842 177.333
                           
c   Define pi
      pi = atan(1.0) * 4.


c    Set the lat and lon range for a match
      latrange = tol/110000.
      lonrange = tol/110000./cos(slat*pi/180.) 
c      print *,'lat lon tol latrange lonrange '
c     .       ,slat,inlon,tol,latrange,lonrange
     


c    Rewined the file and skip the header comments to find the first entry
                              
      rewind(luotll,iostat=ioerr) 
      if(ioerr.ne.0) call report_stat('FATAL','GRDTAB','get_otl_list'
     .   ,' ','Error rewinding luotll',ioerr)
      endhead = .false.
      do while (.not.endhead )
        read(luotll,'(a)',iostat=ioerr) line
        if(ioerr.ne.0) call report_stat('FATAL','GRDTAB','get_otl_list'
     .   ,' ','Error reading header of station table',ioerr)
        if( line(4:13).eq.'END HEADER') endhead = .true.
      enddo
                 
c    Read through the station list for a match
                      
      lstn = .false.
      do while ( .not.lstn ) 

c       look for the station header
        found_stnhd = .false.
        do while (.not.found_stnhd )  
            
c         read and decode lines for Scherneck table
          if( otllsrc.eq.'OSO' ) then 
            read(luotll,'(a)',err=98,end=99,iostat=ioerr) line 
            if( line(41:49).eq.'RADI TANG' ) then
              found_stnhd = .true.
c             check lat and lon   
              read(line(59:78),'(2f10.0)',iostat=ioerr,err=98) tlon,tlat
              if (slon.lt.0. ) slon = slon + 360.  
              if( abs(slat-tlat).lt.latrange .and.
     .           abs(slon-tlon).lt.lonrange ) then
                lstn = .true.     
c               record what was used in grdtab.out
                write(luprnt,'(a,a8,2x,a14,2(f10.4))')  
     .             '  OTL  list ',otlgmod,line(4:17),tlon,tlat
              endif
              if( lstn ) then
c               now find the next non-comment line to begin reading tides
                found_tide = .false.
                do while (.not.found_tide )
                  read(luotll,'(a)',err=98,end=99,iostat=ioerr) line  
                  if( line(1:2).eq.'  ') then
                    read(line,*,iostat=ioerr,err=98) 
     .                 (otides(i,1),i=1,11)
                    do j=2,6
                     read(luotll,*,iostat=ioerr,err=98) 
     .                    (otides(i,j),i=1,11)
                    enddo 
                    found_tide = .true.
                  endif
c               end of loop skipping comments before tides
                enddo
              endif
            endif 

c         read and decode lines for Matsamoto model
          elseif ( otllsrc.eq.'NAO' ) then  

            read(luotll,'(a433)',err=98,end=99,iostat=ioerr) line
            if( line(23:29).eq.'lon/lat' ) then
              found_stnhd = .true.
c             check lat and lon     
              read(line(30:59),'(2f15.10)',iostat=ioerr) tlon,tlat
              if (slon.lt.0. ) slon = slon + 360.  
              if( abs(slat-tlat).lt.latrange .and.
     .           abs(slon-tlon).lt.lonrange ) then
                lstn = .true.      
              endif
              if( lstn ) then
c               now find the next non-comment line to begin reading tides
                found_tide = .false.
                do while (.not.found_tide ) 
                  read(luotll,'(a433)',err=98,end=99,iostat=ioerr) line
                  if( line(1:2).eq.'  ') then
                    read(line,*,iostat=ioerr,err=98) 
     .                       (otides(i,1),i=1,54)
                     do j=2,6 
                     read(luotll,*,iostat=ioerr,err=98) 
     .                       (otides(i,j),i=1,54)
                    enddo 
                    found_tide = .true.
                  endif
c               end of loop skipping comments before tides
                enddo
              endif
            endif 

c         end of if on model
          endif  

c       end of loop searching for station header
        enddo             

c     end of station loop
      enddo   
               
      return

   98 call report_stat('FATAL','GRDTAB','get_otl_list',' '
     .                ,'Error reading ocean tide station table',ioerr)
      
c     return after finding no match
   99 return
      end

