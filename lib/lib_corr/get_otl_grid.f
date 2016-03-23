      subroutine get_otl_grid(  slat,slon )
c
c     Interpolate a binary grid file to get values for ocean tidal loading for 
c     a particular station.  Coded from old utils/grid_interp.f.
c     Written by R. King  9 August 2006       
c
c                              
c     File values are assumed to be complex radial displacements and 
c     complex tangential potential for each constituent.  
c
c     OSO grids have  11 constituents (44 values) in the order
c     M2   S2   N2   K2   K1   O1   P1   Q1   Mf   Mm   Ssa   
c                    
c     NAO grids have 21 constituents (84 values) in the order   
c     M2   S2   K1   O1   N2   P1   K2   Q1   M1   J1   OO1  
c     2N2  Mu2  Nu2  L2   T2   Mtm  Mf   Mm   SSsa Sa 
c      
c     Output values for OSO are the same as the input; for NAO the minor
c     constituents are evaluated using the admittance, giving a total of 54,
c     in the order
c      
c     M2   S2   K1   O1   N2   P1   K2   Q1   M1   J1   OO1 
c     2N2  Mu2  Nu2  L2   T2   2Q1  Sigma1 Q1'   Rho1 O1'  Tau1  M1'  Kai1
c     Pi1  P1'  K1'' K1'  Psi1 Phi1 Theta1 J1'   OO1'      Eps.2 Mu2' N2' 
c     Nu2' Gamma2   Alpha2   M2'   Beta2   Delta2   Lambda2   S2'   R2
c     K2'  Zeta2   Eta2   Eta2'   Mtm   Mf   Mm   Ssa   Sa
c 
c
c     The grid is arranged in 360 latitude rings at 0.5-deg spacing from 90S to 89.5N.
c     and 720 longitude values within each ring from 0.5E to 360E.   
c
      implicit none     
c
      include '../include/grdtab.h'
      include '../include/model.h'
c            
c Input calling arguments
c
c     slat     r*4  latitude of site (decimal deg)
c     slon     r*4  longitude of site (decimal deg)
c
      real*4 slat,slon
c     
c Input from grdtab.h  
c
c     otlgver   r*4      version number of grid file
c     otlgswap  logical  true if the grid file needs to be byte-swapped
c     otlgmod   c*8      model name 
c     otlgsrc   c*3      institutional source of tides (OSO or NAO
c     otlglat   i*4      number of latitude values on grid
c     otlglon   i*4      number of longitude values on grid 
c     notlg     i*4      number of tidal components available by interpolating
c                        the grid.  For OSO, =11; for NAO, 21 are read from the
c                        grid but admittance is used to compute 33 minor 
c
c  Output in common /ufcom/ in model.h
c
c     otides(54,6) r*4  tidal components for site
         
c     1st subscript (col) gives the tidal constituent (11 or 54; see above)
c     2nd subscript (row) gives the component: Up Amp   West Amp   South Amp  (meters)
c                                              Up Phase West Phase South Phase (degrees)
c     
c  Local
c
      real*4        ottmp(54,6)
c
c  Record #s and values read from the grid file (linear interpolaton)
      integer*4     irecbox(2,2)
      real*4        box(2,2,4,21)           
      real*8        ampmj (16), phsmj (16)
      real*8        ampmn1(17), phsmn1(17)
      real*8        ampmn2(16), phsmn2(16)  
                              
      integer*4     ioerr,nrecs,irec,i,j,k,l,m
      integer*4     nwave
      real*4        d,dinv,slat1,slon1,slat2,slon2,dlat,dlon,z1,z2
      character*256 message     
      logical       first_call
c
c  External functions
      integer*4     grdpos
      real*4        terp2,gradx2,grady2
c      
c  Set for initial calculations
      data          first_call/.true./     
csjz  save          first_call, nwave,d,dinv
c
c  Degrees to radians
      real*4        convd
      data          convd/0.017453292/
           

c  Initial calculations
        
csjz  if( first_call ) then 
      if(otlgsrc .eq. "NAO") then
         nwave = 21
      else if (otlgsrc .eq. "OSO") then
         nwave = 11
      endif
c
c     print *,'otlgsrc nwave ',otlgsrc,nwave
c     total number of records (for bounds checking)
      nrecs = otlglon * otlglat + 1                                
c
c     print *,'GET_OTL_GRID otlglon otlglat nrecs',otlglon,otlglat,nrecs
c     latitude and longitude intervals (0.5 or 1. degree, usually)
      dlat = 180./float(otlglat)
      dlon = 360./float(otlglon) 
c
      if( abs(dlat-dlon).gt.1.e-2 ) then 
c        loose tolerance is because #lat rings may be 361  
c        print *,'dat dlon ',dlat,dlon
         write(*,*) 'Lat and lon spacing not equal: something wrong'
      else
        d = dlon 
        dinv = 1./d
      endif
csjz  first_call = .false.
csjz  endif
c
c     Get the corners of the box 
c    (always viewed north up regardless of latitude order of grid,
c     accounted for in the formulas in function grdpos)

      slon1 = float(int(dinv*slon))/dinv
c     if longitude grid point is zero, use value for 360. (end of lat ring)
      if( slon1.eq.  0. ) slon1 = 360.    
      slon2 = slon1 + d
      if( slon2.gt.360. ) slon2 = slon2 - 360.
      if( slat.ge.0.) then
          slat1 =float( int(dinv*slat))/dinv
      else
          slat1 = -(float(int(2.*(-slat)))+1.)/2.
      endif 
      slat2 = slat1 + d
c
c     lower left 
      irecbox(1,1) = grdpos(otlgns,otlglon,otlglat,d,slon1,slat1) 
c     upper left
      irecbox(1,2) = grdpos(otlgns,otlglon,otlglat,d,slon1,slat2)   
c     lower right
      irecbox(2,1) = grdpos(otlgns,otlglon,otlglat,d,slon2,slat1)
c     upper right
      irecbox(2,2) = grdpos(otlgns,otlglon,otlglat,d,slon2,slat2)
c     print *,'New HGS otlgns slon1 slon2 slat1 slat2 irecbox '
c    .       ,otlgns,slon1,slon2,slat1,slat2,((irecbox(i,j),i=1,2),j=1,2)
c
c     Read the values from the direct-access file
      
      do j=1,2
         do i=1,2
c           add one to account for the first (header) record
            irec = irecbox(i,j) + 1 
            if( irec.le.0 ) then
                write(*,*) 'Requested grid file record < 0'
            endif
            if( irec.gt.nrecs ) then
               write(*,'(a,2f6.0,a,i6,a)') 
     &                 'Requested record '
     .                 ,irec,'for lat,lon ',slat,slon
     .                 ,'exceeds records on grid file (',nrecs,')'
            endif                                                       
c           read complex rad, complex ptan for 11 constituents
            read(luotlg,rec=irec,iostat=ioerr) 
     +          ((box(i,j,k,l),k=1,4),l=1,nwave)
            if( ioerr.ne.0 ) then
                write(*,*) 'Error reading grid file'
            endif
         enddo
      enddo

*     If file is byte-swapped, then switch the values just read
      if( otlgswap ) then
          call swap_bytes(4,box,2*2*4*nwave)
      end if
c
c     Bilinear interpolation on the 2x2 boxes to get values and gradients

c     Interpolate for the radial value

      do m=1,nwave
c
c **     for debugging    
c        print *,'RADIAL'
c        interpolate the complex values  
c        print *,'slon slat slon1 slat1 dlon dlat '
c    .          , slon,slat,slon1,slat1,dlon,dlat
c        print *,'box 11 21 22 12 ',
c    .            box(1,1,1,m),box(2,1,1,m),box(2,2,1,m),box(1,2,1,m)
         z1 = terp2(slon,slat,slon1,slat1,dlon,dlat,box(1,1,1,m)) 
c        print *,'z1 box ',z1,((box(i,j,1,1),i=1,2),j=1,2)
         z2 = terp2(slon,slat,slon1,slat1,dlon,dlat,box(1,1,2,m)) 
c        print *,'  z2 box ',z2,((box(i,j,2,1),i=1,2),j=1,2)
c        amplitude       
         otides(m,1) = sqrt(z1**2+z2**2)
c        phase
         otides(m,4) = atan2(z2,z1)/convd  
c        print *,'amp phase ',otides(m,1),otides(m,4)
c
      enddo 
c
c**   DEBUG            
c     stop           

c     Take the linear gradients to get the horizontal values

      do m=1,nwave    

c **     for debugging
c        do m=1,1
c        difference the complex values
c        west       
c       'right' in grid is east, but need gradient positive west
c        print *,'WEST' 
c        box(1,1,3,m) = 5.
c        box(2,1,3,m) = 15.
c        box(2,2,3,m) = 15.
c        box(1,2,3,m) = 5.
         z1 = -gradx2(slon,slat,slon1,slat1,dlon,dlat,box(1,1,3,m))   
c        print *,'z1 box ',z1,((box(i,j,3,1),i=1,2),j=1,2)
         z2 = -gradx2(slon,slat,slon1,slat1,dlon,dlat,box(1,1,4,m))   
c        print *,'  z2 box ',z2,((box(i,j,4,1),i=1,2),j=1,2)
c        amplitude  
c        units of potential values on Scherneck grid are (rad/Erad), convert to m/deg
         otides(m,2) = sqrt(z1**2+z2**2)/convd/cos(slat*convd)
c        phase     
         otides(m,5) = atan2(z2,z1)/convd    
c        print *,'amp phase ',otides(m,2),otides(m,5)
c        south         
c       'up' in grid is south and need gradient positive south, so no sign change 
c        should be needed; BUT empirically we need it to match the station table
c ***    no, in current scheme, 'up' is north; so if we still need gradient positive
c        south, we need a sign change (as we had empirically before)
c        why?
c        print *,'SOUTH'
         z1 = -grady2(slon,slat,slon1,slat1,dlon,dlat,box(1,1,3,m))    
c        print *,'z1 box ',z1,((box(i,j,3,1),i=1,2),j=1,2)   
c        grid is positive north but need gradient positive south
         z2 = -grady2(slon,slat,slon1,slat1,dlon,dlat,box(1,1,4,m))  
c        print *,'  z2 box ',z2,((box(i,j,4,1),i=1,2),j=1,2)
c        amplitude       
         otides(m,3) = sqrt(z1**2+z2**2)/convd
c        phase
         otides(m,6) = atan2(z2,z1)/convd    
c        print *,'amp phase ',otides(m,3),otides(m,6)
      enddo
c     print *,'End  slat slon slat1 slon1 ',slat,slon,slat1,slon1

c     Trap a problem with the CSR grids at the South Pole
                          
c     print *,'otidemod slat ',otidemod,slat
c     write(*,*) otidemod
      if( otlgmod(1:3).eq.'CSR' .and. slat.lt.-89.5 ) then
          write(*,*) 
     &   'CSR grids in error at South Pole, set components = 0.'
          do m=1,nwave
             do j=1,6
                otides(m,j) = 0.d0
             enddo
          enddo
      endif
c     stop
           
c     If the station-file model is NAO, rearrange the order of the
c     largest 11 components to match what's expected:  
c                   Scherneck (11):  M2 S2 N2 K2 K1 O1 P1 Q1 MF MM SSA  
c                   Matsumoto (54):  M2 S2 K1 O1 N2 P1 K2 Q1 M1 
c     /                              J1 OO1 2N2 Mu2 Nu2 L2 T2 2Q1
c                                    Sigma1 Q1' Rho1 O1' Tau1 M1' Kai1
c                                    Pi1 P1' K1'' K1' Psi1 Phi1 Theta1
c                                    J1' OO1' Eps.2 Mu2' N2' Nu2' Gamma2
c                                    Alpha2 M2' Beta2 Delta2 Lambda2 S2' R2
c                                    K2' Zeta2 Eta2 Eta2' Mtm Mf Mm Ssa Sa
c 
c       OSO: M2 S2 N2 K2 K1 O1 P1 Q1 Mf Mm Ssa 
c       NAO: M2 S2 K1 O1 N2 P1 K2 Q1           ..Mf(51) Mm(52) Ssa(53) Sa(54)  
c
      if( otlgsrc.eq.'NAO' ) then 
          do j = 1,6
             do i = 1,54
                ottmp (i,j) = otides(i,j)
                otides(i,j) = 0.0
             enddo
          enddo
          do j = 4,6
             do i = 1,54
                if (ottmp(i,j).lt.0.d0) then
                   ottmp(i,j) = ottmp(i,j) + 360.d0
                endif
             enddo
          enddo
          do j = 1,6
             do i = 1,16          ! Major 16
                otides( i,j) = ottmp( i,j)
             enddo
             otides(50,j) = ottmp(17,j) ! Mtm
             otides(51,j) = ottmp(18,j) ! Mf
             otides(52,j) = ottmp(19,j) ! Mm
             otides(53,j) = ottmp(20,j) ! Ssa
             otides(54,j) = ottmp(21,j) ! Sa
          enddo  

c -----<Infer minor tides from major ones >-----
c 
        do j = 1,3              ! 1:UP, 2:West, 3:South
c
           ampmj( 1) = ottmp( 8,j)   ! Q1  amp in m
           phsmj( 1) = ottmp( 8,j+3) ! Q1  phs in degree
           ampmj( 2) = ottmp( 4,j)   ! O1  amp
           phsmj( 2) = ottmp( 4,j+3) ! O1  phs
           ampmj( 3) = ottmp( 9,j)   ! M1  amp
           phsmj( 3) = ottmp( 9,j+3) ! M1  phs
           ampmj( 4) = ottmp( 6,j)   ! P1  amp
           phsmj( 4) = ottmp( 6,j+3) ! P1  phs
           ampmj( 5) = ottmp( 3,j)   ! K1  amp
           phsmj( 5) = ottmp( 3,j+3) ! K1  phs
           ampmj( 6) = ottmp(10,j)   ! J1  amp
           phsmj( 6) = ottmp(10,j+3) ! J1  phs
           ampmj( 7) = ottmp(11,j)   ! OO1 amp
           phsmj( 7) = ottmp(11,j+3) ! OO1 phs
           ampmj( 8) = ottmp(12,j)   ! 2N2 amp
           phsmj( 8) = ottmp(12,j+3) ! 2N2 phs
           ampmj( 9) = ottmp(13,j)   ! Mu2 amp
           phsmj( 9) = ottmp(13,j+3) ! Mu2 phs
           ampmj(10) = ottmp( 5,j)   ! N2  amp
           phsmj(10) = ottmp( 5,j+3) ! N2  phs
           ampmj(11) = ottmp(14,j)   ! Nu2 amp
           phsmj(11) = ottmp(14,j+3) ! Nu2 phs
           ampmj(12) = ottmp( 1,j)   ! M2 amp
           phsmj(12) = ottmp( 1,j+3) ! M2 phs
           ampmj(13) = ottmp(15,j)   ! L2 amp
           phsmj(13) = ottmp(15,j+3) ! L2 phs
           ampmj(14) = ottmp(16,j)   ! T2 amp
           phsmj(14) = ottmp(16,j+3) ! T2 phs
           ampmj(15) = ottmp( 2,j)   ! S2 amp
           phsmj(15) = ottmp( 2,j+3) ! S2 phs
           ampmj(16) = ottmp( 7,j)   ! K2 amp
           phsmj(16) = ottmp( 7,j+3) ! K2 phs
c
           call infminor(ampmj, phsmj, ampmn1, phsmn1, ampmn2, phsmn2)
c
           do i = 17,33         ! Minor diurnal
              otides(i,j  ) = ampmn1(i-16) 
              otides(i,j+3) = phsmn1(i-16) 
           enddo
           do i = 34,49         ! Minor semi-diurnal
              otides(i,j  ) = ampmn2(i-33) 
              otides(i,j+3) = phsmn2(i-33)
           enddo
c
        enddo                   ! j
      endif
      write(luprnt,'(a,a8)') '  OTL  grid ',otlgmod              

      return
      end


c********************************************************************************************************

      integer*4 function grdpos( otlgns,m,n,d,x,y )

c     calculate lat/lon position in an OSO grid using
c     formulas from H-G Scherneck 2006/9/4

      integer*4 m,n,i,j     

      real*4 x,y,d

      character*1 otlgns
         
c      element position in m x n array
      i = mod(nint(x/d)+m-1,m) + 1  
      if( otlgns.eq.'N' ) then
          j= nint((90.0-y)/d) + 1  
      else
          j = nint((90.0+y)/d) + 1  
      endif

      grdpos  = (j-1)*m + i   
      return
      end

***********************************************************************************************************

      real*4 function terp2(x,y,x1,y1,dx,dy,za)
c                 
c     Linearly interpolate a 2-d table of dimensions 2 x 2.
c     Coded by R. King from Press et al. Numerical Recipes, Cambridge Univ. Press, 1986

c     x, y     : coordinates of point to be interpolated
c     x1,y1    : coordinates of lower-left corner of grid square with 
c                corners numbered 1,2,3,4 counterclockwise 
c     dx,dy    : tabular spacing in x and y
c     za(2,2)  : values of function at grid square corners (z1, z2, z3, z4)
c                corresponding to (1,1), (2,1), (2,2), (1,2)


      implicit none

      real*4 x,y,x1,y1,dx,dy,za(2,2),t,u

      t = (x-x1)/dx
      u = (y-y1)/dy  

c     print *,'TERP2 11 21 22 12 ',za(1,1),za(2,1),za(2,2),za(1,2)
      terp2 =  (1.-t)*(1.-u)*za(1,1) 
     &        + t*(1.-u)*za(2,1)
     &        + t*u*za(2,2) + (1.-t)*u*za(1,2)
c
      return
      end

c
      real*4 function gradx2(x,y,x1,y1,dx,dy,za)

c     R. King 28 Apr 2000
c     Compute the linear gradient in the x direction of a 2-d table of dimensions 2 x 2.
c     Based on differentiating the formula for a bilinear interpolation in Press et al.,
c     Numerical Recipes, Cambridge Univ. Press, 1986  

c     x, y     : coordinates of point to be interpolated
c     x1,y1    : coordinates of lower-left corner of grid square with 
c                corners numbered 1,2,3,4 counterclockwise 
c     dx,dy    : tabular spacing in x and y
c     za(2,2)  : values of function at grid square corners (z1, z2, z3, z4)
c                corresponding to (1,1), (2,1), (2,2), (1,2)
c
      implicit none
c
      real*4 x,y,x1,y1,dx,dy,za(2,2),t,u
c
      t = (x-x1)/dx
      u = (y-y1)/dy 
      gradx2 = -(1.-u)*za(1,1) + (1.-u)*za(2,1) + u*za(2,2) -u*za(1,2)
      gradx2 = gradx2/dx
c     
      return
c     
      end


      real*4 function grady2(x,y,x1,y1,dx,dy,za)
                         
c     R. King 28 Apr 2000
c     Compute the linear gradient in the y direction of a 2-d table of dimensions 2 x 2.
c     Based on differentiating the formula for a bilinear interpolation in Press et al.,
c     Numerical Recipes, Cambridge Univ. Press, 1986  

c     x, y     : coordinates of point to be interpolated
c     x1,y1    : coordinates of lower-left corner of grid square with 
c                corners numbered 1,2,3,4 counterclockwise 
c     dx,dy    : tabular spacing in x and y
c     za(2,2)  : values of function at grid square corners (z1, z2, z3, z4)
c                corresponding to (1,1), (2,1), (2,2), (1,2)


      implicit none

      real*4 x,y,x1,y1,dx,dy,za(2,2),t,u

      t = (x-x1)/dx
      u = (y-y1)/dy 
      grady2 = -(1.-t)*za(1,1) -t*za(2,1) + t*za(2,2) + (1.-t)*za(1,2)
      grady2 = grady2/dy
      
      return
      end
c
c-----<NAO modification>-----
c
c$infminor
c ----------------------------------------------------------------------
      subroutine infminor(ampmj, phsmj, ampmn1, phsmn1, ampmn2, phsmn2)
c ----------------------------------------------------------------------
c
c Infer minor tides from major ones.
c Input : ampmj(16), phsmj(16)
c         Amplitude (m) and Phase (deg) of major 16 constituents.
c         Constituent order: Q1, O1, M1, P1, K1, J1, OO1,
c                            2N2, Mu2, N2, Nu2, M2, L2, T2, S2, K2
c Output : ampmn1(17), phsmn1(17)
c          Amplitude (m) and Phase (deg) of minor diurnal constituents
c          inferred by admittance interpolation.
c          Constituent order: 2Q1, Sigma1, Q1', Rho1, O1', Tau1, M1',
c                             Kai1, Pi1, P1', K1'', K1', Psi1, Phi1,
c                             Theta1, J1', OO1'
c        : ampmn2(16), phsmn2(16)
c          Amplitude (m) and Phase (deg) of minor semi-diurnal constituents
c          inferred by admittance interpolation.
c          Constituent order: Eps.2, Mu2', N2', Nu2', Gamma2, Alpha2,
c                             M2', Beta2, Delta2, Lambda2, S2', R2,
c                             K2', Zeta2, Eta2, Eta2'
c 
c
      implicit none
c
      real*8 ampmj (16), phsmj (16)
      real*8 ampmn1(17), phsmn1(17)
      real*8 ampmn2(16), phsmn2(16)
      real*8 eqamj (16), frqmj (16)
      real*8 eqamn1(17), frqmn1(17)
      real*8 eqamn2(16), frqmn2(16)
      real*8 cpd(16), wt(16), Ccoef(3), Scoef(3)
      real*8 cpsmj(16), spsmj(16)
      real*8 pi, deg, two, twopi, freq, cpdi, ci, si, R
      real*8 biasmi, bipsmi
c
      integer*4 k, kk, i, nwmjdu, nwmjsd
c
      parameter (two = 2.d0)
      parameter (nwmjdu = 7, nwmjsd = 9) 
c
      data frqmj 
     +/ 0.000064959d0, 0.000067598d0, 0.000070282d0, 0.000072523d0,
     +  0.000072921d0, 0.000075560d0, 0.000078245d0, 0.000135240d0,
     +  0.000135594d0, 0.000137880d0, 0.000138233d0, 0.000140519d0,
     +  0.000143158d0, 0.000145245d0, 0.000145444d0, 0.000145842d0/
c
      data frqmn1
     +/ 0.000062319d0, 0.000062673d0, 0.000064948d0, 0.000065312d0,
     +  0.000067587d0, 0.000065334d0, 0.000070293d0, 0.000070635d0,
     +  0.000072324d0, 0.000072512d0, 0.000072910d0, 0.000072932d0,
     +  0.000073120d0, 0.000073319d0, 0.000075207d0, 0.000075571d0,
     +  0.000078255d0/
c
      data frqmn2
     +/ 0.000132954d0, 0.000135583d0, 0.000137869d0, 0.000138222d0,
     +  0.000140166d0, 0.000140320d0, 0.000140508d0, 0.000140718d0,
     +  0.000140917d0, 0.000142805d0, 0.000145433d0, 0.000145643d0,
     +  0.000145853d0, 0.000148128d0, 0.000148482d0, 0.000148492d0/
c
      data eqamj
     +/ 0.072136d0, 0.376763d0, 0.029631d0, 0.175307d0, ! Q1 , O1, M1 , P1
     +  0.529876d0, 0.029630d0, 0.016212d0, 0.023009d0, ! K1 , J1, OO1, 2N2
     +  0.027768d0, 0.173881d0, 0.033027d0, 0.908184d0, ! Mu2, N2, Nu2, M2
     +  0.025670d0, 0.024701d0, 0.422535d0, 0.114860d0/ ! L2 , T2, S2 , K2
c
      data eqamn1
     +/ 0.009545d0, 0.011520d0, 0.013607d0, 0.013702d0, ! 2Q1 , Sigma1, Q1'   , Rho1
     +  0.071081d0, 0.004914d0, 0.005946d0, 0.005667d0, ! O1' , Tau1  , M1'   , Kai1
     +  0.010251d0, 0.001973d0, 0.010492d0, 0.071886d0, ! Pi1 , P1'   , K1''  , K1'
     +  0.004145d0, 0.007545d0, 0.005666d0, 0.005875d0, ! Psi1, Phi1  , Theta1, J1'
     +  0.010385d0/                                     ! OO1'
c
      data eqamn2
     +/ 0.006709d0, 0.001037d0, 0.006484d0, 0.001232d0, ! Eps.2 , Mu2'   , N2' , Nu2'
     +  0.002728d0, 0.003123d0, 0.033885d0, 0.002749d0, ! Gamma2, Alpha2 , M2' , Beta2
     +  0.001066d0, 0.006697d0, 0.000946d0, 0.003536d0, ! Delta2, Lambda2, S2' , R2
     +  0.034240d0, 0.001228d0, 0.006422d0, 0.002799d0/ ! K2'   , Zeta2  , Eta2, Eta2'
c
      data pi      /3.14159265358979d0/
      data deg     /5.729577951308232d+1/
c
      twopi = two*pi
c
c -----< Diurnal Admittance Fit >-----
c
      do k = 1,nwmjdu
         cpd(k) = frqmj(k)*86400.d0/twopi
         freq   = frqmj(k)
         call calR(freq,R)
         cpsmj(k) = ampmj(k)*dcos(phsmj(k)/deg)/(eqamj(k)*R)
         spsmj(k) = ampmj(k)*dsin(phsmj(k)/deg)/(eqamj(k)*R)
         wt(k)    = eqamj(k)
      enddo
c
      call fitadm(cpd,cpsmj,wt,Ccoef,nwmjdu)
      call fitadm(cpd,spsmj,wt,Scoef,nwmjdu)
c
      do i = 1,17
c
         cpdi = frqmn1(i)*86400.d0/twopi
         ci = Ccoef(1)*dcos(twopi*two*cpdi) 
     +      + Ccoef(2)*dsin(twopi*two*cpdi)
     +      + Ccoef(3)
         si = Scoef(1)*dcos(twopi*two*cpdi) 
     +      + Scoef(2)*dsin(twopi*two*cpdi)
     +      + Scoef(3)
c     
         freq = frqmn1(i)
         call calR(freq,R)
         ci = ci*R
         si = si*R
c     
         if (dabs(ci).gt.0.d0) then
            bipsmi = datan2(si,ci)
         else if (dabs(si).gt.0.d0) then
            bipsmi = pi*0.5d0
         else
            bipsmi = pi*1.5d0
         endif
         if (bipsmi.lt.0.d0) then
            bipsmi = bipsmi + 2.d0*pi
         endif
         biasmi = dsqrt(ci*ci + si*si)*eqamn1(i)
c     
         ampmn1(i) = biasmi
         phsmn1(i) = bipsmi*deg
c
      enddo
c
c -----< Semi-diurnal Admittance Fit >-----
c
      do k = 1,nwmjsd
         kk = k + 7
         cpd(k) = frqmj(kk)*86400.d0/twopi
         freq   = frqmj(kk)
         cpsmj(k) = ampmj(kk)*dcos(phsmj(kk)/deg)/eqamj(kk)
         spsmj(k) = ampmj(kk)*dsin(phsmj(kk)/deg)/eqamj(kk)
         wt(k)    = eqamj(kk)
      enddo
c
      call fitadm(cpd,cpsmj,wt,Ccoef,nwmjsd)
      call fitadm(cpd,spsmj,wt,Scoef,nwmjsd)
c
      do i = 1,16
c
         cpdi = frqmn2(i)*86400.d0/twopi
         ci = Ccoef(1)*dcos(twopi*two*cpdi) 
     +      + Ccoef(2)*dsin(twopi*two*cpdi)
     +      + Ccoef(3)
         si = Scoef(1)*dcos(twopi*two*cpdi) 
     +      + Scoef(2)*dsin(twopi*two*cpdi)
     +      + Scoef(3)
c     
         if (dabs(ci).gt.0.d0) then
            bipsmi = datan2(si,ci)
         else if (dabs(si).gt.0.d0) then
            bipsmi = pi*0.5d0
         else
            bipsmi = pi*1.5d0
         endif
         if (bipsmi.lt.0.d0) then
            bipsmi = bipsmi + 2.d0*pi
         endif
         biasmi = dsqrt(ci*ci + si*si)*eqamn2(i)
c     
         ampmn2(i) = biasmi
         phsmn2(i) = bipsmi*deg
c
      enddo
c
      return
      end
c
c$calR
c ------------------------------------------------------------
      subroutine calR(f,R)
c ------------------------------------------------------------
c
c     f is frequency in rad/sec
c
      implicit none
c
      real*8 k, k0, k1, h, h0, h1
      real*8 pi, twopi
      real*8 f, R, fe, f0, fo1, gamma, gammao1, Omega, Omega0
c
      data pi      /3.14159265358979d0/
c
      parameter  (k0  = 0.298d0   , h0  = 0.603d0) 
      parameter  (k1  = -0.00123d0, h1  = -0.00246d0)
      parameter  (fe  = 86164.09054d0/86400.d0)       ! cpsd
      parameter  (f0  = 1.d0 + 1.d0/433.2d0)          ! cpsd
      parameter  (fo1 = 0.9295357067d0*fe)            ! cpsd
      parameter  (Omega0 = 7.292115d-5)               ! rad/sec
c
      twopi = 2.d0*pi
      f     = f*86400.d0/twopi*fe       ! cycle/sidereal day
      Omega = Omega0*86400.d0/twopi*fe  ! cycle/sidereal day
c
      k = k0 + k1*(f-fo1)/(f0-f)
      h = h0 + h1*(f-fo1)/(f0-f)
      gamma   = 1.d0 + k - h
      gammao1 = 1.d0 + k0 - h0
c
      R = gamma/gammao1
c
      return
      end
c
c$fitadm
c ------------------------------------------------------------
      subroutine fitadm(cpd,adm,wt,coef,nd)
c ------------------------------------------------------------
c
      implicit none
c
      real*8    f1, f2, eps
      integer*4 k, k1, k2, i, nd
c
      parameter (f1  = 1.9972622214d0)
      parameter (f2  = 2.0000000000d0)
      parameter (eps = 1.d-4)
      parameter (k   = 3)
c
      real*8    cpd(*), adm(*), wt(*), coef(3)
      real*8    hth(k,k), htd(k), h(k)
c
      real*8    pi, two, twopi, f, z, w, d, dd
      integer*4 indx(k)
c
      data pi      /3.14159265358979d0/
c
      two   = 2.d0
      twopi = two*pi
c
c -----< Initialize matrix >-----
c
      do k1 = 1,k
         do k2 = 1,k
            hth(k2,k1) = 0.d0
         enddo
      enddo
c     
      do k1 = 1,k
         htd(k1) = 0.d0
         indx(k1) = 0
      enddo
c
      do i = 1,nd
c
         f = cpd(i)
         z = adm(i)
         w = wt(i)
c     
c Exclude T2 & S2 from fitting
         if ((dabs(f-f1).gt.eps).and.(dabs(f-f2).gt.eps)) then
c
            h(1) = dcos(twopi*two*f)
            h(2) = dsin(twopi*two*f)
            h(3) = 1.d0
            d    = z
c     
            call calhth(h,k,hth,w)
            call calhtd(h,k,d,htd,w)
c     
         endif
c
      enddo
c
      call ludcmp(hth,k,k,indx,dd)  ! (HTH) IS LU DECOMPOSED
      call lubksb(hth,k,k,indx,htd) !  HTD RETURNS SOLUTION
c
      do i = 1,k
         coef(i) = htd(i)
      enddo
c
      return
      end
c
c$ludcmp
c -----------------------------------------------------------
      subroutine ludcmp(a,n,np,indx,dd)
c -----------------------------------------------------------
c
c INPUT : A(N,N)
c OUTPUT: A(N,N) LU DECOMPOSED.
c INDX   OUTPUT VECTOR. DD:
c
      implicit none
c
      integer*4 n, np
      real*8 tiny
      parameter (tiny = 1.d-25)
c
      real*8    a(np,np), vv(2000) 
      integer*4 indx(n)
c
      real*8 dd, aamax, sum, dum
      integer*4 imax, i, j, k
c
      imax = 0
      dd   = 1.d0
c
      do i = 1,n
         aamax = 0.d0
         do j = 1,n
            if (abs(a(i,j)).gt.aamax) then
               aamax=abs(a(i,j))
            endif
         enddo
         if (aamax.eq.0.d0) then
            stop 'singular matrix'
         endif
         vv(i) = 1.d0/aamax
      enddo
c     
      do j = 1,n
c     
         if (j.gt.1) then
            do i = 1,j-1
               sum = a(i,j)
               if (i.gt.1) then
                  do k = 1,i-1
                     sum = sum - a(i,k)*a(k,j)
                  enddo
                  a(i,j) = sum
               endif
            enddo
         endif
c     
         aamax = 0.d0
c     
         do i = j,n
            sum = a(i,j)
            if (j.gt.1) then
               do k = 1,j-1
                  sum = sum - a(i,k)*a(k,j)
               enddo
               a(i,j) = sum
            endif
            dum = vv(i)*abs(sum)
            if (dum.ge.aamax) then
               imax  = i
               aamax = dum
            endif
         enddo
c     
         if (j.ne.imax) then
c     
            do k = 1,n
               dum       = a(imax,k)
               a(imax,k) = a(j   ,k)
               a(j,k)    = dum
            enddo
            dd       = -dd
            vv(imax) = vv(j)
         endif
c     
         indx(j) = imax
c     
         if (j.ne.n) then
            if (a(j,j).eq.0.d0) a(j,j) = tiny
            dum = 1.d0/a(j,j)
            do  i = j+1,n
               a(i,j) = a(i,j)*dum
            enddo
         endif
c     
      enddo
c     
      if (a(n,n).eq.0.d0) a(n,n) = tiny
c
      return
      end
c
c$lubksb
c -----------------------------------------------------------
      subroutine lubksb(a,n,np,indx,b)
c -----------------------------------------------------------
c
c SOLVES N LINEAR EQ.  A * X = B  BY LU DECOMPOSITION
c
      implicit none
c
      integer*4 n, np 
      integer*4 indx(n)
      integer*4 ii, i, ll, j
      real*8    a(np,np), b(n)
      real*8    sum
c
      ii = 0
c
      do i = 1,n
c
         ll    = indx(i)
         sum   = b(ll)
         b(ll) = b(i)
c
         if (ii.ne.0) then
            do j = ii,i-1
               sum = sum - a(i,j)*b(j)
            enddo
         elseif (sum.ne.0.d0) then
            ii = i
         endif
c
         b(i) = sum
c
      enddo
c
      do i = n,1,-1
         sum = b(i)
         if(i.lt.n)then
            do j = i+1,n
               sum = sum - a(i,j)*b(j)
            enddo
         endif
         b(i) = sum/a(i,i)
      enddo
c     
      return
      end
c
c$calhth
c -----------------------------------------------------------
      subroutine calhth(h,k,hth,w)
c -----------------------------------------------------------
c
c HTH(K,K)= TR(H) * (H)
c
      implicit none
c
      integer*4 i, j, k
      real*8    hth(k,k),h(k)
      real*8    w
c
      do i = 1,k
         do j = 1,k
            hth(i,j) = hth(i,j) + h(i)*h(j)*w*w
         enddo
      enddo
c     
      return
      end
c
c$calhtd
c -----------------------------------------------------------
      subroutine calhtd(h,k,d,htd,w)
c -----------------------------------------------------------
c
c HTD = TR(H) * D
c
      implicit none
c
      integer*4 k, i
      real*8    h(k),htd(k)
      real*8    w, d
c
      do i = 1,k
         htd(i) = htd(i) + h(i)*d*w*w
      enddo
c
      return
      end
c
c-----</NAO modification>-----
