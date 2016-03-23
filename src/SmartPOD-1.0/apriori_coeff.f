*
*  subroutine apriori_coeff
*      
      subroutine apriori_coeff( iepo,iobs,idx_iamb, 
     &                          NNZA,NRA,RA,CA,A)
c
c=======================================================================
c     ****f* SmartPPP/apriori_coeff.f
c
c   FUNCTION   
c   
*     apriori_coeff of the apriori information equation
c
c   INPUTS
c
c     iepo        integer          epoch number
c     isat        integer          satellite number
c     iamb_isat   integer          ambiguity number for iobs
c
c   OUTPUT
c
c      A          real*8           apriori_coefficent with CSR format
c     CA          integer          column index of A, CSR format
c     RA          integer          row index for A and CA
c     NNZA        integer          Number of Non-Zero of A
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/08/20                   build
c
c     ***
c
C     $Id: apriori_coeff.f,v 1.0 2009/08/20 $
c=======================================================================
c
      implicit none
c
c     include
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
c     input/output
c
      integer       iobs,iepo
      integer       idx_iamb
c
      real*8        A(MAX_NNZA)
      integer       NRA, CA(MAX_NNZA), RA(MAX_NRA+1)
      integer       NNZA
c
c     local
c
c     loop
c
      integer       i,j,k
c
c++   SPACEBORNE
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
c**      Model2:receiver clock-decoupled L3
         if(observ_model.EQ.2 .or. observ_model.EQ.4)then
c
c&&         apriori partial derivatives for N3
c
c             drou
c           ----------
c           d(lamda*N)
c
c           row no. of the apriori_coefficent matrix
c
            NRA     =  iobs
            RA(NRA) =  NNZA + 1
c
c&&         partial derivatives of ambiguity parameters
c
c           increase the total nonzero elements of array a
            NNZA     =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<apriori_coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) = 1.0d0
            CA(NNZA) = idx_iamb
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RA(NRA+1)= NNZA+1
c
            if(NRA+1.GT.MAX_NRA)then
               write(*,*) '<apriori_coeff> error'
               write(*,*) ' enlarge MAX_NRA in SmartPPP.h'
               stop
            endif
c
         endif
c
c++   GEODETIC
c
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c
         write(*,*) 'SmartPPP/apriori_coeff'
         write(*,*) 'Please make modification for ground receiver'
         stop
c
      else
c
      endif
c
      return
c
      end
