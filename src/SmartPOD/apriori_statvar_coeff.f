*
*  subroutine apriori_coeff
*      
      subroutine apriori_statvar_coeff(iobs,iepo1,iepo2,
     &                                 NNZA,NRA,RA,CA,A)
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
      integer       iobs
      integer       iepo1, iepo2
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
c**      Model3: traditional L3,P3 + state_variation
         if(    observ_model.EQ.3)then
c
c&&         partial derivatives of state variation, xv
c
c
            NRA      =  iobs + 1
            RA(NRA)  =  NNZA + 1
c*
            NNZA     =  NNZA + 1
c
             A(NNZA) = -1.0d0
            CA(NNZA) = (iepo1-1)*4 + 1
c
            NNZA     =  NNZA + 1
c*
             A(NNZA) =  1.0d0
            CA(NNZA) = (iepo2-1)*4 + 1
c
c&&         partial derivatives of state variation, yv
c
            NRA      =  iobs + 2
            RA(NRA)  =  NNZA + 1
c
            NNZA     =  NNZA + 1
c
             A(NNZA) = -1.0d0
            CA(NNZA) = (iepo1-1)*4 + 2
c
            NNZA     =  NNZA + 1
c
             A(NNZA) =  1.0d0
            CA(NNZA) = (iepo2-1)*4 + 2
c
c&&         partial derivatives of state variation, yv
c
c
            NRA      =  iobs + 3
            RA(NRA)  =  NNZA + 1
c*
            NNZA     =  NNZA + 1
c
             A(NNZA) = -1.0d0
            CA(NNZA) = (iepo1-1)*4 + 3
c*
            NNZA     =  NNZA + 1
c
             A(NNZA) =  1.0d0
            CA(NNZA) = (iepo2-1)*4 + 3
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RA(NRA+1)=  NNZA+1
c
            if(NRA+1.GT.MAX_NRA)then
               write(*,*) '<apriori_coeff> error'
               write(*,*) ' enlarge MAX_NRA in SmartPPP.h'
               stop
            endif
c
c**      Model4:receiver clock-decoupled L3 + state_variation
         elseif(observ_model.EQ.4)then
c
c&&         partial derivatives of state variation, xv
c
c
            NRA      =  iobs + 1
            RA(NRA)  =  NNZA + 1
c*
            NNZA     =  NNZA + 1
c
             A(NNZA) = -1.0d0
            CA(NNZA) = (iepo1-1)*4 + 1
c
            NNZA     =  NNZA + 1
c*
             A(NNZA) =  1.0d0
            CA(NNZA) = (iepo2-1)*4 + 1
c
c&&         partial derivatives of state variation, yv
c
            NRA      =  iobs + 2
            RA(NRA)  =  NNZA + 1
c
            NNZA     =  NNZA + 1
c
             A(NNZA) = -1.0d0
            CA(NNZA) = (iepo1-1)*4 + 2
c
            NNZA     =  NNZA + 1
c
             A(NNZA) =  1.0d0
            CA(NNZA) = (iepo2-1)*4 + 2
c
c&&         partial derivatives of state variation, yv
c
c
            NRA      =  iobs + 3
            RA(NRA)  =  NNZA + 1
c*
            NNZA     =  NNZA + 1
c
             A(NNZA) = -1.0d0
            CA(NNZA) = (iepo1-1)*4 + 3
c*
            NNZA     =  NNZA + 1
c
             A(NNZA) =  1.0d0
            CA(NNZA) = (iepo2-1)*4 + 3
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RA(NRA+1)=  NNZA+1
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
