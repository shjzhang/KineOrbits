c
c   subroutine statvar
c
      subroutine statvar(time0,time1,nsat0,nsat1,
     &                   aPRN0,aPRN1,flag0,flag1,L30,L31,P30,P31,
     &                   x0,sig_x0,xv0,sig_xv0)
c
c=======================================================================
c     ****f* SmartPPP/phasediflib
c
c   FUNCTION   
c   
c     compute state variation for consecutive epochs.
c
c   INPUTS
c
c     time0,time1,nsat0,nsat1
c     aPRN0,aPRN1,flag0,flag1,L30,L31,P30,P31
c
c   OUTPUT
c
c     xv0,sig_xv0
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: phasediflib.f,v 1.0 2009/08/21 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
      integer       nmax
      parameter    (nmax=MAX_SAT_NUM+5)
c
      integer       iter
      integer       iter_q
c
      logical       convgd
c
      integer       nsat
      integer       flag(MAX_SAT_NUM)
      integer       aPRN(MAX_SAT_NUM)
c
      real*8        cL30(MAX_SAT_NUM)
      real*8        cP30(MAX_SAT_NUM)
      real*8        cL31(MAX_SAT_NUM)
      real*8        cP31(MAX_SAT_NUM)
c
      integer       q0(MAX_SAT_NUM),q1(MAX_SAT_NUM)
c
      real*8        V(MAX_SAT_NUM), sig_s
      real*8        sum_v,alfa
c
      logical       clean
c!
      integer       nobs
      integer       iobs0, iepo0, iamb0, nobs0
      integer       nsat0
      integer       flag0(MAX_SAT_NUM)
      integer       aPRN0(MAX_SAT_NUM)
c
      real*8        L30(MAX_SAT_NUM)
      real*8        P30(MAX_SAT_NUM)
      real*8        time0
c!
      integer       iobs1, iepo1, iamb1,nobs1
      integer       nsat1
      integer       flag1(MAX_SAT_NUM)
      integer       aPRN1(MAX_SAT_NUM)
c
      real*8        L31(MAX_SAT_NUM)
      real*8        P31(MAX_SAT_NUM)
      real*8        time1
c
      real*8        x0 (nmax)
      real*8        sig_x0(nmax)
c
c     output
      real*8        xv0(6)
      real*8        sig_xv0(6)
c
      real*8        dxv0(6)
      real*8        dxv1(6)
c
c     local 
c
      integer       ndim
      real*8        A  (MAX_SAT_NUM,6)
      real*8        P  (MAX_SAT_NUM,MAX_SAT_NUM)
      real*8        L  (MAX_SAT_NUM,1)
      real*8        sig, Qxvxv(5,5)
c
      integer       i,j,k
c
      do i=1,MAX_SAT_NUM
         cL30   (i) = 0.0d0
         cP30   (i) = 0.0d0
         cL31   (i) = 0.0d0
         cP31   (i) = 0.0d0
      enddo
c
      do i=1,6
         sig_xv0(i) = 0.0d0
      enddo
c
      do i=1,MAX_SAT_NUM
         flag    (i) = 0.0d0
      enddo
c
      do i=1,MAX_SAT_NUM
         aPRN    (i) = 0.0d0
      enddo
c
c     common observables
c
      nsat = 0
      do i=1, nsat1
         do k=1, nsat0
            if(aPRN0(k).EQ.aPRN1(i))then
               nsat       = nsat + 1
c              observables
               cL30(nsat) = L30(k)
               cP30(nsat) = P30(k)
               cL31(nsat) = L31(i)
               cP31(nsat) = P31(i)
c              iPRN
               aPRN(nsat) = aPRN0(k)
               flag(nsat) = flag0(k)
c              write(*,*) nsat, aPRN(nsat),cL30(nsat),cL31(nsat)
            endif
         enddo
      enddo
c
c     q0,q1 setting, 0=good,9=bad, first iteration, qi(nsat)=0
c
      do i=1, nsat
         q0(i) = 0
         q1(i) = 0
      enddo
c
      if(nsat.lt.4)then
c
         goto 222
c
      endif
c
c     iteration
c     *********
c
      iter_q = 1
c
      do while(iter_q.lt.MAX_ITER)
c
         write(*,*) 'outlier iter', iter_q
c
c step1: q0 setting
c
         do i=1, nsat
            q0(i) = q1(i)
         enddo
c
c        quasi-stable observables: q0(i) = 0 !!!
c        
         nobs = 0
         do i=1, nsat
            if(q0(i).EQ.0)then
               nobs = nobs + 1
            endif
         enddo
c
c step2: LEAST SQUARE           
c
c        iteration           
c
         iter = 1
         convgd = .false.
c
c        INITIAL
c   
         call xv_dim(ndim)
c   
         call xv_ini(ndim,time0,time1,xv0,dxv0)
c
c        write(*,*) (xv0(k),k=1,3)
c
         do while(iter.lt.MAX_ITER)
c
c        write(*,*) ' iter times',iter
c
         call xv_update(ndim,xv0,dxv0)
c
c+++     compose observation equation: NOTES tmp_time_fore !!!
c
         call compsobseqxv(nsat,aPRN,
     &                     time0,time1,cL30,cL31,cP30,cP31,
     &                     x0,ndim,xv0,q0,nobs,A,P,L)
c
c+++     solve   observation equation
c   
c        write(*,*) '  nobs',nobs
c
         call solveobseqxv(nobs,ndim,A,P,L,dxv0,sig,Qxvxv)
c
c+++     converged?
c
         call xv_convgd(ndim,dxv0,convgd)
c
         if(convgd)then
c
            EXIT
c
         endif
c
         iter = iter + 1
c
         enddo
c
c        write(*,*) '  iters convgd'
c        write(*,*) (xv0(k),k=1,ndim)
c
c        END of LEAST SQUARE
c        *******************
c
c step3: calculate all the observables' residual with xv0
c
         call xv_residual(nsat,aPRN,time0,time1,
     &                    cL30,cL31,cP30,cP31,ndim,xv0,A,P,v)
c
c step4: sig_s = alfa*sum(abs(v_i))/n, cluster label 
c
         nobs = 0
         alfa = 1.5
         do while(nobs<4)
c
            sum_v = 0.0d0
            do i=1,nsat
               sum_v = sum_v + dabs(v(i))                 
            enddo
c           
            alfa  = alfa + 0.05              
            sig_s = alfa*sum_v/nsat
c           sig_s = 0.054
c
            if(alfa.gt.5.0)then
c
               goto 222
c
            endif
c
            nobs = 0
            do i=1,nsat
               if(dabs(v(i)).lt.sig_s)then
                  nobs = nobs + 1
                  q1(i) = 0
               else
                  q1(i) = 9
               endif
            enddo
c
         enddo
c
CZ       write(*,*) 'iPRN,q0,v,q1'
CZc
CZ       do i=1, nsat
CZ          write(*,*) aPRN(i),q0(i),v(i),q1(i)
CZ       enddo
c        
c step 4:q1 = q0 ?
c   
         clean = .true.   
         do i=1,nsat
c
            if(q1(i).NE.q0(i))then
               clean = .false.
            endif
c
          enddo
c
c step 5:all outliers cleaned?!
c
         if(clean)then
            EXIT
         endif
c
         iter_q = iter_q + 1
c
      enddo
c
c     END of iterq
c     ************
c
      sig_xv0(1) = sig*dsqrt( Qxvxv(1,1) )
      sig_xv0(2) = sig*dsqrt( Qxvxv(2,2) )
      sig_xv0(3) = sig*dsqrt( Qxvxv(3,3) )
      sig_xv0(4) = sig*dsqrt( Qxvxv(4,4) )
c
      write(*,*) 'statvar,sig',sig,(sig_xv0(k),k=1,3)
c
      return
c
222   continue
c
      xv0(1)     = 0.0d0
      xv0(2)     = 0.0d0
      xv0(3)     = 0.0d0
      xv0(4)     = 0.0d0
c
      sig_xv0(1) = 9.0d+10
      sig_xv0(2) = 9.0d+10
      sig_xv0(3) = 9.0d+10
      sig_xv0(4) = 9.0d+10
c
      return
c
      end
