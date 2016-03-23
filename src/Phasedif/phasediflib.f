c
c  subroutine phasediflib
c
      subroutine phasediflib(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ)
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
c     rnxvsn     real*8         rinex version
c     csat_sys   characeter     returned satellite systems in rinex 
c                               file
c     nobs_typ   integer        observation types number for existed 
c                               satellite systems.
c     cobs_typ   character      observation types for existed 
c                               satellite systems.
c
c   OUTPUT
c
c     pos        real*8         point positioning position
c     dtr        real*8         receiver clock
c     Elv        real*8         Elvation
c     zpd        real*8         zenith path delay
c     GDOP       real*8         Geometric DOP value
c
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
c     input
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
c     output
c
      real*8        pos(MAX_EPO,3)
      real*8        dtr(MAX_EPO)
      real*8        Elv(MAX_EPO,MAX_SAT_EPO)
      real*8        zpd(MAX_EPO)
      real*8        GDOP(MAX_EPO)
c
c     local
c
      real*8        EPOCH
      integer       EPOCH_nsat
      integer       EPOCH_flag
      integer       iPRN
      character*3   cPRN
      integer       EPOCH_iPRN
      character*3   EPOCH_cPRN
      real*8        EPOCH_OBS(MAX_OBS_TYP)       
      integer       EPOCH_LLI(MAX_OBS_TYP)
      integer       EPOCH_SNR(MAX_OBS_TYP)
c
      integer       idx_L1, idx_L2, idx_LA 
      integer       idx_C1, idx_P1, idx_P2
      integer       idx_S1, idx_S2, idx_SA
c
      integer       tmp_nsat
c
      real*8        tmp_time
      real*8        tmp_time_fore
      integer       tmp_flag(MAX_SAT_NUM)
      integer       tmp_aPRN(MAX_SAT_NUM)
c
      real*8        tmp_L1, tmp_L2, tmp_P1, tmp_P2
      real*8        tmp_L3(MAX_SAT_NUM)
      real*8        tmp_P3(MAX_SAT_NUM)
c
      logical       more
c
c     phasediflib
c
      integer       iter
      integer       iter_q
c
      logical       convgd
c
c     outlier 
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
      integer       ndim
      real*8        A  (MAX_SAT_NUM,6)
      real*8        P  (MAX_SAT_NUM,MAX_SAT_NUM)
      real*8        L  (MAX_SAT_NUM,1)
      real*8        sig, Qxvxv(5,5)
c
      real*8        xv0(6)
      real*8        xv1(6)
      real*8        dxv0(6)
      real*8        dxv1(6)
c
c     loop
      integer       i, k, irec
c
      do i=1,MAX_SAT_NUM
         cL30   (i) = 0.0d0
         cP30   (i) = 0.0d0
         cL31   (i) = 0.0d0
         cP31   (i) = 0.0d0
          L30   (i) = 0.0d0
          P30   (i) = 0.0d0
          L31   (i) = 0.0d0
          P31   (i) = 0.0d0
         tmp_L3 (i) = 0.0d0
         tmp_P3 (i) = 0.0d0
      enddo
c
      do i=1,MAX_SAT_NUM
         flag    (i) = 0.0d0
         flag0   (i) = 0.0d0
         flag1   (i) = 0.0d0
         tmp_flag(i) = 0.0d0
      enddo
c
      do i=1,MAX_SAT_NUM
         aPRN    (i) = 0.0d0
         aPRN0   (i) = 0.0d0
         aPRN1   (i) = 0.0d0
         tmp_aPRN(i) = 0.0d0
      enddo
c
      idx_L1 = 0
      idx_L2 = 0
c
      if(rnxvsn.lt.3.00)then
c
         do i=1, nobs_typ(1)
           if(    trim(cobs_typ(1,i)).eq.'L1')then
             idx_L1 = i
           elseif(trim(cobs_typ(1,i)).eq.'L2')then
             idx_L2 = i
           elseif(trim(cobs_typ(1,i)).eq.'LA')then
             idx_LA = i
           elseif(trim(cobs_typ(1,i)).eq.'C1')then
             idx_C1 = i
           elseif(trim(cobs_typ(1,i)).eq.'P1')then
             idx_P1 = i
           elseif(trim(cobs_typ(1,i)).eq.'P2')then
             idx_P2 = i
           elseif(trim(cobs_typ(1,i)).eq.'S1')then
             idx_S1 = i
           elseif(trim(cobs_typ(1,i)).eq.'S2')then
             idx_S2 = i
           elseif(trim(cobs_typ(1,i)).eq.'SA')then
             idx_SA = i
           endif
         enddo
c
c        double-frequency receiver ??
         if(idx_L1.EQ.0.or.idx_L2.EQ.0)then
            write(*,*) 'SmartPPP/extrobs'
            write(*,*) '  not double-frequency GPS receiver'
            stop
         endif
c
      else
c
         write(*,*) "SmartPPP/extrobs"
         write(*,*) '  Please modifiy to process rinex 3.00' 
         stop
c
      endif
c
      iobs1 = 0
      iepo1 = 0
      iamb1 = 0
      time1 = 0
c
      iobs0 = 0
      iepo0 = 0
      iamb0 = 0
      time0 = 0
c
c     read observation data block
c     ===========================
c
      more = .true.
      tmp_nsat = 0
      tmp_time = 0.0d0
      tmp_time_fore = 0.0d0
c
 100  continue
c
      read(111, fmt=2000, end=300)
     +      irec, 
     +      EPOCH,       EPOCH_iPRN, (EPOCH_OBS(k),
     +      EPOCH_LLI(k),EPOCH_SNR(k),k=1,nobs_typ(1))
c
 2000 format(I6, F18.7, (X,I3), 12(F14.3,I1,I1))
c
      tmp_time = EPOCH
c
c     continue reading observables at the same epoch
c
 110  continue
c
c     finish this epoch reading ???
c
      if(tmp_time.gt.tmp_time_fore.and.tmp_nsat.gt.0)then
c
         goto 200
c
      endif
c
c     pass the outliers
c
      if(EPOCH_LLI(idx_L1).EQ.9)then
c
         goto 100
c
      endif
c
      tmp_nsat           = tmp_nsat + 1
      tmp_flag(tmp_nsat) = EPOCH_LLI(idx_L1)
      tmp_aPRN(tmp_nsat) = EPOCH_iPRN
c
      tmp_L1             = EPOCH_OBS(idx_L1)*lam_L1_GPS
      tmp_L2             = EPOCH_OBS(idx_L2)*lam_L2_GPS
      tmp_P1             = EPOCH_OBS(idx_P1)
      tmp_P2             = EPOCH_OBS(idx_P2)
c
c     LC combination
c
      tmp_L3(tmp_nsat)   = 
     &      (f1_GPS**2*tmp_L1-f2_GPS**2*tmp_L2)/(f1_GPS**2-f2_GPS**2)
      tmp_P3(tmp_nsat)   =
     &      (f1_GPS**2*tmp_P1-f2_GPS**2*tmp_P2)/(f1_GPS**2-f2_GPS**2)
c
c     store the last epoch time
c
      tmp_time_fore = tmp_time
c
      goto 100
c
c     END read EPOCH data block
c     *************************
c
 200  continue
c
c     single point positioning
c     ========================
c
      if(tmp_nsat.GE.5)then
c
         iepo1 = iepo1 + 1
         time1 = tmp_time_fore
         nsat1 = tmp_nsat
c
         do i=1, nsat1
c           observables
            L31(i)   = tmp_L3(i)
            P31(i)   = tmp_P3(i)
c           PRN 
            aPRN1(i) = tmp_aPRN(i)
            flag1(i) = tmp_flag(i)
         enddo
c
c        write(*,*) iepo1,time1,nsat1,iepo0,time0,nsat0
c
c        solve the time variation with v(i) detection
c        ============================================
c
         if(dabs(time1-time0).lt.30)then
c
c           initial
c           *******
c   
C$          call xv_dim(ndim)
C$c   
C$          call xv_ini(ndim,time0,time1,xv0,dxv0)
C$c
C$c         here the input position is the dynamic one, then
C$c         output the state variation directly!
C$c
C$          goto 222

c           common observed satellite for time0 and time1
c
            nsat = 0
            do i=1, nsat1
               do k=1, nsat0
                  if(aPRN0(k).EQ.aPRN1(i))then
                     nsat       = nsat + 1
c                    observables
                     cL30  (nsat) = L30(k)
                     cP30  (nsat) = P30(k)
                     cL31  (nsat) = L31(i)
                     cP31  (nsat) = P31(i)
c                    iPRN
                     aPRN(nsat) = aPRN0(k)
                     flag(nsat) = flag0(k)
c                    write(*,*) nsat, aPRN(nsat),cL30(nsat),cL31(nsat)
                  endif
               enddo
            enddo
c
c           q0,q1 setting, 0=good,9=bad, first iteration, qi(nsat)=0
c
            do i=1, nsat
               q0(i) = 0
               q1(i) = 0
            enddo
c
            if(nsat.lt.4)then
               xv0(1) = 9999.0d0
               xv0(2) = 9999.0d0
               xv0(3) = 9999.0d0
               xv0(4) = 9999.0d0
               goto 222
            endif
c
c           iteration
c           *********
c
            iter_q = 1
c
            do while(iter_q.lt.MAX_ITER)
c
               write(*,*) 'outlier iter', iter_q
c
c step1:       q0 setting
c
               do i=1, nsat
                  q0(i) = q1(i)
               enddo
c
c              quasi-stable observables: q0(i) = 0 !!!
c            
               nobs = 0
               do i=1, nsat
                  if(q0(i).EQ.0)then
                     nobs = nobs + 1
                  endif
               enddo
c
c step2:       LEAST SQUARE           
c
c              iteration           
c
               iter = 1
               convgd = .false.
c
c              INITIAL
c   
               call xv_dim(ndim)
c   
               call xv_ini(ndim,time0,time1,xv0,dxv0)
c
c              write(*,*) (xv0(k),k=1,3)
c
               do while(iter.lt.MAX_ITER)
c
c              write(*,*) ' iter times',iter
c
               call xv_update(ndim,xv0,dxv0)
c
c+++           compose observation equation: NOTES tmp_time_fore !!!
c
               call comps_xvobseq(nsat,aPRN,
     &                            time0,time1,cL30,cL31,cP30,cP31,
     &                            ndim,xv0,q0,nobs,A,P,L)
c
c+++           solve   observation equation
c   
c              write(*,*) '  nobs',nobs
c
               call solve_xvobseq(nobs,ndim,A,P,L,dxv0,sig,Qxvxv)
c
c+++           converged?
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
c              write(*,*) '  iters convgd'
c              write(*,*) (xv0(k),k=1,ndim)
c
c              END of LEAST SQUARE
c              *******************
c
c step3:       calculate all the observables' residual with xv0
c
               call xv_residual(nsat,aPRN,time0,time1,
     &                          cL30,cL31,cP30,cP31,ndim,xv0,A,P,v)
c
c step4:       sig_s = alfa*sum(abs(v_i))/n, cluster label 
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
c
                  if(alfa.gt.5.0)then
c
                     xv0(1) = 9999.0d0
                     xv0(2) = 9999.0d0
                     xv0(3) = 9999.0d0
                     xv0(4) = 9999.0d0
c
                     goto 222
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
CZ             write(*,*) 'iPRN,q0,v,q1'
CZc
CZ             do i=1, nsat
CZ                write(*,*) aPRN(i),q0(i),v(i),q1(i)
CZ             enddo
c              
c step 4:      q1 = q0 ?
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
c step 5:      all outliers cleaned?!
c
               if(clean)then
                  EXIT
               endif
c
               iter_q = iter_q + 1
c
            enddo
c
            write(*,*) 'outlier iter convgd'
c
c           END of iterq
c           ************
c
222         continue
c
            write(*,'(2(I6,F14.3),4F14.3)') 
     &            iepo1,time1,iepo0,time0,(xv0(k),k=1,4)
c
c***        write parameters such as : position, res, etc ...
c
            if(xv0(4).NE.9999.0d0)then
               call xv_wrt(nsat,aPRN,time0,time1,v,ndim,xv0,sig,Qxvxv)
            endif
c
        endif
c
c       ENDIF(time1-time0)
c
c       store current information into temporary arry
c       =============================================
c
         iepo0 = iepo1
         time0 = time1
         nsat0 = nsat1 
c
         do i=1, nsat0
c           observables
            L30(i)   = L31(i)
            P30(i)   = P31(i)
c           PRN 
            aPRN0(i) = aPRN1(i)
            flag0(i) = flag1(i)
         enddo
c
      endif
c
      tmp_nsat = 0
c
c     END of DATA
c
      if(.not.more)then
c
         goto 400
c
      endif
c
      goto 110
c
  300 continue
c
      more = .false.
c
c     read the last EPOCH observation data block
      goto 200
c
  400 continue
c
      return
c
      end
