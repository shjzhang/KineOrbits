c
c  subroutine ppos
c
      subroutine ppos(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ)
c
c=======================================================================
c     ****f* SmartPPP/ppos
c
c   FUNCTION   
c   
c     point positioning with P3 combination 
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
C     $Id: ppos.f,v 1.0 2009/07/27 $
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
      real*8        time
      real*8        tmp_time
      real*8        tmp_time_fore
      integer       tmp_flag(MAX_SAT_NUM)
      integer       tmp_aPRN(MAX_SAT_NUM)
c
      real*8        tmp_L1, tmp_L2, tmp_C1,tmp_P1, tmp_P2
      real*8        tmp_L3(MAX_SAT_NUM)
      real*8        tmp_P3(MAX_SAT_NUM)
c
      real*8        dis
c
c     outlier detection
c     *****************
c
      integer       iter_q
      integer       q0(MAX_SAT_NUM),q1(MAX_SAT_NUM)
c
      real*8        V(MAX_SAT_NUM), sig_s
      real*8        sum_v,alfa
c
      logical       more
      logical       clean
c
c     ppos
c
      integer       iter
      integer       ndim
      logical       convgd
      integer       iobs, iepo, iamb
      integer       nobs
c
      real*8        A  (MAX_SAT_NUM,6)
      real*8        P  (MAX_SAT_NUM,MAX_SAT_NUM)
      real*8        L  (MAX_SAT_NUM,1)
      real*8        sig, Qxx(6,6)
c
      real*8         x0(6)
      real*8         x1(6)
      real*8        dx0(6)
c
c     loop
      integer       i, k, irec
      integer       ii,kk,jj
c
      do i=1,MAX_SAT_NUM
c
         tmp_L3(i) = 0.0d0
         tmp_P3(i) = 0.0d0
c
      enddo
c
      do i=1,6
         x0(i) = 0.0d0
         x1(i) = 0.0d0
      enddo
c
      x1(1) = 4696989.7140   
      x1(2) =  723994.2030
      x1(3) = 4239678.3310 
c
      idx_L1 = 0
      idx_L2 = 0
      idx_C1 = 0
      idx_P1 = 0
      idx_P2 = 0
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
c     read observation data block
c     ===========================
c
      iobs = 0
      iepo = 0
      iamb = 0
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
      tmp_C1             = EPOCH_OBS(idx_C1)
      if(idx_P1.EQ.0.AND.idx_C1.NE.0)then
      tmp_P1             = EPOCH_OBS(idx_C1)
      endif
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
         iepo = iepo + 1
c
         time = tmp_time_fore
c
         call x_dim1(ndim)
c   
         call x_ini1(ndim,x1,x0,dx0)
c
c        outlier detection
c        =================
c
c        initial
c
c        q0,q1 setting, 0=good,9=bad, first iteration, qi(nsat)=0 
c
         do i=1, tmp_nsat
            q0(i) = 0
            q1(i) = 0
         enddo
c
c        iteration
c        *********
c   
         iter_q = 1
c
         do while(iter_q.lt.MAX_ITER)       
c
c           write(*,*) 'outlier iter', iter_q
c
c step1:    q0 setting
c
            do i=1, tmp_nsat
               q0(i) = q1(i)
            enddo
c
c           quasi-stable observables: q0(i) = 0 !!!
c
            k = 0
            do i=1, tmp_nsat
               if(q0(i).EQ.0)then
                  k = k + 1
               endif
            enddo
c
c           if good number of this epoch less than 4, then exit
c           and set the position 0.000
c
            if(k.lt.4)then
               do i=1,6
                  x0(i) = 0.0d0
               enddo
               goto 444
            endif
c
c step2:    LEAST SQUARE
c
c!!!        iteration
c
            iter = 1
            convgd = .false.
c
            do while(iter.lt.MAX_ITER)
c
            write(*,*) ' iter times',iter
c
            call x_update1(ndim,x0,dx0)
c
c+++        compose observation equation: NOTES tmp_time_fore !!!
c
            write(*,*) 'a'
            write(*,*) x0(1), x0(2),x0(3)
            call compsObsEq1(tmp_nsat,tmp_aPRN,tmp_L3,tmp_P3,tmp_flag,
     &                       time,q0,ndim,x0,A,P,L)
c
c+++        solve   observation equation
c   
            write(*,*) 'b'
            call solveObsEq1(k,ndim,A,P,L,dx0,sig,Qxx)
c
c+++        converged?
c
            write(*,*) 'c'
            call x_convgd1(ndim,dx0,convgd)
            write(*,*) 'dx0', dx0(1), dx0(2), dx0(3)
c
            if(convgd)then
               EXIT
            endif
c
            iter = iter + 1

            write(*,*) 'd'
c
            enddo
c
c           END of LEAST SQUARE
c           *******************
c
c step3:    calculate all the observables' residual with x0
c
            call residual(tmp_nsat,tmp_aPRN,tmp_L3,tmp_P3,tmp_flag,
     &                    time,q0,ndim,x0,A,P,V)
c
c step4:    sig_s = alfa*sum(abs(v_i))/n, cluster label 
c
            k = 0
            alfa = 10.0
            do while(k<4)
c
               sum_v = 0.0d0
               do i=1,tmp_nsat
                  sum_v = sum_v + dabs(v(i))                 
               enddo
c              
               alfa  = alfa + 0.05              
               sig_s = alfa*sum_v/tmp_nsat
c
               k = 0
               do i=1,tmp_nsat
                  if(dabs(v(i)).lt.sig_s)then
                     k = k + 1
                     q1(i) = 0
                  else
                     q1(i) = 9
                  endif
               enddo
c
            enddo
c
c           write(*,*) 'iPRN,q0,v,q1'
c
c           do i=1, tmp_nsat
c              write(*,*) tmp_aPRN(i),q0(i),v(i),q1(i)
c           enddo
c           
c step 4:   q1 = q0 ?
c   
            clean = .true.   
            do i=1,tmp_nsat
c
               if(q1(i).NE.q0(i))then
                  clean = .false.
               endif
c
             enddo
c
c step 5:   all outliers cleaned?!
c
            if(clean)then
               EXIT
            endif
c
            iter_q = iter_q + 1
c
         enddo       
c
c        END of IterQ
c        ************
c
         write(*,*) '!!convgd', iepo, time, alfa, sig_s
c
c***     write parameters such as : position, res, etc ...
c
444      continue
c
         call x_wrt1(tmp_nsat,tmp_time_fore,tmp_aPRN,L,ndim,x0,sig,Qxx)
c
         x1(1) = x0(1)
         x1(2) = x0(2)
         x1(3) = x0(3)
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
