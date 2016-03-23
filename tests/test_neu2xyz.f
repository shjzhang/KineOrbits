c       
      program test_neu2xyz
c
      character*3 xyz, neu
      integer ichan,j,n
      real*8  xhat_neu(3),yhat_neu(3),zhat_neu(3)
      real*8  xhat_r(3),yhat_r(3),khat(3)
      real*8  xhat_rt(3),yhat_rt(3),zhat_rt(3)
      real*8  xhat_t(3),yhat_t(3),zhat_t(3)
      real*8  rxyz(3)
      real*8  rllh(3),rotmat(3,3),xrot(3,3)
      real*8  dot,pi,one
      real*8  d_r(3),d_t(3)
      real*8  pos_neg,len_d_r,len_d_t
      real*8  tmp1,tmp2(3),tmp3(3),tmp4(3)
      real*8  dipoldel(2)
      real*8  freql1,freql2,vlight
      real*8  evec0(3,2),evec(6,2),r1(3),r1leng
c
      parameter ( pi = 3.1415926535897932D0 )
      parameter ( one = 1.D0 )

      save first, dphi_prev
c
      data xyz/'XYZ'/, neu/'NEU'/
      data xhat_neu/ 1.0, 0.0, 0.0/, yhat_neu/0.0,-1.0, 0.0/
      data zhat_neu/ 1.0, 0.0, 0.0/

c-----Convert receiver antenna dipole unit vectors from local NEU coordinates
c     to geocentric earth fixed cartesian vectors.
c
      rxyz(1) = 10000
      rxyz(2) = 10000
      rxyz(3) = 10000

      call rotate_geod(xhat_neu,xhat_rt,neu,xyz,rxyz,rllh,rotmat)
c
      write(*,*) 'xhat_rt'
      write(*,*) (xhat_rt(j),j=1,3)
c
      call neu2xyz(rxyz,xhat_neu, xhar_rt)
c
      write(*,*) 'xhat_rt'
      write(*,*) (xhat_rt(j),j=1,3)
c
      call rotate_geod(yhat_neu,yhat_rt,neu,xyz,rxyz,rllh,rotmat)
c
      write(*,*) 'yhat_rt'
      write(*,*) (yhat_rt(j),j=1,3)
c
      call neu2xyz(rxyz,yhat_neu,yhar_rt)
c
      write(*,*) 'yhat_rt'
      write(*,*) (yhat_rt(j),j=1,3)
c
      call rotate_geod(zhat_neu,zhat_rt,neu,xyz,rxyz,rllh,rotmat)
c
      write(*,*) 'zhat_rt'
      write(*,*) (zhat_rt(j),j=1,3)
c
      call neu2xyz(rxyz,zhat_neu,zhar_rt)
c
      write(*,*) 'zhat_rt'
      write(*,*) (zhat_rt(j),j=1,3)
c
      return
c
      end
