      subroutine iau_pom(xp, yp, rot_pom)
c
c Function:
c   Form the matrix of polar motion for a given date.
c
c Input   :
c   xp, yp      coordinates of the pole  unit: radian
c 
c Output  :
c   rot_pom     polar motion matrix  
c
c Notes
c   (1) xp and yp are the coordinate of the pole, in 
c       radians. In a geocentric right-handed triad u,v,w, 
c       where the w-axis points at the north geographic pole,
c       the v-axis points towards the origin of longitudes and
c       the u axis completes the system, XP = +u and YP = -v.
c   (2) The POM are formed by "R(-xp)*R(-yp)", which can be found
c       from the book Satellite Orbits, p.185.
c
c Author  : 
c   S.J. Zhang, 2007/04/22
c
      implicit none
c
      real*8 xp, yp, rot_pom(3,3)
c initial
      call rot_ini(rot_pom)
c rotate by the x-axis, y-axis
      call rot_x(-yp, rot_pom)
      call rot_y(-xp, rot_pom)
c
      return
c
      end
