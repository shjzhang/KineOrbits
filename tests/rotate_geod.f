CTITLE ROTATE_GEOD

      subroutine rotate_geod(in_comp,out_comp, in_sys,out_sys,
     .   site_pos, loc_coord, rot_matrix)

c
c     Routine to rotate coordinate corrections 'in_comp' in system
c     'in_sys' to values 'out_comp' in system 'out_sys'.  The
c     site global XYZ coordinates are in site_pos.  As by products
c     the colatiude, longitude and radius are returned (loc_coord)
c     and the rotation matrix between the systems are returned.
c
c Variables
c ---------
c in_comp -- vector of coordinate corections in system 'in_sys'
c in_sys  -- the coordinate system of in_comp.  Can only be
c     'XYZ' Global geocentric system, or
c     'NEU' local topocentric north, east, up system.
c out_comp -- corrections in out_sys
c out_sys  -- the output coordinate system.  May be the same as
c        in_sys.
c site_pos -- the global XYZ coordinates of site (given by user)
c loc_coord -- colatitude, longitude and radius of site (computed here)
c rot_matrix -- matric to rotate from in_sys to out_sys (may be a unit
c     matrix if in_sys = out_sys)
c

      real*8 in_comp(3), out_comp(3), site_pos(3), loc_coord(3),
     .    rot_matrix(3,3)

*   i,j,k       - Loop counters
*   id          - Dummy argument for loglu

      integer*4 i,j
c
      character*(*) in_sys, out_sys
c
c
c.... Start execution.  See if in and out systems are the same
*                                    ! this is easy
      if( in_sys.eq.out_sys ) then
         do i = 1, 3
            out_comp(i) = in_comp(i)
            do j = 1,3
               rot_matrix(i,j) = 0.d0
            end do
*                                     ! set diagonal
            rot_matrix(i,i) = 1.d0
         end do
c
*                                     ! then we need to convert
      else
c
c....    Get rotation between XYZ and NEU (we will use transpose
c        of this matrix if we wish to go NEU to XYZ)
         call XYZ_to_GEOD(rot_matrix, site_pos, loc_coord)
c
*                                           ! apply rotation rot_matrix
         if( out_sys(1:3).eq.'NEU' ) then
            do i = 1,3
               call dvdot(out_comp(i), rot_matrix(i,1),3, in_comp,1,3)
            end do
c
*                           ! FINISHED
            return
c
         end if
c
c....    See if XYZ
*                                           ! apply transpose of rot_matrix
         if( out_sys(1:3).eq.'XYZ') then
            do i = 1,3
               call dvdot(out_comp(i), rot_matrix(1,i),1, in_comp,1,3)
            end do
c
c....       Now transpose rot_matrix ! See Page 9-15 VIS section of
c           relocable libaries
            do i = 1,2
               call dvswp(rot_matrix(i,i+1),3,rot_matrix(i+1,i),1, 3-i)
            end do
c
*                           ! FINISHED
            return
c
         end if
c
c....    Could not find system
         write(*,100) out_sys
  100    format(/" Error in ROTATE_CRD: could not find coordinate",
     .      " system ",a,/," Coordinates are not being rotated")
c
*                           ! we needed to rotate system
      end if
c
      return
      end

