*
*  subroutine linear_search
*      
      subroutine linear_search(key, ndata, datas, ikey, find)
c
c=======================================================================
c     ****f* SmartPPP/linear_search
c
c   FUNCTION   
c   
*     search the input data 'key' in the datas array, and the index are
*     returned.
c
c   INPUTS
c
*     key          integer      time to search   
*     datas(*)     integer      datas
*     ndata        integer      data numbers
c
c   OUTPUT
c
c     find         logical      searched variable
c
c   REVISION
c
c     2009/07/29                modified the header 
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: linear_search.f,v 1.0 2009/07/27 $
c=======================================================================
*
*     Variables
*         
      IMPLICIT NONE
c
c     Save variables
c
c     Input/Output Variables
c
      integer   ndata
c
      integer   key
      integer   datas(ndata)
c
c     Local Variables
c
      integer   i
      integer   ikey
c
      logical   find
c
c
c     initialization
c
      find = .false.
      ikey = 1
c
      do while(ikey .LE. ndata)
c        find ?
         if( key.EQ.datas(ikey) )then
             find = .true.
             return
         endif
c        index of key increase
         ikey = ikey + 1
      enddo
c
      return
c
      end
