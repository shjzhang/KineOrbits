*
*  subroutine binary_search
*      
      subroutine binary_search(t_int, datas, ndata, imid, find)
c
c=======================================================================
c     ****f* SmartPPP/linear_interp
c
c   FUNCTION   
c   
*     seach the datas's element which is nearest to t_int, and return the
*     index of the datas returned in imid.
c
c   INPUTS
c
*     t_int        real         time to search   
*     datas(*)     real         datas
*     ndata        integer      data numbers
c
c   OUTPUT
c
*     imid         integer      left of the number which is close to
c
c   REVISION
c
c     2009/08/01                modify header
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: linear_interp.f,v 1.0 2007/06/01 $
c=======================================================================
*
      IMPLICIT NONE
c
c     input/output
c
      integer   ndata
c
      real*8    t_int
      real*8    datas(ndata)
c
c     local 
c
      integer   i
      integer   ileft
      integer   iright
      integer   imid
c
      logical   find
c
      save      ileft
      data      ileft  /1/
c
c     initialization
c
      ileft  = 1
      iright = ndata
      find   = .false.
c
      if(t_int.lt.datas(1))then
         imid = 1
         return
      endif
c
      if(t_int.gt.datas(ndata))then
         imid = ndata
         return
      endif
c
      imid = ileft
c
      ileft = imid -5
      if(ileft.lt.1) ileft = 1
      iright = imid + 5
      if(iright.gt.ndata) iright = ndata
c
      if(    t_int.lt.datas(ileft)
     &   .or.t_int.ge.datas(iright))then
      ileft = 1
      iright = ndata
      endif
c
c     interpolation with binary search method
c     
c     Notes!!!
c
c     This subroutine is revised from the binary search in book.
c     and is suitable for real-type data search.
c
      do while(ileft.LT.(iright-1))
         imid  = (ileft + iright)/2
         if(     t_int.LT.datas(imid) )then
            iright = imid 
         elseif( t_int.GT.datas(imid) )then
            ileft  = imid 
         elseif( t_int.EQ.datas(imid) )then
            find = .true.
            return 
         endif
      enddo
c
      imid = ileft
c
      return
c
      end
