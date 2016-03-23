c
c   subroutine find_rnxhdr
c
      subroutine find_rnxhdr(clbl, nhdr, crnx_hdrlbl, crnx_hdrsec,
     &                             rrec, rrnx_hdrlbl, rrnx_hdrsec)
c
c=======================================================================
c     ****f* fmttrs/find_rnxhdr
c
c   FUNCTION   
c   
c     Returned rinex record number, header label, and header section 
c     information, which match the input 'hdrlbl' 
c
C   ARGUMENTS
c
c     hdrlbl       character (i) header label string
c     crnx_hdrlbl  character (i) rinex header label array
c     crnx_hdrsec  character (i) rinex header section array
c     rrec         integer   (o) returned record numbe of 'hdrlbl'
c     rrnx_hdrlbl  character (o) returned header label array of 'hdrlbl'
c     rrnx_hdrsec  character (o) retruned header section array of 
c                                'hdrlbl'
c
c   COPYRIGHT
c
c     Copyright(c) 2006-         Shoujian Zhang
c                                School of Geodesy and Geomatics 
c                                Wuhan University
c     ***
c
C     $Id: find_rnxhdr.f,v 1.0 2009/07/04 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
c     input/output
c
      character*20 clbl
      integer      nhdr
      character*20 crnx_hdrlbl(80)
      character*60 crnx_hdrsec(80)
c
      integer      rrec
      character*20 rrnx_hdrlbl(80)
      character*60 rrnx_hdrsec(80)
c
c     local variables
c
      integer      irec, nrec
c
      rrec = 0
c
      do irec=1, nhdr
c
         if(crnx_hdrlbl(irec).EQ.clbl)then
c
            rrec = rrec + 1
            rrnx_hdrlbl(rrec) = crnx_hdrlbl(irec)
            rrnx_hdrsec(rrec) = crnx_hdrsec(irec)
c
         endif
c
      enddo
c
      return
c
      end
