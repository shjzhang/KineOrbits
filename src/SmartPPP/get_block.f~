*
*  subroutine get_block
*      
      subroutine get_block(iPRN, block_type)
c
c=======================================================================
c     ****f* SmartPPP/get_block
c
c   FUNCTION   
c   
c     read IGS antex file, which is used to correct:
c     1)  the receiver's PCO and PCV
c     2)  the GNSS's PCO and PCV
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     NONE
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: get_block.f,v 1.0 2009/07/31 $
c=======================================================================
*         
      implicit none
c
      include       '../../include/igs.h'
      include       '../../include/atx.h'
      include       '../../include/rinex.h'
      include       '../../include/rinex.conf.h'
      include       '../../include/SmartPPP.h'
      include       '../../include/SmartPPP.conf.h'
c
      integer       iPRN
      character*20  block_type
c
      character*20  sat_id(MAX_PRN)
c
      common /sat_id/ sat_id
c
      block_type = sat_id(iPRN)
c
      end
