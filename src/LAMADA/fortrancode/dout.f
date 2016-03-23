      subroutine DOUT (lun, text, vec, l1, l2)

c*ver version 1, dd. 01-02-93
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol spools double precision vector to a file

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par lun    input    logical unit number of file
c*par text   input    text to accompany the data
c*par vec    input    double precision vector which has to be spooled
c*par l1     input    first element of vector to be spooled
c*par l2     input    last element of vector to be spooled

      implicit double precision (a-h, o-z)

      double precision 
     +  vec (*)
      character 
     +  text*(*)

      write (lun,'(/4x,2a,i4,a,i4,a/)') text, ' ( ',l1,' - ',l2,' )'
      write (lun,'((4x,6(1x,g11.4)))') (vec(i),i=l1,l2)
      write (lun,*)

      return
      end
