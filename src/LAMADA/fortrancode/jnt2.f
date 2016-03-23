      integer function JNT2 (d) 
      
c*ver version 1, dd. 19-05-95
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol 'ceiling' (rounding towards +infinity) of a double precision number

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par d      input    double precision number
c*par JNT2   output   ceiled number                                           

      implicit double precision (a-h, o-z)
  
      if (d.le.0) then
         JNT2 = Int(d)
      else
         JNT2 = Int(d) + 1
      endif
      
      return
      end
