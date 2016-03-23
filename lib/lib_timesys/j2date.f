     
C Procedure           J2DATE
C
      SUBROUTINE      J2DATE( JD, YEAR, MONTH, DAY )
C
C
C                            Purpose
C
C  This subroutine (Julian date 2 calendar DATE) converts the input inte
C  Julian date to the corresponding Gregorian calendar date. Since the J
C  date is an integer, this correspondence is exact for noon of the cale
C  date.
C
C  The algorithm for this conversion is taken from the following article
C  Tantzen,R.T., "Communications of the ACM", Volume 6, Number 8, August
C  Algorithm 199, page 444.
C
C
C                            Input_Arguments
C
C  JD       is the integer Julian date.
C
C
C                            Output_Arguments
C
C  YEAR     is the year number.
C  MONTH    is the month number.
C  DAY      is the day number.
C
C
C                            Output_Arguments
C
C                            Declarations_of_External_Functions
C
C                            Declarations_of_Input_and_Output_Arguments
C
      INTEGER            JD
      INTEGER            YEAR
      INTEGER            MONTH
      INTEGER            DAY
C
C
C                            Declarations_of_Local_Variables
C
      INTEGER            J
      INTEGER            Y
      INTEGER            M
      INTEGER            D
C
C
C                     Method
C-&
C***********************************************************************
C
      J = JD
C
C
      J = J - 1721119
      Y = (4*J-1)/146097
      J = 4*J - 1 - 146097*Y
      D = J/4
      J = (4*D+3)/1461
      D = 4*D + 3 -1461*J
      D = (D+4)/4
      M = (5*D-3)/153
      D = 5*D - 3 - 153*M
      D = (D+5)/5
      Y = 100*Y + J
      IF( M .LT. 10 )THEN
         M = M + 3
      ELSE
         M = M - 9
         Y = Y + 1
      END IF
C
C
      YEAR   = Y
      MONTH  = M
      DAY    = D
C
      RETURN
      END 