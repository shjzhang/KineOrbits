       
C Procedure           DATE2J
C
      INTEGER FUNCTION     DATE2J( YEAR, MONTH, DAY )
C
C
C                            Purpose
C
C  This integer function (calendar DATE 2 Julian date) takes the input
C  Gregorian calendar date and returns as its functional value the
C  corresponding integer Julian date. Since the Julian date is an intege
C  this correspondence is exact for noon of the output calendar date.
C
C  The algorithm for this conversion is taken from the following article
C  Tantzen,R.T., "Communications of the ACM", Volume 6, Number 8, August
C  Algorithm 199, page 444.
C
C
C                            Input_Arguments
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
      INTEGER            YEAR
      INTEGER            MONTH
      INTEGER            DAY
C
C
C                            Declarations_of_Local_Variables
C
      INTEGER            Y
      INTEGER            M
      INTEGER            D
C
      INTEGER            C
      INTEGER            YA
C
C
C                     Method
C-&
C***********************************************************************
C
      Y = YEAR
      M = MONTH
      D = DAY
C
      IF( M .GT. 2 )THEN
         M = M - 3
      ELSE
         M = M + 9
         Y = Y - 1
      END IF
C
      C  = Y/100
      YA = Y - 100*C
      DATE2J = (146097*C)/4 + (1461*YA)/4 + (153*M+2)/5 + D + 1721119
C
      RETURN
      END   
