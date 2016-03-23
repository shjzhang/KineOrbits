       
C Procedure               SEC2JD
C
      DOUBLE PRECISION FUNCTION     SEC2JD( SEC )
C
C
C                            Purpose
C
C  This subroutine (SEConds 2 Julian Date) take  input time in seconds 
c  past the Julian reference date (JDREF) for this  and returns the corresponding 
c  Julian date as its functional value.
C
C
C                            Input_Arguments
C
C  SEC  is the seconds past the reference date for this library.
C
C
C                            Declarations_of_External_Functions
C
      DOUBLE PRECISION   JDREF
C
C
C                            Declarations_of_Input_and_Output_Arguments
C
      DOUBLE PRECISION   SEC
C
C
C                            Declarations_of_Local_Variables
C
      DOUBLE PRECISION   SECDAY
      PARAMETER        ( SECDAY = 86400.0D0 )
c
c     divide JD to two part, the integer one and the fraction one
c
      DOUBLE PRECISION   JD_INT
      DOUBLE PRECISION   JD_FRAC
C
C
C                            Method
C-&
C***********************************************************************
C
C1    Divide the input seconds past the Julian reference date by the num
C1    of seconds in a day and add the Julian reference date.
      SEC2JD  = SEC/SECDAY + JDREF()
C
      RETURN
      END   
