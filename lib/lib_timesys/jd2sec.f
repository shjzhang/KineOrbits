       
C Procedure                  JD2SEC
C
      DOUBLE PRECISION FUNCTION     JD2SEC( JD )
C
C
C                            Purpose
C
C  This subroutine (Julian Date 2 SEConds) takes an input Julian date an
C  returns the corresponding time in seconds past the reference date (JD
C  for this library as its functional value.
C
C
C                            Input_Arguments
C
C  JD  is the Julian date to convert.
C
C
C                            Declarations_of_External_Functions
C
      DOUBLE PRECISION   JDREF
C
C 
C                            Declarations_of_Input_and_Output_Arguments
C
      DOUBLE PRECISION   JD
C
C
C                            Declarations_of_Local_Variables
C
      DOUBLE PRECISION   SECDAY
      PARAMETER        ( SECDAY = 86400.0D0 )
C
C
C                            Method
C-&
C***********************************************************************
C
C1    Subtract the Julian reference date from the input Julian date and
C1    multiply by the number of seconds in a day (86400).
      JD2SEC = ( JD - JDREF() ) * SECDAY
C
      RETURN
      END   