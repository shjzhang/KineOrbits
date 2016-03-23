       
C Procedure               SEC2JD
C
      subroutine  SEC2JDD( SEC1, SEC2, JD1, JD2 )
C
C
C                            Purpose
C
C  This subroutine (SEConds 2 Julian Date, Double Parts.Double Precision) take  
C  input time in seconds past the Julian reference date (JDREF) for this  and 
c  returns the corresponding Julian date as its functional value.Julian date 
c  is returned as two parts, the integer part and the fractional part. 
c  
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
      DOUBLE PRECISION   SEC1
      DOUBLE PRECISION   SEC2
      DOUBLE PRECISION   SEC1_Frac
      DOUBLE PRECISION   SEC2_TMP
C
C
C                            Declarations_of_Local_Variables
C
      DOUBLE PRECISION   SECDAY
      PARAMETER        ( SECDAY = 86400.0D0 )
c
c     divide JD to two part, the integer one and the fraction one
c
      DOUBLE PRECISION   JD1
      DOUBLE PRECISION   JD2
C
C
C                            Method
C-&
C***********************************************************************
C
C1    Divide the input seconds past the Julian reference date by the num
C1    of seconds in a day and add the Julian reference date.
c
c
      SEC1_Frac  = dMOD(SEC1, SECDAY)
c     SEC2       = SEC2 + SEC1_Frac
      SEC2_TMP   = SEC2 + SEC1_Frac
c
      JD1        = (SEC1 - SEC1_FRAC)/SECDAY + JDREF()
      JD2        = (SEC2_TMP)/SECDAY
C
      RETURN
      END   
