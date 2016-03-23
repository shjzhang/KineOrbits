     
C Procedure                  JDREF
C
      DOUBLE PRECISION FUNCTION     JDREF( )
C
C
C                            Purpose
C
C  This double precision function (Julian Date REFerence) will return th
C  the current Julian reference epoch for this library. Any time convers
C  routine in this library which outputs double precision seconds will o
C  seconds past this reference epoch. Note that this function must be ca
C  with an empty argument list: JDREF().
C
C> Subroutine JDSET may be called to change this reference epoch from th
C  default value of 2451545.0 which is J2000.0 (January 1, 2000, 12 hour
C
C  The alternate entry point, JDNEW, is reserved for use by other routin
C  this library and should never be called by the user.
C
C
C                            Input_Arguments
C
C                            Output_Arguments
C
C                            Declarations_of_External_Functions
C
C                            Declarations_of_Input_and_Output_Arguments
C
C                            Declarations_of_Local_Variables
C
C     Declare the alternate entry point:
      DOUBLE PRECISION   JDNEW
C
      DOUBLE PRECISION   REFDAT
C
      DOUBLE PRECISION   JDSAVE
C
C
      SAVE
C
C                            Data_Statements
C
      DATA               JDSAVE     /2451545.0D0/
C
C
C                            Method
C***********************************************************************
C
C1    Entry point JDREF.
C
      JDREF = JDSAVE
C
      RETURN
C
C***********************************************************************
C
C1    Entry point JDNEW.
C1    This entry point will change the Julian reference epoch to the val
C     the input argument REFDAT.
C
      ENTRY     JDNEW( REFDAT )
C
      JDSAVE = REFDAT
      JDNEW  = JDSAVE
C
      RETURN
      END   