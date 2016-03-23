     
C Procedure           CH2DP
C
      DOUBLE PRECISION FUNCTION    CH2DP( STRING )
C
C
C                     Purpose
C
C  This double precision function (CHaracter 2 Double Precision) takes t
C  character string STRING and returns the double precision equivalent.
C  string STRING must contain a legitimate number, either floating point
C  integer. Any blanks in the input STRING are ignored.
C
C
C                     Input_Arguments
C
C  STRING   is a character string.
C
C
C                     Declarations_of_External_Functions
C
C                     Declarations_of_Input_and_Output_Arguments
C
      CHARACTER*(*)        STRING
C
C
C                     Declarations_of_Local_Variables
C
      DOUBLE PRECISION     DP
      INTEGER              ISTAT
C
C
C                     Data_Statements
C
C                     Method
C-&
C***********************************************************************
C1  Main Routine
C***********************************************************************
C
      CALL CH2DPX( STRING, .TRUE., DP, ISTAT )
C
      CH2DP  =  DP
C
      RETURN
      END        
C  Use   MAXEXP = INT( LOG10( D1MACH(2) ) )
C  to check if input numbers are out of range???
C