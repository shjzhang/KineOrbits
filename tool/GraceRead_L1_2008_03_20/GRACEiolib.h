/* Version Information: @(#) GRACEiolib.h      1.78 03/06/06 */
/*---------------------------------------------------------------------------->
/  This file is available on:
/  http://gipsy.jpl.nasa.gov/grace_level1/GRACE_FORMATS
/
/  Change list:
/
/  glk    09/25/01    Added change list to comments section
/  glk    09/25/01    Modified IHK1X format
/  glk    09/25/01    added define statement for MAXSENSORNAME
/  glk    10/08/01    changed ACC1A qualflag bits 0 and 1
/                     bits are reversed to comply with qualflag philosphy
/                     of 0 = nominal 1 = off nominal
/  glk    10/08/01    defined quaflag bits 0 and 1 for MAG TNK products
/  glk    10/15/01    Added typedefs for ILG1A IHK1A IHK1B MAS1A MAS1B
/  glk    10/15/01    Added struct for ILG1A
/  glk    10/15/01    added define statement for MAXLOGNAME
/  glk    10/19/01    added struct for TIM1X
/  glk    10/26/01    added invalid ACC (GDEL) timing to ACC_1A qualflg
/  glk    10/29/01    added additional prodcuts to PRDID description and
/                     added struct used for each defined product
/  glk    11/01/01    added intermediate Level 1A products: GNI1A TDP1A SCI1A
/                     to documentation
/  glk    12/22/01    added Mass of cold gas per tank to MAS1X
/  glk    12/28/01    change tnk obs from float to doubles
/  glk    12/31/01    added CLK1B flags
/  glk    01/03/02    added linear acc residuals to format ACC1B
/  glk    01/10/02    added CMT_command and SCA2K_command structs
/  glk    01/11/02    added GNV1B flags
/  wib    02/05/02    document kbr1b_t better
/  glk    02/06/02    added valid flag to SCA1A
/  glk    02/11/02    added range rate/acceleration corrections for 
/                     light time/geometric correction to KBR1B
/  glk    02/11/02    added missed antenna state packet flag in TIM1X
/  glk    02/13/02    added appropriate names for VAS VCM VGN VGB VGO and VSL
/                     in comment lines
/  glk    02/22/02    added K and Ka frequencies to  OSCFQ
/  glk    02/22/02    added flags to XXXVO and OSCFQ
/  glk    03/04/02    renamed XXVVO and quaternions products + new product VKB1B
/  ljr    04/04/02    reshuffle SCA1A format: record all flags in sca_mode,
/                     write Residual in sca_config[0], define some qualflg bits
/  glk    04/12/02    change wording on iono correction for KBR1B product
/  glk    04/12/02    change MAG1X product wording 1A -> data in MAG frame
/                     1B -> mag data in SRF frame + change units to microT
/  glk    05/09/02    added on_time and unaccounted flags to THR1X
/  glk    08/13/02    added flags for missing obdh mapping and missing CLK1B
/                     data to MAG1X_t,MAS1X_t,IHK1X_t,ACC1A_t,THR1X_,TNK1X_t
/  wib    09/11/02    try to document THR1X_
/  glk    03/18/03    added PCI1A product and struct
/  glk    05/12/03    added ACC1A Tenhz_count and Mhz_count
/  glk    05/16/03    added PCI and ACC1B bit comments
/  scw    05/21/03    added ACC1B qualflg bit 2 comments
/  glk    06/06/03    change comments/meaning of GFD1X L2_phase and K_phase
/                     observations
/  glk    08/18/03    change comments/meaning of GFD1X bits 4 and 5 of qualflag
/  glk    08/18/03    change comments/meaning of SCA1A bit 4 
/  glk    08/29/04    ACC1A bit 5 used for reception of IPU nav packet by OBDH
/
/  File naming conventions and binary formats for level-1A/B products
/  
/  PRDID_YYYY-MM-DD_S_RL.EXT
/  
/  
/  PRDID = product identification label, e.g. ACC1B .....
/          see below for the complete current list
/  YYYY  = year
/  MM    = month
/  DD    = day of month
/  S     = GRACE satellite identifier 
/          (A,B or X= combined product of GRACE A and B)
/  RL    = Data product version number
/  EXT   = file extension indicating binary (dat) or ascii (asc) files
/  
/  An example: ACC1B_2003-03-03_A_00.dat
/  
/  This file contains level-1B Accelerometer data from the GRACE A satellite  
/  for March 3rd 2003. This file is version 0
/
/  Level1A files use the native frames as defined by each instrument
/  Level1B files use the Science Reference Frame (SRF) for coordinates and
/  rotations
/  
/  the Following PRDID are being used:
/  
/  PRDID struct   Description
/        used
/  ACC1A ACC1A_t  Level 1A Accelerometer data (including housekeeping)   
/  ACC1B ACC1B_t  Level 1B Accelerometer data
/  CLK1A CLK1B_t  Level 1A Satellite Clock solution (based on IPU nav solution)
/  CLK1B CLK1B_t  Level 1B Satellite Clock solution (from OD software + CLK1A)
/  GNV1A GNV1A_t  Level 1A On board navigation solution
/  GNI1A GNV1B_t  Level 1A Intermediate GRACE satellite orbit solution
/  GNV1B GNV1B_t  Level 1B navigation solution (from OD software)
/  TDP1A GIPSY    Level 1A Intermediate GRACE satellite clock solution
/  GPS1A GFD1X_t  Level 1A GPS flight data
/  GPS1B GFD1X_t  Level 1B GPS flight data
/  ILG1A ILG1X_t  Level 1A IPU log messages
/  KBR1A GFD1X_t  Level 1A KBR ranging data
/  KBR1B KBR1B_t  Level 1B KBR ranging data
/  MAG1A MAG1X_t  Level 1A Magnetic Torque Rod Activation + Magnetometer data
/  MAG1B MAG1X_t  Level 1B Magnetic Torque Rod Activation + Magnetometer data
/  MAS1A MAS1X_t  Level 1A Spacecraft mass as a function of time
/  MAS1B MAS1X_t  Level 1B Spacecraft mass as a function of time
/  PCI1A PCI1A_t  Level 1A Spacecraft Phase Center to CG correction and derivatives
/  USO1B OSCFQ_t  Oscilator frequency data (derived from OD software output)
/  SCA1A SCA1A_t  Level 1A star camera data (both raw SCA in one file per SC)
/  SCI1A SCA1B_t  Level 1A intermediate star camera data (compressed/combined)
/  SCA1B SCA1B_t  Level 1B star camera data (compressed/combined SCA data) 
/  THR1A THR1X_t  Level 1A thruster activation data
/  THR1B THR1X_t  Level 1B thruster activation data
/  TNK1A TNK1X_t  Level 1A Gas tank sensor + auxiliary data for COM management
/  TNK1B TNK1X_t  Level 1B Gas tank sensor + auxiliary data for COM management
/  VGN1B XXXVO_t  Vector offset file for GPS Main Antenna in SRF
/  VGB1B XXXVO_t  Vector offset file for GPS Backup Navigation Antenna in SRF
/  VGO1B XXXVO_t  Vector offset file for GPS Occultation Antenna in SRF
/  VSL1B XXXVO_t  Vector offset file for SLR Corner cube reflector offset in SRF
/  VCM1B XXXVO_t  Vector offset file for Center of Mass solution from
/                 calibration maneuvers or tracking model in SRF
/  VKB1B XXXVO_t  Vertor offset for KBR phase centers in SRF
/  QKS1B SCA1B_t  Rotation from Star Camera Frames into K-Band Frame 
/  QSA1B SCA1B_t  Rotation From Star Camera Frames into  SRF 
/  QSB1B SCA1B_t  Rotation From Satellite Body frame into SRF
/  AHK1A ACC1A_t  Level 1A Accelerometer House keeping data
/  AHK1B ACC1A_t  Level 1B Accelerometer House keeping data
/  IHK1A IHK1X_t  Level 1A IPU House keeping data
/  IHK1B IHK1X_t  Level 1B IPU House keeping data
/  TIM1A TIM1X_t  Level 1A OBDH time mapping to GPS time
/  TIM1B TIM1X_t  Level 1B OBDH time mapping to GPS time
/  HRT1A HRT1X_t  Level 1A High Resolution Temperature measurements (HK)
/  HRT1B HRT1X_t  Level 1B High Resolution Temperature measurements (HK)
/------------------------------------------------------------------------------
/
/  Each file contains an ascii header of multiple 80 byte records. 
/  The last ascii header record is labeled "END OF HEADER". 
/  After the last header record, multiple data records are written. The
/  data records are defined according to the PRDID mention above. To find the
/  associated structs for each PRDID see the struct type PRDID_t (e.g. ACC1A_t)
/  below**. The data are stored in the sequence defined in the structs below.
/
/  All files are designed to be readable without any of the information 
/  contained in the header.
/
/  **EXCEPTIONS: For Struct definition of PRDID's GPS1A,GPS1B,KBR1A see 
/        GFD1X_t definition!!
/                For Struct definition for PRDID AHK1A/B see ACC1A struct
/                  
/
/------------------------------------------------------------------------------
/
/  The Ascii header for each format  contains the following information:
/
/  PRODUCER AGENCY               : NASA                                            
/  PRODUCER INSTITUTION          : JPL                                             
/  FILE TYPE ipACC1BF            : 8                                               
/  FILE FORMAT 0=BINARY 1=ASCII  : 1                                               
/  NUMBER OF HEADER RECORDS      : 19                                              
/  SOFTWARE VERSION              : @(#) Bin2AsciiLevel1.c       1.5 03/11/01       
/  SOFTWARE LINK TIME            : @(#) 2001-04-17 08:10:27 glk  bart              
/  REFERENCE DOCUMENTATION       : GRACE Level 1 Software Handbook                 
/  SATELLITE NAME                : GRACE B                                         
/  SENSOR NAME                   : ACC GRACE B                                     
/  TIME EPOCH (GPS TIME)         : 2000-01-01 12:00:00                             
/  TIME FIRST OBS(SEC PAST EPOCH): 99921000.000000 (2003-03-02 23:50: 0.00)        
/  TIME LAST OBS(SEC PAST EPOCH) : 99935400.000000 (2003-03-03 03:50: 0.00)        
/  NUMBER OF DATA RECORDS        : 2881                                            
/  PRODUCT CREATE START TIME(UTC): 2001-04-17 15:10:59                             
/  PRODUCT CREATE END TIME(UTC)  : 2001-04-17 15:10:59                             
/  FILESIZE (BYTES)              : 157194                                          
/  FILENAME                      : ACC1B_2003-03-02_B_01.pass                      
/  PROCESS LEVEL (1A OR 1B)      : 1B                                              
/  INPUT FILE NAME               : TMBJ0<-GR1-0-RDC-RT-SC+NZ_2003_158_10_24_1_2.bj 
/  INPUT FILE TIME TAG (UTC)     : TMBJ0<-2003-06-07 10:24:00 by RDC
/          ..
/  (list of all input files plus)
/  (creation time tag information)
/          ..
/  END OF HEADER 
/
/  Each header record is 80 bytes long and the records with data start after
/  the header record labelled "END OF HEADER
/  
/  Note that the strings for SOFTWARE VERSION and SOFTWARE LINK TIME were 
/  designed to work with the unix "what" utility.
/
/  Example:
/  what ACC1B_2003-03-03_B_00.dat 
/
/  ACC1B_2003-03-03_B_00.dat:
/         CombineLevel1.c       1.4 03/09/01         
/         2001-03-09 09:32:55 glk  grande      
/------------------------------------------------------------------------------
/
/  Ascii versions of Level-1B format. 
/
/  For all Level-1B format an ascii version of the data is defined. At this 
/  time no ascii files are defined for the Level-1A format. 
/  The headers for the ascii files are identical to the binary data files with
/  only relevant information changed (e.g. FILE FORMAT). The sequence of the
/  data with one record (or struct) is the same as defined below. All data
/  entries are whitespace delimited. For examples see:
/
/  /goa/local/file_formats/level1B_examples. 
/
/  All binary Level-1B file can be converted into ascii Level-1B format files
/  by using:
/
/  Bin2AsciiLevel1 -binfile ACC1B_2003-03-03_A_00.dat  \
/                  -ascfile ACC1B_2003-03-03_A_00.asc
/
/  In the ascii data set the individual bits of the bit fields are reported. 
/  For example a thruster record looks like this:

/  99921000 0 G B   1 1 1 1 1 1 1 1 1 1 1 1 1 1   00001011
/
/  The qualflag "00001011" should be interpreted in the following way. The bits
/  should be read from right to left:
/
/   qualflag =         00001011
/                      ^^^^^^^^
/                      ||||||||
/   bit nr   =         76543210
/
/  The bit number is reported in the documentation and assigned a specific 
/  quality condition. All bit fields (char, short and long) are represented 
/  the same way.
/
<----------------------------------------------------------------------------*/


#ifndef _GRACEiolib_h_
#define _GRACEiolib_h_

#include "GRACEdefs.h"

#ifdef _mk_extern_
#  define EXTERN
#  define EXTINIT(var,val) var = val
#else
#  define EXTERN extern
#  define EXTINIT(var,val) extern var
#endif


#define NFILETYPEMAX       30       /* Maximum number of file types           */
#define HEADERMAXCHAR      80       /* Maximum number of characters in header */
#define NMAXHEADERREC      100      /* Maximum number of header records       */
#define HEADERLABELMAXCHAR 30       /* Maximum number of header label chars   */
#define MAXINPUTFILELABEL  20       /* Maximum #   of header input file labels*/
#define MAXSENSORNAME    1000       /* Maximum length of IPU sensor names     */
#define MAXLOGNAME       1000       /* Maximum length of IPU log packet       */

#define MAXPRNS            24       /* Maximum number of PRNs available       */
#define MAXTHRSTRS         14       /* Maximum number of thrusters            */
#define NSCACONF           3        /* Size of Star Camera Assemb. config.    */

/* Define Global Header Label and contents arrays  */

EXTERN char FileHeaderLabel[NFILETYPEMAX][NMAXHEADERREC][HEADERMAXCHAR];
EXTERN char FileHeaderContents[NFILETYPEMAX][NMAXHEADERREC][HEADERMAXCHAR];

/*----------------------------------------------------------------------------->
/  Structure definitions
<-----------------------------------------------------------------------------*/

typedef struct HeaderInputFileLabel_t /* struct containing input file label   */
                                      /* info necessary for file headers      */
        {
         char filekey[HEADERMAXCHAR+10];
         char name[HEADERMAXCHAR+10];
         char time_tag[HEADERMAXCHAR+10];
         char software_version[HEADERMAXCHAR+10];
         char linktime[HEADERMAXCHAR+10];
        } HeaderInputFileLabel_t;

typedef struct FileHeader_t         /* Generic file header struct             */
        {
         long filetype;/*               File type identification number       */
                       /* ipGFD1XF    1 GPS Flight Data Format File           */
                       /* ipGPS1AF    1 GPS Flight Data Format File           */
                       /* ipGPS1BF    1 GPS Flight Data Format File           */
                       /* ipKBR1AF    1 GPS Flight Data Format File           */
                       /* ipGNV1AF    2 GPS Navigation Level 1A Format File   */
                       /* ipSCA1AF    3 Star Camera Assembly 1A Data Fmt File */
                       /* ipACC1AF    4 Level 1A Accelerometer Data File      */
                       /* ipGNV1BF    5 GPS Navigation Level 1B Format File   */
                       /* ipSCA1BF    6 Star Camera Assembly 1B Data Fmt File */
                       /* ipKBR1BF    7 Level 1B KBR Data Format File         */
                       /* ipACC1BF    8 Level 1B Accelerometer Data File      */
                       /* ipXXXVOF    9 Vector Orientation Data File          */
                       /* ipIOA1BF   10 Inertial Orientation of ACC File      */
                       /* ipOSCFQF   11 Ulta Stable Oscillator Stability File */
                       /* ipMAG1XF   15 Magnetometer/Magnettorquer Data File  */
                       /* ipTHR1XF   16 Thruster 1B Data File                 */
                       /* ipCLK1BF   17 Clock conversion from rcv to GPS time */
                       /* ipTNK1XF   18 Cold gas tank Data File               */
                       /* ipAHK1XF   19 Accelerometer Housekeeping Data File  */
                       /* ipIHK1XF   20 IPU Housekeeping 1A Data File         */
                       /* ipPCI1AF   24 Phase Center to CG Range cor Data File*/

          long formattype;          /* format indicator                       */
                                    /* 0 = binary format                      */
                                    /* 1 = ascii  format                      */

          long nrecord;             /* number of records in header            */

          long init_flag;           /*flag indicating if struct is initialized*/
                                    /* 0 = struct not initialized             */
                                    /* 1 = struct initialized                 */
                                   
          char   ProducerAgency[HEADERMAXCHAR+10];
          char   ProducerInstitution[HEADERMAXCHAR+10];
          char   SoftwareVersion[HEADERMAXCHAR+10];
          char   LinkTime[HEADERMAXCHAR+10];
          char   Documentation[HEADERMAXCHAR+10];
          char   SatelliteName[HEADERMAXCHAR+10];
          char   SensorName[HEADERMAXCHAR+10];
          char   TimeEpoch[HEADERMAXCHAR+10];
          double TimeFirstObs;
          double TimeLastObs;
          long   NumberObs;
          long   NumberBytes;
          char   ProductCreateStartTime[HEADERMAXCHAR+10];
          char   ProductCreateEndTime[HEADERMAXCHAR+10];
          char   FileName[HEADERMAXCHAR+10];
          char   ProcessLevel[HEADERMAXCHAR+10];

          char   HeaderCards[NMAXHEADERREC][HEADERMAXCHAR+10];
          long   NinputFileLabel;
          HeaderInputFileLabel_t InputFileLabel[MAXINPUTFILELABEL];

        } FileHeader_t;

typedef struct CLK1B_t              /* Level 1B Clock Data Format Record      */
        {
        long            rcv_time;   /* Receiver time, seconds past 12:00:00   */
                                    /* noon 01-Jan-2000                       */
        char            GRACE_id;   /* GRACE satellite id                     */
        byte            clock_id;   /* Clock id                               */
        double          eps_time;   /* Level 1B clock offset where            */
                                    /* GPS time = time_rcv + eps_time (s)     */
        double          eps_err;    /* Formal error on eps_time (s)           */
        double          eps_drift;  /* Clock drift (s/s)                      */
        double          drift_err;  /* Formal error on eps_drift (s/s)        */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = 1 -> linear extrapolation not */
                                    /*               valid AFTER rcv_time     */
                                    /* bit  1 = 1 -> linear extrapolation not */
                                    /*               valid BEFORE rcv_time    */
                                    /* bit  2 = overlap data missing before   */
                                    /*               start midnight           */
                                    /* bit  3 = overlap data missing after    */
                                    /*               start midnight           */
                                    /* bit  4 = overlap data missing before   */
                                    /*               end midnight             */
                                    /* bit  5 = overlap data missing after    */
                                    /*               end midnight             */
                                    /* bit  6 = Not Defined                   */
                                    /* bit  7 = Not Defined                   */
        } CLK1B_t; 

typedef struct GFD1X_t              /* GPS Flight Data Format Record          */
        {
        long           rcvtime_intg;/*Receiver time, integer seconds          */
                                    /* past 12:00:00 noon 01-Jan-2000 (s)     */
        long           rcvtime_frac;/*Receiver time, microseconds part        */
        char            GRACE_id;   /* GRACE satellite id                     */
        byte            prn_id;     /* GPS spacecraft PRN number or GRACE id  */
                                    /* number                                 */
        byte            ant_id;     /* GPS or KBR antenna id on GRACE         */
                                    /* spacecraft                             */
                                    /* ant_id = 0 GPS navigation antenna      */
                                    /* ant_id = 2 GPS occultation antenna     */
                                    /* ant_id = 4 GPS backup nav antenna      */
                                    /* ant_id = 11 KBR antenna                */
                                    /* ant_id =-11 indicates missing interrupt*/
                                    /*             on Ka phase corrected      */
        unsigned short  prod_flag;  /* Product flag. Bitmask set to indicate  */
                                    /* presence of data type according to the */
                                    /* following                              */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = C/A pseudo range              */
                                    /* bit  1 = L1  pseudo range              */
                                    /* bit  2 = L2  pseudo range              */
                                    /* bit  3 = C/A carrier phase             */
                                    /* bit  4 = L1  carrier phase             */
                                    /* bit  5 = L2  carrier phase             */
                                    /* bit  6 = SNR C/A channel               */
                                    /* bit  7 = SNR L1 channel                */
                                    /* bit  8 = SNR L2 channel                */
                                    /* bit  9 = C/A receiver channel          */
                                    /* bit 10 = L1  receiver channel          */
                                    /* bit 11 = L2  receiver channel          */
                                    /* bit 12 = K   band carrier phase        */
                                    /*          or raw L2 carrier phase; GPS1X*/
                                    /* bit 13 = Ka  band carrier phase        */
                                    /* bit 14 = SNR K band                    */
                                    /* bit 15 = SNR Ka band                   */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit 0 = phase break occurred in L1/K/CA*/
                                    /* bit 1 = phase break occurred in L2/Ka  */
                                    /* bit 2 = cycle slip detected in L1/K/CA */
                                    /* bit 3 = cycle slip detected in L2/Ka   */
                                    /* bit 4 = insane K or Ka polynomial coeff*/
                                    /* bit 5 = K or Ka phase is missing       */
                                    /* bit 4 = L1 SNR < 5 for GPS1X,GPI1A     */
                                    /* bit 5 = L2 SNR < 5  for GPS1X,GPI1A    */
                                    /* bit 6 = K SNR < 450                    */
                                    /* bit 7 = Ka SNR < 450                   */
        double          CA_range;   /* C/A pseudo range  (m)                  */
        double          L1_range;   /* L1  pseudo range  (m)                  */
        double          L2_range;   /* L1  pseudo range  (m)                  */
        double          CA_phase;   /* C/A carrier phase (m)                  */
        double          L1_phase;   /* L1  carrier phase (m)                  */
        double          L2_phase;   /* L2  ion-smoothed carrier phase (m)     */
        unsigned short  CA_SNR;     /* SNR C/A channel (units + integration   */ 
                                    /* time) (V/V)                            */
        unsigned short  L1_SNR;     /* SNR L1  channel (V/V)                  */
        unsigned short  L2_SNR;     /* SNR L2  channel (V/V)                  */
        unsigned short  CA_chan;    /* C/A receiver channel                   */
        unsigned short  L1_chan;    /* L1  receiver channel                   */
        unsigned short  L2_chan;    /* L2  receiver channel                   */
        double          K_phase;    /* K band carrier phase (cycles)          */
                                    /* or raw L2 carrier phase (m);GPS1X only */
        double          Ka_phase;   /* Ka band carrier phase (cycles)         */
        unsigned short  K_SNR;      /* SNR K band channel (V/V)               */
        unsigned short  Ka_SNR;     /* SNR Ka band channel (V/V)              */
        } GFD1X_t;

typedef struct GNV1A_t              /* GPS Navigation Level 1A Format Record  */
        {
        long   rcv_time;            /* Receiver time, seconds past            */
                                    /* 12:00:00 noon 01-Jan-2000              */
        byte            n_prns;     /* Number of PRNs used in the solution    */
        char            GRACE_id;   /* GRACE satellite id                     */
        float           chisq;      /* Confidence factor of the solution      */
                                    /* (chi-squared)                          */
        float           cov_mult;   /* Covariance precision multiplier for    */
                                    /* solution                               */
        short           voltage;    /* Clock steering voltage in counts       */
        double          xpos;       /* Position, x value (WGS-84) (m)         */
        double          ypos;       /* Position, y value (WGS-84) (m)         */
        double          zpos;       /* Position, z value (WGS-84) (m)         */
        double          xpos_err;   /* Formal error on x position (m)         */
        double          ypos_err;   /* Formal error on y position (m)         */
        double          zpos_err;   /* Formal error on z position (m)         */
        double          xvel;       /* Velocity along x-axis (WGS-84) (m/s)   */
        double          yvel;       /* Velocity along y-axis (WGS-84) (m/s)   */
        double          zvel;       /* Velocity along z-axis (WGS-84) (m/s)   */
        double          xvel_err;   /* Formal error in velocity along x-axis  */
                                    /* (m/s)                                  */
        double          yvel_err;   /* Formal error in velocity along y-axis  */
                                    /* (m/s)                                  */
        double          zvel_err;   /* Formal error in velocity along z-axis  */
                                    /* (m/s)                                  */
        double          time_offset;/* Time offset between GPS time and       */
                                    /* receiver time (s)                      */
        double      time_offset_err;/* Formal Error Time offset between GPS   */
                                    /* time and receiver time (s)             */
        double          time_drift; /* Time offset drift between GPS time and */
                                    /* receiver time (s/s)                    */
        double          err_drift;  /* Formal error in time offset drift (s/s)*/
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Not Defined                   */
                                    /* bit  1 = Not Defined                   */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = Not Defined                   */
                                    /* bit  7 = Not Defined                   */
                                    /* Following contains the id and az/el    */
                                    /* for each PRN used in the solution.     */
                                    /* Will modify declaration later to make  */
                                    /* these data a set of linked lists       */
        byte       prn_id[MAXPRNS]; /* Identification of PRN                  */
        double     el_prn[MAXPRNS]; /* Elevation of that PRN                  */
        double     az_prn[MAXPRNS]; /* Azimuth of that PRN                    */
        } GNV1A_t;

typedef struct SCA1A_t              /* Star Camera Assembly 1A Data Fmt Record*/
        {
        long            rcv_time;   /* Receiver time, seconds past            */
                                    /* 12:00:00 noon 01-Jan-2000              */
        char            GRACE_id;   /* GRACE satellite id                     */
        byte            sca_id;     /* SCA identification number where        */
                                    /*     1 = Star camera number 1           */
                                    /*     2 = Star camera number 2           */
                                    /*     3 = IMU                            */
        char            sca_desig;  /* Star camera designation                */
                                    /*    'P' = Primary star camera           */
                                    /*    'S' = Secondary star camera         */
        double          quatangle;  /* Cos mu/2 element of quaternion         */
        double          quaticoeff; /* I element of quaternion rotation axis  */
        double          quatjcoeff; /* J element of quaternion rotation axis  */
        double          quatkcoeff; /* K element of quaternion rotation axis  */
        unsigned char   nlocks;     /* Number of locks of Star Camera         */
        unsigned char   nstars;     /* Number of stars                        */
        char   sca_config[NSCACONF];/* SCA configuration:                     */
                                    /* sca_config[0] == SCA "Residual" field  */
                                    /* sca_config[1,2] not defined            */
        byte            sca_mode;   /* SCA "Flags" field                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Valid flag (1=valid,0=invalid)*/
                                    /* bit  1 = "Precision"                   */
                                    /* bit  2 = "Add 1/4 integration period to*/
                                    /*           SCA time tag                 */
                                    /* bit  3 = "Non-Stellar Object           */
                                    /*           (i.e. false stars)"          */
                                    /* bits 4,5 = "Head ID; set to 1 or 2"    */
                                    /* bit  6 = "Orbit Correction             */
                                    /*           (used/not used)"             */
                                    /* bit  7 = "Sequence"                    */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Valid flag (0=valid,1=invalid)*/
                                    /*          (n.b. flipped from sca_mode)  */
                                    /* bit  1 = "Precision"                   */
                                    /* bit  2 = "Add 1/4 integration period to*/
                                    /*           SCA time tag                 */
                                    /* bit  3 = "Non-Stellar Object           */
                                    /*           (i.e. false stars)"          */
                                    /* bit  4 = SCA's not in dual 1 Hz mode   */
                                    /* bit  5 = not defined                   */
                                    /* bit  6 = "Orbit Correction             */
                                    /*           (used/not used)"             */
                                    /* bit  7 = "Sequence"                    */
        } SCA1A_t;

typedef struct ACC1A_t              /* Level 1A Accelerometer Data Format     */
        {
        long           rcvtime_intg;/* Time, seconds past 12:00:00            */
                                    /* noon 01-Jan-2000                       */
        long           rcvtime_frac;/* Time, microseconds part                */
        char           time_ref;    /* Time reference frame where             */
        char           GRACE_id;    /* GRACE satellite id                     */
                                    /*    'R' = Receiver time                 */
                                    /*    'G' = GPS time                      */
        unsigned char   qualflg;    /* Data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = 0 -> GPS Receiver Time        */
                                    /*          1 -> Space Craft Elapsed Time */
                                    /* bit  1 = 0 -> Pulse Sync               */
                                    /*          1 -> no Pulse Sync            */
                                    /* bit  2 = ICU board                     */
                                    /*          (0 = nominal, 1 = redundant   */
                                    /* bit  3 = Invalid ACC (GDEL) timing     */
                                    /* bit  4 = ACC Mode (0 = nrm, 1 = lrm)   */
                                    /*          nrm = Normal Range Mode       */
                                    /*          lrm = Large Range Mode        */
                                    /* bit  5 = IPU nav/timing packet received*/
                                    /* bit  6 = No OBDH->Receiver time mapping*/
                                    /* bit  7 = No Clock correction available */
        unsigned long   prod_flag;  /* Product flag.  Bit mask set to indicate*/
                                    /* presence of data types                 */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = lin_accl_x                    */
                                    /* bit  1 = lin_accl_y                    */
                                    /* bit  2 = lin_accl_z                    */
                                    /* bit  3 = ang_accl_x                    */
                                    /* bit  4 = ang_accl_y                    */
                                    /* bit  5 = ang_accl_z                    */
                                    /* bit  6 = bias_vol                      */
                                    /* bit  7 = vd                            */
                                    /* bit  8 = x1_out                        */
                                    /* bit  9 = x2_out                        */
                                    /* bit 10 = x3_out                        */
                                    /* bit 11 = y1_out                        */
                                    /* bit 12 = y2_out                        */
                                    /* bit 13 = z1_out                        */
                                    /* bit 14 = tesu                          */
                                    /* bit 15 = taicu                         */
                                    /* bit 16 = tisu                          */
                                    /* bit 17 = v15picu                       */
                                    /* bit 18 = v15micu                       */
                                    /* bit 19 = vr5picu                       */
                                    /* bit 20 = tcicu                         */
                                    /* bit 21 = v15psu                        */
                                    /* bit 22 = v15msu                        */
                                    /* bit 23 = v48psu                        */
                                    /* bit 24 = v48msu                        */
                                    /* bit 25 = status                        */
                                    /* bit 26 = ICU block number              */
                                    /* bit 27 = 10Hz clock count              */
                                    /* bit 28 = MHz clock count               */
                                    /* bit 29 = Not currently used            */
                                    /* bit 30 = Not currently used            */
                                    /* bit 31 = Not currently used            */
        double          lin_accl_x; /* Linear acceleration along x-axis       */
                                    /* (m/s**2)                               */
        double          lin_accl_y; /* Linear acceleration along y-axis       */
                                    /* (m/s**2)                               */
        double          lin_accl_z; /* Linear acceleration along z-axis       */
                                    /* (m/s**2)                               */
        double          ang_accl_x; /* Angular acceleration about x-axis      */
                                    /* (rad/s**2)                             */
        double          ang_accl_y; /* Angular acceleration about y-axis      */
                                    /* (rad/s**2)                             */
        double          ang_accl_z; /* Angular acceleration about z-axis      */
                                    /* (rad/s**2)                             */
        double          bias_vol;   /* Proof mass bias voltage (averaged) (V) */
        float           vd;         /* Amplitude of the AC voltage that       */
                                    /* operates the position sensors (Vrms)   */
        float           x1_out;     /* displacement of capacitive sensor X1(m)*/
        float           x2_out;     /* displaacementof capacitive sensor X2(m)*/
        float           x3_out;     /* displaacementof capacitive sensor X3(m)*/
        float           y1_out;     /* displaacementof capacitive sensor Y1(m)*/
        float           y2_out;     /* displaacementof capacitive sensor Y2(m)*/
        float           z1_out;     /* displaacementof capacitive sensor Z (m)*/
        float           tesu;       /* Temperature of SU electronics (deg.C)  */
        float           taicu;      /* Temperature of ICU power supply board  */
                                    /* (deg.C)                                */
        float           tisu;       /* Temperature of internal core (deg. C)  */
        float           v15picu;    /* ICU reference voltage +15 V            */
        float           v15micu;    /* ICU reference voltage -15 V            */
        float           vr5picu;    /* ICU reference voltage +5 V             */
        float           tcicu;      /* Temperature of ICU A/D converter       */
                                    /* board (deg.C)                          */
        float           v15psu;     /* SU voltage +15 V                       */
        float           v15msu;     /* SU voltage -15 V                       */
        float           v48psu;     /* SU voltage +48 V                       */
        float           v48msu;     /* SU voltage -48 V                       */
        char            status;     /* 60 second status bytes                 */
        unsigned short  icu_blk_nr; /* ICU block number                       */
        char            Tenhz_count;/* 10Hz clock count                       */
        long            Mhz_count;  /* Mhz clock count                        */
        } ACC1A_t;

typedef struct GNV1B_t              /* GPS Navigation Level 1B Format Record  */
        {
        long   gps_time;            /* GPS time, seconds past 12:00:00        */
                                    /* noon 01-Jan-2000                       */
        char            GRACE_id;   /* GRACE satellite id                     */
        char            coord_ref;  /* Coordinate reference frame where       */
                                    /*   'E' = Earth-fixed                    */
                                    /*   'I' = Inertial                       */
        double          xpos;       /* Position, x value (ITRF) (m)           */
        double          ypos;       /* Position, y value (ITRF) (m)           */
        double          zpos;       /* Position, z value (ITRF) (m)           */
        double          xpos_err;   /* Formal error on x position (m)         */
        double          ypos_err;   /* Formal error on y position (m)         */
        double          zpos_err;   /* Formal error on z position (m)         */
        double          xvel;       /* Velocity along x-axis (ITRF) (m/s)     */
        double          yvel;       /* Velocity along y-axis (ITRF) (m/s)     */
        double          zvel;       /* Velocity along z-axis (ITRF) (m/s)     */
        double          xvel_err;   /* Formal error in velocity along x-axis  */
                                    /* (m/s)                                  */
        double          yvel_err;   /* Formal error in velocity along y-axis  */
                                    /* (m/s)                                  */
        double          zvel_err;   /* Formal error in velocity along z-axis  */
                                    /* (m/s)                                  */
        unsigned char   qualflg;    /* Data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Not Defined                   */
                                    /* bit  1 = Not Defined                   */
                                    /* bit  2 = overlap data missing before   */
                                    /*               start midnight           */
                                    /* bit  3 = overlap data missing after    */
                                    /*               start midnight           */
                                    /* bit  4 = overlap data missing before   */
                                    /*               end midnight             */
                                    /* bit  5 = overlap data missing after    */
                                    /*               end midnight             */
                                    /* bit  6 = Not Defined                   */
                                    /* bit  7 = formal errors are not         */
                                    /*          available and set to 0.0      */
        } GNV1B_t;

typedef struct SCA1B_t              /* Star Camera Assembly 1B Data Fmt Record*/
        {
        long   gps_time;            /* GPS time, seconds past 12:00:00        */
                                    /* noon 01-Jan-2000                       */
        char            GRACE_id;   /* GRACE satellite id                     */
        byte            sca_id;     /* SCA identification number              */
                                    /*     1 = Star camera number 1           */
                                    /*     2 = Star camera number 2           */
                                    /*     3 = IMU                            */
                                    /*     4 = Combination of 1 + 2           */ 
        double          quatangle;  /* Cos mu/2 element of quaternion         */
        double          quaticoeff; /* I element of quaternion rotation axis  */
        double          quatjcoeff; /* J element of quaternion rotation axis  */
        double          quatkcoeff; /* K element of quaternion rotation axis  */
        double          qual_rss;   /* rss of formal error of quaternions     */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = filled data at T              */
                                    /* bit  1 = filled data at T +/- 1 sec    */
                                    /* bit  2 = filled data at T +/- 2 sec    */
                                    /* bit  3 = data from 1 star camera only  */
                                    /* bit  4 = extrapolated clock correction */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = low rate data from 2nd SCA    */
                                    /* bit  7 = low rate data from 1st SCA    */
        } SCA1B_t;
 
typedef struct KBR1B_t              /* Level 1B KBR Data Format Record        */
        {
        long   gps_time;            /* GPS time, seconds past 12:00:00        */
                                    /* noon 01-Jan-2000                       */
        double          biased_range;/* Biased range between GRACE A and B (m)*/
                                     /* digitally filtered but uncorrected    */ 
                                     /* except for the ionosphere */
                                     /* to correct for the light time, and the*/
                                     /* antenna offsets use:                  */
                                     /*  biased_range + lighttime_corr +      */
                                     /*      ant_centr_corr  meters           */
        double          range_rate; /* Range range between GRACE A and B (m/s)*/
        double          range_accl; /* Range acceleration between GRACE A & B */
                                    /* (m/s**2)                               */
        double          iono_corr;  /* Biased Ionospheric range correction    */
                                    /* between GRACE A&B for Ka freq. (m)     */
        double       lighttime_corr;/* light time range correction between    */
                                    /* GRACE A & B (m)                        */
        double       lighttime_rate;/* light time range rate correction       */
                                    /* between GRACE A & B (m/sec)            */
        double       lighttime_accl;/* light time range acceleration corr     */
                                    /* between GRACE A & B (m/sec^2)          */
        double      ant_centr_corr; /* Antenna phase center range             */
                                    /* correction (m)                         */
        double      ant_centr_rate; /* Antenna phase center range rate        */
                                    /* correction (m/sec)                     */
        double      ant_centr_accl; /* Antenna phase center range acceleration*/
                                    /* correction (m/sec^2)                   */
        unsigned short  K_A_SNR;    /* SNR K band for GRACE A   0.1 db-Hz     */
        unsigned short  Ka_A_SNR;   /* SNR Ka band for GRACE A  0.1 db-Hz     */
        unsigned short  K_B_SNR;    /* SNR K band for GRACE B   0.1 db-Hz     */
        unsigned short  Ka_B_SNR;   /* SNR Ka band for GRACE B  0.1 db-Hz     */

          /* To convert SNR to a 1 sec error in cycles of phase, let x be
             any of K*SNR numbers above:
                   y = 1 sec SNRvoltage = 10**(x/(10*20))
                   sigma_phase  = 1/(2*Pi*y) cycles                           */

                  
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = phase break                   */
                                    /* bit  1 = unreliable PCI data           */
                                    /*          for ant_centr_corr            */
                                    /* bit  2 = interpolated PCI data         */
                                    /*          for ant_centr_corr            */
                                    /* bit  3 = extrapolated clock correction */
                                    /*          > 5s from fit center          */
                                    /* bit  4 = extrapolated clock correction */
                                    /*          < 5s  from fit center         */
                                    /* bit  5 = data corrected for timetag    */
                                    /*          bias of either K or Ka phase  */
                                    /* bit  6 = filled data > 5s from fit     */
                                    /*          center                        */
                                    /* bit  7 = filled data < 5s  from fit    */
                                    /*          center                        */
        } KBR1B_t;
typedef struct PCI1A_t              /* Level 1A Phase Center to CG Data Format*/
        {
        long        gps_time;       /* GPS time, seconds past 12:00:00        */
                                    /* noon 01-Jan-2000                       */
        char        GRACE_id;       /* GRACE satellite id                     */
        double      ant_centr_corr; /* Antenna phase center range             */
                                    /* correction (m)                         */
        double      ant_centr_rate; /* Antenna phase center range rate        */
                                    /* correction (m/sec)                     */
        double      ant_centr_accl; /* Antenna phase center range acceleration*/
                                    /* correction (m/sec^2)                   */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = from 1 star camera only       */
                                    /* bit  1 = filled data                   */
                                    /* bit  2 = unreliable !!                 */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = Not Defined                   */
                                    /* bit  7 = Not Defined                   */
        } PCI1A_t;

typedef struct ACC1B_t              /* Level 1B Accelerometer Data Format     */
        {
        long   gps_time;            /* GPS time, seconds past 12:00:00        */
                                    /* noon 01-Jan-2000                       */
        char            GRACE_id;   /* GRACE satellite id                     */
        double          lin_accl_x; /* Linear acceleration along x-axis       */
                                    /* (m/s**2) (SRF frame)                   */
        double          lin_accl_y; /* Linear acceleration along y-axis       */
                                    /* (m/s**2) (SRF frame)                   */
        double          lin_accl_z; /* Linear acceleration along z-axis       */
                                    /* (m/s**2) (SRF frame)                   */
        double          ang_accl_x; /* Angular acceleration about x-axis      */
                                    /* (rad/s**2) (SRF frame)                 */
        double          ang_accl_y; /* Angular acceleration about y-axis      */
                                    /* (rad/s**2) (SRF frame)                 */
        double          ang_accl_z; /* Angular acceleration about z-axis      */
                                    /* (rad/s**2) (SRF frame)                 */
        double          acl_x_res ; /* Linear acceleration along x-axis       */
                                    /* residual with fit (m/s**2)             */
        double          acl_y_res ; /* Linear acceleration along y-axis       */
                                    /* residual with fit (m/s**2)             */
        double          acl_z_res ; /* Linear acceleration along z-axis       */
                                    /* residual with fit (m/s**2)             */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Vp out of nominal range       */
                                    /* bit  1 = Not Defined                   */
                                    /* bit  2 = if any linear ACC component   */
                                    /*     has fit residual > 10 microns/s^2  */
                                    /* bit  3 = extrapolated clock correction */
                                    /*          >5s but < 15s from fit center */
                                    /* bit  4 = extrapolated clock correction */
                                    /*          < 5s  from fit center         */
                                    /* bit  5 = filled data > 15s from fit    */
                                    /*          center                        */
                                    /* bit  6 = filled data > 5s but < 15s    */
                                    /*          from fit center               */
                                    /* bit  7 = filled data < 5s  from fit    */
                                    /*          center                        */
        } ACC1B_t;

typedef struct XXXVO_t              /* Vector Orientation Data Format         */
        {
        long            gps_time;   /* GPS time, seconds past 12:00:00        */
                                    /* noon 01-Jan-2000                       */
        char            GRACE_id;   /* GRACE satellite id                     */
        double          mag;        /* Magnitude of vector (m)                */
        double          cosx;       /* Direction cosine of vector             */
                                    /* with SRF x-axis                        */
        double          cosy;       /* Direction cosine of vector             */
                                    /* with SRF y-axis                        */
        double          cosz;       /* Direction cosine of vector             */
                                    /* with SRF z-axis                        */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = L1 phase center offset vector */
                                    /* bit  1 = L2 phase center offset vector */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = Not Defined                   */
                                    /* bit  7 = Not Defined                   */
        } XXXVO_t;

typedef struct IOA1B_t              /* Inertial Orientation of ACC data       */
                                    /* format                                 */
        {
        long            gps_time;   /* GPS time, seconds past 12:00:00        */
                                    /* noon 01-Jan-2000                       */
        char            GRACE_id;   /* GRACE satellite id                     */
        double          quatangle;  /* Cos mu/2 element of quaternion         */
        double          quaticoeff; /* I element of quaternion rotation axis  */
        double          quatjcoeff; /* J element of quaternion rotation axis  */
        double          quatkcoeff; /* K element of quaternion rotation axis  */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Not Defined                   */
                                    /* bit  1 = Not Defined                   */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = Not Defined                   */
                                    /* bit  7 = Not Defined                   */
        } IOA1B_t;

typedef struct OSCFQ_t              /* Ulta Stable Oscillator Stability Data  */
                                    /* structure                              */
        {
        long            gps_time;   /* GPS receiver time, seconds past        */
                                    /* 12:00:00 noon 01-Jan-2000              */
        char            GRACE_id;   /* GRACE satellite id                     */
        char            uso_id;     /* USO satellite id                       */
        double          uso_freq;   /* Frequency of USO (Hz)                  */
        double          K_freq;     /* K band frequency of KBR (Hz)           */
        double          Ka_freq;    /* Ka band frequency of KBR (Hz)          */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = 1 -> linear extrapolation not */
                                    /*               valid AFTER rcv_time     */
                                    /* bit  1 = 1 -> linear extrapolation not */
                                    /*               valid BEFORE rcv_time    */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = Not Defined                   */
                                    /* bit  7 = Not Defined                   */
        } OSCFQ_t;

typedef struct THR1X_t              /* Levels 1A and 1B Thrusters Data        */
                                    /* Structure                              */
        {
        long            time_intg;  /* Activation time, integer seconds past  */
                                    /* 12:00:00 noon 01-01-2000               */
        long            time_frac;  /* Activation time, microseconds part     */
        char            time_ref;   /* Time reference frame where             */
                                    /*    'R' = Receiver time                 */
                                    /*    'G' = GSP time                      */
        char            GRACE_id;   /* GRACE satellite id                     */
        unsigned long  thrust_count[MAXTHRSTRS];
                                    /* Count of number of work cycles that    */
                                    /* each thruster has been activiated      */
                                    /* integer will wrap after 4294967295     */
                                   /* see below for list of thrusters */
        unsigned long   on_time  [MAXTHRSTRS];
                                    /* Thruster on time for this time epoch   */
                                    /* (millisec)                             */
        unsigned long   accum_dur[MAXTHRSTRS];
                                    /* Accumulated thruster firing duration   */
                                    /* time (millisec)                        */
                                    /* integer will wrap after 4294967295     */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = 1 On time not calculated      */
                                    /* bit  1 = 1 Multiple unaccounted thrusts*/
                                    /*            prior to current record     */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = No OBDH->Receiver time mapping*/
                                    /* bit  7 = No Clock correction available */
/* This table was copied from /goa/local/grace/file_formats/SDSPSD_v3.0.pdf

 The table below is space delimited with fields:
Location (mm) Force (mN) No. ID XYZ XYZ Ctrl Ptg 
1 A11 -1450 -719 0 0 10 0 Y(-) -Y 
2 A12 -1450    0 -444 0 0 10 P(+) -Z
3 A13 -1450  719 0 0 -10 0 Y(+) +Y
4 A14 -1450    0 275 0 0 -10 P(-) +Z
5 A15     0 -970 300 0 10 0 R(-) -Y
6 A16 0 -467 -300 0 10 0 R(+) -Y 
7 A21 1450 719 0 0 -10 0 Y(-) +Y 
8 A22 1450 0 275 0 0 -10 P(+) +Z 
9 A23 1450 -719 0 0 10 0 Y(+) -Y 
10 A24 1450 0 -444 0 0 10 P(-) -Z 
11 A25 0 467 -300 0 -10 0 R(-) +Y 
12 A26 0 970 300 0 -10 0 R(+) +Y 
13 O11 -1561 -275 0 39.4 6.9 0 dV -X 
14 O21 -1561 275 0 39.4 -6.9 0 dV -X
*/

        } THR1X_t;

typedef struct MAG1X_t              /* Levels 1A and 1B Magnetometer and      */
                                    /* Magnettorquer Data Structure           */
        {
        long            time_intg;  /* Activation time, integer seconds past  */
                                    /* 12:00:00 noon 01-01-2000               */
        long            time_frac;  /* Activation time, microseconds part     */
        char            time_ref;   /* Time reference frame where             */
                                    /*    'R' = Receiver time                 */
                                    /*    'G' = GSP time                      */
        char            GRACE_id;   /* GRACE satellite id                     */
        float           MfvX_RAW;   /* x-axis component of measured earth     */
                                    /* magnetic field (microT)                */
                                    /* [Level 1A in Magnetometer frame]       */
                                    /* [Level 1B in SRF               ]       */
        float           MfvY_RAW;   /* y-axis component of measured earth     */
                                    /* magnetic field (microT)                */
                                    /* [Level 1A in Magnetometer frame]       */
                                    /* [Level 1B in SRF               ]       */
        float           MfvZ_RAW;   /* z-axis component of measured earth     */
                                    /* magnetic field (microT)                */
                                    /* [Level 1A in Magnetometer frame]       */
                                    /* [Level 1B in SRF               ]       */
        float           torque1A;   /* current of magnettorquer 1 A (positive */
                                    /* current x) (mA)                        */
        float           torque2A;   /* current of magnettorquer 2 A (positive */
                                    /* current y) (mA)                        */
        float           torque3A;   /* current of magnettorquer 3 A (positive */
                                    /* current z) (mA)                        */
        float           torque1B;   /* current of magnettorquer 1 B (negative */
                                    /* current x) (mA)                        */
        float           torque2B;   /* current of magnettorquer 2 B (negative */
                                    /* current y) (mA)                        */
        float           torque3B;   /* current of magnettorquer 3 B (negative */
                                    /* current z) (mA)                        */
        float           MF_BCalX;   /* mag field calibration factor for X     */
        float           MF_BCalY;   /* mag field calibration factor for Y     */
        float           MF_BCalZ;   /* mag field calibration factor for Z     */
        float           torque_cal; /* mag torquer calibration factor         */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = 0 -> GPS Receiver Time        */
                                    /*          1 -> Space Craft Elapsed Time */
                                    /* bit  1 = 0 -> Pulse Sync               */
                                    /*          1 -> no Pulse Sync            */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = No OBDH->Receiver time mapping*/
                                    /* bit  7 = No Clock correction available */
        } MAG1X_t;

typedef struct TNK1X_t              /* Levels 1A and 1B Cold Gas Tank Data    */
                                    /* Structure                              */
        {
        long            time_intg;  /* Measurement time, integer seconds past */ 
                                    /* 12:00:00 noon 01-01-2000               */
        long            time_frac;  /* Measurement time, microseconds part    */
        char            time_ref;   /* Time reference frame where             */
                                    /*    'R' = Receiver time                 */
                                    /*    'G' = GPS time                      */
        char            GRACE_id;   /* GRACE satellite id                     */
        char            tank_id;    /* Cold gas tank id                       */
                                    /* Tank 1 on -x axis                      */
                                    /* Tank 2 on +x axis                      */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = 0 -> GPS Receiver Time        */
                                    /*          1 -> Space Craft Elapsed Time */
                                    /* bit  1 = 0 -> Pulse Sync               */
                                    /*          1 -> no Pulse Sync            */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = No OBDH->Receiver time mapping*/
                                    /* bit  7 = No Clock correction available */
        char            prod_flag;  /* Product flag. Bitmask set to indicate  */
                                    /* presence of data type according to the */
                                    /* following                              */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Tank pressure                 */
                                    /* bit  1 = Regulator pressure            */
                                    /* bit  2 = Tank Skin temp (nominal)      */
                                    /* bit  3 = Tank Skin temp (redundant)    */
                                    /* bit  4 = Tank adaptor temp             */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = Not Defined                   */
                                    /* bit  7 = Not Defined                   */
        float           tank_pres;  /* Cold tank internal pressure in bar     */
        float           reg_pres;   /* Pressure at the reference point on the */
                                    /* pressure regulator housing in bar      */
        float           skin_temp;  /* Skin temperature of cold tank in deg C */
        float           skin_temp_r;/* Skin temperature of cold tank:redundant*/
                                    /* in deg C                               */
        float           adap_temp;  /* tank adaptor temperature in deg C      */ 
        } TNK1X_t;
     
typedef struct IHK1X_t              /* Level 1A and 1B IPU Housekeeping Data  */
                                    /* Structure                              */
        {
        long            time_intg;  /* Measurement time, integer seconds past */ 
                                    /* 12:00:00 noon 01-01-2000               */
        long            time_frac;  /* Measurement time, microseconds part    */
        char            time_ref;   /* Time reference frame where             */
                                    /*    'R' = Receiver time                 */
                                    /*    'G' = GPS time                      */
        char            GRACE_id;   /* GRACE satellite id                     */
                                    /*        - Not Defined                   */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Not Defined                   */
                                    /* bit  1 = Not Defined                   */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = No OBDH->Receiver time mapping*/
                                    /* bit  7 = No Clock correction available */

        char            sensortype; /* observation type                       */
                                    /* V = Voltage in Volts                   */
                                    /* T = Temperature in Deg C               */
                                    /* A = Current in Amperes                 */
        double          sensorvalue;/* value of observation                   */
        char            sensorname[MAXSENSORNAME]; 
                                    /* null terminated sensor name            */
        } IHK1X_t;

typedef struct MAS1X_t              /* Levels 1A and 1B Spacecraft Mass       */
        {
        long            time_intg;  /* Measurement time, integer seconds past */ 
                                    /* 12:00:00 noon 01-01-2000               */
        long            time_frac;  /* Measurement time, microseconds part    */
        char            time_ref;   /* Time reference frame where             */
                                    /*    'R' = Receiver time                 */
                                    /*    'G' = GPS time                      */
        char            GRACE_id;   /* GRACE satellite id                     */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Not Defined                   */
                                    /* bit  1 = Not Defined                   */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = No OBDH->Receiver time mapping*/
                                    /* bit  7 = No Clock correction available */
        char            prod_flag;  /* Product flag. Bitmask set to indicate  */
                                    /* presence of data type according to the */
                                    /* following                              */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = SC Mass from thruster usage   */
                                    /* bit  1 = SC Mass error bit 0           */
                                    /* bit  2 = SC Mass from tank observations*/
                                    /* bit  3 = SC Mass errro bit 2           */
                                    /* bit  4 = gas mass tank 1 (thr. usage)  */
                                    /* bit  5 = gas mass tank 2 (thr. usage)  */
                                    /* bit  6 = gas mass tank 1 (tank obs)    */
                                    /* bit  7 = gas mass tank 2 (tank obs)    */
        double         mass_thr;    /* Spacecraft Mass based on thruster usage*/
                                    /* in kg.                                 */
        double         mass_thr_err;/* Spacecraft Mass error from thr. usage  */
                                    /* in kg.                                 */
        double         mass_tnk;    /* Spacecraft Mass from tank observations */
                                    /* in kg.                                 */
        double         mass_tnk_err;/* Spacecraft Mass error from tank obs    */
                                    /* in kg.                                 */
        double        gas_mass_thr1;/* Mass of gas in tank 1 based on thruster*/
                                    /* usage in kg                            */
        double        gas_mass_thr2;/* Mass of gas in tank 2 based on thruster*/
                                    /* usage in kg                            */
        double        gas_mass_tnk1;/* Mass of gas in tank 1 based on tank    */
                                    /* observations in kg                     */
        double        gas_mass_tnk2;/* Mass of gas in tank 2 based on tank    */
                                    /* observations in kg                     */
        } MAS1X_t;

typedef struct ILG1X_t              /* Levels 1A and 1B Spacecraft Mass       */
        {
        long            rcv_time;   /* Receiver time, seconds past 12:00:00   */
                                    /* noon 01-Jan-2000                       */
        long            pkt_count;  /* counts packets with that rcv_time      */
                                    /* (1,2,3,...)                            */
        char            GRACE_id;   /* GRACE satellite id                     */
        char            logpacket[MAXLOGNAME]; 
                                    /* null terminated log packet string      */
        } ILG1X_t;
typedef struct TIM1X_t              /* Mapping of OBDH time to GPS time       */
        {
        long            obdh_time;  /* OBDH time, can be the following        */
                                    /* 1) GPS time seconds past 12:00:00  noon*/
                                    /*    01-Jan-2000                         */
                                    /* 2) Pseudo GPS offset due to sync       */
                                    /* 4) Space Craft elapsed time            */
        char            GRACE_id;   /* GRACE satellite id                     */
        long            TS_suppid;  /* OBDH Timestamp supplementary ID        */
                                    /* val    bits       description          */
                                    /*  0   = [00000000] = SCET               */
                                    /*  1   = [00000001] = GPS time           */
                                    /*  2   = [00000000] = SCET + Pulse Sync  */
                                    /*  3   = [00000011] = GPS time+Pulse Sync*/
                                    /*  7   = [00000111] = GPS time+Pulse Sync*/
                                    /*                     + plus IPU time pkt*/
                                    /*                       received         */
        long           gpstime_intg;/* Measurement time, integer seconds past */ 
                                    /* 12:00:00 noon 01-01-2000               */
        long           gpstime_frac;/* Measurement time, microseconds part    */
        long        first_icu_blknr;/* First ICU block number for Timestamp   */
                                    /* -1 indicates no ACC data available     */
        long        final_icu_blknr;/* Final ICU block number for Timestamp   */
                                    /* -1 indicates no ACC data available     */
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = Delta OBDH time != 1 sec      */
                                    /* bit  1 = Multiple icu blocks           */
                                    /* bit  2 = Sync process started          */
                                    /* bit  3 = GPS time mapping not defined  */
                                    /* bit  4 = Missed Antenna state packet   */
                                    /* bit  5 = gdel flag set in one or more  */
                                    /*          ICU data blocks               */
                                    /* bit  6 = Unable to compute GPS mapping */
                                    /* bit  6 = Not Defined                   */
        } TIM1X_t;

typedef struct CMT_command_t        /* command contents for Mass Trim proc.   */
        {
        char            GRACE_id;   /* GRACE satellite id  ("A" or "B")       */
        long            version;    /* version of this command contents       */
        char            MTE_id;     /* Mass Trim ensembly id (1 or 2)         */
                                    /* null terminated log packet string      */
        double          x_cg;       /* x-coordinate of CG estimate (in m)     */
        double          y_cg;       /* y-coordinate of CG estimate (in m)     */
        double          z_cg;       /* z-coordinate of CG estimate (in m)     */
        double          abs_dist_x; /* absolute distance travelled in x (in m)*/
        double          abs_dist_y; /* absolute distance travelled in y (in m)*/
        double          abs_dist_z; /* absolute distance travelled in z (in m)*/
        double          Mass;       /* Satellite mass estimate (kg)           */
        double          Mass_time;  /* Time tag for sat. mass estimate(gpssec)*/
        char            prd_name[HEADERMAXCHAR]; /*input filename             */
        char            prd_ttag[HEADERMAXCHAR]; /*input filename create ttag */
        } CMT_command_t;
typedef struct SCA2K_command_t      /* command contents for SCA2K quaternions */
        {
        char            GRACE_id;   /* GRACE satellite id  ("A" or "B")       */
        long            version;    /* version of this command contents       */
        double          q0_sca1;    /* q0 element of SCA2K quaternion for SCA1*/
        double          q1_sca1;    /* q1 element of SCA2K quaternion for SCA1*/
        double          q2_sca1;    /* q2 element of SCA2K quaternion for SCA1*/
        double          q3_sca1;    /* q3 element of SCA2K quaternion for SCA1*/
        double          q0_sca2;    /* q0 element of SCA2K quaternion for SCA2*/
        double          q1_sca2;    /* q1 element of SCA2K quaternion for SCA2*/
        double          q2_sca2;    /* q2 element of SCA2K quaternion for SCA2*/
        double          q3_sca2;    /* q3 element of SCA2K quaternion for SCA2*/
                                    /* q0 is the quaternion scalar            */
        char            prd_name[HEADERMAXCHAR]; /*input filename             */
        char            prd_ttag[HEADERMAXCHAR]; /*input filename create ttag */
        } SCA2K_command_t;
typedef struct HRT1X_t              /* Levels 1A and 1B High Resolution       */
                                    /* Temperature measurements               */
        {
        long  time_intg;            /* Measurement time, integer seconds past */ 
                                    /* 12:00:00 noon 01-01-2000               */
        long  time_frac;            /* Measurement time, microseconds part    */
        char  time_ref;             /* Time reference frame where             */
                                    /*    'R' = Receiver time                 */
                                    /*    'G' = GPS time                      */
        char  GRACE_id;             /* GRACE satellite id                     */
        float TEMP_MEP_neg_y ;      /* I/F Support Structure to MEP -y,  deg C*/
        float TEMP_MEP_pos_y ;      /* I/F Support Structure to MEP +y,  deg C*/
        float TEMP_MEPm ;           /* I/F Support Structure to MEP mid, deg C*/
        float TEMP_ICU ;            /* ICU Temperature Reference Point   deg C*/
        float TEMP_ICU_red ;        /* ICU Temperature Reference Point,       */
                                    /*     Redundant                     deg C*/
        float TEMP_ACC_neg_z ;      /* ACC thermal Cage -z               deg C*/
        float TEMP_ACC_pos_z ;      /* ACC thermal Cage +z               deg C*/
        float TEMP_CFRP_pos_x ;     /* CFRP Frame at +x I/F to Baseplate deg C*/
        float TEMP_CFRP_pos_x_red ; /* CFRP Frame at +x I/F to Baseplate      */
                                    /*      Redundant                    deg C*/
        float TEMP_CFRP_neg_x ;     /* CFRP Frame at -x I/F to Baseplate deg C*/
        float TEMP_CFRP_neg_x_red ; /* CFRP Frame at -x I/F to Baseplate      */
                                    /*      Redundant                    deg C*/
        float TEMP_CFRP_neg_y ;     /* CFRP Frame at -y I/F to Baseplate deg C*/
        float TEMP_CFRP_neg_y_red ; /* CFRP Frame at -y I/F to Baseplate      */
                                    /*      Redundant                    deg C*/
        float TEMP_ACCSen ;         /* Harness to ACC Sensor             deg C*/
        float TEMP_ICU_spec ;       /* ICU special                       deg C*/
        float TEMP_MWA_neg_y ;      /* MWA -y Temp. Ref. Point, 2 out 3  deg C*/
        float TEMP_MWA_neg_yoff ;   /* MWA -y Temp. Ref. Point, 2 out 3  deg C*/
                                    /* nominally off                          */
        float TEMP_MWA_pos_y ;      /* MWA +y Temp. Ref. Point, 2 out 3  deg C*/
        float TEMP_MWA_pos_yoff ;   /* MWA +y Temp. Ref. Point, 2 out 3  deg C*/
                                    /* nominally off                          */
        float TEMP_Horn_pos_x ;     /* Horn Aperture +x                  deg C*/
        float TEMP_Horn_pos_x_red ; /* Horn Aperture +x                       */
                                    /*      Redundant                    deg C*/
        float TEMP_HornPl ;         /* Horn / platform I/F               deg C*/
        float TEMP_HornPl_red ;     /* Horn / platform I/F                    */
                                    /*        Redundant                  deg C*/
        float TEMP_HMWA_neg_y ;     /* Harnass to MWA electronics -y     deg C*/
        float TEMP_HMWA_pos_y ;     /* Harnass to MWA electronics +y     deg C*/
        float TEMP_RFSamp ;         /* RF-Sampling Unit                  deg C*/
        float TEMP_USO_neg_y ;      /* USO Temp. Ref. Point, -y          deg C*/
        float TEMP_USO_neg_y_red ;  /* USO Temp. Ref. Point, -y,redun    deg C*/
        float TEMP_USO_pos_y ;      /* USO Temp. Ref. Point, +y          deg C*/
        float TEMP_USO_pos_y_red ;  /* USO Temp. Ref. Point, +y,redun    deg C*/
        unsigned char   qualflg;    /* data quality flag                      */
                                    /* LSB    = bit 0                         */
                                    /* bit  0 = 0 -> GPS Receiver Time        */
                                    /*          1 -> Space Craft Elapsed Time */
                                    /* bit  1 = 0 -> Pulse Sync               */
                                    /*          1 -> no Pulse Sync            */
                                    /* bit  2 = Not Defined                   */
                                    /* bit  3 = Not Defined                   */
                                    /* bit  4 = Not Defined                   */
                                    /* bit  5 = Not Defined                   */
                                    /* bit  6 = No OBDH->Receiver time mapping*/
                                    /* bit  7 = No Clock correction available */
        } HRT1X_t;

typedef GFD1X_t GPS1A_t;
typedef GFD1X_t GPS1B_t;
typedef GFD1X_t KBR1A_t;
typedef ACC1A_t AHK1X_t;
typedef CLK1B_t CLK1A_t;
typedef ILG1X_t ILG1A_t;
typedef IHK1X_t IHK1A_t;
typedef IHK1X_t IHK1B_t;
typedef MAS1X_t MAS1A_t;
typedef MAS1X_t MAS1B_t;
typedef TIM1X_t TIM1A_t;
typedef TIM1X_t TIM1B_t;
typedef HRT1X_t HRT1A_t;
typedef HRT1X_t HRT1B_t;


#undef _mk_extern_

#endif	/* _GRACEiolib_h_ */
