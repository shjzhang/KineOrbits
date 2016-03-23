/* @(#) GRACEio_prototypes.h      1.24 11/28/05 */

#ifndef _GRACEio_prototypes_h_
#define _GRACEio_prototypes_h_

#include "GRACEdefs.h"

/*----------------------------------------------------------------------------->
/ Function Prototypes 
<-----------------------------------------------------------------------------*/

long DecodeTMFileName(
         char *TMFilename ,/*Standard Telemetry Filename                      */ 
         char *Satellite,  /*Satellite indicator (A = GRACE A and B = GRACE B"*/
         char *Version,    /*Version string of file for Date                  */
         char *DateString, /*Timestamp in yyyy-mm-dd hh:mm:ss                 */
         long *DateT2000,  /*Timestamp in seconds past 01/01/2000 12:00:00    */
         char *Station,    /*Downlink Station Name (eg NZ = Neustrelitz)      */
         char *ProcCenter, /*Processing center Name (eg RDC)                  */
         char *DataStream, /*Satellite Data Stream (eg RT)                    */
         char *Datatype    /*Date type (eg HK SC)                             */
         );

/*----------------------------------------------------------------------------*/
void ConstructFileName(
         char *Satellite, /* Satellite indicator (A = GRACE A and B = GRACE B */
         double Time,     /* time for filename time tag (sec past 2000)       */
         long VersionNumber, /* Version number of data product                */
         char *FileTypeName, /* Standard file type name (aka filekey,eg ACC1B)*/
         char *Filename,  /* output filename                                  */
         char *ext        /* file extension                                   */
         );
/*----------------------------------------------------------------------------*/
void ConstructFileNameVersion(
         char *Satellite, /* Satellite indicator (A = GRACE A and B = GRACE B */
         double Time,     /* time for filename time tag (sec past 2000)       */
         char *Version,   /* Version string                                   */
         char *FileTypeName, /* Standard file type name (aka filekey,eg ACC1B)*/
         char *Filename,  /* output filename                                  */
         char *ext        /* file extension                                   */
         );

void LinkTime 
               (
                 char *linktimelabel     /* pointer to link time label        */    
               );

/*----------------------------------------------------------------------------*/
void GetBaseFilename 
               (char *filename,          /* filename (including path)         */
                char *BaseFilename       /* filename without path             */
               );

/*----------------------------------------------------------------------------*/
long GetFileTypeName
               (
                long SectorPointer,       /* file type pointer                */
                char *filetype            /* Pointer to Filetype txt          */
               );

/*----------------------------------------------------------------------------*/
long GetAcc1aProdName
               (
                long SectorPointer,       /* Acc1a prodcuct pointer           */
                char *filetype            /* Pointer to Acc1a product txt     */
               );

/*----------------------------------------------------------------------------*/

long GetHeaderLabelName
               (
                long HeaderRecPointer,    /* header record label pointer      */
                char *filetype            /* Pointer to Filetype txt          */
               );

/*----------------------------------------------------------------------------*/

long GetAcc1aProd
               (
                char *filetype            /* Pointer to Acc1a product txt     */
               );

/*----------------------------------------------------------------------------*/

long GetFileType
               (
                char *filetype            /* Pointer to Filetype txt          */
               );

/*----------------------------------------------------------------------------*/

long GetHeaderLabel
               (
                char *filetype            /* Pointer to Filetype txt          */
               );

/*----------------------------------------------------------------------------*/
void InitializeHeaders();
/*----------------------------------------------------------------------------*/
void InitializeHeaderStruct(FileHeader_t *header);
/*----------------------------------------------------------------------------*/


double GetUTC2000TimeTag
               (
               );
void GetUTCTimeTag
               (
                char            *time_tag /* Pointer to array with UTC timetag*/
               );
/*----------------------------------------------------------------------------*/
boolean WriteFileHeader 
	       (
/* input */
		FILE            *dst,     /* Pointer to data file             */
		FileHeader_t    *header   /* Pointer to header struct         */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/
/*----------------------------------------------------------------------------*/
boolean ReadFileHeader 
	       (
/* input */
		FILE            *src,     /* Pointer to data file             */
/* output */
		FileHeader_t    *header   /* Pointer to header struct         */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/
/*----------------------------------------------------------------------------*/
boolean PutFileHeader 
	       (
/* input */
                FILE *dst,                /* pointer to destination file      */
                FileHeader_t *header,     /* pointer to header struct         */
                long FileType,            /* file type integer pointer        */
                long FormatType,          /* format type binary/ascii         */
                long NHeaderRecord,       /* number of header records         */
                long init_flag,           /* initialization flag 0,1          */
                char *ProdAgency,         /* producer agency label            */
                char *ProdInstitution,    /* producer institution label       */
                char *SoftwareVersion,    /* software version label           */
                char *SoftwareLinkTime,   /* software version label           */
                char *Documentation,      /* documentation label              */
                char *SatName,            /* satellite name label             */
                char *SensorName,         /* sensor name label                */
                char *TimeEpoch,          /* Time epoch label                 */
                double TfirstObs,         /* Time first observation           */
                double TlastObs,          /* Time last observation            */
                long   Nobs,              /* number of observations           */
                char *TimeTag,            /* time tag label FIRST,FINAL       */
                char *FileName,           /* Filename                         */
                char *ProcessLevel        /* Processing Level (1A or 1B)      */
               );
/* return:	1	normal return
                0	End Of File reached 
*/
/*----------------------------------------------------------------------------*/
boolean ModifyFileHeader 
	       (
/* input */
		FileHeader_t    *header,   /* Pointer to header struct        */
                long            RecPointer,/* Pointer to header record        */
                char            *ModString /* Pointer to string to be inserted*/
                                           /* in header                       */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/


/*----------------------------------------------------------------------------*/
boolean ReadGFD1XFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to GPS Flight Data Format*/
                                          /* file                             */
/* output */
		GFD1X_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (GFD1X_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteGFD1XFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to GPS Flight Data Format*/
                                          /* file                             */
		GFD1X_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (GFD1X_t)                 */
	       );

/*----------------------------------------------------------------------------*/
void PrintGFD1XFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		GFD1X_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (GFD1X_t)                 */
	       );


/*----------------------------------------------------------------------------*/
boolean WrAsciiGFD1XFRecord
               (
/* input */
               FILE             *src,     /* Pointer to flight data file      */
               GFD1X_t          *record   /* Pointer to flight data record    */
	       );


/*----------------------------------------------------------------------------*/
boolean ReadGPS1AFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to GPS Flight Data Format*/
                                          /* file                             */
/* output */
		GPS1A_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (GPS1A_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteGPS1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to GPS Flight Data Format*/
                                          /* file                             */
		GPS1A_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (GPS1A_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean PrintGPS1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		GPS1A_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (GPS1A_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean ReadGPS1BFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to GPS Flight Data Format*/
                                          /* file                             */
/* output */
		GPS1B_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (GPS1B_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteGPS1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to GPS Flight Data Format*/
                                          /* file                             */
		GPS1B_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (GPS1B_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean PrintGPS1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		GPS1B_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (GPS1B_t)                 */
	       );


/*----------------------------------------------------------------------------*/
boolean WrAsciiGPS1BFRecord
               (
/* input */
               FILE             *src,     /* Pointer to flight data file      */
               GPS1B_t          *record   /* Pointer to flight data record    */
	       );


/*----------------------------------------------------------------------------*/
boolean ReadKBR1AFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to GPS Flight Data Format*/
                                          /* file                             */
/* output */
		KBR1A_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (KBR1A_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteKBR1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to GPS Flight Data Format*/
                                          /* file                             */
		KBR1A_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (KBR1A_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean PrintKBR1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		KBR1A_t       	*record   /* Pointer to GPS Flight Data Format*/
                                          /* struct (KBR1A_t)                 */
	       );


/*----------------------------------------------------------------------------*/
boolean ReadGNV1AFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to GPS Navigation 1A Data*/
                                          /* Format file                      */
/* output */
		GNV1A_t       	*record   /* Pointer to GPS Navigation 1A Data*/
                                          /* Format struct (GNV1A_t)          */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteGNV1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to GPS Navigation 1A Data*/
                                          /* Format file                      */
		GNV1A_t       	*record   /* Pointer to GPS Navigation 1A Data*/
                                          /* Format struct (GNV1A_t)          */
	       );

/*----------------------------------------------------------------------------*/
void PrintGNV1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		GNV1A_t       	*record   /* Pointer to GPS Navigation 1A Data*/
                                          /* Format struct (GNV1A_t)          */
	       );

/*----------------------------------------------------------------------------*/
boolean WrAsciiGNV1AFRecord
               (
/* input */
               FILE             *src,     /* Pointer to GPS Navigation 1A file*/
               GNV1A_t          *record   /* Pointer to GPS Nav. 1B record    */
	       );


/*----------------------------------------------------------------------------*/
boolean ReadSCA1AFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to SCA 1A Data Format    */
                                          /* file                             */
/* output */
		SCA1A_t       	*record   /* Pointer to SCA 1A Data Format    */
                                          /* struct (SCA1A_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteSCA1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to SCA 1A Data Format    */
                                          /* file                             */
		SCA1A_t       	*record   /* Pointer to SCA 1A Data Format    */
                                          /* struct (SCA1A_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean WrAsciiSCA1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to SCA 1A Data Format    */
                                          /* file                             */
		SCA1A_t       	*record   /* Pointer to SCA 1A Data Format    */
                                          /* struct (SCA1A_t)                 */
	       );

/*----------------------------------------------------------------------------*/
void PrintSCA1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		SCA1A_t       	*record   /* Pointer to SCA 1A Data Format    */
                                          /* struct (SCA1A_t)                 */
	       );



/*----------------------------------------------------------------------------*/
boolean ReadACC1AFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to ACC 1A Data Format    */
                                          /* file                             */
/* output */
		ACC1A_t       	*record   /* Pointer to ACC 1A Data Format    */
                                          /* struct (ACC1A_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteACC1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to ACC 1A Data Format    */
                                          /* file                             */
		ACC1A_t       	*record   /* Pointer to ACC 1A Data Format    */
                                          /* struct (ACC1A_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean WriteAHK1XFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to AHK 1X Data Format    */
                                          /* file                             */
		AHK1X_t       	*record   /* Pointer to AHK 1X Data Format    */
                                          /* struct (AHK1X_t)                 */
	       );

/*----------------------------------------------------------------------------*/
void PrintACC1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		ACC1A_t       	*record   /* Pointer to ACC 1A Data Format    */
                                          /* struct (ACC1A_t)                 */
	       );



/*----------------------------------------------------------------------------*/
boolean ReadGNV1BFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to GPS Navigation 1B Data*/
                                          /* Format file                      */
/* output */
		GNV1B_t       	*record   /* Pointer to GPS Navigation 1B Data*/
                                          /* Format struct (GNV1B_t)          */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteGNV1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to GPS Navigation 1B Data*/
                                          /* Format file                      */
		GNV1B_t       	*record   /* Pointer to GPS Navigation 1B Data*/
                                          /* Format struct (GNV1B_t)          */
	       );

/*----------------------------------------------------------------------------*/
void PrintGNV1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		GNV1B_t       	*record   /* Pointer to GPS Navigation 1B Data*/
                                          /* Format struct (GNV1B_t)          */
	       );

/*----------------------------------------------------------------------------*/
boolean WrAsciiGNV1BFRecord
               (
/* input */
               FILE             *src,     /* Pointer to GPS Navigation 1B file*/
               GNV1B_t          *record   /* Pointer to GPS Nav. 1B record    */
	       );


/*----------------------------------------------------------------------------*/
boolean ReadTIM1XFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to TIM1X Data Format    */
                                          /* file                             */
/* output */
		TIM1X_t       	*record   /* Pointer to TIM1X Data Format    */
                                          /* struct (TIM1X_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/
/*----------------------------------------------------------------------------*/
boolean ReadPCI1AFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to PCI 1A Data Format    */
                                          /* file                             */
/* output */
		PCI1A_t       	*record   /* Pointer to PCI 1A Data Format    */
                                          /* struct (PCI1A_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WritePCI1AFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to PCI 1A Data Format    */
                                          /* file                             */
		PCI1A_t       	*record   /* Pointer to PCI 1A Data Format    */
                                          /* struct (PCI1A_t)                 */
	       );
/*----------------------------------------------------------------------------*/
boolean ReadSCA1BFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to SCA 1B Data Format    */
                                          /* file                             */
/* output */
		SCA1B_t       	*record   /* Pointer to SCA 1B Data Format    */
                                          /* struct (SCA1B_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteSCA1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to SCA 1B Data Format    */
                                          /* file                             */
		SCA1B_t       	*record   /* Pointer to SCA 1B Data Format    */
                                          /* struct (SCA1B_t)                 */
	       );
/*----------------------------------------------------------------------------*/
boolean WriteTIM1XFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to TIM1X  Data Format    */
                                          /* file                             */
		TIM1X_t       	*record   /* Pointer to TIM1X  Data Format    */
                                          /* struct (TIM1X_t)                 */
	       );

/*----------------------------------------------------------------------------*/
void PrintSCA1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		SCA1B_t       	*record   /* Pointer to SCA 1B Data Format    */
                                          /* struct (SCA1B_t)                 */
	       );
/*----------------------------------------------------------------------------*/
void PrintTIM1XFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		TIM1X_t       	*record   /* Pointer to TIM1X Data Format     */
                                          /* struct (TIM1X_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean WrAsciiPCI1AFRecord
               (
/* input */
               FILE             *src,     /* Pointer to star camera file      */
               PCI1A_t          *record   /* Pointer to star camera record    */
	       );
/*----------------------------------------------------------------------------*/
boolean WrAsciiSCA1BFRecord
               (
/* input */
               FILE             *src,     /* Pointer to star camera file      */
               SCA1B_t          *record   /* Pointer to star camera record    */
	       );


/*----------------------------------------------------------------------------*/
boolean WrAsciiTIM1XFRecord
               (
/* input */
               FILE             *src,     /* Pointer to OBDH mapping file     */
               TIM1X_t          *record   /* Pointer to OBDH mapping record   */
	       );

/*----------------------------------------------------------------------------*/
boolean ReadKBR1BFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to KBR 1B Data Format    */
                                          /* file                             */
/* output */
		KBR1B_t       	*record   /* Pointer to KBR 1B Data Format    */
                                          /* struct (KBR1B_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteKBR1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to KBR 1B Data Format    */
                                          /* file                             */
		KBR1B_t       	*record   /* Pointer to KBR 1B Data Format    */
                                          /* struct (KBR1B_t)                 */
	       );

/*----------------------------------------------------------------------------*/
void PrintKBR1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		KBR1B_t       	*record   /* Pointer to KBR 1B Data Format    */
                                          /* struct (KBR1B_t)                 */
	       );
/*----------------------------------------------------------------------------*/
boolean WrAsciiKBR1BFRecord
               (
/* input */
               FILE             *src,     /* Pointer to KBR Level 1B file     */
               KBR1B_t          *record   /* Pointer to KBR Level 1B x record */
	       );


/*----------------------------------------------------------------------------*/
boolean ReadACC1BFRecord 
	       (
/* input */
		FILE		*src,     /* Pointer to ACC 1B Data Format    */
                                          /* file                             */
/* output */
		ACC1B_t       	*record   /* Pointer to ACC 1B Data Format    */
                                          /* struct (ACC1B_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteACC1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to ACC 1B Data Format    */
                                          /* file                             */
		ACC1B_t       	*record   /* Pointer to ACC 1B Data Format    */
                                          /* struct (ACC1B_t)                 */
	       );

/*----------------------------------------------------------------------------*/
void PrintACC1BFRecord 
	       (
/* input */
		FILE		*dst,     /* Pointer to output file           */
		ACC1B_t       	*record   /* Pointer to ACC 1B Data Format    */
                                          /* struct (ACC1B_t)                 */
	       );


/*----------------------------------------------------------------------------*/
boolean WrAsciiACC1BFRecord
               (
/* input */
               FILE             *src,     /* Pointer to Accelerometer file    */
               ACC1B_t          *record   /* Pointer to Accelerometer record  */
	       );


/*----------------------------------------------------------------------------*/
boolean ReadXXXVOFRecord 
	       (
/* input */
		FILE	          *src,   /* Pointer to XXXVOB Data Format    */
                                          /* file                             */
/* output */
		XXXVO_t           *record /* Pointer to XXXVOB Data Format    */
                                          /* struct (XXXVO_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteXXXVOFRecord 
	       (
/* input */
		FILE		  *dst,   /* Pointer to XXXVOB Data Format    */
                                          /* file                             */
		XXXVO_t           *record /* Pointer to XXXVOB Data Format    */
                                          /* struct (XXXVO_t)                 */
	       );

/*----------------------------------------------------------------------------*/
void PrintXXXVOFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to output file          */
		XXXVO_t          *record  /* Pointer to XXXVOB Data Format   */
                                          /* struct (XXXVO_t)                */
	       );

/*----------------------------------------------------------------------------*/
boolean WrAsciiXXXVOFRecord
               (
/* input */
               FILE             *src,     /* Pointer to vector orientation    */
                                          /* file                             */
               XXXVO_t          *record   /* Pointer to vector orientation    */
                                          /* record                           */
	       );

/*----------------------------------------------------------------------------*/

boolean ReadIOA1BFRecord 
	       (
/* input */
		FILE	         *src,    /* Pointer to IOA 1B Data Format    */
                                          /* file                             */
/* output */
		IOA1B_t          *record  /* Pointer to IOA 1B Data Format    */
                                          /* struct (IOA1B_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
boolean WriteIOA1BFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to IOA 1B Data Format    */
                                          /* file                             */
		IOA1B_t          *record  /* Pointer to IOA 1B Data Format    */
                                          /* struct (IOA1B_t)                 */
	       );

/*----------------------------------------------------------------------------*/
void PrintIOA1BFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to output file           */
		IOA1B_t          *record  /* Pointer to IOA 1B Data Format    */
                                          /* struct (IOA1B_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean WrAsciiIOA1BFRecord
               (
/* input */
               FILE             *src,     /* Pointer to accelerometer inertial*/
                                          /* orientation file                 */
               IOA1B_t          *record   /* Pointer to accelerometer inertial*/
                                          /* orientation record               */
	       );

/*----------------------------------------------------------------------------*/
boolean ReadOSCFQFRecord 
	       (
/* input */
		FILE	         *src,    /* Pointer to OSCFQ Data Format     */
                                          /* file                             */
/* output */
		OSCFQ_t          *record  /* Pointer to OSCFQ Data Format     */
                                          /* struct (OSCFQ_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/

/*----------------------------------------------------------------------------*/
void PrintOSCFQFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to output file           */
		OSCFQ_t          *record  /* Pointer to OSCFQ Data Format     */
                                          /* struct (OSCFQ_t)                 */
	       );



/*----------------------------------------------------------------------------*/
boolean WrAsciiOSCFQFRecord
               (
/* input */
               FILE             *src,     /* Pointer to USO file              */
               OSCFQ_t          *record   /* Pointer to USO record            */
	       );

/*----------------------------------------------------------------------------*/
boolean WriteOSCFQFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to OSCFQ  Data Format    */
                                          /* file                             */
		OSCFQ_t          *record  /* Pointer to OSCFQ  Data Format    */
                                          /* struct (OSCFQ_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean ReadMAG1XFRecord 
	       (
/* input */
		FILE		 *src,    /* Pointer to MAG Data Format      */
                                          /* file                            */
		MAG1X_t          *record  /* Pointer to MAG Data Format      */
                                          /* struct (MAG1X_t)                */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/


/*----------------------------------------------------------------------------*/
void PrintMAG1XFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to output file          */
		MAG1X_t          *record  /* Pointer to MAG Data Format      */
                                          /* struct (MAG1X_t)                */
	       );



/*----------------------------------------------------------------------------*/
boolean WrAsciiMAG1XFRecord
               (
/* input */
               FILE             *src,     /* Pointer to MAG file             */
               MAG1X_t          *record   /* Pointer to MAG record           */
	       );


/*----------------------------------------------------------------------------*/
boolean WriteMAG1XFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to MAG Data Format       */
                                          /* file                             */
		MAG1X_t          *record  /* Pointer to MAG Data Format       */
                                          /* struct (MAG1X_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean ReadIHK1XFRecord 
	       (
/* input */
		FILE		 *src,    /* Pointer to IPU HK   Data Format  */
                                          /* file                             */
		IHK1X_t          *record  /* Pointer to IPU HK   Data Format  */
                                          /* struct (IHK1X_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/


/*----------------------------------------------------------------------------*/
void PrintIHK1XFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to output file           */
		IHK1X_t          *record  /* Pointer to IPU HK   Data Format  */
                                          /* struct (IHK1X_t)                 */
	       );



/*----------------------------------------------------------------------------*/
boolean WrAsciiIHK1XFRecord
               (
/* input */
               FILE             *src,     /* Pointer to IPU HK   file         */
               IHK1X_t          *record   /* Pointer to IPU HK   record       */
	       );

/*----------------------------------------------------------------------------*/
boolean WriteIHK1XFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to IPU HK   Data Format  */
                                          /* file                             */
		IHK1X_t          *record  /* Pointer to IPU HK   Data Format  */
                                          /* struct (THR1X_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean ReadTHR1XFRecord 
	       (
/* input */
		FILE		 *src,    /* Pointer to Thruster Data Format  */
                                          /* file                             */
		THR1X_t          *record  /* Pointer to Thruster Data Format  */
                                          /* struct (THR1X_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/


/*----------------------------------------------------------------------------*/
void PrintTHR1XFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to output file           */
		THR1X_t          *record  /* Pointer to Thruster Data Format  */
                                          /* struct (THR1X_t)                 */
	       );



/*----------------------------------------------------------------------------*/
boolean WrAsciiTHR1XFRecord
               (
/* input */
               FILE             *src,     /* Pointer to Thruster file         */
               THR1X_t          *record   /* Pointer to Thruster record       */
	       );

/*----------------------------------------------------------------------------*/
boolean WriteTHR1XFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to Thruster Data Format  */
                                          /* file                             */
		THR1X_t          *record  /* Pointer to Thruster Data Format  */
                                          /* struct (THR1X_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean ReadTNK1XFRecord      
               (
/* input */
                FILE             *src,    /* Pointer to Tank Data Format      */
                                          /* file                             */
                TNK1X_t          *record  /* Pointer to Tank Data Format      */
                                          /* struct (TNK1X_t)                 */
               );
/* return:      1       normal return
                0       End Of File reached
*/             
                 
               
/*----------------------------------------------------------------------------*/
void PrintTNK1XFRecord
               (    
/* input */    
                FILE             *dst,    /* Pointer to output file           */
                TNK1X_t          *record  /* Pointer to Tank Data Format      */
                                          /* struct (TNK1X_t)                 */
               );

               
                
/*----------------------------------------------------------------------------*/
boolean WrAsciiTNK1XFRecord
               (
/* input */
               FILE             *src,     /* Pointer to Tank file             */
               TNK1X_t          *record   /* Pointer to Tank Data record      */
               );
                
/*----------------------------------------------------------------------------*/
boolean WriteTNK1XFRecord
               (
/* input */
                FILE             *dst,    /* Pointer to Tank Data Format      */
                                          /* file                             */
                TNK1X_t          *record  /* Pointer to Tank Data Format      */
                                          /* struct (TNK1X_t)                 */
               );


/*----------------------------------------------------------------------------*/
boolean ReadMAS1XFRecord 
	       (
/* input */
		FILE		 *src,    /* Pointer to Mass Data Format      */
                                          /* file                             */
		MAS1X_t          *record  /* Pointer to Mass Data Format      */
                                          /* struct (MAS1X_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/


/*----------------------------------------------------------------------------*/
void PrintMAS1XFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to output file           */
		MAS1X_t          *record  /* Pointer to Mass Data Format      */
                                          /* struct (MAS1X_t)                 */
	       );



/*----------------------------------------------------------------------------*/
boolean WrAsciiMAS1XFRecord
               (
/* input */
               FILE             *src,     /* Pointer to Mass file             */
               MAS1X_t          *record   /* Pointer to Mass Data record      */
	       );

/*----------------------------------------------------------------------------*/
boolean WriteMAS1XFRecord 
	       (
/* input */
		FILE		 *dst,    /* Pointer to Mass Data Format      */
                                          /* file                             */
		MAS1X_t          *record  /* Pointer to Mass Data Format      */
                                          /* struct (MAS1X_t)                 */
	       );

/*----------------------------------------------------------------------------*/
boolean ReadHRT1XFRecord(
/* input */
               FILE *src,                /* Pointer to HRT file               */ 
               HRT1X_t *record           /* Pointer to HRT struct file        */
               );
/* return:	1	normal return
                0	End Of File reached 
*/

boolean WrAsciiHRT1XFRecord(FILE *dst, HRT1X_t *record);
boolean WriteHRT1XFRecord(FILE *dst, HRT1X_t *record);
void  PrintHRT1XFRecord(FILE *dst, HRT1X_t *record);

/*----------------------------------------------------------------------------*/
boolean ReadAHK1AFRecord  
/* Not yet implemented */
	       (
/* input */
		FILE		 *src,    /* Pointer to Accel. HK Data Format */
                                          /* file                             */
		AHK1X_t          *record  /* Pointer to Accel. HK Data Format */
                                          /* struct (AHK1A_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/


/*----------------------------------------------------------------------------*/
void PrintAHK1AFRecord 
/* Not yet implemented */
	       (
/* input */
		FILE		 *dst,    /* Pointer to output file           */
		AHK1X_t          *record  /* Pointer to Accel. HK data Format */
                                          /* struct (AHK1A_t)                 */
	       );


/*----------------------------------------------------------------------------*/
boolean WriteAHK1AFRecord 
/* Not yet implemented */
	       (
/* input */
		FILE		 *dst,    /* Pointer to Accel. HK Data Format */
                                          /* file                             */
		AHK1X_t          *record  /* Pointer to Accel. HK Data Format */
                                          /* struct (AHK1A_t)                 */
	       );
/*----------------------------------------------------------------------------*/
boolean ReadIHK1AFRecord 
/* Not yet implemented */
	       (
/* input */
		FILE		 *src,    /* Pointer to IPU HK Data Format    */
                                          /* file                             */
		IHK1X_t          *record  /* Pointer to IPU HK Data Format    */
                                          /* struct (IHK1A_t)                 */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/


/*----------------------------------------------------------------------------*/
void PrintIHK1AFRecord 
/* Not yet implemented */
	       (
/* input */
		FILE		 *dst,    /* Pointer to output file           */
		IHK1X_t          *record  /* Pointer to IPU HK data Format    */
                                          /* struct (IHK1A_t)                 */
	       );


/*----------------------------------------------------------------------------*/
boolean WriteIHK1AFRecord 
/* Not yet implemented */
	       (
/* input */
		FILE		 *dst,    /* Pointer to IPU HK Data Format    */
                                          /* file                             */
		IHK1X_t          *record  /* Pointer to IPU HK Data Format    */
                                          /* struct (IHK1A_t)                 */
	       );


/*----------------------------------------------------------------------------*/
long VerifyFilename (

/* input */
                char *filename,         /* input filename to be verified      */
                char *BaseFilename,     /* input filename with path removed   */
                char *FileKey,          /* retrieved filekey (valid file only)*/
                int *year,              /* retrieved year (valid file only)   */
                int *month,             /* retrieved month (valid file only)  */
                int *day,               /* retrieved day  (valid file only)   */
                char *SatId,            /* retrieved satid (valid file only)  */
                long *VersionNumber,    /* retrieved vs No. (valid file only) */
                char *VersionChar,      /* retrieved vs Str (valid file only) */
                char *ext               /* retrieved ext (valid file only)    */
                );
/*----------------------------------------------------------------------------*/
void MakeL1BOutputFilename (
                char *l1a_inputfile,      /* input l1a filename               */
                char *l1b_inputfilename,  /* input l1b filename or null       */
                char *l1b_outputfilename, /* output l1b filename              */
                char *l1b_reportfilename, /* output l1b report filename       */
                char *l1b_charversion,    /* version string for l1b filenames */
                                          /* in case of standard filename     */
                char *l1b_filesat         /* satellite id, ignore if set to \0*/
                );
/*----------------------------------------------------------------------------*/
long little_endian();
/*----------------------------------------------------------------------------*/
long swapbyte(
                char buf[],               /* byte array to be swapped         */
                size_t num_bytes          /* size of variable to be swapped   */
             );
/*----------------------------------------------------------------------------*/
size_t fread_grace(
                void *ptr, 
                size_t  size,  
                size_t  nitems,  
                FILE *stream
             );  
/*----------------------------------------------------------------------------*/
size_t fwrite_grace(
                void *ptr, 
                size_t  size,  
                size_t  nitems,  
                FILE *stream
             );  
boolean ReformatRCStag(char *SoftwareVersion,    /* RCS software version label*/
                       char *NewLabel            /* Processing level (1A or 1B)*/
                      );
boolean LoadClockFile(FILE *clk,double **xt, double **yt,long *Np);
void AddTSsuppid2qualflg(signed char TSsuppId, unsigned char *qualflg);
long Check4GPSinQualflag(unsigned char qualflg);
boolean ReadILG1XFRecord(FILE *src, ILG1X_t *record);
boolean WriteILG1XFRecord(FILE *dst, ILG1X_t *record);
long OBDH2RCVRtime(long obdh_intg, long obdh_frac, long *gps_intg, long *gps_frac,
                  long icu_blk_nr, FILE *tim);
long Write_CMT_command(CMT_command_t *record);
long Write_SCA2K_command(SCA2K_command_t *record);
boolean AddInputFile2Header(FileHeader_t *header,char *filekey, 
                            char *input_filename,char *input_file_ttag,
                            char *version, char *linktime);
boolean CopyInputFile2Header(FileHeader_t *src_header,char *filekey, 
                             FileHeader_t *dst_header);




#endif	/* _GRACEio_prototypes_h_ */
