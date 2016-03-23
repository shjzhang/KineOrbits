/* %Z% %M%      %I% %G% */

#ifndef _GRACEbj_iolib_h_
#define _GRACEbj_iolib_h_

#include "GRACEdefs.h"

#ifdef _mk_extern_
#  define EXTERN
#  define EXTINIT(var,val) var = val
#else
#  define EXTERN extern
#  define EXTINIT(var,val) extern var
#endif


#define NFILETYPEMAX       20       /* Maximum number of file types           */
#define HEADERMAXCHAR      80       /* Maximum number of characters in header */
#define NMAXHEADERREC      100      /* Maximum number of header records       */
#define HEADERLABELMAXCHAR 30       /* Maximum number of header label chars   */

/* file type pointers */
#define ipGFDF       1              /* GPS Flight Data Format File            */
#define ipGNV1AF     2              /* GPS Navigation Level 1A Format File    */
#define ipSCA1AF     3              /* Star Camera Assembly 1A Data Fmt File  */
#define ipACC1AF     4              /* Level 1A Accelerometer Data File       */
#define ipGNV1BF     5              /* GPS Navigation Level 1B Format File    */
#define ipSCA1BF     6              /* Star Camera Assembly 1B Data Fmt File  */
#define ipKBR1BF     7              /* Level 1B KBR Data Format File          */
#define ipACC1BF     8              /* Level 1B Accelerometer Data File       */
#define ipVecOrienF  9              /* Vector Orientation Data File           */
#define ipIOACC1BF  10              /* Inertial Orientation of ACC File       */
#define ipOscStabF  11              /* Ultra Stable Oscillator Stability File */
#define ipGPS1AF    12              /* GPS Flight data level 1A in ipGFDF fmt */
#define ipGPS1BF    13              /* GPS Flight data level 1B in ipGFDF fmt */
#define ipKBR1AF    14              /* KBR data level 1A in ipGFDF format     */

/* Standard Header record pointers */

#define iphProducerAgency            0
#define iphProducerInstitution       1
#define iphFileType                  2
#define iphFileFormat                3
#define iphHeadNrecord               4
#define iphSoftwareVersion           5
#define iphDocumentation             6
#define iphSatelliteName             7
#define iphSensorName                8
#define iphTimeEpoch                 9
#define iphTimeFirstObs             10
#define iphTimeLastObs              11
#define iphNumberObs                12
#define iphProductCreateStartTime   13
#define iphProductCreateEndTime     14

/* Define Global Header Label and contens arrays  */

EXTERN char FileHeaderLabel[NFILETYPEMAX][NMAXHEADERREC][HEADERMAXCHAR];
EXTERN char FileHeaderContents[NFILETYPEMAX][NMAXHEADERREC][HEADERMAXCHAR];

/*----------------------------------------------------------------------------->
/  Structure definitions
<-----------------------------------------------------------------------------*/

typedef struct bjSCAMratt_t         /* BlackJack Data: Star Camera            */
                                    /*   Packet: ratt                         */
                                    /* Contains the attitude quaterions for   */
                                    /* the reference attitude to the other    */
                                    /* spacecraft                             */
        {
        long            time_tag;   /* Time, in receiver seconds of this      */
                                    /* solution                               */
        unsigned char   flags;      /* A bit-field flag byte, bits as follows:*/
                                    /* 0x01==Valid (i.e. Confidence << 11)    */
                                    /* 0x10==Other ID > mine                  */
                                    /* 0x20==Other ID < mine                  */
        char            confidence; /* Confidence of solution, < 11 is valid  */
        char            age;        /* Age of ephermeris, in hrs (3600 secs)  */
        char            id;         /* id of the uplinked elements on which   */
                                    /* this attitude is based                 */
        long            quaticoeff; /* I element of quaternion rotation       */
                                    /* axis * sin(mu/2)                       */
        long            quatjcoeff; /* J element of quaternion rotation       */
                                    /* axis * sin(mu/2)                       */
        long            quatkcoeff; /* K element of quaternion rotation       */
                                    /* axis * sin(mu/2)                       */
        long            quatangle;  /* Cos mu/2 element of quaternion         */
                                    /* (*2147483648.0)                        */
        } bjSCAMratt_t;

typedef struct bjSCAMattd_t         /* BlackJack Data: Star Camera            */
                                    /*   Packet: attd                         */
                                    /* Contains information describing the    */
                                    /* star camera configuration              */
        {
        long            time_tag;   /* Time, in receiver seconds of this      */
                                    /* solution                               */
        char            flags;      /* A bit-field flag byte, bits as follows:*/
                                    /* 0x01==valid (i.e. confidence < 11)     */
                                    /* 0x02==precision                        */
                                    /* 0x04==quat not RA-Dec                  */
                                    /* 0x08==non-stellar object               */
                                    /* 0x30==head id (two bits) Head 1,2, or  */
                                    /* 3 as in SCAParameters packet           */
        char            confidence; /* Confidence of solution, < 11 valid     */
        char            nlocks;     /* Number of locks of Star Camera         */
        char            nstars;     /* Number of stars                        */
        long            quaticoeff; /* I element of quaternion rotation       */
                                    /* axis * sin(mu/2)                       */
        long            quatjcoeff; /* J element of quaternion rotation       */
                                    /* axis * sin(mu/2)                       */
        long            quatkcoeff; /* K element of quaternion rotation       */
                                    /* axis * sin(mu/2)                       */
        long            quatangle;  /* Cos mu/2 element of quaternion         */
                                    /* (*2147483648.0)                        */
        } bjSCAMattd_t;

typedef struct bjSCAMmode_t         /* BlackJack Data: Star Camera            */
                                    /*   Packet: mode                         */
                                    /* Contains mode information for the      */
                                    /* star camera software                   */ 
        {
        char            mode;       /* Current mode                           */
                                    /* mode==0, stand-by mode                 */
                                    /* mode==1, attitude mode, one camera     */ 
                                    /* mode==2, attitude mode, two cameras    */ 
                                    /* mode==3, attitude mode, all available  */
                                    /* cameras                                */
                                    /* mode==-1, test image mode              */
                                    /* mode==-2, simulation mode              */
        } bjSCAMmode_t;

typedef struct bjRCVMadcp_t         /* BlackJack Data: Receiver Management    */
                                    /*   Packet: adcp                         */
                                    /* Contains the onboard sensor values     */
                                    /* that may be temperature or voltage or  */
                                    /* calculated currents                    */
        {
        long            adctime;    /* Time, in receiver seconds, of this     */
                                    /* solution                               */
                                    /* Each sensor value repeats until        */
                                    /* DataLen-8 bytes transmitted            */
        double          sensorvalue;/* Value associated with the named sensor */
                                    /* (unit specified below)                 */
        char            sensortype; /* Unknown values should be considered    */
                                    /* valid and ignored:  This value will    */
                                    /* never be NULL                          */
                                    /* 'T' (0x54) for temperature in degrees C*/
                                    /* 'V' (0x56) for voltage in volts        */
                                    /* 'A' (0x41) for current in amperes      */
        char            sensorname[16]; /* NULL-terminated value name, NULL   */
                                    /* means unnamed (Note: arbitrarily       */
                                    /* declared to be <= 16 bytes ??)         */ 
        } bjRCVMadcp_t;



typedef struct bjRCVMadcf_t         /* BlackJack Data: Receiver Management    */
                                    /*   Packet: adcf                         */
                                    /* Contains the onboard sensor values     */
                                    /* that may be temperature or voltage or  */
                                    /* calculated currents.  Fixed-point      */
                                    /* version of ADC packet, for use on      */
                                    /* GRACE only                             */
        {
        long            adctime;    /* Time, in receiver seconds, of this     */
                                    /* solution                               */
                                    /* Each sensor value repeats until        */
                                    /* DataLen-8 bytes transmitted            */
        double          sensorvalue;/* Value associated with the named sensor */
                                    /* (unit specified below), in ten-        */
                                    /* thousandths of value                   */
        char            sensortype; /* Unknown values should be considered    */
                                    /* valid and ignored:  This value will    */
                                    /* never be NULL                          */
                                    /* 'T' (0x54) for temperature in degrees C*/
                                    /* 'V' (0x56) for voltage in volts        */
                                    /* 'A' (0x41) for current in amperes      */
        char            sensorname[16]; /* NULL-terminated value name, NULL   */
                                    /* means unnamed (Note: arbitrarily       */
                                    /* declared to be <= 16 bytes ??)         */ 
        } bjRCVMadcf_t; 

typedef struct bjOBSDqfit_t
        {
        unsigned long   obstime;    /* Time in receiver seconds, of the fit   */
                                    /* point                                  */
        unsigned char   prn_id;     /* PRN of this observable data.           */
                                    /*      0x32 = 32 GHz observables         */
                                    /*      0x33 = 24 GHz observables         */
                                    /* from the GRACE tone-tracking s/w       */
        char            ant_input;  /* ASIC Antenna number (0-11)             */
        unsigned char   obs_type;   /* Data flags (TBD)                       */
        char            sample_intv;/* Sample interval for the fit data       */
        unsigned char   ca_chn;     /* Receiver chan, modulo 128, for the     */
                                    /* CA data.  Value > 128 means that no P2 */
                                    /* or P1 data follows this data           */
        short           ca_snr;     /* SNR in volts/volt for CA channel       */
        double          ca_phase;   /* Carrier phase measurement for CA chn   */
                                    /* in L1 cycles                           */
        double          ca_range;   /* Tau measurement for CA chn's data in   */
                                    /* CA chips                               */ 
                                    /*                                        */
                                    /* If P1 or P2 is enabled, these P2 values*/
                                    /* are output:                            */
        unsigned char   p2_chn;     /* Receiver chn, modulo 128, for the P2   */
                                    /* data; value > 128 means no P1 data     */
                                    /* follows                                */
        short           p2_srn;     /* SRN in volts/volt for P2 chn           */
        float           p2_phase;   /* Difference between P2 carrier phase    */
                                    /* (scaled to L1) and CA carrier phase    */
                                    /* in L1 cycles                           */
        float           p2_range;   /* Difference between P2 range (tau) and  */
                                    /* CA rand in CA chips                    */
                                    /*                                        */
                                    /* If P1 is enabled, these P1 values are  */
                                    /* output:                                */
        unsigned char   p1_chan;    /* Receiver chn for P1 data               */
        short           p1_snr;     /* SRN in volts/volt for P1 chn           */
        float           p1_phase;   /* Difference between P1 carrier phase    */
                                    /* and CA carrier phase in L1 cycles      */
        float           p1_range;   /* Difference between P1 range (tau) and  */
                                    /* CA range in CA chips                   */
                                    /*                                        */
                                    /* If any residual output is enabled,     */
                                    /* these values are output:               */ 
        float           ca_doppler; /* CA code carrier phase doppler in L1    */
                                    /* cycles per second                      */
        float         ca_dopp_rate; /* CA code carrier phase doppler rate in  */
                                    /* L1 cycles per second per second        */
                                    /*                                        */
                                    /* If residual output is enabled, these   */
                                    /* are output depending on type enabled:  */
                                    /*   0xFC = CA Phase                      */
                                    /*   0xF1 = P1 Phase                      */
                                    /*   0xF2 = P2 Phase                      */
                                    /*   0xF3 = 32 GHz Phase                  */ 
                                    /*   0xF4 = 24 GHz Phase                  */ 
        char           ph_resid_typ;/* Date type byte for following 3 fields: */
        char           ph_scalfact; /* Residual scale factor (dimensionless)  */
        char          ph_resid_rate;/* Rate for residual data (Hz)            */
        float    ph_residual[MAXHZ];/* An array of elements with residual     */
                                    /* rate entries for ea. second of sample  */
                                    /* interval; each element contains the    */
                                    /* difference between the actual value    */
                                    /* and the quadratic fit for the data     */ 
                                    /*                                        */
                                    /* If residual output is enabled, these   */
                                    /* are output depending on type enabled:  */
                                    /*   0xAC = CA Amplitude                  */
                                    /*   0xA1 = P1 Amplitude                  */
                                    /*   0xA2 = P2 Amplitude                  */
                                    /*   0xA3 = 32 GHz Amplitude              */ 
                                    /*   0xA4 = 24 GHz Amplitude              */ 
        char          amp_resid_typ;/* Date type byte for following 3 fields: */
        char          amp_scalfact; /* Amplitude scale factor (dimensionless) */
        char         amp_resid_rate;/* Rate for amplitude data (Hz)           */
        float   amp_residual[MAXHZ];/* An array of elements with amplitude    */
                                    /* rate entries for ea. second of sample  */
                                    /* interval; each element contains the    */
                                    /* difference between the actual value    */
                                    /* and the quadratic fit for the data     */ 

/*----------------------------------------------------------------------------->
/ Function Prototypes 
<-----------------------------------------------------------------------------*/


/*----------------------------------------------------------------------------*/
boolean ReadbjSCAMattdPacket 
	       (
/* input */
		FILE	         *src,    /* Pointer to BlackJack file        */
                                          /* file                             */
/* output */
		bjSCAMattd_t     *packet  /* Pointer to bjSCAMattd packet     */
                                          /* struct (jbSCAMattd_t )           */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/


/*----------------------------------------------------------------------------*/
void PrintbjSCAMattdPacket 
	       (
/* input */
		FILE	         *dst,    /* Pointer to BlackJack file        */
		bjSCAMattd_t     *packet  /* Pointer to bjSCAMattd packet     */
                                          /* struct (bjSCAMattd_t )           */
	       );



/*----------------------------------------------------------------------------*/
boolean ReadbjSCAMrattPacket 
	       (
/* input */
		FILE	         *src,    /* Pointer to BlackJack file        */
                                          /* file                             */
/* output */
		bjSCAMratt_t     *packet  /* Pointer to bjSCAMratt packet     */
                                          /* struct (jbSCAMratt_t )           */
	       );
/* return:	1	normal return
                0	End Of File reached 
*/


/*----------------------------------------------------------------------------*/
void PrintbjSCAMrattPacket 
	       (
/* input */
		FILE	         *dst,    /* Pointer to BlackJack file        */
		bjSCAMratt_t     *packet  /* Pointer to bjSCAMratt packet     */
                                          /* struct (bjSCAMratt_t )           */
	       );

#endif	/* _GRACEbj_iolib_h_ */
