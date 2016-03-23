/*     %Z% %M% %I% %G% */

#include "GRACEfiletype.h"

#ifndef _GRACEtelemetry_h_ 
#define _GRACEtelemetry_h_ 

#define MAXPACKET           500
#define MAXNRPACKETS       (MAXPACKET+1) 
#define PKT_NOTDEFINED       -1
#define NOPKTPOINTER         -1
#define PACKETCHARMAX       100
#define NRTMSTATS             4
#define IPTOTALPKT            0
#define IPGOODPKT             1
#define IPBADPKT              2
#define IPBADBYTES            3
#define IPINVALIDPACKET       MAXPACKET
#define NRICUCOEF             4
#define NRICUCALTEMPS         3
#define NRICUBOARDS           2
#define NRICUSATS             2
#define IPICUBOARDNOM         0
#define IPICUBOARDRED         1
#define IPICUGRACEA           0
#define IPICUGRACEB           1

#define IPPREVTS              0
#define IPCURRTS              1
#define IPNEXTTS              2
#define MAXICUPKTS            100

#define MAXHKCALCHAR          200
#define MAXHKCALLINES         200
#define MAXPOLYNORDER         10
#define NOT_USED              0L
#define USE                   1L
#define MAXHKSAT              2L

#define NTHRUST_MAX           100
#define MAXTHRUSTERS          14
#define IPGRACEA              0
#define IPGRACEB              1

#define NSOESENSORS          28
#define IPSOEIPU              0
#define IPSOEKBR              1
#define IPSOESCA              2
#define IPSOEACC              3
#define IPSOEUSO              4
#define IPSOEVGN              5
#define IPSOEVGB              6
#define IPSOEVGO              7
#define IPSOEVSL              8
#define IPSOEVCM              9
#define IPSOEVKB             10
#define IPSOEQSA             11
#define IPSOEQSB             12
#define IPSOEMTE1            13
#define IPSOEMTE2            14
#define IPSOEQKS             15
#define IPSOEIPUR            16
#define IPSOEK_MI            17
#define IPSOEKAMI            18
#define IPSOEKTOFF           19
#define IPSOEAOCS            20
#define IPSOEICUVP           21
#define IPSOEACCR            22
#define IPSOEACCT            23
#define IPSOEMANV            24
#define IPSOECMCAL           25
#define IPSOEKBRCAL          26
#define IPSOEOCC             27

#define GPSTIMEOFFSET -630763200

#define MAXAPPDATA 65535 /* this number is determined by adding all max packet*/
                         /* lengths in file GRACE_TM_properties.txt. This is  */
                         /* the maximum amount of bytes that can occur before */
                         /* a time stamp packet must be found in the byte     */
                         /* stream. 65535 is chose because max unsigned short */

#define NTHRUSTER  7
#define MAXPARAM  10
#define MAXWORD  100

typedef struct TMpackets_t            /* struct for TM packet properties      */
        {   
          char name[PACKETCHARMAX];   /* TM packet name                       */
          long PacketId;              /* Packet Id                            */
          long MinSuppPacketId;       /* Minimum Supplemental PacketId        */
          long MaxSuppPacketId;       /* Minimum Supplemental PacketId        */
          long MinPacketLength;       /* Minimum Packet length (bytes)        */
          long MaxPacketLength;       /* Maximum Packet Length (bytes)        */
        } TMpackets_t;

typedef struct TimeStamp_t            /* struct for data in Time Stamp packet */
        {
          unsigned char *PacketId;     /* Timestamp packet Id                  */
          signed char *SuppPacketId;   /* Timestamp supplementary packet Id    */
          unsigned short *Dlen;        /* Timestamp data packet length         */
          long *Time;                  /* time tag (GPS sec past 01/06/80 0:0:0*/
        } TimeStamp_t;

typedef struct ApplicationPkt_t        /* struct for data in any application   */
                                       /* packet                               */
        {
          unsigned char *PacketId;     /* Application packet Id                */
          signed char *SuppPacketId;   /* Application supplementary packet Id  */
          unsigned short *Dlen;        /* Application data packet length       */
          unsigned char *Data;         /* pointer to data segment in App packet*/
        } ApplicationPkt_t;

typedef struct ICUDAcoef_t             /* coefficient for ICU digital to analog*/
                                       /* data conversion                      */
        {
          char sccsid[MAXHKCALCHAR];   /* sccs id of set up file               */
          double temps[NRICUCALTEMPS]; /* temperature for coefficients in deg C*/
          double coeff[NRACC1APODS][NRICUCOEF][NRICUCALTEMPS]; /* coefficients */
        } ICUDAcoef_t;

typedef struct ICU_packet_t            /* struct containing ICU packet info    */
        {
          unsigned short pkt_length;   /* icu packet length (including header  */
          char     pkt_name;           /* icu packet name                      */
          char     pkt_chksum_status;  /* icu checksum status                  */
                                       /* 0 = OK, 1 = invalid                  */
          unsigned short block_nr;     /* icu packet block number              */ 
        } ICU_packet_t;

typedef struct ICUAP_info_t            /* struct containing ICU application    */
                                       /* packet information                   */
        {
          unsigned char AP_pktId;      /* Application Packet Id                */
          char AP_SuppId;              /* Application Packet Supplementary Id  */
          unsigned short AP_Dlen;      /* Application Packet Length            */
          long TimeStamp[3];           /* previous, current and next timestamp */
          char TS_SuppId[3];           /* prev, current and next timestamp     */
                                       /* supplementary ID where               */
                                       /* 0 = SCET (space craft elapsed time)  */
                                       /* 1 = GPS time                         */
                                       /* 2 = SCET + PulseSync                 */
                                       /* 3 = GPS time + PulseSync             */
          long NicuPkts;               /* number of ICU pkts in ICU AP packet  */
          ICU_packet_t pkt[MAXICUPKTS];/* icu packet info for all ICU packets  */
          unsigned short blocknrs[3];  /* previous and next ICU block numbers  */
        } ICUAP_info_t;

typedef struct TM_MAG_t     /*            Magnetometer HK packet struct        */
        {
          short Updated;    /*1 indicates record data updated                  */
          short MfvX_RAW;   /*x-axis component of measured earth magnetic field*/
          short MfvY_RAW;   /*y-axis component of measured earth magnetic field*/
          short MfvZ_RAW;   /*z-axis component of measured earth magnetic field*/
          short Torque1A;   /*current of magnetorquer 1 A (positive current x) */
          short Torque2A;   /*current of magnetorquer 2 A (positive current y) */
          short Torque3A;   /*current of magnetorquer 3 A (positive current z) */
          short Torque1B;   /*current of magnetorquer 1 B (negative current x) */
          short Torque2B;   /*current of magnetorquer 2 B (negative current y) */
          short Torque3B;   /*current of magnetorquer 3 B (negative current z) */
          short MF_BCalX;   /*mag field calibration factors (x)                */
          short MF_BCalY;   /*mag field calibration factors (y)                */
          short MF_BCalZ;   /*mag field calibration factors (z)                */
          short MT_Kr;      /*mag torquer calibration factor                   */
        } TM_MAG_t;
typedef struct TM_ANARAW_SCT_t /* Standard Calibration Thermistor(SCT) packet  */
        {
          short value00;            /* Reference value for next 3 values       */ 
          short value01;            /* Harness to MWA electronics +y red.      */
          short value02;            /* ACC thermal Cage +z red                 */
          short value03;            /* RFEA                                    */
          short value04;            /* Reference value for next 15 values      */
          short value05;            /* I/F Support Structure to MEP -y,red.    */
          short value06;            /*ACC thermal Cage -z red.                 */
          short value07;            /*OBDH                                     */
          short tank_adap_negx;     /*Tank  adapter -x                         */
          short value09;            /*Thruster Valve x ATH 12, 14, OTH 11, 21  */
          short value10;            /*Platform nadir +y front                  */
          short value11;            /*Platform nadir -y mid                    */
          short value12;            /*Thruster Valve front y                   */
          short value13;            /*Thruster Valve front z                   */
          short tank_skin_negx;     /*Tank sphere -x                           */
          short value15;            /*Solar Array -y side +x                   */
          short value16;            /*MLI Heater-y                             */
          short value17;            /*MLI Heater +y                            */
          short value18;            /*MLI Heater MEP                           */
          short value19;            /*Harness to MWA electronics -y red.       */
          short value20;            /*Reference value for next 15 analog values*/
          short value21;            /*I/F Support Structure to MEP +y, red.    */
          short value22;            /*Harness to ACC Sensor red.               */
          short value23;            /*Battery +/-y                             */
          short value24;            /*PCDU                                     */
          short value25;            /*Thruster Valve -y ATH 15,16,11,23        */
          short value26;            /*Thruster Valve +x ATH 24, 22             */
          short value27;            /*Platform nadir y front                   */
          short value28;            /*I/F GPS Zenith Antenna                   */
          short value29;            /*Thruster Valve front +y                  */
          short value30;            /*Thruster Valve front +z                  */
          short tank_skin_posx_red; /*Tank sphere +x redundant                 */
          short value32;            /*Solar Array +y side +x                   */
          short value33;            /*MLI Heater-y, redundant                  */
          short value34;            /*MLI Heater +y, redundant                 */
          short value35;            /*MLI Heater MEP, redundant                */
          short value36;            /*Reference value of next 12 values        */
          short value37;            /*RF-Sampling Unit red.                    */
          short value38;            /*Battery +/-y redundant                   */
          short tank_adap_posx;            /*Tank adapter +x                   */
          short value40;            /*Thruster Valve +y ATH 26, 25 13, 21      */
          short value41;            /*Magnetometer on Boom                     */
          short value42;            /*Platform nadir +y mid                    */
          short value43;            /*Spare                                    */
          short value44;            /*Spare  candidate: IMU TBD,               */
          short tank_skin_posx;     /*Tank sphere +x                           */
          short tank_skin_negx_red; /*Tank sphere -x redundant                 */
          short value47;            /*+x Panel Inside                          */
          short value48;            /*I/F Support Structure to MEP mid red     */ 
          short SctR0;              /*calibration parameter R0                 */
          short SctRref;            /*calibration parameter Rref               */
        } TM_ANARAW_SCT_t;

typedef struct TM_PWR_PCDUHK_t      /* PCDU HK data packet                     */
        {
          short value00;            /* Reference Voltage                       */
          short value01;            /* (First) Solar Array Input Current 1 Main*/
          short value02;            /* (First) Solar Array Input Current 1 Red */
          short value03;            /* OBDH 1 Current                          */
          short value04;            /* OBDH 2 Current                          */
          short value05;            /* Receiver 1 Current                      */
          short value06;            /* Receiver 2 Current                      */
          short value07;            /* Transmitter Main Current                */
          short value08;            /* Transmitter Red. Current                */
          short value09;            /* MTE Main Current                        */
          short value10;            /* MTE Red. Current                        */
          short value11;            /* USO Right Current                       */
          short value12;            /* USO Left Current                        */
          short value13;            /* PARSU IPU Main Current                  */
          short value14;            /* PARSU IPU Red. Current                  */
          short value15;            /* SuperSTAR Accelerometer Current ICU Main*/
          short value16;            /* SuperSTAR Accelerometer Current ICU Red.*/
          short value17;            /* Getter Pump Current                     */
          short value18;            /* PARSU MWA1 Current                      */
          short value19;            /* (First Module) Heaters combined Current1*/
          short value20;            /* Battery Charge Current Main             */
          short value21;            /* Battery Charge Current Red.             */
          short value22;            /* Battery Discharge Current Main          */
          short value23;            /* Battery Discharge Current Red.          */
          short value24;            /* Battery Temperature Main                */
          short value25;            /* Battery Temperature Red.                */
          short value26;            /* Battery Cell Pressure Main              */
          short value27;            /* Battery Cell Pressure Red.              */
          short value28;            /* Battery (Full) Voltage Main             */
          short value29;            /* Battery (Full) Voltage Red.             */
          short value30;            /* Battery 1 / 2 Voltage                   */
          short value31;            /* PARSU MWA2 Current                      */
          short regpress_1;         /* (First) Pressure Transducer Low 1       */
          short tnkpress_1;         /* (First) Pressure Transducer High 1      */
          short value34;            /* OC Thruster Combined Current            */
          short value35;            /* (Second Module)Heaters combined Current2*/
          short value36;            /* (Second)Solar Array Input Current 2 Main*/
          short value37;            /* (Second)Solar Array Input Current 2 Red.*/
          short regpress_2;         /* (Second) Pressure Transducer Low 2      */
          short tnkpress_2;         /* (Second) Pressure Transducer High 2     */
          short value40;            /* IMU (Gyro) Supply Main                  */
          short value41;            /* AC Thruster Combined Current            */
          short value42;            /* Analog HK Spare                         */
        } TM_PWR_PCDUHK_t;
typedef struct TM_AOC_STAT_t           /* AOCS status HK packet                */
        {
          short NumErrs;               /* number of errors occured (wrap count)*/
          short NumCmdsReceived;       /* number of cmds rcvd (wrap count)     */
          short NumCmdsRejected;       /* number of cmds rejected (wrap count) */
          short ActCtlFlags;           /* actuator control related flags       */
                                       /* b15: 1=>Thruster control by Ground;  */
                                       /*      0=>by OBSW                      */
                                       /* b14: 1=>Mag.Torquer control by Ground*/
                                       /*      0=>by OBSW                      */
                                       /* b13-b12: reserved                    */
                                       /* b11-b0: Thruster Enable bits         */
                                       /* b11 corresponds-- to THRUSTER_12,.., */
                                       /* b0 to THRUSTER_1                     */
          long AccumThrOnTimes[12];    /* accum. thruster on-times [ms] (wrap) */ 
                                       /* for each 12 aoc thrusters            */
          long AccumThrActivations[12];/* activation counters (wrap count)     */
                                       /* for each 12 aoc thrusters            */
          long ActivationTimeStamp;    /* TimeStamp [s] of most recent thruster*/
                                       /* activation                           */
          long AocModeChanges;         /* accum. number of mode changes (wrap) */
        } TM_AOC_STAT_t;

typedef struct tm_sc_stat_t            /* Timestamp + SC packet statistics     */
        {
          long count;                  /* timestamp count in TM file           */
          long TStime;                 /* timestamp time                       */
          long TSstate;                /* timestamp suppid state               */
                                       /* 0 = SCET                             */
                                       /* 1 = GPS Time                         */
                                       /* 2 = SCET + Pulse sync                */
                                       /* 3 = GPS Time + Pulse sync            */
          long TSstate_prev;           /* previous timestamp suppid state      */
                                       /* 0 = SCET                             */
                                       /* 1 = GPS Time                         */
                                       /* 2 = SCET + Pulse sync                */
                                       /* 3 = GPS Time + Pulse sync            */
          long TSdt;                   /* Delta Time between current and       */
                                       /* previous TStime                      */
          long IPUpktid;               /* IPU application packet Id            */
          long IPUpktsize;             /* IPU application packet size          */
          long ICUpktid;               /* ICU application packet Id            */
          long ICUpktsize;             /* ICU application packet size          */
          long NICUblocks;             /* Number of ICU datablock in ICU ap pkt*/
          long DeltaBlockNr;           /* Delta Block number with first data   */
                                       /* block number of previous ICU ap pkt  */
          long blocknrs[100];          /* ICU block numbers                    */
          long blockgdel_flag[100];    /* ICU blocks for which gdel is set     */
                                       /* 0 = gdel flag not set, 1 = flag set  */
         } tm_sc_stat_t;

typedef struct hkcal_raw_t                /* struct containing raw hkcal info  */
         {
           char on;                       /* function is active on = 1 else = 0*/
           long npar;                     /* # of parameters need for function */
           long par_ndx[MAXPOLYNORDER];   /* look up indeces for parameters    */
                                          /* used in function                  */
         } hkcal_raw_t;

typedef struct hkcal_poly_t               /* struct containing poly hk cal info*/
         {
           char on;                       /* function is active on = 1 else = 0*/
           long norder;                   /* order of polynomial               */
           double coeff[MAXPOLYNORDER];   /* polynomial coefficients           */
           long npar;                     /* # of parameters need for function */
           long par_ndx[MAXPOLYNORDER];   /* look up indeces for parameters    */
                                          /* used in function                  */
         } hkcal_poly_t;

typedef struct hkcal_scale_t               /* struct containing scale hkcal info*/
         {
           char on;                       /* function is active on = 1 else = 0*/
           double scale;                  /* scale factor                      */
           long npar;                     /* # of parameters need for function */
           long par_ndx[MAXPOLYNORDER];   /* look up indeces for parameters    */
                                          /* used in function                  */
         } hkcal_scale_t;

typedef struct hkcal_f2args_t             /* struct containing f2 arguments    */
         {
           char on;                       /* function is active on = 1 else = 0*/
           char arg2[MAXHKCALCHAR];       /* argument 2 to f2 function         */
           long arg3;                     /* argument 3 to f2 function (long)  */
           long npar;                     /* # of parameters need for function */
           long par_ndx[MAXPOLYNORDER];   /* look up indeces for parameters    */
                                          /* used in function                  */
         } hkcal_f2args_t;

typedef struct hkcal_f3args_t             /* struct containing f3 arguments    */
         {
           char on;                       /* function is active on = 1 else = 0*/
           char arg2[MAXHKCALCHAR];       /* argument 2 to f3 function         */
           long arg3;                     /* argument 3 to f3 function (long)  */
           long npar;                     /* # of parameters need for function */
           long par_ndx[MAXPOLYNORDER];   /* look up indeces for parameters    */
                                          /* used in function                  */
         } hkcal_f3args_t;

typedef struct hkcal_f4args_t             /* struct containing f4 arguments    */
         {
           char on;                       /* function is active on = 1 else = 0*/
           char arg1[MAXHKCALCHAR];       /* argument 1 to f4 function         */
           char arg2[MAXHKCALCHAR];       /* argument 2 to f4 function         */
           char arg3[MAXHKCALCHAR];       /* argument 2 to f4 function         */
           double arg4;                   /* argument 4 to f4 function (double)*/
           long npar;                     /* # of parameters need for function */
           long par_ndx[MAXPOLYNORDER];   /* look up indeces for parameters    */
                                          /* used in function                  */
         } hkcal_f4args_t;

typedef struct hkcal_f5args_t             /* struct containing f5 arguments    */
         {
           char on;                       /* function is active on = 1 else = 0*/
           char arg1[MAXHKCALCHAR];       /* argument 1 to f5 function         */
           char arg2[MAXHKCALCHAR];       /* argument 2 to f5 function         */
           long npar;                     /* # of parameters need for function */
           long par_ndx[MAXPOLYNORDER];   /* look up indeces for parameters    */
                                          /* used in function                  */
         } hkcal_f5args_t;

typedef struct hkcal_f6args_t             /* struct containing f6 arguments    */
         {
           char on;                       /* function is active on = 1 else = 0*/
           char arg1[MAXHKCALCHAR];       /* argument 1 to f6 function         */
           char arg2[MAXHKCALCHAR];       /* argument 2 to f6 function         */
           char arg3[MAXHKCALCHAR];       /* argument 2 to f6 function         */
           long npar;                     /* # of parameters need for function */
           long par_ndx[MAXPOLYNORDER];   /* look up indeces for parameters    */
                                          /* used in function                  */
         } hkcal_f6args_t;

typedef struct hkcal_f7args_t             /* struct containing f7 arguments    */
         {
           char on;                       /* function is active on = 1 else = 0*/
           char arg1[MAXHKCALCHAR];       /* argument 1 to f7 function         */
           char arg2[MAXHKCALCHAR];       /* argument 2 to f7 function         */
           long npar;                     /* # of parameters need for function */
           long par_ndx[MAXPOLYNORDER];   /* look up indeces for parameters    */
                                          /* used in function                  */
         } hkcal_f7args_t;

typedef struct hk_cal_info_t              /* digital to analog conversion info*/
         {
           char param_code[MAXHKCALCHAR]; /* parameter code                    */           
           char acronym[MAXHKCALCHAR];    /* acronym used by GSOC for parameter*/
           char param_type[MAXHKCALCHAR]; /* parameter type:                   */ 
                                          /*         BAS = basic DER = derived */ 
           long start_byte;               /* start byte within packet          */
                                          /* = 1 parameter does not depend on  */
                                          /*     any packet data               */
                                          /* != 1 parameter does depend on     */
                                          /*      packet data at this byte     */
           long start_bit;                /* start bit for this parameter      */
           long nbits;                    /* number of bits to be read         */
           char int_type[MAXHKCALCHAR];   /* integer type:                     */
                                          /*     USG = unsigned ,SIG = signed  */             
           char output[MAXHKCALCHAR];     /* write output to file indicator    */
           char output_format[MAXHKCALCHAR];/* output format I,F,E,T,H,B       */
           long ndigits;                  /* number of digits after decimal    */
           char cal_method[MAXHKCALCHAR]; /* calibration method                */
                                          /* scale(x)                          */
                                          /* poly(An,An-1,....,A0)             */
                                          /* intpol(x0,y0,x1,y1,...,xN,yN)     */
           hkcal_raw_t raw;               /* raw   function parameters         */
           hkcal_scale_t  scale;          /* scale function parameters         */
           hkcal_poly_t   poly;           /* poly  function parameters         */
           hkcal_f2args_t f2;             /* f2    function parameters         */
           hkcal_f3args_t f3;             /* f3    function parameters         */
           hkcal_f4args_t f4;             /* f4    function parameters         */
           hkcal_f5args_t f5;             /* f5    function parameters         */
           hkcal_f6args_t f6;             /* f6    function parameters         */
           hkcal_f7args_t f7;             /* f7    function parameters         */
         } hk_cal_info_t;

typedef struct thruster_info_t
         {
           double mass_dot;               /* nominal mass flow rate (kg/sec)   */
           double k_factor;               /* correction factor for mass_dot    */
           double mass_dot_sigma;         /* sigma for mass_dot     (kg/sec)   */
           double k_factor_sigma;         /* sigma for k_factor     (no dim)   */
           long string_ndx;               /* index to which string thruster    */
                                          /* belongs 1 or 2. i.e sourc of gas  */
                                          /* tank 1 or 2                       */
           long ntable;                   /* number of thruster firings listed */
                                          /* in input table. Only used for     */
                                          /* orbit thrusters 13+14             */
           double start_time[NTHRUST_MAX];/* start time thrust (GPS sec)       */
           double final_time[NTHRUST_MAX];/* stop  time thrust (GPS sec)       */
           double accum_time[NTHRUST_MAX];/* accumulate on time                */
           long  thrust_count[NTHRUST_MAX];/* thrust event count               */
           double start_time_sigma[NTHRUST_MAX];/* sigma for start time (sec)  */
           double final_time_sigma[NTHRUST_MAX];/* sigma for stop  time (sec)  */
         } thruster_info_t;

typedef struct mas_com_info_t
         {
           char sccsid[MAXHKCALCHAR];     /* sccs id of set up file            */
           char grace_id;                 /* satellite id (A or B)             */
           double mass_zero;              /* initial satellite mass (no gas) kg*/
           double mass_gas_zero_1;        /* initial gas mass tank 1 in kg     */
           double pres_gas_zero_1;        /* initial pressure tank 1 in bar    */
           double temp_gas_zero_1;        /* initial temperature tank 1 (deg C)*/
           double mass_gas_zero_2;        /* initial gas mass tank 1 in kg     */
           double pres_gas_zero_2;        /* initial pressure tank 1 in bar    */
           double temp_gas_zero_2;        /* initial temperature tank 1 (deg C)*/
           double mass_zero_sigma;        /* sigma on mass_zero in kg          */
           double mass_gas_zero_1_sigma;  /* sigma on mass_gas_zero_1 in kg    */
           double pres_gas_zero_1_sigma;  /* sigma on pres_gas_zero_1 in bar   */
           double temp_gas_zero_1_sigma;  /* sigma on temp_gas_zero_1 in deg C */
           double mass_gas_zero_2_sigma;  /* sigma on mass_gas_zero_2 in kg    */
           double pres_gas_zero_2_sigma;  /* sigma on pres_gas_zero_2 in bar   */
           double temp_gas_zero_2_sigma;  /* sigma on temp_gas_zero_2 in deg C */
           thruster_info_t thruster[MAXTHRUSTERS]; /* info for all thrusters   */
         } mas_com_info_t;

typedef struct sensor_soe_info_t
         {
           long   nevents;                /* number of events              */
           double *event_times;           /* pointer to event times array  */
           double *event_ndx;             /* pointer to event indices array*/
           long   *nvalues;               /* number of values per event    */
           double **values;               /* values per event              */
         } sensor_soe_info_t;

TMpackets_t GRACE_TMpackets[MAXNRPACKETS];
mas_com_info_t GRACE_MAScom[2];

long GRACE_TMpointer[MAXNRPACKETS];
long GRACE_TMstats[MAXNRPACKETS][NRTMSTATS];

ICUDAcoef_t GRACE_ICUcal[NRICUSATS][NRICUBOARDS];

hk_cal_info_t GRACE_HKcal[MAXHKSAT][MAXHKCALLINES];

sensor_soe_info_t GRACE_SOE[2][NSOESENSORS];

/*>>>> prototypes <<<<*/

long LoadTMproperties();
long LoadHKCALproperties();
long LoadSOEproperties(char *soe_filename);
long LoadMASCOMproperties();
long LoadICUcal();
void PrintICUAP_info(FILE *dst, ICUAP_info_t *info,long Informative,long Flush);
long EncodeICUpacket(unsigned char PktId,char SuppPktId,unsigned short AP_Dlen,
                     char *AP_Data,
                     long RefTime, char TS_AP_Suppid, ACC1A_t *screcs, 
                     long *Nscrec, ACC1A_t *hkrecs, long *Nhkrec,
                     char GRACE_id,double Temperature,long ICUbinary,
                     long *NicuPkts, ICUAP_info_t *icupkt_info);
long EncodeMAGpacket(unsigned char PktId,char SuppPktId, unsigned short AP_Dlen,
                     char *AP_Data, long RefTime, char TS_AP_Suppid, 
                     MAG1X_t *magrec, char GRACE_id,double Temperature, 
                     long MAGbinary);
long EncodeAOCSpacket(unsigned char PktId,char SuppPktId, unsigned short AP_Dlen,
                      char *AP_Data, long RefTime, char TS_AP_Suppid,
                      THR1X_t *thrrec, char GRACE_id,double Temperature,
                      long THRbinary, double SCET_offset);
long EncodeANARAWpacket(unsigned char PktId,char SuppPktId, unsigned short AP_Dlen,
                        char *AP_Data, long RefTime, char TS_AP_Suppid, 
                        TNK1X_t *tnkrecA, TNK1X_t *tnkrecB, char GRACE_id,
                        double Temperature, long TNKbinary, long NewTNKrec);
long EncodePWR_PCDUHKpacket(unsigned char PktId,char SuppPktId,
                            unsigned short AP_Dlen,
                        char *AP_Data, long RefTime, char TS_AP_Suppid,
                        TNK1X_t *tnkrecA, TNK1X_t *tnkrecB, char GRACE_id,
                        double Temperature, long TNKbinary, long NewTNKrec);
long EncodeHRTpacket(unsigned char PktId,char SuppPktId, unsigned short AP_Dlen,
                     char *AP_Data, long RefTime, char TS_AP_Suppid,
                     HRT1X_t *hrtrec, char GRACE_id);
void FillTimStruct(char GRACE_id, tm_sc_stat_t *tm_stat,TIM1X_t *tim);
long PickLong(char *buf);
long Pick3Int(char *buf);
short Pick12bitInt(char *buf);
short PickShort(char *buf);
long ReadApplicationPacket(FILE *src,unsigned char *AP_PktId, char *AP_SuppId,
                          unsigned short *AP_Dlen, char *AP_Data);
long WriteApplicationPacket(FILE *dst,unsigned char *AP_PktId, char *AP_SuppId,
                          unsigned short *AP_Dlen, char *AP_Data, short byte_offset);
long roundint(double x);
long FindFirstGPSTag(FILE *src);
long GetParamNdx(char *name,long isat);
long InitHKComputationParameters(char **Param_names, long nparam_names,
                                 long *var_ndx, long *nvar_ndx, long isat);
double raw(double raw_value);
double scale(double raw_value,double scale_factor);
double poly(double raw_value,double *coeff, long ncoeff);
double f2(double raw_value,double arg1, double bias);
double f3(double raw_value,double arg1, double scale);
double f4(double raw_value,double arg1,double arg2, double arg3, double bias);
double f5(double arg1, double arg2);
double f6(double raw_value,double arg1,double arg2, double arg3);
double f7(double arg1, double arg2);
double ExtractValueFromHKTM(long cal_ndx,char *AP_data, long isat);
long GetThrAccumTimeCount(long thr_number, long ipsat, double T2000,
                          double *accum_time, long *accum_count);
long GetSOEParamNdx(char *param_name);
void GetSOEParamName(long isensor, char *param_name);
long GetActiveSensor(char *sensor_name, char *sat_name,
                     double start_time, double end_time,
                     long *NactiveIntervals, double *start_time_interval,
                     double *end_time_interval,
                     double **soe_values, long *nr_soe_values,
                     long nint_max, long nsoe_max);
double TankVolume(double pressure,long TankId, char GRACE_id);
double ComputeGN2GasMass(double presssure, double temperature, double Volume,
                         double He_percentage);
long good_TS_packet(char * pkt);
long good_AP_packet(char * pkt, long Input_Dlen);
long FindNextTimeGoodPkt(FILE *src,unsigned char *AP_PktId, char *AP_SuppId,
                         unsigned short *AP_Dlen, char *AP_Data,FILE *err,
                         unsigned short *ErrorPacketLength);


typedef struct {
                 char parname[1000];
                 double *time;
                 double *rate;
                 long n;
               } massflow_t;

typedef struct {
                 long npar;
                 double mass_used;
                 double obs_weight;
                 double partials_read[MAXPARAM];
                 double reg_pressure;
                 double gas_temperature;
                 double thr_time;
                 double tank_time;
                 double mass_used_prev;
                 double tank_mass_calc;
                 long nwords;
               } massflowreg_t;

long ReadTankRegres(FILE *src,massflowreg_t *record);
long WriteTankRegres(FILE *dst,massflowreg_t *record);
long LoadMassFlowFromTdp(FILE *tdp, massflow_t *massflow, char *tdp_filename);

#endif  /* _GRACEtelemetry_h_ */



