/* @(#) GRACEfiletype.h      1.14 08/14/04 */

/* note: everytime a filetype or header label is added, the following two
   definitions for NRFILETYPES and NRHEADERLABES must be altered to reflect
   the total amount of file types and/or header labels
*/

#ifndef _GRACEfiletype_h_ 
#define _GRACEfiletype_h_ 

#define NRFILETYPES    26
#define NRHEADERLABELS 19
#define NRACC1APODS    58

static char *FileTypeName[] = { "ipGFD1XF", "ipGNV1AF", "ipSCA1AF", "ipACC1AF", 
                                "ipGNV1BF", "ipSCA1BF", "ipKBR1BF", "ipACC1BF", 
                                "ipXXXVOF", "ipIOA1BF", "ipOSCFQF", "ipGPS1AF", 
                                "ipGPS1BF", "ipKBR1AF", "ipMAG1XF", "ipTHR1XF", 
                                "ipCLK1BF", "ipTNK1XF", "ipAHK1XF", "ipIHK1XF",
                                "ipCLK1AF", "ipILG1XF", "ipMAS1XF", "ipTIM1XF", 
                                "ipPCI1AF", "ipHRT1XF" };

static char *HeaderLabelName[] = {"iphProducerAgency", "iphProducerInstitution", 
                                  "iphFileType","iphFileFormat","iphHeadNrecord", 
                                  "iphSoftwareVersion", "iphSoftwareLinkTime",
                                  "iphDocumentation", "iphSatelliteName", 
                                  "iphSensorName", "iphTimeEpoch", 
                                  "iphTimeFirstObs", "iphTimeLastObs", 
                                  "iphNumberObs", 
                                  "iphProductCreateStartTime", 
                                  "iphProductCreateEndTime",
                                  "iphNumberBytes",
                                  "iphFileName","iphProcessLevel"};

static short FileTypePointer[] = { 1,  2,  3,  4, 
                                   5,  6,  7,  8, 
                                   9, 10, 11,  1, 
                                   1,  1, 15, 16, 
                                  17, 18,  4, 20,
                                  17, 21, 22, 23, 
                                  24, 26 };
 
static short HeaderLabelPointer[] = { 0,  1, 
                                      2,  3,  4, 
                                      5,  6, 
                                      7,  8, 
                                      9, 10, 
                                     11, 12, 
                                     13, 
                                     14,
                                     15,
                                     16,
                                     17, 18}; 

static char *Acc1aProdName[] = { "iblin_accl_x", "iblin_accl_y", "iblin_accl_z",
                                 "ibang_accl_x", "ibang_accl_y", "ibang_accl_z",
                                 "ibbias_vol", "ibx1_out", "ibx2_out", "ibx3_out",
                                 "iby1_out", "iby2_out", "ibz1_out", "ibvd",
                                 "ibtesu", "ibtisu", "ibtaicu", "ibtcicu",
                                 "ibvr5picu", "ibv15picu", "ibv15micu", "ibv15psu",
                                 "ibv15msu", "ibv48psu", "ibv48msu","ibstatus",
                                 "ibvolt2aclx_nrm", "ibvolt2acly_nrm", "ibvolt2aclz_nrm",
                                 "ibvolt2acax_nrm", "ibvolt2acay_nrm", "ibvolt2acaz_nrm", 
                                 "ibvolt2aclx_lrm", "ibvolt2acly_lrm", "ibvolt2aclz_lrm",
                                 "ibvolt2acax_lrm", "ibvolt2acay_lrm", "ibvolt2acaz_lrm",
                                 "ibvolt2x1_nrm",   "ibvolt2x2_nrm",   "ibvolt2x3_nrm",
                                 "ibvolt2y1_nrm",   "ibvolt2y2_nrm",   "ibvolt2z_nrm",
                                 "ibvolt2vp_nrm",   "ibvolt2vd_nrm",
                                 "ibvolt2x1_lrm",   "ibvolt2x2_lrm",   "ibvolt2x3_lrm",
                                 "ibvolt2y1_lrm",   "ibvolt2y2_lrm",   "ibvolt2z_lrm",
                                 "ibvolt2vp_lrm",   "ibvolt2vd_lrm",
                                 "ibvolt2tisu_nrm",   "ibvolt2tesu_nrm",
                                 "ibvolt2tisu_lrm",   "ibvolt2tesu_lrm"};

static short Acc1aProdPointer[] = {  0,  1,  2, 
                                     3,  4,  5,
                                     6,  7,  8, 9,
                                    10, 11, 12, 13,
                                    14, 15, 16, 17,
                                    18, 19, 20, 21,
                                    22, 23, 24, 25,
                                    26, 27, 28,
                                    29, 30, 31, 
                                    32, 33, 34,
                                    35, 36, 37,
                                    38, 39, 40,
                                    41, 42, 43,
                                    44, 45,
                                    46, 47, 48,
                                    49, 50, 51,
                                    52, 53,
                                    54, 55,
                                    56, 57};

#endif  /* _GRACEfiletype_h_ */

