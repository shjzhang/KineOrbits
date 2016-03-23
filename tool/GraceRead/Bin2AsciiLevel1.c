#define _mk_extern_
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"

static char SccsId[] = "$Id: Bin2AsciiLevel1.c,v 1.41 2006/09/29 18:00:15 glk Exp $";

#define DUMPALL -1

void DumpACC1AF(FILE *src, FILE *dst,long Ndump,long ACC1A_count,long AHK1A_vp,long RmGdel,long ACC1A_ang, long ACC1A_lin,double Acc1aZbias);
void DumpGNV1AF(FILE *src, FILE *dst,long Ndump);
void DumpGFD1XF(FILE *src, FILE *dst,long Ndump,long Ksnr,long  K_phase_only,long Ka_phase_only);
void DumpSCA1AF(FILE *src, FILE *dst,long Ndump);
void DumpACC1BF(FILE *src, FILE *dst,long Ndump);
void DumpGNV1BF(FILE *src, FILE *dst,long Ndump);
void DumpKBR1BF(FILE *src, FILE *dst,long Ndump);
void DumpSCA1BF(FILE *src, FILE *dst,long Ndump);
void DumpSCA1AF(FILE *src, FILE *dst,long Ndump);
void DumpXXXVOF(FILE *src, FILE *dst,long Ndump);
void DumpIOA1BF(FILE *src, FILE *dst,long Ndump);
void DumpOSCFQF(FILE *src, FILE *dst,long Ndump);
void DumpCLK1BF(FILE *src, FILE *dst,long Ndump);
void DumpMAG1XF(FILE *src, FILE *dst,long Ndump);
void DumpTHR1XF(FILE *src, FILE *dst,long Ndump);
void DumpIHK1XF(FILE *src, FILE *dst,long Ndump);
void DumpTNK1XF(FILE *src, FILE *dst,long Ndump, long PresTnk, 
                long TempTnk,long RegTnk, long AdapTnk, long TempRedTnk);
void DumpMAS1XF(FILE *src, FILE *dst,long Ndump, long MassTnk, long MassThr);
void DumpILG1XF(FILE *src, FILE *dst,long Ndump);
void DumpTIM1XF(FILE *src, FILE *dst,long Ndump);
void DumpPCI1AF(FILE *src, FILE *dst,long Ndump);
void DumpHRT1XF(FILE *src, FILE *dst,long Ndump);

long InsideInterval(double time);

double Tfinal,Tstart;

main (int argc, char *argv[])
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of any Level 1 product
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
<-----------------------------------------------------------------------------*/

{
  FILE         *src,*dst;

  FileHeader_t header;

  double       Acc1aZbias;

  char         BinFilename[HEADERMAXCHAR];
  char         AscFilename[HEADERMAXCHAR];
  char         TimeTag[HEADERMAXCHAR];
  char         file_format[HEADERMAXCHAR];
  char         NobsChar[HEADERMAXCHAR];
  char         FileTypeName[HEADERMAXCHAR];
  char         LinkTimeLabel[HEADERMAXCHAR];
  char         NrBytes[HEADERMAXCHAR];

  long         i,ShowHeader,arg_ndx,DumpNrec,AscFileDefined,HeaderOnly;
  long         MassTnk,MassThr,Ksnr;
  long         PresTnk,TempTnk,RegTnk,AdapTnk,TempRedTnk,ACC1A_count,AHK1A_vp;
  long         RmGdel,ACC1A_ang,ACC1A_lin;
  long         Ka_phase_only,K_phase_only;

/*----------------------------------------------------------------------------->
/ check usage
<-----------------------------------------------------------------------------*/

  if (argc < 2)
  {
   fprintf(stderr,
           "\n usage: %s -binfile Level1_Bin_File [-ascfile Level1_Asc_File] [-nohead] [-nrec] [-head_only]\n\n",
           argv[0]);
   fprintf(stderr,"         [-masstnk tnk_id (3=both tanks,0=none)] [-massthr tnk_id (3=both tanks,0=none)]\n");
   fprintf(stderr,"         [-pres_tnk tnk_id] [-temp_tnk tnk_id]\n");
   fprintf(stderr,"         [-reg_tnk tnk_id] [-adap_tnk tnk_id] [-tempred_tnk tnk_id]\n");
   fprintf(stderr,"         [-tstart Tstart] [-tfinal Tfinal] [-acc1a_count] [-acc1a_ang] [-acc1a_lin] [-ahk1a_vp]\n");
   fprintf(stderr,"         [-acc1a_zbias Acc1aZbias (def 0)] [-ksnr] [-ka] [-k] [-rm_gdel]\n");
   fprintf(stderr,"         If -ascfile is not defined then output will go to standard out\n");
   fprintf(stderr,"\n");
   exit(1);
  }

/*----------------------------------------------------------------------------->
/ Intialize Header information and other relevant info
<-----------------------------------------------------------------------------*/

  InitializeHeaders();

  LinkTime(LinkTimeLabel);

  MassTnk = 4;
  arg_ndx = FindOpt(argc,argv, "-masstnk");
  if (arg_ndx != -1) MassTnk  = GetLongArgv(argc,argv,"-masstnk");

  MassThr = 4;
  arg_ndx = FindOpt(argc,argv, "-massthr");
  if (arg_ndx != -1) MassThr  = GetLongArgv(argc,argv,"-massthr");

  PresTnk = 0;
  arg_ndx = FindOpt(argc,argv, "-pres_tnk");
  if (arg_ndx != -1) PresTnk  = GetLongArgv(argc,argv,"-pres_tnk");

  TempTnk = 0;
  arg_ndx = FindOpt(argc,argv, "-temp_tnk");
  if (arg_ndx != -1) TempTnk  = GetLongArgv(argc,argv,"-temp_tnk");

  TempRedTnk = 0;
  arg_ndx = FindOpt(argc,argv, "-tempred_tnk");
  if (arg_ndx != -1) TempRedTnk  = GetLongArgv(argc,argv,"-tempred_tnk");

  RegTnk = 0;
  arg_ndx = FindOpt(argc,argv, "-reg_tnk");
  if (arg_ndx != -1) RegTnk  = GetLongArgv(argc,argv,"-reg_tnk");

  AdapTnk = 0;
  arg_ndx = FindOpt(argc,argv, "-adap_tnk");
  if (arg_ndx != -1) AdapTnk  = GetLongArgv(argc,argv,"-adap_tnk");

  ShowHeader = 1;
  arg_ndx = FindOpt(argc,argv, "-nohead");
  if (arg_ndx != -1) ShowHeader  = 0;

  Ksnr = 0;
  arg_ndx = FindOpt(argc,argv, "-ksnr");
  if (arg_ndx != -1) Ksnr  = 1;

  Ka_phase_only = 0;
  arg_ndx = FindOpt(argc,argv, "-ka");
  if (arg_ndx != -1) Ka_phase_only  = 1;

  K_phase_only = 0;
  arg_ndx = FindOpt(argc,argv, "-k");
  if (arg_ndx != -1) K_phase_only  = 1;

  RmGdel = 0;
  arg_ndx = FindOpt(argc,argv, "-rm_gdel");
  if (arg_ndx != -1) RmGdel  = 1;

  HeaderOnly = 0;
  arg_ndx = FindOpt(argc,argv, "-head_only");
  if (arg_ndx != -1) HeaderOnly  = 1;

  DumpNrec = DUMPALL;
  arg_ndx = FindOpt(argc,argv, "-nrec");
  if (arg_ndx != -1) DumpNrec  = GetLongArgv(argc,argv,"-nrec");

  Tstart = -1.0e32;
  arg_ndx = FindOpt(argc,argv, "-tstart");
  if (arg_ndx != -1) Tstart  = GetDoubleArgv(argc,argv,"-tstart");

  Tfinal = 1.0e32;
  arg_ndx = FindOpt(argc,argv, "-tfinal");
  if (arg_ndx != -1) Tfinal  = GetDoubleArgv(argc,argv,"-tfinal");

  Acc1aZbias = 0.0;
  arg_ndx = FindOpt(argc,argv, "-acc1a_zbias");
  if (arg_ndx != -1) Acc1aZbias  = GetDoubleArgv(argc,argv,"-acc1a_zbias");

  AscFileDefined = 0;
  arg_ndx = FindOpt(argc,argv, "-ascfile");
  if (arg_ndx != -1) AscFileDefined = 1;

  ACC1A_count = 0;
  arg_ndx = FindOpt(argc,argv, "-acc1a_count");
  if (arg_ndx != -1) ACC1A_count = 1;

  ACC1A_ang = 0;
  arg_ndx = FindOpt(argc,argv, "-acc1a_ang");
  if (arg_ndx != -1) ACC1A_ang = 1;

  ACC1A_lin = 0;
  arg_ndx = FindOpt(argc,argv, "-acc1a_lin");
  if (arg_ndx != -1) ACC1A_lin = 1;

  AHK1A_vp = 0;
  arg_ndx = FindOpt(argc,argv, "-ahk1a_vp");
  if (arg_ndx != -1) AHK1A_vp = 1;

/*----------------------------------------------------------------------------->
/ Open files
<-----------------------------------------------------------------------------*/

  strcpy (BinFilename,GetStrArgv(argc,argv,"-binfile"));

  src = fopen(BinFilename,"r");
  if (src == NULL)
  {
   fprintf(stderr," Level 1 Product file  %s cannot be opened !! \n",
           BinFilename);
   exit(1);
  }


  if (AscFileDefined == 1)
  {
    strcpy (AscFilename,GetStrArgv(argc,argv,"-ascfile"));
    dst = fopen(AscFilename,"w");
    if (dst == NULL)
    {
     fprintf(stderr," Level 1 Product file  %s cannot be opened !! \n",
             AscFilename);
     exit(1);
    }
  }
  else
  {
    strcpy(AscFilename,"Standard_Out");
    dst = stdout;
  }

/*----------------------------------------------------------------------------->
/ Read Header information and write updated header to Ascii file
<-----------------------------------------------------------------------------*/

  if (ReadFileHeader(src,&header) == false)
  {   
    fprintf(stderr,"\n Problem reading file header for file %s\n",BinFilename);
    fprintf(stderr," See message above for problem\n\n");
    exit(1);
  }

  GetUTCTimeTag(TimeTag);
  strcat(TimeTag," by ");
  strcat(TimeTag,getenv("LOGNAME"));
 
  header.formattype = 1;
  strcpy(NrBytes,"Not Determined because output is stdout");

  if (!HeaderOnly)
  {
    sprintf(file_format,"%d",header.formattype);
    ModifyFileHeader(&header,GetHeaderLabel("iphFileFormat"),file_format);
    ModifyFileHeader(&header,GetHeaderLabel("iphFileName"),AscFilename);
    ModifyFileHeader(&header,GetHeaderLabel("iphNumberBytes")           , NrBytes);
  }

  if (ShowHeader != 0 ) WriteFileHeader(dst,&header);

  if (HeaderOnly) exit(0);

/*----------------------------------------------------------------------------->
/ Based on header information Dump Records
<-----------------------------------------------------------------------------*/

  if (header.filetype == GetFileType("ipGFD1XF"))  
    DumpGFD1XF(src,dst,DumpNrec,Ksnr,K_phase_only,Ka_phase_only);
  else if (header.filetype == GetFileType("ipACC1AF"))  
    DumpACC1AF(src,dst,DumpNrec,ACC1A_count,AHK1A_vp,RmGdel,ACC1A_ang,ACC1A_lin,Acc1aZbias);
  else if (header.filetype == GetFileType("ipACC1BF"))  
    DumpACC1BF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipGNV1AF"))  
    DumpGNV1AF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipGNV1BF"))  
    DumpGNV1BF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipKBR1BF"))  
    DumpKBR1BF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipSCA1AF"))  
    DumpSCA1AF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipSCA1BF"))  
    DumpSCA1BF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipPCI1AF"))  
    DumpPCI1AF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipXXXVOF"))  
    DumpXXXVOF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipIOA1BF"))  
    DumpIOA1BF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipOSCFQF"))  
    DumpOSCFQF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipMAG1XF"))  
    DumpMAG1XF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipHRT1XF"))  
    DumpHRT1XF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipTHR1XF"))  
    DumpTHR1XF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipIHK1XF"))  
    DumpIHK1XF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipCLK1BF"))  
    DumpCLK1BF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipTNK1XF"))  
    DumpTNK1XF(src,dst,DumpNrec, PresTnk,TempTnk,RegTnk,AdapTnk,TempRedTnk);
  else if (header.filetype == GetFileType("ipMAS1XF"))  
    DumpMAS1XF(src,dst,DumpNrec,MassTnk,MassThr);
  else if (header.filetype == GetFileType("ipILG1XF"))  
    DumpILG1XF(src,dst,DumpNrec);
  else if (header.filetype == GetFileType("ipTIM1XF"))  
    DumpTIM1XF(src,dst,DumpNrec);
  else  
  {  
    GetFileTypeName(header.filetype,FileTypeName);
    if (strcmp(FileTypeName,"NOT_DEFINED") != 0)
    {
      fprintf(stderr,"File Type %s can not be converted into ascii format!!\n\n",
                      FileTypeName);
    }
    else
    {
      fprintf(stderr," FileTypePointer = %d is not defined or filetype pointer refers to Level1A data!!\n",  
                       header.filetype);  
      fprintf(stderr," Check Header of input File!\n\n");  
      exit(1);  
    }
  }

  if (DumpNrec != DUMPALL)
  {
    sprintf(NobsChar,"%d",DumpNrec);
    ModifyFileHeader(&header,GetHeaderLabel("iphNumberObs"), NobsChar);
  }
  GetUTCTimeTag(TimeTag);
  strcat(TimeTag," by ");
  strcat(TimeTag,getenv("LOGNAME"));

  sprintf(NrBytes,"%d",ftell(dst));

  ModifyFileHeader(&header,GetHeaderLabel("iphNumberBytes")           , NrBytes);

  if (ShowHeader != 0 && dst != stdout) WriteFileHeader(dst,&header);

  fclose(src);
  fclose(dst);

  exit(0);

}
void DumpGFD1XF(FILE *src, FILE *dst,long Ndump,long Ksnr,long  K_phase_only,long Ka_phase_only)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of GPS Flight Data Format in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  GFD1X_t record;

  long           ndump;

  unsigned short mask,mask_kbr_phase;

  double         time;
  
  ndump = 1;

  mask = 0;
  SetShortBit(&mask,14);
  SetShortBit(&mask,15);

  mask_kbr_phase = 0;
  if (K_phase_only) SetShortBit(&mask_kbr_phase,12);
  if(Ka_phase_only) SetShortBit(&mask_kbr_phase,13);

  while(ReadGFD1XFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    time = (double) record.rcvtime_intg + 1e-6*(double)record.rcvtime_frac;
    if (InsideInterval(time)) continue;

    if (Ksnr) record.prod_flag = record.prod_flag & mask; 
    if (K_phase_only) record.prod_flag  = record.prod_flag & mask_kbr_phase;
    if (Ka_phase_only) record.prod_flag = record.prod_flag & mask_kbr_phase;

    if (record.prod_flag) WrAsciiGFD1XFRecord(dst,&record); 
    ndump++;
  }
}
void DumpACC1AF(FILE *src, FILE *dst,long Ndump,long ACC1A_count,long AHK1A_vp,
                long RmGdel,long ACC1A_ang, long ACC1A_lin, double Acc1aZbias)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Accelerometer Data level 1A file in file src
/
/ coded by: Gerhard L.H. Kruizinga                07/24/01
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
/        ACC1A_count Dump Time tag and count info only
/        AHK1A_vp    Dump Time tag and vp info only
/        RmGdel do not dump records for which gdel flag is set
/        ACC1A_count Dump Time tag and angular accelerations only
/        Acc1aZbias  Bias to be added to ACC Z-lin measurement when printing
<-----------------------------------------------------------------------------*/
{
  ACC1A_t record;

  long           ndump;

  double         time;

  char           Bits[8],Bits32[32]; 
  
  ndump = 1;

  while(ReadACC1AFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    time = (double) record.rcvtime_intg + 1e-6*(double)record.rcvtime_frac;
    if (InsideInterval(time)) continue;

    if (RmGdel)
    {
      GetCharBits( record.qualflg, Bits );
      if( Bits[1] ) continue;      /* remove record with no Pulse Sync */
      if( Bits[3] ) continue;      /* remove record with invalid timetag */
    }

    if (ACC1A_count)
    {
      record.prod_flag = 0;
      SetLongBit(&record.prod_flag,26L);
      SetLongBit(&record.prod_flag,27L);
      SetLongBit(&record.prod_flag,28L);
    }
    if (ACC1A_ang)
    {
      GetLongBits( record.prod_flag, Bits32 );

      record.prod_flag = 0;

      if (Bits32[3] != 1 || Bits32[4] != 1 || Bits32[5] != 1) 
      {
        continue;
      }
      else
      {
        SetLongBit(&record.prod_flag,3L);
        SetLongBit(&record.prod_flag,4L);
        SetLongBit(&record.prod_flag,5L);
      }
    }
    if (ACC1A_lin)
    {
      GetLongBits( record.prod_flag, Bits32 );

      record.prod_flag = 0;

      if (Bits32[0] != 1 || Bits32[1] != 1 || Bits32[2] != 1) 
      {
        continue;
      }
      else
      {
        SetLongBit(&record.prod_flag,0L);
        SetLongBit(&record.prod_flag,1L);
        SetLongBit(&record.prod_flag,2L);
      }
    }
    if (AHK1A_vp)
    {
      record.prod_flag = 0;
      SetLongBit(&record.prod_flag,6L);
    }

    if (record.prod_flag == 0 && (ACC1A_ang || ACC1A_lin) ) continue;

    if (Acc1aZbias != 0.0) record.lin_accl_z += Acc1aZbias;

    WrAsciiACC1AFRecord(dst,&record);
    ndump++;
  }
}
void DumpACC1BF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Accelerometer Data level 1B file in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  ACC1B_t record;

  long           ndump;
  double         time;
  
  ndump = 1;

  while(ReadACC1BFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    time = (double) record.gps_time;
    if (InsideInterval(time)) continue;
    WrAsciiACC1BFRecord(dst,&record);
    ndump++;
  }
}
void DumpGNV1AF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Navigation Solution Data level 1B file in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  GNV1A_t record;

  long           ndump;
  double         time;
  
  ndump = 1;

  while(ReadGNV1AFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    time = (double) record.rcv_time;
    if (InsideInterval(time)) continue;
    WrAsciiGNV1AFRecord(dst,&record);
    ndump++;
  }
}
void DumpGNV1BF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Navigation Solution Data level 1B file in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  GNV1B_t record;

  long           ndump;
  
  ndump = 1;

  while(ReadGNV1BFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiGNV1BFRecord(dst,&record);
    ndump++;
  }
}
void DumpKBR1BF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of KBR Data level 1B file in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  KBR1B_t record;

  long           ndump;
  
  ndump = 1;

  while(ReadKBR1BFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiKBR1BFRecord(dst,&record);
    ndump++;
  }
}
void DumpSCA1AF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of SCA Data level 1A file in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  SCA1A_t record;

  double  time;

  long           ndump;
  
  ndump = 1;

  while(ReadSCA1AFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    time = (double) record.rcv_time;
    if (InsideInterval(time)) continue;
    WrAsciiSCA1AFRecord(dst,&record);
    ndump++;
  }
}
void DumpSCA1BF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of SCA Data level 1B file in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  SCA1B_t record;

  long           ndump;
  
  ndump = 1;

  while(ReadSCA1BFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiSCA1BFRecord(dst,&record);
    ndump++;
  }
}
void DumpPCI1AF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of PCI Data level 1A file in file src
/
/ coded by: Gerhard L.H. Kruizinga                03/18/03
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  PCI1A_t record;

  long           ndump;

  double         time;
  
  ndump = 1;

  while(ReadPCI1AFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    time = (double) record.gps_time;
    if (InsideInterval(time)) continue;
    WrAsciiPCI1AFRecord(dst,&record);
    ndump++;
  }
}
void DumpXXXVOF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Vector Orientation Data in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  XXXVO_t record;

  long           ndump;
  
  ndump = 1;

  while(ReadXXXVOFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiXXXVOFRecord(dst,&record);
    ndump++;
  }
}
void DumpIOA1BF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of ACC Inertial Orientation Data in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  IOA1B_t record;

  long           ndump;
  
  ndump = 1;

  while(ReadIOA1BFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiIOA1BFRecord(dst,&record);
    ndump++;
  }
}
void DumpOSCFQF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Oscilator Stability Data in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/23/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  OSCFQ_t record;

  long           ndump;
  
  ndump = 1;

  while(ReadOSCFQFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiOSCFQFRecord(dst,&record);
    ndump++;
  }
}
void DumpCLK1BF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Clock conversion from rcv to gps in file src
/
/ coded by: Gerhard L.H. Kruizinga                09/06/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  CLK1B_t  record;

  long           ndump;
  
  ndump = 1;

  while(ReadCLK1BFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiCLK1BFRecord(dst,&record);
    ndump++;
  }
}
void DumpMAG1XF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Magnetic Torquer information in file src
/
/ coded by: Gerhard L.H. Kruizinga                09/06/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  MAG1X_t record;

  long           ndump;
  
  ndump = 1;

  while(ReadMAG1XFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiMAG1XFRecord(dst,&record);
    ndump++;
  }
}
void DumpHRT1XF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Magnetic Torquer information in file src
/
/ coded by: Gerhard L.H. Kruizinga                08/06/04
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  HRT1X_t record;

  long           ndump;
  
  ndump = 1;

  while(ReadHRT1XFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiHRT1XFRecord(dst,&record);
    ndump++;
  }
}
void DumpTHR1XF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Thruster activation information in file src
/
/ coded by: Gerhard L.H. Kruizinga                09/06/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  THR1X_t  record;

  long           ndump;
  
  ndump = 1;

  while(ReadTHR1XFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiTHR1XFRecord(dst,&record);
    ndump++;
  }
}
void DumpIHK1XF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Thruster activation information in file src
/
/ coded by: Gerhard L.H. Kruizinga                09/06/00
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  IHK1X_t  record;

  long           ndump;
  
  ndump = 1;

  while(ReadIHK1XFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiIHK1XFRecord(dst,&record);
    ndump++;
  }
}
void DumpILG1XF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of IPU log message in file src
/
/ coded by: Gerhard L.H. Kruizinga                10/19/01
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  ILG1X_t  record;

  long           ndump;
  
  ndump = 1;

  while(ReadILG1XFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WriteILG1XFRecord(dst,&record);
    ndump++;
  }
}
void DumpTIM1XF(FILE *src, FILE *dst,long Ndump)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of OBDH time mapping to gps time in file src
/
/ coded by: Gerhard L.H. Kruizinga                10/19/01
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
<-----------------------------------------------------------------------------*/
{
  TIM1X_t  record;

  long           ndump;
  
  ndump = 1;

  while(ReadTIM1XFRecord(src,&record) == true)
  {
    if (ndump > Ndump && Ndump != DUMPALL) continue;
    WrAsciiTIM1XFRecord(dst,&record);
    ndump++;
  }
}
void DumpMAS1XF(FILE *src, FILE *dst,long Ndump, long MassTnk, long MassThr)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of SC Mass information in file src
/
/ coded by: Gerhard L.H. Kruizinga                09/24/01
/
/ input:
/        src    pointer to source file to dump contents from
/        dst    pointer to file to dump contents to
/        Ndump  Maximum Number of records to dump
/ DumpOptions:
/ MassTnk  0: do not dump mass derived from tnk obs
/          1: dump mass for tank 1 based on tnk obs
/          2: dump mass for tank 2 based on tnk obs
/          3: dump satellite mass based on tnk obs
/          4: dump all masses based on tnk obs
/ MassThr  0: do not dump mass derived from thr obs
/          1: dump mass for tank 1 based on thr obs
/          2: dump mass for tank 2 based on thr obs
/          3: dump satellite mass based on thr obs
/          4: dump all masses based on thr obs
<-----------------------------------------------------------------------------*/
{
  MAS1X_t  record;

  long           ndump;
  
  ndump = 1;

  while(ReadMAS1XFRecord(src,&record) == true)
  {
    /*>>>> modify prod flag based on MassTnk and MassThr <<<<<*/

    switch (MassTnk)
    {
       case 0:
             UnSetCharBit(&record.prod_flag,2L);
             UnSetCharBit(&record.prod_flag,3L);
             UnSetCharBit(&record.prod_flag,6L);
             UnSetCharBit(&record.prod_flag,7L);
             break;
       case 1:
             UnSetCharBit(&record.prod_flag,2L);
             UnSetCharBit(&record.prod_flag,3L);
             UnSetCharBit(&record.prod_flag,7L);
             break;
       case 2:
             UnSetCharBit(&record.prod_flag,2L);
             UnSetCharBit(&record.prod_flag,3L);
             UnSetCharBit(&record.prod_flag,6L);
             break;
       case 3:
             UnSetCharBit(&record.prod_flag,0L);
             UnSetCharBit(&record.prod_flag,1L);
             UnSetCharBit(&record.prod_flag,4L);
             UnSetCharBit(&record.prod_flag,5L);
             UnSetCharBit(&record.prod_flag,6L);
             UnSetCharBit(&record.prod_flag,7L);
             break;
    }

    switch (MassThr) 
    {
       case 0:
             UnSetCharBit(&record.prod_flag,0L);
             UnSetCharBit(&record.prod_flag,1L);
             UnSetCharBit(&record.prod_flag,4L);
             UnSetCharBit(&record.prod_flag,5L);
             break;
       case 1:
             UnSetCharBit(&record.prod_flag,0L);
             UnSetCharBit(&record.prod_flag,1L);
             UnSetCharBit(&record.prod_flag,5L);
             break;
       case 2:
             UnSetCharBit(&record.prod_flag,0L);
             UnSetCharBit(&record.prod_flag,1L);
             UnSetCharBit(&record.prod_flag,4L);
             break;
       case 3:
             UnSetCharBit(&record.prod_flag,2L);
             UnSetCharBit(&record.prod_flag,3L);
             UnSetCharBit(&record.prod_flag,4L);
             UnSetCharBit(&record.prod_flag,5L);
             UnSetCharBit(&record.prod_flag,6L);
             UnSetCharBit(&record.prod_flag,7L);
             break;
    }

    if (!record.prod_flag) continue;

    if (ndump > Ndump && Ndump != DUMPALL) continue;

    WrAsciiMAS1XFRecord(dst,&record);

    ndump++;
  }
}
void DumpTNK1XF(FILE *src, FILE *dst,long Ndump, long PresTnk, long TempTnk,
                long RegTnk, long AdapTnk, long TempRedTnk)
/*----------------------------------------------------------------------------->
/ purpose:  Dump Contents of Tank Sensor information in file src
/
/ coded by: Gerhard L.H. Kruizinga                09/06/00
/
/ input:
/        src         pointer to source file to dump contents from
/        dst         pointer to file to dump contents to
/        Ndump       Maximum Number of records to dump
/        PresTnk     Tankid to dump pressure data for
/        TempTnk     Tankid to dump temperature data for
/        RegTnk      Tankid to dump regulator pressure data for
/        AdapTnk     Tankikd to dump Adaptor temperature data for
/        TempRedTnk  Tankid to dump Redundant temperature data for
<-----------------------------------------------------------------------------*/
{
  TNK1X_t  record;

  long           ndump,tank_id;

  char           number[10];
  
  ndump = 1;

  number[1] = '\0';

  while(ReadTNK1XFRecord(src,&record) == true)
  {
    number[0] = record.tank_id;
    sscanf(number,"%d",&tank_id);
    if (PresTnk == 0 && TempTnk == 0 &&  RegTnk == 0 &&
        AdapTnk == 0 && TempRedTnk == 0)
    {
      ndump++;
    }
    else
    {
      if (PresTnk != tank_id && TempTnk != tank_id && 
          RegTnk != tank_id  && AdapTnk != tank_id && TempRedTnk != tank_id) 
      {
        record.prod_flag = 0;
      }
      else
      {
        if (!PresTnk || PresTnk != tank_id) 
        {
          UnSetCharBit(&record.prod_flag,0L);
        }
 
        if (!RegTnk || RegTnk != tank_id) 
        {
          UnSetCharBit(&record.prod_flag,1L);
        }

        if (!TempTnk || TempTnk != tank_id)
        {
          UnSetCharBit(&record.prod_flag,2L);
        }

        if (!TempRedTnk || TempRedTnk != tank_id) 
        {
          UnSetCharBit(&record.prod_flag,3L);
        }

        if (!AdapTnk || AdapTnk != tank_id) 
        {
          UnSetCharBit(&record.prod_flag,4L);
        }
      }

      ndump++;
    }

    if (!record.prod_flag) continue;

    if (ndump > Ndump && Ndump != DUMPALL) continue;

    WrAsciiTNK1XFRecord(dst,&record);

  }
}
long InsideInterval(double time)
/*----------------------------------------------------------------------------->
/ purpose:  Return if time is within Tstart and Tfinal
/
/ coded by: Gerhard L.H. Kruizinga                02/18/03
/
/ return 0L within interval
/       -1L prior to interval
/        1L post interval
<-----------------------------------------------------------------------------*/
{
  if (time < Tstart) return -1L;
  if (time > Tfinal) return 1L;
  return 0L;
}
