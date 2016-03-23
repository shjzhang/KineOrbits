c
c   subroutine parse_conf
c
      subroutine parse_conf(conf_file)
c
c=======================================================================
c     ****f* SmartPPP/parse_conf
c
c   FUNCTION   
c   
c     Parse the parameters from the configure files in /etc directory.
c     These parameters will determine the internal processing 
c   
C   ARGUMENTS
C   
c     conf_file character input configure file name
c
c   COPYRIGHT
c
c     Copyright(c) 2006-    Shoujian Zhang
c                           School of Geodesy and Geomatics
c                           Wuhan University.
c     ***
c
C     $Id: parse_conf.f,v 1.0 2009/06/28 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/qualicontr.h'
      include      '../../include/qualicontr.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
      character*100 conf_file
c
      character*100 line
      character*100 label, value, value1, value2, value3 
c
      character     flag, comma
c
      character*3   yes
c
      integer       i, j, k, ios
c
      logical       alive
c
      open(unit=123, file=conf_file, status='old', iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'SmartPPP/parse_conf.f'
         write(*,*) '  open conf file error'
         write(*,*) '  please set correct HOPES_HOME path and '
         write(*,*) '  the parse_conf.f file path'
         stop
      endif
c
c     parse rinex configure file
c     ==========================
c
      if(index(conf_file,'rinex.conf'   ) .gt. 1)then
c
 101     continue
c
         read(123, '(A100)', end=102) line   
c
         flag = line(1:1)
c
         if(flag.NE.'#')then
c
            read(line,*) label, value 
c
            if(    trim(label).EQ.'MARKER_TYPE')then
c
               read(line,*) label, MARKER_TYPE
c
            elseif(trim(label).EQ.'ARP'        )then
c
               read(line,*) label, ARP_DX, ARP_DY, ARP_DZ
c
            elseif(trim(label).EQ.'MASS_CENTER'        )then
c
               read(line,*) label, MAS_DX, MAS_DY, MAS_DZ
c
            endif
c
         endif
c
         goto 101
c
 102     continue
c
      endif
c
c     parse qualicontr configure file
c     ==========================
c
      if(index(conf_file,'qualicontr.conf'   ) .gt. 1)then
c
 201     continue
c
         read(123, '(A100)', end=202) line   
c
         flag = line(1:1)
c
         if(flag.NE.'#')then
c
            read(line,*) label, value 
c
            if(    trim(label).EQ.'ischeme'    )then
c
               read(line,*) label, ischeme
c
            elseif(trim(label).EQ.'min_snr'    )then
c
               read(line,*) label, min_snr
c
            elseif(trim(label).EQ.'min_elv'    )then
c
               read(line,*) label, min_elv
c
            elseif(trim(label).EQ.'snr_or_elv' )then
c
               read(line,*) label, snr_or_elv
c
            elseif(trim(label).EQ.'max_arc_gap')then
c
               read(line,*) label, max_arc_gap
c
            elseif(trim(label).EQ.'min_arc_pnt')then
c
               read(line,*) label, min_arc_pnt
c
            endif
c
         endif
c
         goto 201
c
 202     continue
c
      endif
c
c     parse SmartPPP configure file
c     =============================
c
      if(index(conf_file,'SmartPPP.conf') .gt. 1)then
c
 301     continue
c
         read(123, '(A100)', end=302) line   
c
         flag = line(1:1)
c
         if(flag.NE.'#')then
c
            read(line,*) label, value 
c
            if(    trim(label).EQ.'estimator'    )then
c
               read(line,*) label, estimator
c
            elseif(trim(label).EQ.'observ_model' )then
c
               read(line,*) label, observ_model
c
            elseif(trim(label).EQ.'trop_model' )then
c
               read(line,*) label, trop_model
c
            elseif(trim(label).EQ.'trop_mapp_func' )then
c
               read(line,*) label, trop_mapp_func
c
c           Least square 
c
            elseif(trim(label).EQ.'trop_estmt_intv' )then
c
               read(line,*) label, trop_estmt_intv
c
            elseif(trim(label).EQ.'trop_estmt_func' )then
c
               read(line,*) label, trop_estmt_func
c
c           A-Priori Standard deviation for Kal
c
            elseif(trim(label).EQ.'sigma_pos_kal' )then
c
               read(line,*) label, sigma_pos_kal
c
            elseif(trim(label).EQ.'sigma_vel_kal' )then
c
               read(line,*) label, sigma_vel_kal
c
            elseif(trim(label).EQ.'process_noise_clock' )then
c
               read(line,*) label, process_noise_clock
c
            elseif(trim(label).EQ.'process_noise_trop' )then
c
               read(line,*) label, process_noise_trop
c
            endif
c
         endif
c
         goto 301
c
 302     continue
c
      endif
c
c     parse SmartPOD configure file
c     =============================
c
CZ      if(index(conf_file,'SmartPOD.conf') .gt. 1)then
CZc
CZ 401     continue
CZc
CZ         read(123, '(A100)', end=402) line   
CZc
CZ         flag = line(1:1)
CZc
CZ         if(flag.NE.'#')then
CZc
CZ            read(line,*) label, value 
CZc
CZ            if(    trim(label).EQ.'estimator'    )then
CZc
CZ               read(line,*) label, estimator
CZc
CZc++         Least Square
CZc
CZc           a-priori std: position
CZ            elseif(trim(label).EQ.'sigma_pos_lsq' )then
CZc
CZ               read(line,*) label, sigma_pos_lsq
CZc
CZc           a-priori std: velocity
CZ            elseif(trim(label).EQ.'sigma_vel_lsq' )then
CZc
CZ               read(line,*) label, sigma_vel_lsq
CZc
CZc           a-priori std: atmospheric drag coefficients
CZ            elseif(trim(label).EQ.'sigma_atmcoef_lsq' )then
CZc
CZ               read(line,*) label, sigma_atmcoef_lsq
CZc
CZc           a-priori std: solar radiation pressure scaling coefficients
CZ            elseif(trim(label).EQ.'sigma_srpcoef_lsq' )then
CZc
CZ               read(line,*) label, sigma_srpcoef_lsq
CZc
CZc           a-priori std: emperical accelerations radial
CZ            elseif(trim(label).EQ.'sigma_empaccr_lsq' )then
CZc
CZ               read(line,*) label, sigma_empaccr_lsq
CZc
CZc           a-priori std: emperical accelerations along
CZ            elseif(trim(label).EQ.'sigma_empacca_lsq' )then
CZc
CZ               read(line,*) label, sigma_empacca_lsq
CZc
CZc           a-priori std: emperical accelerations cross
CZ            elseif(trim(label).EQ.'sigma_empaccc_lsq' )then
CZc
CZ               read(line,*) label, sigma_empaccc_lsq
CZc
CZc           auto-correlation time/interval size: emperical accelerations
CZ            elseif(trim(label).EQ.'interv_empacc_lsq' )then
CZc
CZ               read(line,*) label, interv_empacc_lsq
CZc
CZc++         A-Priori Standard deviation for Kal
CZc
CZc           a-priori std: position
CZ            elseif(trim(label).EQ.'sigma_pos_kal' )then
CZc
CZ               read(line,*) label, sigma_pos_kal
CZc
CZc           a-priori std: velocity
CZ            elseif(trim(label).EQ.'sigma_vel_kal' )then
CZc
CZ               read(line,*) label, sigma_vel_kal
CZc
CZc           a-priori std: atmospheric drag coefficients
CZ            elseif(trim(label).EQ.'sigma_atmcoef_kal' )then
CZc
CZ               read(line,*) label, sigma_atmcoef_kal
CZc
CZc           a-priori std: solar radiation pressure scaling coefficients
CZ            elseif(trim(label).EQ.'sigma_srpcoef_kal' )then
CZc
CZ               read(line,*) label, sigma_srpcoef_kal
CZc
CZc           a-priori std: emperical accelerations radial
CZ            elseif(trim(label).EQ.'sigma_empaccr_kal' )then
CZc
CZ               read(line,*) label, sigma_empaccr_kal
CZc
CZc           a-priori std: emperical accelerations along
CZ            elseif(trim(label).EQ.'sigma_empacca_kal' )then
CZc
CZ               read(line,*) label, sigma_empacca_kal
CZc
CZc           a-priori std: emperical accelerations cross
CZ            elseif(trim(label).EQ.'sigma_empaccc_kal' )then
CZc
CZ               read(line,*) label, sigma_empaccc_kal
CZc
CZc           a-priori std: IF bias
CZ            elseif(trim(label).EQ.'sigma_IF_Bias_kal' )then
CZc
CZ               read(line,*) label, sigma_IF_Bias_kal
CZc
CZc           a-priori std: clock error
CZ            elseif(trim(label).EQ.'sigma_clkerr_kal' )then
CZc
CZ               read(line,*) label, sigma_clkerr_kal
CZc
CZc           auto-correlation time/interval size: emperical accelerations
CZ            elseif(trim(label).EQ.'interv_empacc_kal' )then
CZc
CZ               read(line,*) label, interv_empacc_kal
CZc
CZc           auto-correlation time/interval size: clock error
CZ            elseif(trim(label).EQ.'interv_clkerr_kal' )then
CZc
CZ               read(line,*) label, interv_clkerr_kal
CZc
CZ            elseif(trim(label).EQ.'process_noise_empaccr' )then
CZc
CZ               read(line,*) label, process_noise_empaccr
CZc
CZ            elseif(trim(label).EQ.'process_noise_empacca' )then
CZc
CZ               read(line,*) label, process_noise_empacca
CZc
CZ            elseif(trim(label).EQ.'process_noise_empaccc' )then
CZc
CZ               read(line,*) label, process_noise_empaccc
CZc
CZ            elseif(trim(label).EQ.'process_noise_clkerr' )then
CZc
CZ               read(line,*) label, process_noise_clkerr
CZc
CZ            endif
CZc
CZ         endif
CZc
CZ         goto 401
CZc
CZ 402     continue
CZc
CZ      endif
c
      return
c
      end
