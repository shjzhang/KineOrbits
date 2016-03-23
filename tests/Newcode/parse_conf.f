c
c   subroutine parse_conf
c
      subroutine parse_conf(conf_file)
c
c=======================================================================
c     ****f* qualicontr/parse_conf
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
      include      '../../include/rinex.conf.h'
      include      '../../include/qualicontr.conf.h'
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
         write(*,*) 'qualicontr/parse_conf.f'
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
               write(*,'(A,A,A)') 
     &                 ' Is the Marker Type :', trim(MARKER_TYPE), ' ? '
               read(*,*) yes
c
               if(trim(yes).NE.'y'.and.trim(yes).NE.'yes')then
                  write(*,*) 'Marker Type is not matching'
                  stop
               endif
c
            elseif(trim(label).EQ.'ARP'        )then
c
               read(line,*) label, ARP_DX, ARP_DY, ARP_DZ
               write(*,'(A, 3(F4.2,x), A)') 
     &                 ' Do the ARP coordinates equal with ',
     &                   ARP_DX, ARP_DY, ARP_DZ, ' ? '
c
               read(*,*) yes
               if(trim(yes).NE.'y'.and.trim(yes).NE.'yes')then
                  write(*,*) 'ARP coordinates are not correct'
                  stop
               endif
c
            elseif(trim(label).EQ.'MASS_CENTER'        )then
c
               read(line,*) label, MAS_DX, MAS_DY, MAS_DZ
               write(*,'(A, 3(F4.2,x), A)') 
     &                 ' Do the mass center coordinates equal with ',
     &                   MAS_DX, MAS_DY, MAS_DZ, ' ? '
c
               read(*,*) yes
               if(trim(yes).NE.'y'.and.trim(yes).NE.'yes')then
                  write(*,*) 'Mass center coordinates are not correct'
                  stop
               endif
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
      if(index(conf_file,'smartppp.conf') .gt. 1)then
      endif
c
c     parse SmartPOD configure file
c     =============================
c
      if(index(conf_file,'smartpod.conf') .gt. 1)then
      endif
c
      return
c
      end
