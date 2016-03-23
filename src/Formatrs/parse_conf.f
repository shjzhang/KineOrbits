c
c   subroutine parse_conf
c
      subroutine parse_conf(conf_file)
c
c=======================================================================
c     ****f* fmttrs/parse_conf
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
c     include      '../../include/smartppp.conf.h'
c     include      '../../include/smartpod.conf.h'
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
         write(*,*) 'fmttrs/parse_conf.f'
         write(*,*) 'open conf file error'
         stop
      endif
c
c     parse rinex configure file
c     ==========================
c
      if(index(conf_file,'rinex.conf'   ) .gt. 1)then
c
100      continue
c
         read(123, '(A100)', end=200) line   
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
c              write(*,'(A,A,A)') 
c    &                 ' Is the Marker Type :', trim(MARKER_TYPE), ' ? '
c              read(*,*) yes
c
c              if(trim(yes).NE.'y'.and.trim(yes).NE.'yes')then
c                 write(*,*) 'Marker Type is not matching'
c                 stop
c              endif
c
            elseif(trim(label).EQ.'ARP'        )then
c
               read(line,*) label, ARP_DX, ARP_DY, ARP_DZ
c              write(*,'(A, 3(F4.2,x), A)') 
c    &                 ' Do the ARP coordinates equal with ',
c    &                   ARP_DX, ARP_DY, ARP_DZ, ' ? '
c
c              read(*,*) yes
c              if(trim(yes).NE.'y'.and.trim(yes).NE.'yes')then
c                 write(*,*) 'ARP coordinates are not correct'
c                 stop
c              endif
c
            elseif(trim(label).EQ.'MASS_CENTER'        )then
c
               read(line,*) label, MAS_DX, MAS_DY, MAS_DZ
c              write(*,'(A, 3(F4.2,x), A)') 
c    &                 ' Do the mass center coordinates equal with ',
c    &                   MAS_DX, MAS_DY, MAS_DZ, ' ? '
c
c              read(*,*) yes
c              if(trim(yes).NE.'y'.and.trim(yes).NE.'yes')then
c                 write(*,*) 'Mass center coordinates are not correct'
c                 stop
c              endif
c
            endif
c
         endif
c
         goto 100
c
200      continue
c
      endif
c
c     parse qualicontr1 configure file
c     ==========================
c
      if(index(conf_file,'qualicontr1.conf'   ) .gt. 1)then
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
