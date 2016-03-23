#!/bin/bash
#
#	Function
# 
#	  Precise Point Positioning for static, kinematic, dynamic receiver with
#	  SmartPPP software
#
#	Author
#
#	  Shoujian Zhang
#
#	COPYWRIGHT
# 
#	  Copyright(c) 2006- 	School of Geodesy and Geomatics,
#	  					    Wuhan University
#
################################################################################
#
#	while read line
#	do 
##
#		ihpxfile=`echo $line | awk '{print $1}'`
#		isp3file=`echo $line | awk '{print $2}'`
#		iclkfile=`echo $line | awk '{print $3}'`
#		iattfile=`echo $line | awk '{print $4}'`
#		iposfile=`echo $line | awk '{print $5}'`
#		oposfile=`echo $line | awk '{print $6}'`
##
#		SmartPPP -ihpx $ihpxfile -isp3 $isp3file -iclk $iclkfile \
#		         -iatt $iattfile -ipos $iposfile -opos $oposfile < SmartPPP.in 
##
#	done < filelist.ppp1
#
 	SmartPPP -ihpx ../../data/rinex/grace/GPS1B_2008-08-01_A_01.rnx.hp.cln \
 	  		 -isp3 ../../data/ephemeris/2009/COD14905.EPH	\
 			 -iclk ../../data/ephemeris/2009/COD14905.CLK_05S  \
 			 -iatt ../../data/att/GA-OG-1B-SCAATT+JPL-SCA1B_2008-08-01_A_01.asc \
 			 -ipos ../../data/pos/GPS1B_2008-08-01_A_01.pos.spp \
 			 -opos ../../data/pos/GPS1B_2008-08-01_A_01.ppp.new 
#
#	state variation data for least square method, which is optional
#	SmartPPP -ihpx ../../data/rinex/1240/ajac1240.09o.hp.cln \
#	  		 -isp3 ../../data/ephemeris/COD15301.EPH	  	\
#			 -iclk ../../data/ephemeris/COD15301.CLK        \
#			 -iatt ../../data/att/GA-OG-1B-SCAATT+JPL-SCA1B_2008-08-01_A_01.asc \
#			 -ipos ../../data/pos/ajac1240.pos.spp \
# 		     -isvr ../../data/pos/GPS1B_2008-08-01_A_01.gfz.svr \
#			 -opos ../../data/pos/ajac1240.pos.STDL3 < SmartPPP.in 
#
#   END
#
