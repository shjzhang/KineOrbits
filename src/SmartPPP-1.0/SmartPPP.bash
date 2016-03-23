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
	  SmartPPP -ihpx ../../data/rinex/grace/GPS1B_2008-08-01_A_01.rnx.hp.cln \
	  		   -isp3 ../../data/ephemeris/2009/COD14905.EPH	\
			   -iclk ../../data/ephemeris/2009/COD14905.CLK  \
			   -iatt ../../data/att/GA-OG-1B-SCAATT+JPL-SCA1B_2008-08-01_A_01.asc \
			   -ipos ../../data/pos/GPS1B_2008-08-01_A_01.pos.spp \
			   -opos ../../data/pos/GPS1B_2008-08-01_A_01.ppp.new < SmartPPP.in 
#
#	  END
#

