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
while read line
do
	irnxfile=`echo $line | awk '{print $1}'`
	ornxfile=`echo $line | awk '{print $2}'`
#
	echo $irnxfile $ornxfile
#
	rnxtrs -i $irnxfile -o $ornxfile 
#
#done < filelist.rnxtrs
done < filelist.rnxtrs.europ


#
#	  END
#

