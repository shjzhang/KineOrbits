#!/bin/bash
#
#
#	Funtion
#	=======
#	
#	Kinematic pod for GRACE	
#	
#	author
#	======
#
#	-----------+-----------+---------------------------------------------------
#
#	S.J. Zhang	2008/04/08	build this shell script
#
################################################################################
#
    while read line
    do 
        bin_file=`echo $line | awk '{print $1}'`
        asc_file=`echo $line | awk '{print $2}'`
#
		gps1x2rnx.e -gps1x $bin_file -rnx $asc_file 
		
#
    done < filelist.GPS1B
#
#
#	END
#
