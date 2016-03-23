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
		Bin2AsciiLevel1.e -binfile $bin_file -ascfile $asc_file 
		
#
    done < filelist.GNV1B
#
#
#	END
#
