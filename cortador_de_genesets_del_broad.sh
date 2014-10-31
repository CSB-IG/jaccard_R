#/bin/bash

for i in $(ls *.txt);
	do 
		sed '2d' $i > "sed_"$i
		#echo "sed_"$i
		#echo $i
	done
	
	# > "new\%%~nxi"
