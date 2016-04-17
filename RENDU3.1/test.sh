#/bin/sh

FILES="tests/*.cnf"



echo "-------"

for f in $FILES
do
	./resol $f
	echo ""
done

echo "---------"
#echo "MINISAT"
#echo "---------"

#for f in $FILES
#do
#	minisat $f
#	echo ""
#done
