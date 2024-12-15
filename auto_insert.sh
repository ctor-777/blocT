
echo length, array_first, time_array, time_list
for i in $(seq 1 $1) 
do
	sleep 1s
	array_first=$(($RANDOM % 2))
	length=$(($RANDOM * 10 % 49000 + 1000))
	echo -ne "test $i/$1\r" >&2
	gcc -o insert -D N=$length insert_test.c
	echo -n  "$length, "
	echo -n  "$array_first, "
	./insert $array_first
	
done
