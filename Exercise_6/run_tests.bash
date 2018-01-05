for n in {1..5}
do
	/usr/bin/time --format="%E" ./vm big_loop.b > test; 
done

