if [ $# -eq 0 ]; then
	for n in {1..5}
	do
		/usr/bin/time --format="%E" ./vm_switch big_loop.b > test;
	done
else
	for n in {1..5}
	do
		/usr/bin/time --format="%E" ./$1 big_loop.b > test;
	done
fi