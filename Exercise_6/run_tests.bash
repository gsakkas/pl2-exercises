if [ $# -eq 0 ]; then
	for n in {1..5}
	do
		/usr/bin/time --format="%E" ./vm_switch big_loop.b > __test_out__;
	done
else
	for n in {1..5}
	do
		/usr/bin/time --format="%E" ./$1 big_loop.b > __test_out__;
	done
fi
rm -f __test_out__