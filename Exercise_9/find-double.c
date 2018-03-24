#include <stdbool.h>

#define MAXV 1000000

int findDouble(int N, int a[]) {
	bool f[MAXV];
	for (int i = 1; i <= MAXV; ++i) f[i-1] = false;
	for (int i = 0; i < N; ++i)
		if (f[a[i]-1]) return a[i]; else f[a[i]-1] = true;
	return 0;
}