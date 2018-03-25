/* Run with:
 *
 * frama-c -wp -wp-prover alt-ergo -wp-rte -wp-timeout 10 -wp-verbose 0 find-double.c -then -report
 *
 */

#include <stdbool.h>

#define MAXV 1000000

/*@ predicate doublesInside(integer N, int* a, integer res) =
  @		\exists integer ii; \exists integer jj;
  @			0 <= ii < N && 0 <= jj < N && ii != jj ==> a[ii] == a[jj] == res;
  @*/

/*@ predicate noDoublesInside(integer N, int* a) =
  @		\forall integer ii; \forall integer jj;
  @			0 <= ii < N && 0 <= jj < N && ii != jj ==> a[ii] != a[jj];
  @*/

/*@ predicate doublesFound(integer N, int* a, integer res) =
  @		(res > 0) ? doublesInside(N, a, res) : noDoublesInside(N, a);
  @*/

/*@ requires 1 <= N <= MAXV;
  @	requires \valid(a + (0..N-1));
  @ requires \forall integer p; 0 <= p < N ==> 1 <= a[p] <= MAXV;
  @	ensures  doublesFound(N, a, \result);
  @*/
int findDouble(int N, int a[]) {
	bool f[MAXV];
	/*@ loop invariant	1 <= i <= MAXV+1;
	  @ loop invariant	\forall integer p; 1 <= p < i ==> f[p-1] == false;
	  @ loop assigns	i, f[0..(MAXV-1)];
	  @ loop variant	MAXV - i;
	  @*/
	for (int i = 1; i <= MAXV; ++i) f[i-1] = false;
	/*@ loop invariant	0 <= i <= N;
	  @ loop invariant	\forall integer p; 0 <= p < i ==> f[a[p]-1] != false;
	  @ loop invariant	noDoublesInside(i, a);
	  @ loop assigns	i, f[0..(MAXV-1)];
	  @ loop variant	N - i;
	  @*/
	for (int i = 0; i < N; ++i)
		if (f[a[i]-1]) return a[i]; else f[a[i]-1] = true;
	return 0;
}
