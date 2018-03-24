/* Run with:
 *
 * frama-c -wp -wp-prover alt-ergo -wp-rte -wp-timeout 10 -wp-verbose 0 find-double.c -then -report
 *
 */

#include <stdbool.h>

#define MAXV 1000000

/*@ predicate foundDoubles{L}(integer N, int* a) =
  @		\exists integer i; \exists integer j; 0 <= i < N && 0 <= j < N && i != j ==> a[i] == a[j];
  @*/

/*@ logic integer check_if_exists{L}(integer N, int* a, int d) =
  @		(N == 0) ? 0 :
  @			(a[0] == d) ? 1 : check_if_exists(N-1, a+1, d);
  @*/

/*@ logic integer doubles{L}(integer N, int* a) =
  @		(N <= 1) ? 0 :
  @  		(check_if_exists(N-1, a+1, a[0]) == 1) ?
  @				a[0] : doubles(N-1, a+1);
  @*/

/*@ requires 1 <= N <= MAXV;
  @	requires \valid(a + (0..N-1));
  @ requires \forall integer p; 0 <= p < N ==> 1 <= a[p] <= MAXV;
  @	ensures \result == doubles(N, a);
  @*/
int findDouble(int N, int a[]) {
	bool f[MAXV];
	/*@ loop invariant	1 <= i <= MAXV+1;
	  @ loop assigns	i, f[0..(MAXV-1)];
	  @ loop variant	MAXV - i;
	  @*/
	for (int i = 1; i <= MAXV; ++i) f[i-1] = false;
	/*@ loop invariant	0 <= i <= N;
	  @ loop invariant	\forall integer p; 0 <= p < i ==> f[a[p]-1] != false;
	  @ loop invariant	!foundDoubles(i, a);
	  @ loop assigns	i, f[0..(MAXV-1)];
	  @ loop variant	N - i;
	  @*/
	for (int i = 0; i < N; ++i)
		if (f[a[i]-1]) return a[i]; else f[a[i]-1] = true;
	return 0;
}

/*@ lemma doubs_in_empty:
  @		\forall int* p; \valid(p) ==> !foundDoubles(0, p);
  @*/

/*@ lemma doubles_in_empty:
  @		\forall int* p; \valid(p) ==> doubles(0, p) == 0;
  @*/

/*@ lemma doubles_in_one_elem:
  @		\forall int* p; \valid(p) ==> doubles(1, p) == 0;
  @*/