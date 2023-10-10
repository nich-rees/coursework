#include <stdio.h>
#include <stdint.h>

int main() {
	uint32_t NMAX = 1425640; // will cap out at 4 billion
	uint32_t MMAX = 475213;

	for(uint32_t n = 1; n < NMAX; n += 2) {
		for(uint32_t m = 1; m < MMAX; m += 2) {
			if(((3*m + 1) % n == 0) && ((n*n + 3) % m == 0)){
				printf("((n,m) = (%d,%d)\n",n,m);
			}
		}
	}
	printf("done");
	return 0;
}
