#include <stdio.h>
#include <stdint.h>

int main() {
	uint16_t n = 1;
	uint64_t two = 2;
	uint16_t m = 1;
	uint64_t three = 3;

	int64_t diff;

	while (n < 64) {
		diff = two - three;
		//printf("(n,m) = (%d,%d): %d - %d = %d\n",n,m,two,three,diff);
		if (diff == 7) {
			printf("nice\n");
			printf("(n,m) = (%d,%d): %d - %d = %d\n",n,m,two,three,diff);
			two = two*2;
			n++;
		} else if (diff > 7) {
			three = three*3;
			m++;
		} else {
			two = two*2;
			n++;
		}
	}


	uint64_t pow = 16;

	for (int i = 5; i<64; i++) {
		pow = pow*2;
		printf("2^%d mod 243 = %d\n", i, pow % 243);
	}
	printf("done");
	return 0;
}
