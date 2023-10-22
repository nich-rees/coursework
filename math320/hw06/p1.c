#include <stdio.h>
#include <stdint.h>
#include <math.h>

int main() {
	uint16_t a1 = 1;
	uint16_t b1 = 2;

	double an = a1;
	double bn = b1;

	double diff;
	double guess;

	for(unsigned int n = 1; n < 10; n++) {
		diff = bn - an;
		guess = (b1 - a1)/pow(2,n-1);
		printf("n = %d$, an = %lf, bn = %lf, bn - an = %lf, guess = %lf\n", n, an, bn, diff, guess);
		double temp_a = an;
		an = sqrt(an*bn);
		bn = (an + bn)/2;
	}
	printf("done");
	return 0;
}
