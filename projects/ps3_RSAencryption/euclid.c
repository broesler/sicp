/*==============================================================================
 *     File: euclid.c
 *  Created: 11/16/2016, 21:54
 *   Author: Bernie Roesler
 *
 *  Description: 
 *
 *============================================================================*/

// C program to demonstrate working of extended
// Euclidean Algorithm
#include <stdio.h>

// function declarations
int gcdExtended(int a, int b, int *x, int *y);

// Driver Program
int main()
{
	int x, y;
	int a = 27, b = 4;
	int g = gcdExtended(a, b, &x, &y);
	int ans = a*x + b*y;
	printf("gcd(%d, %d) = %d, x = %d, y = %d, ans = %d\n",
		a, b, g, x, y, ans);
	return 0;
}

// C function for extended Euclidean Algorithm
int gcdExtended(int a, int b, int *x, int *y)
{
	// Base Case
	if (a == 0)
	{
		*x = 0;
		*y = 1;
		return b;
	}

	int x1, y1; // To store results of recursive call
	int q = b/a;
	int r = b%a;
	printf("a = %4d; b = %4d; q = %4d; r = %4d; x1 = %6d; y1 = %6d\n", a, b, q, r, x1, y1);
	int gcd = gcdExtended(b%a, a, &x1, &y1);

	// Update x and y using results of recursive
	// call
	*x = y1 - (b/a) * x1;
	*y = x1;

	return gcd;
}

/* // C function for extended Euclidean Algorithm */
/* int gcdExtended(int a, int b, int *x, int *y) */
/* { */
/* 	// Base Case */
/* 	if (a == 0) */
/* 	{ */
/* 		*x = 0; */
/* 		*y = 1; */
/* 		return b; */
/* 	} */
/*  */
/* 	int x1, y1; // To store results of recursive call */
/* 	int q = b/a; */
/* 	int r = b%a; */
/* 	printf("a = %4d; b = %4d; q = %4d; r = %4d; x1 = %6d; y1 = %6d\n", a, b, q, r, x1, y1); */
/* 	int gcd = gcdExtended(b%a, a, &x1, &y1); */
/*  */
/* 	// Update x and y using results of recursive */
/* 	// call */
/* 	*x = y1 - (b/a) * x1; */
/* 	*y = x1; */
/*  */
/* 	return gcd; */
/* } */

/*==============================================================================
 *============================================================================*/
