/*==============================================================================
 *     File: euclid.c
 *  Created: 11/16/2016, 21:54
 *   Author: Bernie Roesler
 *
 *  Description: 
 *
 *============================================================================*/

/* C program to demonstrate working of extended */
/* Euclidean Algorithm */
#include <stdlib.h>
#include <stdio.h>

/* function declarations */
int gcdExtended(int a, int b, int *x, int *y);
int gcdExtended_mine(int a, int b, int *x, int *y);

/* Driver Program */
int main(int argc, char *argv[])
{
    if (argc < 3)
    {
        printf("Usage: requires 2 arguments.\n");
        exit(-2);
    }

    int a = 0, b = 0;
    sscanf(argv[1], "%d", &a);
    sscanf(argv[2], "%d", &b);
    printf("a = %d, b = %d\n", a, b);

    int x = 0, 
        y = 0,
        g = 0,
        ans = 0;

    /* #<{(| Theirs |)}># */
    /* printf("---------- Theirs: ----------\n"); */
    /* g = gcdExtended(a, b, &x, &y); */
    /* ans = a*x + b*y; */
    /* printf("gcd(%d, %d) = %d, x = %d, y = %d, ans = %d\n", a, b, g, x, y, ans); */

    /* Try mine */
    x = 0; y = 0;
    printf("----------   Mine: ----------\n");
    g = gcdExtended_mine(a, b, &x, &y);
    ans = a*x + b*y;
    printf("gcd(%d, %d) = %d, x = %d, y = %d, ans = %d\n", a, b, g, x, y, ans);

    return 0;
}

/* C function for extended Euclidean Algorithm */
int gcdExtended_mine(int a, int b, int *x, int *y)
{
    /* Base Case */
    if (b == 0)
    {
        *x = 1;
        *y = 0;
        return a;
    }

    int x1 = 0, y1 = 0; /* To store results of recursive call */
    int q = a/b;
    int r = a%b;
    
    /* Print in reverse stack order (x,y) have not yet been updated */
    /* printf("a = %4d; b = %4d; q = %4d; r = %4d\n", a, b, q, r); */

    /* Recursive call: */
    int gcd = gcdExtended_mine(b, r, &x1, &y1);

    /* Update x and y using results of recursive call */
    *x = y1;
    *y = x1 - q * y1;

    /* print in order stack is evaluated */
    printf("a = %4d; b = %4d; q = %4d; r = %4d; x = %4d; y = %4d\n", 
            a, b, q, r, *x, *y);

    return gcd;
}

/* C function for extended Euclidean Algorithm */
int gcdExtended(int a, int b, int *x, int *y)
{
    /* Base Case */
    if (a == 0)
    {
        *x = 0;
        *y = 1;
        return b;
    }

    int x1, y1; /* To store results of recursive call */
    int q = b/a;
    int r = b%a;
    printf("a = %4d; b = %4d; q = %4d; r = %4d\n", a, b, q, r);

    /* Recursive call: */
    int gcd = gcdExtended(b%a, a, &x1, &y1);

    /* Update x and y using results of recursive call */
    *x = y1 - (b/a) * x1;
    *y = x1;

    return gcd;
}

/*==============================================================================
 *============================================================================*/
