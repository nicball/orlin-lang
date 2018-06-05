#include <stdio.h>

long _main(char*, char*);
int main () {
    long re = _main(NULL, NULL);
    printf("%ld\n", re);
    return 0;
}
