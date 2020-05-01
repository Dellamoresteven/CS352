#include <iostream>
#include <stdlib.h>
#include <stdio.h>
using namespace std;

int x = 2;
namespace outer {
    int x = 10;
    namespace inner {
        int z = x;
    }
}

int main() {
    int a[] = {10,20,30};
    cout << *a+1;
}