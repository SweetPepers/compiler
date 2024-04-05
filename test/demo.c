#include <stdlib.h>
#include <stdio.h>

int main(){
  int a = 5;
  int b = 0;
  b = a + a--;
  printf("i + i-- : %d\n", b); // 9
  a = 5;
  b = a + --a;
  printf("i + --i : %d\n", b); // 8
  a = 5;
  b =  a-- + a;
  printf("i-- + i : %d\n", b); // 9
  a = 5;
  b = --a + a;
  printf("--i + i : %d\n", b); // 8
  a = 5;
  b = --a + --a;
  printf("--i + --i : %d\n", b); // 6

  int i=2; 
  printf("%d\n",  i + ++i * i++); // 16 

}

/*  
clang
i + i-- : 10
i + --i : 9
i-- + i : 9
--i + i : 8
--i + --i : 7
11
*/

/* 
gcc
i + i-- : 9
i + --i : 8
i-- + i : 9
--i + i : 8
--i + --i : 6
16
*/