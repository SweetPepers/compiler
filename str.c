#include "rvcc.h"

// 字符串数组
// typedef struct {
//   char **Data;  // 数据内容
//   int Capacity; // 能容纳字符串的容量
//   int Len;      // 当前字符串的数量，Len ≤ Capacity
// } StringArray;

// 压入字符串数组
void strArrayPush(StringArray *Arr, char *S) {
  // 没有数据
  if (!Arr->Data){
    Arr->Data = calloc(8, sizeof(char*));
    Arr->Len = 0;
    Arr->Capacity = 8;
  }
  // 满了
  if (Arr->Len == Arr->Capacity){
    int Capacity = Arr->Capacity;
    Arr->Data = realloc(Arr->Data, 2*Capacity*sizeof(char*)); // 原来的数据还在
    Arr->Capacity = Capacity*2;
    // 清空新开辟的空间
    for (int I = Arr->Len; I < Arr->Capacity; I++)
      Arr->Data[I] = NULL;
  }

  Arr->Data[Arr->Len++] = S;
}

// 格式化后返回字符串
char *format(char *Fmt, ...) {
  char *Buf;
  size_t BufLen;
  // 将字符串对应的内存作为I/O流
  FILE *Out = open_memstream(&Buf, &BufLen);

  va_list VA;
  va_start(VA, Fmt);
  // 向流中写入数据
  vfprintf(Out, Fmt, VA);
  va_end(VA);

  fclose(Out);
  return Buf;
}