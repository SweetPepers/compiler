#include "rvcc.h"

int main(int argc, char *argv[])
{
  // printf("%s\n", argv[1]);
  if (argc != 2)
  {
      error("%s: invalid number of arguments", argv[0]);
  }
  // 解析Argv[1]，生成终结符流
  Token *Tok = tokenize(argv[1]);

  // 解析终结符流
  Node *Nd = parse(Tok);

  // 生成代码
  codegen(Nd);

  return 0;
}