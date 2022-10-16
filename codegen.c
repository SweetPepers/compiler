#include "rvcc.h"

// 语义分析与代码生成
static void genStmt(Node *Nd);
static void genExpr(Node *Nd);

// 输出文件
static FILE *OutputFile;

// 输出字符串到目标文件并换行
static void printLn(char *Fmt, ...) {
  va_list VA;

  va_start(VA, Fmt);
  vfprintf(OutputFile ,Fmt, VA);
  va_end(VA);

  fprintf(OutputFile, "\n");
}

// 代码段计数
static int count(void) {
  static int I = 1;
  return I++;
}

// 对齐到Align的整数倍
static int alignTo(int N, int Align) {
  // (0,Align]返回Align
  return (N + Align - 1) / Align * Align;
}


// 记录栈深度
static int Depth;

// 用于函数参数的寄存器们
static char *ArgReg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};

// 当前的函数
static Obj *CurrentFn;


// 压栈，将结果临时压入栈中备用
// sp为栈指针，栈反向向下增长，64位下，8个字节为一个单位，所以sp-8
// 当前栈指针的地址就是sp，将a0的值压入栈
// 不使用寄存器存储的原因是因为需要存储的值的数量是变化的。
static void push(void) {
  printLn("  addi sp, sp, -8");
  printLn("  sd a0, 0(sp)");
  Depth++;
}

// 弹栈，将sp指向的地址的值，弹出到a1
static void pop(char *Reg) {
  printLn("  ld %s, 0(sp)", Reg);
  printLn("  addi sp, sp, 8");
  Depth--;
}



// 计算给定节点的绝对地址
// 如果报错，说明节点不在内存中
static void genAddr(Node *Nd) {
  switch (Nd->Kind){
    case ND_VAR:
      if(Nd->Var->IsLocal){
        // 偏移量是相对于fp的    
        printLn("  # 获取局部变量%s的栈内地址为%d(fp)", Nd->Var->Name, Nd->Var->Offset);
        printLn("  addi a0, fp, %d", Nd->Var->Offset);
      }else{ // 全局变量
        printLn("  # 获取全局变量%s的地址", Nd->Var->Name);
        printLn("  la a0, %s", Nd->Var->Name);  // 全局变量存放在符号表中, data段
      }
      return;
    case ND_DEREF:
      // 解引用*
      genExpr(Nd->LHS);
      return;

    default:
      break;
  }

  errorTok(Nd->Tok, "not an lvalue");
}

// 加载a0指向的值
static void load(Type *Ty) {
  if (Ty->Kind == TY_ARRAY)
    return;

  printLn("  # 读取a0中存放的地址, 得到的值存入a0");
  if(Ty->Size == 1){
    printLn("  lb a0, 0(a0)");
  }else{
    printLn("  ld a0, 0(a0)");
  }
}

// 将a0存入栈顶值(为一个地址)
static void store(Type *Ty) {
  pop("a1");
  printLn("  # 将a0的值, 写入到a1中存放的地址");
  if (Ty->Size == 1){
    printLn("  sb a0, 0(a1)");
  }else{
    printLn("  sd a0, 0(a1)");
  }
};

// 生成表达式
static void genExpr(Node *Nd) {
  // .loc 文件编号 行号
  printLn("  .loc 1 %d", Nd->Tok->LineNo);
  // 生成各个根节点
  switch (Nd->Kind) {
  // 加载数字到a0
  case ND_NUM:
    printLn("  li a0, %d", Nd->Val);
    return;
  // 对寄存器取反
  case ND_NEG:
    genExpr(Nd->LHS);
    // neg a0, a0是sub a0, x0, a0的别名, 即a0=0-a0
    printLn("  neg a0, a0");
    // printLn("  sub a0, x0, a0");
    return;
      // 变量
  case ND_VAR:
    // 计算出变量的地址，然后存入a0
    genAddr(Nd);
    // 访问a0地址中存储的数据，存入到a0当中
    load(Nd->Ty);
    return;
  // 赋值
  case ND_ASSIGN:
    // 左部是左值，保存值到的地址
    genAddr(Nd->LHS);
    push();
    // 右部是右值，为表达式的值
    genExpr(Nd->RHS);
    store(Nd->Ty);
    return;
  // 解引用 *   *addr  load addr to a0   then load 0(a0) to a0
  case ND_DEREF:
    genExpr(Nd->LHS);
    load(Nd->Ty);
    return;
  // 取地址
  case ND_ADDR: // &a / & *a
    genAddr(Nd->LHS);
    return;
  // 语句表达式
  case ND_STMT_EXPR:
    for (Node *N = Nd->Body; N; N = N->Next)
      genStmt(N);
    return;
  // 函数调用
  case ND_FUNCALL: {
    // 记录参数个数
    int NArgs = 0;
    // 计算所有参数的值，正向压栈
    for (Node *Arg = Nd->Args; Arg; Arg = Arg->Next) {
      genExpr(Arg);
      push();
      NArgs++;
    }

    // 反向弹栈，a0->参数1，a1->参数2……
    for (int i = NArgs - 1; i >= 0; i--)
      pop(ArgReg[i]);

    // 调用函数
    printLn("  # 调用%s函数", Nd->FuncName);
    printLn("  call %s", Nd->FuncName);
    return;
  }
    return;
  default:
    break;
  }

  // 递归到最右节点
  genExpr(Nd->RHS);
  // 将结果压入栈
  push();
  // 递归到左节点
  genExpr(Nd->LHS);
  // 将结果弹栈到a1
  pop("a1");

  // 生成各个二叉树节点
  switch (Nd->Kind) {
  case ND_ADD: // + a0=a0+a1
    printLn("  add a0, a0, a1");
    return;
  case ND_SUB: // - a0=a0-a1
    printLn("  sub a0, a0, a1");
    return;
  case ND_MUL: // * a0=a0*a1
    printLn("  mul a0, a0, a1");
    return;
  case ND_DIV: // / a0=a0/a1
    printLn("  div a0, a0, a1");
    return;
  case ND_EQ:
  case ND_NE:
    // a0=a0^a1，异或指令
    printLn("  xor a0, a0, a1");

    if (Nd->Kind == ND_EQ)
      // a0==a1
      // a0=a0^a1, sltiu a0, a0, 1
      // 等于0则置1
      printLn("  seqz a0, a0");
    else
      // a0!=a1
      // a0=a0^a1, sltu a0, x0, a0
      // 不等于0则置1
      printLn("  snez a0, a0");
    return;
  case ND_LT:
    printLn("  slt a0, a0, a1");
    return;
  case ND_LE:
    // a0<=a1等价于
    // a0=a1<a0, a0=a1^1
    printLn("  slt a0, a1, a0");
    printLn("  xori a0, a0, 1");
    return;
  default:
    break;
  }

  errorTok(Nd->Tok, "invalid expression");
}

// 生成语句
static void genStmt(Node *Nd) {
  // .loc 文件编号 行号
  printLn("  .loc 1 %d", Nd->Tok->LineNo);
  switch(Nd->Kind){
    // 生成for / while循环语句
    case ND_FOR: {
      // 代码段计数
      int C = count();
      // 生成初始化语句
      if (Nd->Init)
        genStmt(Nd->Init);
      // 输出循环头部标签
      printLn(".L.begin.%d:", C);
      // 处理循环条件语句
      if (Nd->Cond) {
        // 生成条件循环语句
        genExpr(Nd->Cond);
        // 判断结果是否为0，为0则跳转到结束部分
        printLn("  beqz a0, .L.end.%d", C);
      }
      // 生成循环体语句
      genStmt(Nd->Then);
      // 处理循环递增语句
      if (Nd->Inc)
        // 生成循环递增语句
        genExpr(Nd->Inc);
      // 跳转到循环头部
      printLn("  j .L.begin.%d", C);
      // 输出循环尾部标签
      printLn(".L.end.%d:", C);
      return;
    }
    // 生成if语句
    case ND_IF: {
      // 代码段计数
      int C = count();
      // 生成条件内语句
      genExpr(Nd->Cond);
      // 判断结果是否为0，为0则跳转到else标签
      printLn("  beqz a0, .L.else.%d", C);
      // 生成符合条件后的语句
      genStmt(Nd->Then);
      // 执行完后跳转到if语句后面的语句
      printLn("  j .L.end.%d", C);
      // else代码块，else可能为空，故输出标签
      printLn(".L.else.%d:", C);
      // 生成不符合条件后的语句
      if (Nd->Els)
        genStmt(Nd->Els);
      // 结束if语句，继续执行后面的语句
      printLn(".L.end.%d:", C);
      return;
    }
    // 生成代码块，遍历代码块的语句链表
    case ND_BLOCK:
      for (Node *N = Nd->Body; N; N = N->Next)
        genStmt(N);
      return;
    case ND_RETURN:
      genExpr(Nd->LHS);
      // 无条件跳转语句，跳转到.L.return段
      // j offset是 jal x0, offset的别名指令
      printLn("  # 跳转到.L.return.%s段", CurrentFn->Name);
      printLn("  j .L.return.%s", CurrentFn->Name);
      return;
    // 生成表达式语句
    case ND_EXPR_STMT:
      genExpr(Nd->LHS);
      return;
    default:
      break;
  }

  errorTok(Nd->Tok, "invalid statement");
}


// 根据变量的链表计算出偏移量
static void assignLVarOffsets(Obj *Prog) {
  // 为每个函数计算其变量所用的栈空间
  for (Obj *Fn = Prog; Fn; Fn = Fn->Next) {
    if(! Fn->IsFunction) // 不是函数
      continue;
    int Offset = 0;
    // 读取所有变量
    for (Obj *Var = Fn->Locals; Var; Var = Var->Next) {
      // 每个变量分配size字节
      Offset += Var->Ty->Size;
      // 为每个变量赋一个偏移量，或者说是栈中地址
      Var->Offset = -Offset;
    }
    // 将栈对齐到16字节
    Fn->StackSize = alignTo(Offset, 16);
  }
}

void genFun(Obj *Fn){
  printLn("\n  # 定义全局%s段", Fn->Name);
  printLn("  .globl %s", Fn->Name);
  printLn("  .text");  // 后面要有 .data
  printLn("# =====%s段开始===============", Fn->Name);
  printLn("# %s段标签", Fn->Name);
  printLn("%s:", Fn->Name);
  CurrentFn = Fn;

  // 栈布局
  //-------------------------------// sp
  //              ra
  //-------------------------------// ra = sp-8
  //              fp
  //-------------------------------// fp = sp-16
  //             变量
  //-------------------------------// sp = sp-16-StackSize
  //           表达式计算
  //-------------------------------//

  // Prologue, 前言
  // 将ra寄存器压栈,保存ra的值
  printLn("  # 将ra寄存器压栈,保存ra的值");
  printLn("  addi sp, sp, -16"); // 分配两个位置
  printLn("  sd ra, 8(sp)");
  // 将fp压入栈中，保存fp的值
  printLn("  sd fp, 0(sp)");
  // 将sp写入fp
  printLn("  mv fp, sp");

  // 偏移量为实际变量所用的栈大小
  printLn("  addi sp, sp, -%d", Fn->StackSize);

  int I = 0;
  for (Obj *Var = Fn->Params; Var; Var = Var->Next) {
    printLn("  # 将%s寄存器的值存入%s的栈地址", ArgReg[I], Var->Name);
    if(Var->Ty->Size == 1){
      printLn("  sb %s, %d(fp)", ArgReg[I++], Var->Offset);
    }else{
      printLn("  sd %s, %d(fp)", ArgReg[I++], Var->Offset);
    }
  }
  // 生成语句链表的代码
  genStmt(Fn->Body);
  assert(Depth == 0);

  // Epilogue，后语
  // 输出return段标签
  printLn(".L.return.%s:", Fn->Name);
  // 将fp的值改写回sp
  printLn("  mv sp, fp");
  // 将最早fp保存的值弹栈，恢复fp。
  printLn("  ld fp, 0(sp)");
  // 将ra寄存器弹栈,恢复ra的值
  printLn("  # 将ra寄存器弹栈,恢复ra的值");
  printLn("  ld ra, 8(sp)");
  printLn("  addi sp, sp, 16");

  // 返回
  printLn("  ret");
}

// .data 全局变量
static void emitData(Obj *Prog) {
  for (Obj *Var = Prog; Var; Var = Var->Next) {
    if (Var->IsFunction)
      continue;

    printLn("  # 数据段标签");
    printLn("  .data");
    // 判断是否有初始值
    if (Var->InitData) {
      printLn("%s:", Var->Name);
      // 打印出字符串的内容，包括转义字符
      printLn("  # 字符串字面量");
      for (int I = 0; I < Var->Ty->Size; ++I) {
        char C = Var->InitData[I];
        if (isprint(C))
          printLn("  .byte %d\t# %c", C, C);
        else
          printLn("  .byte %d", C);
      }
    } else {
      printLn("  # 全局段%s", Var->Name);
      printLn("  .globl %s", Var->Name);
      printLn("%s:", Var->Name);
      printLn("  # 全局变量零填充%d位", Var->Ty->Size);
      printLn("  .zero %d", Var->Ty->Size);
    }
  }
}

// .test 函数
void emitText(Obj *Prog) {
  // 为每个函数单独生成代码
  for (Obj *Fn = Prog; Fn; Fn = Fn->Next) {
    if(!Fn->IsFunction)
      continue;
    genFun(Fn);
  }
}

// 代码生成入口函数，包含代码块的基础信息
void codegen(Obj *Prog, FILE *out){
  // 设置目标文件的文件流指针
  OutputFile = out;
  // 计算局部变量的偏移量
  assignLVarOffsets(Prog);
  // 生成数据
  emitData(Prog);
  // 生成代码
  emitText(Prog);
}