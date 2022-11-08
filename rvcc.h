// 使用POSIX.1标准
// 使用了strndup函数
#define _POSIX_C_SOURCE 200809L

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <strings.h>

//
// 共用头文件，定义了多个文件间共同使用的函数和数据
//

typedef struct Type Type;
typedef struct Token Token;
typedef struct Node Node;
typedef struct Member Member;

//
// 字符串
//

char *format(char *Fmt, ...);
//
// 终结符分析，词法分析
// tokenize.c
// 为每个终结符都设置种类来表示
typedef enum {
  TK_IDENT,   // 标记符, 可以表示变量名 函数名等
  TK_PUNCT,   // 操作符如： + - ; <= == != 
  TK_KEYWORD, // 关键字
  TK_STR,     // 字符串字面量
  TK_NUM,     // 数字
  TK_EOF,     // 文件终止符，即文件的最后
} TokenKind;

// parse.c
// 终结符结构体

struct Token {
  TokenKind Kind; // 种类
  Token *Next;    // 指向下一终结符
  int64_t Val;    // 值
  char *Loc;      // 在解析的字符串内的位置
  int Len;        // 长度
  Type *Ty;       // TK_STR使用
  char *Str;      // 字符串字面量, 包括'\0';

  int LineNo; // 行号
};

// 去除了static用以在多个文件间访问
// 报错函数
void error(char *Fmt, ...);
void errorAt(char *Loc, char *Fmt, ...);
void errorTok(Token *Tok, char *Fmt, ...);
// 判断Token与Str的关系
bool equal(Token *Tok, char *Str);
Token *skip(Token *Tok, char *Str);
bool consume(Token **Rest, Token *Tok, char *Str);

// 词法分析
Token *tokenizeFile(char *Path);

// 指rvcc源文件的某个文件的某一行出了问题，打印出文件名和行号
#define unreachable() error("internal error at %s:%d", __FILE__, __LINE__)

//
// 生成AST（抽象语法树），语法解析
//

// 变量 或 函数
typedef struct Obj Obj;
struct Obj {
  Obj *Next;       // 指向下一对象
  char *Name;      // 变量名
  Type *Ty;        // 变量类型
  bool IsLocal;    // 局部变量还是全局变量

  // 局部变量
  int Offset;      // fp的偏移量

  // 函数或全局变量
  bool IsFunction;   // 是否为函数
  bool IsDefinition; // 是否为函数定义
  bool IsStatic;     // 是否为文件域内的(函数)

  // 全局变量
  char *InitData;

  // 函数
  Obj *Params;     // 形参
  Node *Body;      // 函数体
  Obj *Locals;     // 本地变量
  int StackSize;   // 栈大小
};


// AST的节点种类
typedef enum {
  ND_ADD,       // +
  ND_SUB,       // -
  ND_MUL,       // *
  ND_DIV,       // /
  ND_MOD,       // %
  ND_NEG,       // 负号-
  ND_EQ,        // ==
  ND_NE,        // !=
  ND_LT,        // <
  ND_LE,        // <=
  ND_ASSIGN,    // 赋值 = 
  ND_COMMA,     // , 逗号
  ND_MEMBER,    // . 结构体成员访问
  ND_ADDR,      // 取地址 &
  ND_DEREF,     // 解引用 *  dereference
  ND_NOT,       // !，非
  ND_BITNOT,    // ~, 按位取非
  ND_BITAND,    // &, 按位与
  ND_BITOR,     // |, 按位或
  ND_BITXOR,    // ^, 按位异或
  ND_LOGAND,    // &&，与
  ND_LOGOR,     // ||，或
  ND_RETURN,    // 返回
  ND_IF,        // if
  ND_FOR,       // "for" 或 "while" 循环
  ND_BLOCK,     // {...}, 代码块
  ND_GOTO,      // goto, 直接跳转
  ND_LABEL,     // 标签语句
  ND_FUNCALL,   // 函数调用
  ND_EXPR_STMT, // 表达式语句
  ND_STMT_EXPR, // 语句表达式
  ND_VAR,       // 变量
  ND_NUM,       // 整形
  ND_CAST,      // 类型转换
} NodeKind;

// AST中二叉树节点
struct Node {
  NodeKind Kind; // 节点种类
  Node *Next;    // 下一节点，指代下一语句
  Token *Tok;    // 节点对应的终结符
  Type *Ty;      // 节点中数据的类型

  Node *LHS;     // 左部，left-hand side
  Node *RHS;     // 右部，right-hand side
  
  // "if"语句 或者 "for" 语句
  Node *Cond; // 条件内的表达式
  Node *Then; // 符合条件后的语句
  Node *Els;  // 不符合条件后的语句
  Node *Init; // 初始化语句
  Node *Inc;  // 递增语句

  // "break" 标签
  char *BrkLabel;

  // 代码块 或语句表达式 
  Node *Body;    

  // 结构体成员访问
  Member *Mem;
  
  // 函数调用
  char *FuncName; // 函数名
  Type *FuncType; // 函数类型
  Node *Args;     // 函数参数

  // goto和标签语句
  char *Label;
  char *UniqueLabel;
  Node *GotoNext;

  Obj *Var;       // 存储ND_VAR种类的变量
  int64_t Val;    // 存储ND_NUM种类的值
};

//
// 类型系统
//

typedef enum {
  TY_VOID,    // void类型
  TY_BOOL,    // _Bool布尔类型
  TY_CHAR,    // char字符类型
  TY_SHORT,   // short短整型
  TY_INT,     // int整型
  TY_LONG,    // long整型
  TY_ENUM,    // enum枚举
  TY_PTR,     // 指针
  TY_FUNC,    // 函数
  TY_ARRAY,   // 数组
  TY_STRUCT,  // 结构体
  TY_UNION,   // 联合体
} TypeKind;

struct Type{
  TypeKind Kind;  // 种类
  int Size;       // 大小, sizeof返回值
  int Align;      // 对齐
  
  Type *Base;     // 指向的类型
  Token *Name;    // 类型对应的名称, 变量名 函数名等

  // 数组
  int ArrayLen; // 数组长度, 元素总个数

  // 结构体
  Member *Mems;

  // 函数类型
  Type *ReturnTy; // 函数返回的类型
  Type *Params;   // 形参
  Type *Next;     // 下一类型
};

// 结构体成员
struct Member {
  Member *Next; // 下一成员
  Type *Ty;     // 类型
  Token *Tok;   // 报错信息
  Token *Name;  // 名称
  int Offset;   // 偏移量
};

// 全局变量, 定义在type.c中
extern Type *TyVoid;
extern Type *TyBool;
extern Type *TyChar;
extern Type *TyInt;
extern Type *TyLong;
extern Type *TyShort;

// 判断是否为整型
bool isInteger(Type *TY);
// 复制类型
Type *copyType(Type *Ty);
// 构建一个指针类型，并指向基类
Type *pointerTo(Type *Base);
// 为节点内的所有节点添加类型
void addType(Node *Nd);
// 数组类型
Type *arrayOf(Type *Base, int Len);
// 枚举类型
Type *enumType(void);
// 结构体类型
Type *structType(void);
// 函数类型
Type *funcType(Type *ReturnTy);

// 类型转换，将表达式的值转换为另一种类型
Node *newCast(Node *Expr, Type *Ty);
// 语法解析入口函数
Obj *parse(Token *Tok);

//
// 语义分析与代码生成
//

// 代码生成入口函数
void codegen(Obj *Prog, FILE *out);
// 对齐
int alignTo(int N, int Align);