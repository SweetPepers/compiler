shell 别有事没事加空格

fprintf(stderr, "%*s", Pos, "");   打印了pos個空格
### 4 token流 构造

```c
// 为每个终结符都设置种类来表示
typedef enum
{
    TK_PUNCT, // 操作符如： + - 
    TK_NUM,   // 数字
    TK_EOF,   // 文件终止符，即文件的最后
} TokenKind;

// 终结符结构体
typedef struct Token Token;
struct Token
{
    TokenKind Kind; // 种类
    Token *Next;    // 指向下一终结符
    int Val;        // 值
    char *Loc;      // 在解析的字符串内的位置
    int Len;        // 长度
};

```
跳过空格啥的

### 5 支持 * / () 也就是优先级
简单的加减乘除的抽象语法树  加上括号  表示优先级
![](./picture/%E5%9B%9B%E5%88%99%E8%BF%90%E7%AE%97%E8%AF%AD%E6%B3%95%E6%A0%91.jpg)

```c
// AST节点种类
typedef enum {
  ND_ADD, // +
  ND_SUB, // -
  ND_MUL, // *
  ND_DIV, // /
  ND_NUM, // (整型 int)  数字
} NodeKind;


// AST中二叉树节点
typedef struct Node Node;
struct Node {
  NodeKind Kind; // 节点种类
  Node *LHS;     // 左部，left-hand side
  Node *RHS;     // 右部，right-hand side
  int Val;       // 存储ND_NUM种类的值
};

// expr = mul ("+" mul | "-" mul)*
// mul = primary ("*" primary | "/" primary)*
// primary = "(" expr ")" | num
static Node *expr(Token **Rest, Token *Tok);
static Node *mul(Token **Rest, Token *Tok);
static Node *primary(Token **Rest, Token *Tok);

static void genExpr(Node *Nd) 
// 递归将最右节点入栈  解析完左子树之后弹出
```

### 6 一元运算符
一元运算符优先级高于乘除

```c
// expr = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-") unary | primary
// primary = "(" expr ")" | num
```

### 7 == != > < >= <=
优先级
```c

// 优先级
// expr = equality
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-") unary | primary
// primary = "(" expr ")" | num
```

判断操作符占几个字节(1 or 2)
```c
// 判断Str是否以SubStr开头
static bool startsWith(char *Str, char *SubStr) {
  // 比较LHS和RHS的N个字符是否相等
  return strncmp(Str, SubStr, strlen(SubStr)) == 0;
}

// 读取操作符
static int readPunct(char *Ptr) {
  // 判断2字节的操作符
  if (startsWith(Ptr, "==") || startsWith(Ptr, "!=") || startsWith(Ptr, "<=") ||
      startsWith(Ptr, ">="))
    return 2;

  // 判断1字节的操作符
  return ispunct(*Ptr) ? 1 : 0;
}
```


### 8 代码重构, 将main分割为多个文件
- `codegen.c` 语义分析与代码生成  通过栈操作解析语法树生成代码
- `parse.c`   生成AST, 根据token序列生成抽象语法树   语法分析
- `tokenize.c`将输入字符串解析为一个一个token   词法分析


### 9 支持;分隔语句

- rvcc.h 里面
添加NodeKind::ND_EXPR_STMT, // 表达式语句 : 表示是一个语句

- parse.h 添加新语法规则
语法规则
```c
// program = stmt*
// stmt = exprStmt
// exprStmt = expr ";"
// expr = equality
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-") unary | primary
// primary = "(" expr ")" | num

```
program由多个表达式语句构成, 采用链表存储多个语句
```c
struct Node {
  // new!!
  Node *Next;    // 下一节点，指代下一语句
};
```
- codegen.c
生成多个语句, 为每个语句生成代码
