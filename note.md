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
