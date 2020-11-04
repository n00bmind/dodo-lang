
struct TypeSpec
{
    enum class Kind
    {
        None,
        Name,
        Func,
        Array,
        Pointer,
        Const,
        Tuple,
    };
    
    SourcePos pos;
    Kind kind;

    union
    {
        String name;
        //...
    };
};


struct Expr
{
    enum class Kind
    {
        None,
        Paren,
        Int,
        Real,
        Str,
        Name,
        Cast,
        Call,
        Index,
        Field,
        Compound,
        Unay,
        Binary,
        Ternary,
        Sizeof,
        Typeof,
        OffsetOf,
    };

    SourcePos pos;
    Kind kind;

    union
    {
        struct
        {
            Expr* expr;
        } paren;
        union
        {
            u64 intValue;
            f64 floatValue;
            String strValue;
        } literal;
        struct
        {
            Expr* expr;
            TokenKind op;
        } unary;
        struct
        {
            Expr* left;
            Expr* right;
            TokenKind op;
        } binary;
        struct
        {
            Expr* cond;
            Expr* thenExpr;
            Expr* elseExpr;
        } ternary;
        struct
        {
            Array<Expr*> args;
            Expr* expr;
        } call;
    };
};


struct StmtList;


struct FuncArgDecl
{
    SourcePos pos;
    String name;
    TypeSpec* type;
};

struct EnumItem
{
    SourcePos pos;
    String name;
    Expr* initValue;
};

struct AggregateItem
{
    SourcePos pos;
    String name;
    TypeSpec* type;
};

struct Decl
{
    enum class Kind
    {
        None,
        Enum,
        Struct,
        Union,
        Var,
        Const,  //?
        Func,
        Import, //?
    };

    SourcePos pos;
    String name;
    Kind kind;

    union
    {
        struct
        {
            Array<EnumItem> items;
            TypeSpec* type;
        } enum_;
        struct
        {
            Array<AggregateItem> items;
        } aggregate;
        struct
        {
            TypeSpec* type;
            Expr* initExpr;
            bool isConst;   //?
        } var;
        struct
        {
            Array<FuncArgDecl> args;
            TypeSpec* returnType;
            // TODO Lambdas?
            // Varargs
            StmtList* body;
        } func;
    };
};


struct ElseIf
{
    StmtList* block;
    Expr* cond;
};

struct Stmt
{
    enum class Kind
    {
        None,
        Decl,
        Assign,
        If,
        While,
        For,
        Switch,
        Break,
        Continue,
        Block,
        Return,
    };

    SourcePos pos;
    Kind kind;

    union
    {
        Decl* decl;
        StmtList* block;
        struct
        {
            Expr* left;
            Expr* right;
            TokenKind op;
        } assign;
        struct
        {
            Array<ElseIf> elseIfs;
            StmtList* thenBlock;
            StmtList* elseBlock;
            Expr* cond;
        } if_;
        struct
        {
            StmtList* block;
            Expr* cond;
        } while_;
        struct
        {
            StmtList* block;
            Expr* range;
            Stmt* init;
            Stmt* next;
        } for_;
    };
};

struct StmtList
{
    SourcePos pos;
    Array<Stmt> stmts;
};

