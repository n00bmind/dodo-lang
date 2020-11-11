struct Expr;

struct TypeSpec
{
    enum Kind
    {
        None,
        Name,
        Func,
        Array,
        Pointer,
        //String,       // This is basically the same as an array of byte. Could just be an alias?
        //Tuple,      // ?
    };
    
    SourcePos pos;
    TypeSpec* base;
    Kind kind;

    union
    {
        struct
        {
            ::Array<TypeSpec*> args;
            TypeSpec* returnType;
            // TODO 
            //bool hasVarargs;
        } func;
        ::Array<char const*> names;
        Expr* arraySize;
    };
};


struct CompoundField
{
    enum Kind
    {
        Default,
        Name,
        Index,
    };

    SourcePos pos;
    union
    {
        char const* name;
        Expr* index;
    };
    Expr *initValue;
    Kind kind;
};

struct Expr
{
    enum Kind
    {
        None,
        Int,
        Float,
        Str,
        Name,
        Cast,
        Call,
        Index,
        Field,
        Compound,
        Unary,
        Binary,
        Ternary,
        Sizeof,
        //Typeof,
        //OffsetOf,
    };

    SourcePos pos;
    Kind kind;

    union
    {
        char const* name;
        struct
        {
            Expr* expr;
        } paren;
        struct
        {
            union
            {
                u64 intValue;
                f64 floatValue;
                String strValue;
            };
            Token::LiteralMod modifier;
        } literal;
        struct
        {
            Expr* expr;
            TokenKind::Enum op;
        } unary;
        struct
        {
            Expr* left;
            Expr* right;
            TokenKind::Enum op;
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
            Expr* func;
        } call;
        struct
        {
            Expr* base;
            Expr* index;
        } index;
        struct
        {
            Expr* base;
            char const* name;
        } field;
        struct
        {
            Expr* expr;
        } sizeof_;
        struct
        {
            Array<CompoundField> fields;
        } compound;
    };
};


struct StmtList;


struct FuncArg
{
    SourcePos pos;
    char const* name;
    TypeSpec* type;
};

struct EnumItem
{
    SourcePos pos;
    char const* name;
    Expr* initValue;
    u32 index;
};

struct Decl;

struct AggregateItem
{
    enum Kind
    {
        Field,
        SubAggregate,
    };

    SourcePos pos;
    Kind kind;
    union
    {
        struct
        {
            Array<char const*> names;
            TypeSpec* type;
        };
        // TODO Make this just a normal aggregate Decl with an 'owner' pointing to the enclosing aggregate?
        Decl* subAggregate;
    };
};

struct Decl
{
    enum Kind
    {
        None,
        Enum,
        Struct,
        Union,
        Var,
        //Const,  //?
        Func,
        Import, //?
    };

    SourcePos pos;
    char const* name;
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
            // TODO Lambdas?
            // TODO Varargs
            // TODO Multiple return types
            Array<FuncArg> args;
            TypeSpec* returnType;
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
            TokenKind::Enum op;
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
    // TODO Test with a big enough parsed file what happens when we make Stmt be pointers instead of inline inside the array..
    // Does that increase cache efficiency or reduce it?
    // When each Stmt is a pointer, it will live pretty close in memory to the Expr etc. that form it, however when they're
    // inline they're reallocated at the very end of the StmtList that contains them (to make them all contiguous), so in this
    // case the optimum solution could well be counter intuitive.
    Array<Stmt> stmts;
};

