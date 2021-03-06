// TODO 
// TODO Use 32 bit handles instead of pointers for any nodes allocated in the permanent arena
// TODO Each handle is just an offset from the beginning of the arena (plus maybe some flags) so it can be resolved at any time
// TODO If we decide to move AST nodes to their own pool, we can simply change it to be the index in the pool, which would be even better
// TODO (same for Symbols and Types)
// TODO 
struct Expr;
struct TypeSpec;

struct FuncArgSpec
{
    TypeSpec* type;
    Expr* defaultValue;
    bool isVararg;
};

struct TypeSpec
{
    enum Kind
    {
        None = 0,
        Name,
        Func,
        Array,
        Pointer,
        //String,       // This is basically the same as an array view of byte. Could just be an alias?
        //Tuple,      // ?
    };
    
    SourcePos pos;
    //Type* resolvedType;
    Kind kind;

    union
    {
        struct
        {
            ::Array<FuncArgSpec> args;
            ::Array<TypeSpec*> returnTypes;
        } func;
        struct
        {
            TypeSpec* base;
            Expr* length;        // Can be null
            bool isView;
        } array;
        struct
        {
            TypeSpec* base;
        } ptr;
        ::Array<char const*> names;
    };
};


struct ConstValue
{
    // TODO Array of T? 'Complex' types?
    union
    {
        String strValue = {};
        uintptr_t ptrValue;
        f64 floatValue;
        i64 intValue;
        u64 bitsValue;
    };

    bool BoolValue() const { return bitsValue != 0ull; }
};

struct Type;
struct Symbol;

struct ResolvedExpr
{
    Type* type;
    ConstValue constValue;
    bool isLvalue;
    bool isConst;
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

struct ArgExpr
{
    Expr* expr;
    Expr* nameExpr;   // Optional
};

struct Decl;

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
        Comma,
        Range,
        Lambda,
        Sizeof,     //?
        //Typeof,
        //OffsetOf,
    };

    // Annotated type from the result of the resolve
    ResolvedExpr resolvedExpr;
    SourcePos pos;
    Kind kind;

    union
    {
        struct
        {
            Expr* cond;
            Expr* thenExpr;
            Expr* elseExpr;
        } ternary;
        struct
        {
            Expr* left;
            Expr* right;
            TokenKind::Enum op;
        } binary;
        struct
        {
            Expr* expr;
            TokenKind::Enum op;
        } unary;
        Array<Expr*> commaExprs;

        struct
        {
            Array<ArgExpr> args;
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
            bool isMeta;
        } field;
        Array<CompoundField> compoundFields;
        struct
        {
            TypeSpec* type;
            Expr* expr;
        } cast;

        struct
        {
            union
            {
                String strValue;
                i64 intValue;
                f64 floatValue;
            };
            Token::LiteralMod modifier;
        } literal;
        struct
        {
            char const* ident;
            // Annotated symbol from the resolution phase
            Symbol* symbol;
        } name;

        Expr* parenExpr;
        Expr* sizeofExpr;
        struct
        {
            Expr* lowerBound;     // Inclusive
            Expr* upperBound;     // Exclusive, null when iterating over an iterable
        } range;
        struct
        {
            Decl* decl;
        } lambda;
    };
};


struct Stmt;

struct StmtList
{
    SourcePos pos;
    // TODO Test with a big enough parsed file what happens when we make Stmt be pointers instead of inline inside the array..
    // Does that increase cache efficiency or reduce it?
    // When each Stmt is a pointer, it will live pretty close in memory to the Expr/Decl etc. that it is formed of, however when they're
    // inline they're reallocated at the very end of the StmtList that contains them (to make them all contiguous), so in this
    // case the optimum solution could well be counter intuitive.
    Array<Stmt*> stmts;
    String globalPath;
    char const* name;
    StmtList* parent;
    i32 childCount;
};


struct DirectiveArg
{
    SourcePos pos;
    char const* name;
    Expr* expr;
};

struct NodeDirective
{
    SourcePos pos;
    char const* name;
    Array<DirectiveArg> args;
};

struct Node
{
    enum Flags
    {
        None = 0,
        SkipCodegen = 0x1,
    };

    Array<NodeDirective> directives;
    SourcePos pos;
    StmtList* parentBlock;
    u32 flags;
};


struct FuncArgDecl
{
    SourcePos pos;
    char const* name;
    TypeSpec* type;
    Expr* defaultValue;
    bool isVararg;
};

struct EnumItem
{
    SourcePos pos;
    char const* name;
    Expr* initExpr;
};

struct Decl : public Node
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

    // Annotated type from the result of the resolve
    Type* resolvedType;

    Array<char const*> names;
    Kind kind;

    union
    {
        struct
        {
            Array<FuncArgDecl> args;
            Array<TypeSpec*> returnTypes;
            StmtList* body;
        } func;

        struct
        {
            Array<EnumItem> items;
            TypeSpec* type;
        } enum_;
        struct
        {
            Array<Decl*> items;
        } aggregate;

        struct
        {
            // NOTE Synthetic variables may not have a type spec
            TypeSpec* type;
            Expr* initExpr;
            bool isConst;
        } var;
    };
};


struct ElseIf
{
    StmtList* block;
    Expr* cond;
};

struct SwitchCase
{
    StmtList* block;
    // TODO Ranges (create a Range expr)
    Expr* expr;         // Default case has null expr
    bool isDefault;
};

struct Stmt : public Node
{
    enum Kind
    {
        None,
        Expr,
        Decl,
        Assign,
        If,
        While,
        For,
        Switch,
        Break,
        Continue,
        Return,
        Block,
    };

    Kind kind;

    union
    {
        struct
        {
            Array<ElseIf> elseIfs;
            StmtList* thenBlock;
            StmtList* elseBlock;
            ::Expr* cond;
        } if_;
        struct
        {
            StmtList* block;
            ::Expr* cond;
            bool isDoWhile;
        } while_;
        struct
        {
            StmtList* block;
            char const* indexName;
            ::Expr* rangeExpr;
        } for_;
        struct
        {
            Array<SwitchCase> cases;
            ::Expr* expr;
        } switch_;

        struct
        {
            ::Expr* left;
            ::Expr* right;
            TokenKind::Enum op;
        } assign;

        ::Decl* decl;
        ::Expr* expr;
        StmtList* block;
    };
};

