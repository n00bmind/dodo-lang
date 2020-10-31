// Crazyballs prepro shit stolen from http://jhnet.co.uk/articles/cpp_magic
//
// NOTE MSVC of course was playing its own game, and so I had to insert DEFERs all along the chain to make this work at all,
// meaning all this surely has zero portability

#define FIRST(a, ...) a
#define SECOND(a, b, ...) b

#define EMPTY()

#define EVAL(...) EVAL1024(__VA_ARGS__)
#define EVAL1024(...) EVAL512(EVAL512(__VA_ARGS__))
#define EVAL512(...) EVAL256(EVAL256(__VA_ARGS__))
#define EVAL256(...) EVAL128(EVAL128(__VA_ARGS__))
#define EVAL128(...) EVAL64(EVAL64(__VA_ARGS__))
#define EVAL64(...) EVAL32(EVAL32(__VA_ARGS__))
#define EVAL32(...) EVAL16(EVAL16(__VA_ARGS__))
#define EVAL16(...) EVAL8(EVAL8(__VA_ARGS__))
#define EVAL8(...) EVAL4(EVAL4(__VA_ARGS__))
#define EVAL4(...) EVAL2(EVAL2(__VA_ARGS__))
#define EVAL2(...) EVAL1(EVAL1(__VA_ARGS__))
#define EVAL1(...) __VA_ARGS__

#define DEFER1(m) m EMPTY()
#define DEFER2(m) m EMPTY EMPTY()()
#define DEFER3(m) m EMPTY EMPTY EMPTY()()()
#define DEFER4(m) m EMPTY EMPTY EMPTY EMPTY()()()()

//#define IS_PROBE(...) SECOND(__VA_ARGS__, 0)
#define IS_PROBE(...) DEFER1(SECOND)(__VA_ARGS__, 0)
#define PROBE() NIL, 1

#define CAT(a,b) a ## b

// Negation
// Should be 1: NOT(0)
// Should be 0: NOT(1)
// Should be 0: NOT(not zero)
// Should be 0: NOT()
#define NOT(x) IS_PROBE(CAT(_NOT_, x))
#define _NOT_0 PROBE()

// Turn 0 into 0 and anything else into 1
// Should be 0: BOOL(0)
// Should be 1: BOOL(1)
// Should be 1: BOOL(123)
// Should be 1: BOOL(not zero)
// Should be 1: BOOL()  <- this produces a warning in MSVC
//#define BOOL(x) NOT(NOT(x))
#define BOOL(x) DEFER1(NOT)(NOT(x))

// If/else logic for macros
// Should be "it was zero": IF_ELSE(0)(it was non-zero)(it was zero)
// Should be "it was non-zero": IF_ELSE(1)(it was non-zero)(it was zero)
// Should be "it was non-zero": IF_ELSE(123)(it was non-zero)(it was zero)
//#define IF_ELSE(condition) _IF_ELSE(BOOL(condition))
#define IF_ELSE(condition) EVAL(DEFER2(_IF_ELSE)(BOOL(condition)))
#define _IF_ELSE(condition) CAT(_IF_, condition)

#define _IF_1(...) __VA_ARGS__ _IF_1_ELSE
#define _IF_0(...)             _IF_0_ELSE

#define _IF_1_ELSE(...)
#define _IF_0_ELSE(...) __VA_ARGS__

// Find out whether there are any arguments
// Should be 0: HAS_ARGS()
// Should be 1: HAS_ARGS(0)
// Should be 1: HAS_ARGS(0, 1, 2)
#define HAS_ARGS(...) BOOL(FIRST(_END_OF_ARGUMENTS_ __VA_ARGS__)())
#define _END_OF_ARGUMENTS_() 0

// Map a macro to a list of arguments
// #define GREET(x) Hello, x!
// EVAL(MAP(GREET, Mum, Dad, Adam, Joe))
// Should be: Hello, Mum! Hello, Dad! Hello, Adam! Hello, Joe!
// NOTE Couldn't make it work. Head hurts.
#define MAP(m, first, ...)           \
  m(first)                           \
  IF_ELSE(HAS_ARGS(__VA_ARGS__))(    \
    DEFER2(_MAP)()(m, __VA_ARGS__)   \
  )(                                 \
    /* Do nothing, just terminate */ \
  )
#define _MAP() MAP

