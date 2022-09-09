#define T_PROGR     1
#define T_CONST     2
#define T_TYPE      3
#define T_ARRAY     4
#define T_SET       5
#define T_OF        6
#define T_RECORDS   7
#define T_VAR       8
#define T_FORWARD   9
#define T_FUNCTION  10
#define T_PROCEDURE 11
#define T_INTEGER   12
#define T_REAL      13
#define T_BOOLEAN   14
#define T_CHAR      15
#define T_BEGIN     16
#define T_END       17
#define T_IF        18
#define T_THEN      19
#define T_ELSE      20
#define T_WHILE     21
#define T_DO        22
#define T_FOR       23
#define T_DOWNTO    24
#define T_TO        25
#define T_WITH      26
#define T_READ      27
#define T_WRITE     28



//  Identifier
#define T_ID        29

//  Constants
#define T_ICONST    30
#define T_RCONST    31
#define T_BCONST    32
#define T_CCONST    33
#define T_SCONST    34

//  Operators
#define T_RELOP     35
#define T_ADDOP     36
#define T_OROP      37
#define T_MULDIVANDOP     38
#define T_NOTOP     39
#define T_INOP      40



//  Other tokens
#define T_LPAREN    41
#define T_RPAREN    42
#define T_SEMI      43
#define T_COMMA     44
#define T_EQU       45
#define T_LBRACK    46
#define T_RBRACK    47
#define T_DOT       48
#define T_COLON     49
#define T_ASSIGN    50
#define T_DOTDOT    51

//  EOF
#define T_EOF       0

// Beautify
char* TOKEN_NAME[] = {"T_EOF", "T_PROGR", "T_CONST", "T_TYPE", "T_ARRAY", "T_SET", "T_OF", "T_RECORDS", "T_VAR", "T_FORWARD", "T_FUNCTION", "T_PROCEDURE", "T_INTEGER", "T_REAL", "T_BOOLEAN", "T_CHAR", "T_BEGIN", "T_END", "T_IF", "T_THEN", "T_ELSE", "T_WHILE", "T_DO", "T_FOR", "T_DOWNTO", "T_TO", "T_WITH", "T_READ", "T_WHITE", "T_ID", "T_ICONST", "T_RCONST", "T_BCONST", "T_CCONST","T_SCONST","T_RELOP","T_ADDOP","T_ORROP","T_MULDIVANDOP","T_NOTOP","T_INOP","T_LPAREN","T_RPAREN","T_SEMI","T_COMMA","T_EQU","T_LBRACK","RBRACK","T_DOT","T_COLON","T_ASSIGN","T_DOTDOT"};