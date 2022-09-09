/* Tsigkopoulos Spyridon    2117201 */
/* Kararrhgas Ioannis       2117072 */
%{
   
    #include <stdio.h>
    #include <stdlib.h>
    #include "extra/hashtbl.h"
    #include <string.h>
    #include <stdbool.h>
    #include <ctype.h>
	#include <math.h>
    
    // Here i added new content 
    /* CHANGE:Handling Hashtable */
	#include "hashtbl.h" 
	#include "hashtbl.c"
    char str[50]; //NEW

    extern FILE *yyin;
    extern int yylex();
    extern void yyerror(const char* err);

    HASHTBL *hashtbl; /* declare Hashtable */
    /* define the starting scope */
    int scope = 0;   
%}

%error-verbose

%union{
    int intval;
    float realval;
    _Bool boolval;
    char charval;
    char* strval;
}

/*%token  <type>             T_CHAR      <number>    "description"  */ 
%token  <strval>            T_PROGRAM                   "program"
%token  <strval>            T_CONST                     "const"
%token  <strval>            T_TYPE                      "type"
%token  <strval>            T_ARRAY                     "array"
%token  <strval>            T_SET                       "set"
%token  <strval>            T_OF                        "of"    
%token  <strval>            T_RECORD                    "record"
%token  <strval>            T_VAR                       "var"
%token  <strval>            T_FORWARD                   "forward"
%token  <strval>            T_FUNCTION                  "function"
%token  <strval>            T_PROCEDURE                 "procedure"
%token  <strval>            T_INTEGER                   "integer"
%token  <strval>            T_REAL                      "real"
%token  <strval>            T_BOOLEAN                   "boolean"
%token  <strval>            T_CHAR                      "char"
%token  <strval>            T_BEGIN                     "begin"
%token  <strval>            T_END                       "end"
%token  <strval>            T_IF                        "if"
%token  <strval>            T_THEN                      "then"
%token  <strval>            T_ELSE                      "else"
%token  <strval>            T_WHILE                     "while"
%token  <strval>            T_DO                        "do"
%token  <strval>            T_FOR                       "for"
%token  <strval>            T_DOWNTO                    "downto"
%token  <strval>            T_TO                        "to"
%token  <strval>            T_WITH                      "with"
%token  <strval>            T_READ                      "read"
%token  <strval>            T_WRITE                     "write"
%token  <strval>            T_STRING                    "string"

// ID
%token  <strval>            T_ID                        "id"

//variables
%token  <intval>            T_ICONST                    "iconst"
%token  <realval>           T_RCONST                    "rconst"
%token  <boolval>           T_BCONST                    "bconst"
%token  <charval>           T_CCONST                    "cconst"
%token  <strval>            T_SCONST                    "sconst"

//operators
%token  <strval>           T_RELOP                     "< or > or <= or >= or <>"
%token  <strval>           T_ADDOP                     "+ or -"
%token  <strval>           T_OROP                      "OR"
%token  <strval>           T_MULDIVANDOP               "* or / or DIV or MOD or AND"
%token  <strval>           T_NOTOP                     "NOT"
%token  <strval>           T_INOP                      "IN"

//other lectical units
%token  <strval>            T_LPAREN                    "("
%token  <strval>            T_RPAREN                    ")"
%token  <strval>            T_SEMI                      ";"
%token  <strval>            T_DOT                       "."
%token  <strval>            T_COMMA                     ","
%token  <strval>            T_EQU                       "="
%token  <strval>            T_COLON                     ":"
%token  <strval>            T_LBRACK                    "["
%token  <strval>            T_RBRACK                    "]"
%token  <strval>            T_ASSIGN                    ":="
%token  <strval>            T_DOTDOT                    ".."
                       
//  EOF
%token  <strval>            T_EOF               0       "EOF"

%type <strval> program header declarations constdefs constant_defs expression variable expressions constant setexpression elexpressions 
%type <strval> elexpression typedefs type_defs type_def dims limits limit typename standard_type fields field identifiers vardefs variable_defs 
%type <strval> subprograms subprogram sub_header formal_parameters parameter_list pass comp_statement statements statement assignment 
%type <strval> if_statement if_tail while_statement for_statement subprogram_call io_statement read_list read_item write_list write_item

%left             T_INOP
%left             T_RELOP
%left             T_EQU
%left             T_OROP
%left             T_ADDOP
%left             T_MULDIVANDOP
%left             T_NOTOP   
/* CHANGE: here i changed the turn that they appear*/
%left             T_LPAREN   
%left             T_RPAREN  
%left             T_SEMI   
%left             T_DOT 
%left             T_COMMA
%left             T_COLON
%left             T_LBRACK
%left             T_RBRACK
%left             T_DOTDOT
%left             T_ASSIGN
             
%nonassoc        LOWER_THAN_ELSE
%nonassoc        T_ELSE

%start program

//Grammar       // Here i added new content in some of them
%%
								
program:                        body T_END { hashtbl_get(symbol_table, scope); scope--; } subprograms
								;

header:type T_FUNC T_ID { hashtbl_insert(symbol_table, $3, (void *) &yylineno, scope); } T_LPAREN formal_parameters T_RPAREN
      | T_SUBRTN T_ID { hashtbl_insert(symbol_table, $2, (void *) &yylineno, scope); } T_LPAREN formal_parameters T_RPAREN
      | T_SUBRTN T_ID { hashtbl_insert(symbol_table, $2, (void *) &yylineno, scope); }                                             

								;
declarations: declarations type vars
            | declarations T_COMMON cblock_list
            | declarations T_DATA vals
            | %empty
            ;
								;
constdefs:                      T_CONST constant_defs T_SEMI
                            | %empty                                                { }

								;
constant_defs:                  constant_defs T_SEMI T_ID T_EQU expression                                { hashtbl_insert(hashtbl, $3, NULL, scope);}
                            | T_ID T_EQU expression                                                       { hashtbl_insert(hashtbl, $1, NULL, scope);}

								;
expression: expression T_OROP expression
          | expression T_ANDOP expression
          | expression T_RELOP expression
          | expression T_ADDOP expression
          | expression T_MULOP expression
          | expression T_DIVOP expression
          | expression T_POWEROP expression
          | T_NOTOP expression
          | T_ADDOP expression
          | variable
          | constant
          | T_LPAREN expression T_RPAREN
          | T_LENGTH T_LPAREN expression T_RPAREN
          ;
								;
variable: T_ID { hashtbl_insert(symbol_table, $1, (void *) &yylineno, scope); } T_LPAREN expressions T_RPAREN
        | T_ID { hashtbl_insert(symbol_table, $1, (void *) &yylineno, scope); }
        ;
								;
expressions:  expressions T_COMMA expression
           |  expression    
			;
constant: T_ICONST
        | T_RCONST
        | T_BCONST
        | T_CCONST
    
								;
setexpression:                  T_LBRACK elexpressions T_RBRACK
                            | T_LBRACK T_RBRACK
    
								;
elexpressions:                  elexpressions T_COMMA elexpression
                            | elexpression
    
								;
elexpression:                   expression T_DOTDOT expression
                            | expression

								;
typedefs:                       T_TYPE type_defs T_SEMI
                            | %empty                                                { }

								;
type_defs:                      type_defs T_SEMI T_ID T_EQU type_def                                    { hashtbl_insert(hashtbl, $3, NULL, scope);}
                            | T_ID T_EQU type_def                                                       { hashtbl_insert(hashtbl, $1, NULL, scope);}
    
								;
type_def:                       T_ARRAY T_LBRACK dims T_RBRACK T_OF typename
                            | T_SET T_OF typename
                            | T_RECORD fields T_END
                            | T_LPAREN identifiers T_RPAREN
                            | limit T_DOTDOT limit

								;
dims:                           dims T_COMMA limits
                            | limits

								;
limits:                         limit T_DOTDOT limit
                            | T_ID                                                                      { hashtbl_insert(hashtbl, $1, NULL, scope);}

								;
limit:                          T_ADDOP T_ICONST
                            | T_ADDOP T_ID
                            | T_ICONST
                            | T_CCONST
                            | T_BCONST
                            | T_ID

								;
typename: standard_type
                            | T_ID                                                                      { hashtbl_insert(hashtbl, $1, NULL, scope);}

								;
standard_type:                  T_INTEGER | T_REAL | T_BOOLEAN | T_CHAR

								;
fields:                         fields T_SEMI field
                            | field
    
								;
field:                          identifiers T_COLON typename

								;
identifiers:                    identifiers T_COMMA T_ID                                                { hashtbl_insert(hashtbl, $3, NULL, scope);}
                            | T_ID                                                                      { hashtbl_insert(hashtbl, $1, NULL, scope);}
    
								;
vardefs:                        T_VAR variable_defs T_SEMI
                            | %empty                                                { }
    
								;
variable_defs:                  variable_defs T_SEMI identifiers T_COLON typename
                            | identifiers T_COLON typename
    
								;
subprograms:                subprograms subprogram T_SEMI                                           
                            | %empty                                               
    
								;
subprogram:                 header { scope++; } body T_END { hashtbl_get(symbol_table, scope); scope--; }
								;
sub_header:                   T_FUNCTION T_ID formal_parameters T_COLON standard_type                 { hashtbl_insert(hashtbl, $2, NULL, scope);}
                            | T_PROCEDURE T_ID formal_parameters                                        { hashtbl_insert(hashtbl, $2, NULL, scope);}
                            | T_FUNCTION T_ID                                                           { hashtbl_insert(hashtbl, $2, NULL, scope);}
    
								;
formal_parameters:type vars T_COMMA formal_parameters
                 |type vars                                               
    
								;
parameter_list:                 parameter_list T_SEMI pass identifiers T_COLON typename
                            | pass identifiers T_COLON typename
    
								;
pass:                           T_VAR | %empty                                                { }

								;
comp_statement:                 T_BEGIN statements T_END

								;
statements:                     statements T_SEMI statement
                            | statement

								;
statement:  simple_statement
         |  compound_statement
         ;                                              { }

								;
assignment:                     variable T_ASSIGN expression
								;
if_statement: T_IF T_LPAREN expression T_RPAREN label T_COMMA label T_COMMA label
            | T_IF T_LPAREN expression T_RPAREN { scope++; } simple_statement
            ;
if_tail:                        T_ELSE statement
                            | %empty %prec LOWER_THAN_ELSE                                                { }
    
								;
while_statement:                T_WHILE expression T_DO statement

								;
for_statement:                  T_FOR T_ID T_ASSIGN iter_space T_DO statement                          { hashtbl_insert(hashtbl, $2, NULL, scope); }

								;
iter_space: expression T_COMMA expression step
          ;
with_statement:                 T_WITH variable T_DO statement

								;
subprogram_call:                T_ID                                                                    { hashtbl_insert(hashtbl, $1, NULL, scope);}
                            | T_ID T_LPAREN expressions T_RPAREN                                        { hashtbl_insert(hashtbl, $1, NULL, scope);}
    
								;
io_statement: T_READ read_list
            | T_WRITE write_list
            ;
read_list: read_list T_COMMA read_item
         |  read_item
         ;
read_item:  variable
         |  T_LPAREN read_list T_COMMA T_ID { hashtbl_insert(symbol_table, $4, (void *) &yylineno, scope); } T_ASSIGN iter_space T_RPAREN
         ;
write_list: write_list T_COMMA write_item
          | write_item
          ;

write_item: expression
          | T_LPAREN write_list T_COMMA T_ID { hashtbl_insert(symbol_table, $4, (void *) &yylineno, scope); } T_ASSIGN iter_space T_RPAREN
          ;


%%

int main(int argc, char *argv[]){
	int token;     

// Here i added new content
    /* Initialize the Hashtable */
    if(argc==2){
    yyin = fopen(argv[1], "r");

    if (!(hashtbl = hashtbl_create(23, NULL))){
        puts("Error, failed to initialize hashtable");
        exit(EXIT_FAILURE);
        }

	if(argc > 1){       
		yyin = fopen(argv[1], "r");

		if (yyin == NULL){
			perror ("Error opening file"); 
			exit(EXIT_FAILURE);
		}
	 }        
    printf("\n[Line %d]\n", yylineno);
    yyparse();

	fclose(yyin);
    hashtbl_destroy(hashtbl);
	return 0;
}
// Here i added new content
void yyerror(const char* err){
  error_count++;

  printf("[ERROR in LINE %d: %s]\n", yylineno, err);
  
  if(error_count == 5){
    printf("Max number of ERRORS detected! Exiting ...\n");
    exit(EXIT_FAILURE);
  }
}