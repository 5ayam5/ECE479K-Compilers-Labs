/*
 *  cool.y
 *              Parser definition for the COOL language.
 *
 */
%{
#include <iostream>
#include "cool_tree.h"
#include "utils.h"
#include <vector>
#include <tuple>

/* Add your own C declarations here */


/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

extern int yylex();           /* the entry point to the lexer  */
extern int curr_lineno;
extern std::string curr_filename;
Program ast_root;            /* the result of the parse  */
int omerrs = 0;              /* number of errors in lexing and parsing */

/*
   The parser will always call the yyerror function when it encounters a parse
   error. The given yyerror implementation (see below) justs prints out the
   location in the file where the error was found. You should not change the
   error message of yyerror, since it will be used for grading puproses.
*/
void yyerror(const char *s);

/*
   The VERBOSE_ERRORS flag can be used in order to provide more detailed error
   messages. You can use the flag like this:

     if (VERBOSE_ERRORS)
       fprintf(stderr, "semicolon missing from end of declaration of class\n");

   By default the flag is set to 0. If you want to set it to 1 and see your
   verbose error messages, invoke your parser with the -v flag.

   You should try to provide accurate and detailed error messages. A small part
   of your grade will be for good quality error messages.
*/
extern int VERBOSE_ERRORS;

typedef std::tuple<Symbol, Symbol, Expression> *LetVar;
typedef std::vector<LetVar> *LetList;

%}

/* A union of all the types that can be the result of parsing actions. */
%union {
  bool boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  LetVar let_var;
  LetList let_list;
  Expressions expressions;
  char *error_msg;
}

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.

   The integer following token declaration is the numeric constant used
   to represent that token internally.  Typically, Bison generates these
   on its own, but we give explicit numbers to prevent version parity
   problems (bison 1.25 and earlier start at 258, later versions -- at
   257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 FOR 283 ERROR 284

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/

/* Complete the nonterminal list below, giving a type for the semantic
  value of each non terminal. (See section 3.6 in the bison 
  documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class

/* You will want to change the following line. */
%type <features> dummy_feature_list
%type <feature> feature
%type <formals> formal_list
%type <formal> formal
%type <expression> expression
%type <let_list> let_list
%type <let_var> let_var
%type <expressions> expression_list
%type <cases> case_list
%type <case_> case

/* Precedence declarations go here. */
%left '.'
%left '@'
%left '~'
%left ISVOID
%left '*' '/'
%left '+' '-'
%nonassoc '<' LE '='
%left NOT
%right ASSIGN
%right IN

%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program : class_list { ast_root = program($1); }
        ;

class_list
        : class ';'            /* single class */
                { $$ = single_Classes($1); }
        | class_list class ';' /* several classes */
                { $$ = append_Classes($1,single_Classes($2)); }
        | class_list error ';' /* error in class definition */
                { $$ = $1; }
        ;

/* If no parent is specified, the class inherits from the Object class. */
class  : CLASS TYPEID '{' dummy_feature_list '}'
                { $$ = class_($2,idtable.add_string("Object"),$4,
                              stringtable.add_string(curr_filename)); }
        | CLASS TYPEID INHERITS TYPEID '{' dummy_feature_list '}'
                { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
        ;

/* Feature list may be empty, but no empty features in list. */
dummy_feature_list:        /* empty */
                {  $$ = nil_Features(); }
        | feature ';' dummy_feature_list
                { $$ = append_Features(single_Features($1),$3); }
        | error ';' dummy_feature_list
                { $$ = $3; }
        ;

feature: OBJECTID ':' TYPEID { $$ = attr($1,$3,no_expr()); }
        | OBJECTID ':' TYPEID ASSIGN expression { $$ = attr($1,$3,$5); }
        | OBJECTID '(' formal_list ')' ':' TYPEID '{' expression '}'
                { $$ = method($1,$3,$6,$8); }
        ;

formal_list:        /* empty */
                { $$ = nil_Formals(); }
        | formal formal_list
                { $$ = append_Formals(single_Formals($1),$2); }
        ;

formal: OBJECTID ':' TYPEID { $$ = formal($1,$3); }
        ;

expression: OBJECTID ASSIGN expression { $$ = assign($1,$3); }
        | expression '.' OBJECTID '(' expression_list ')' { $$ = dispatch($1,$3,$5); }
        | expression '@' TYPEID '.' OBJECTID '(' expression_list ')' { $$ = static_dispatch($1,$3,$5,$7); }
        | IF expression THEN expression ELSE expression FI { $$ = cond($2,$4,$6); }
        | WHILE expression LOOP expression POOL { $$ = loop($2,$4); }
        | '{' expression_list '}' { $$ = block($2); }
        | LET let_list IN expression {
                        auto let_list = *$2;
                        auto first = *let_list.back();
                        auto ret = let(std::get<0>(first), std::get<1>(first), std::get<2>(first), $4);
                        for (auto it = ++let_list.rbegin(); it != let_list.rend(); ++it) {
                                ret = let(std::get<0>(**it), std::get<1>(**it), std::get<2>(**it), ret);
                        }
                        $$ = ret;
                }
        | CASE expression OF case_list ESAC { $$ = typcase($2,$4); }
        | NEW TYPEID { $$ = new_($2); }
        | ISVOID expression { $$ = isvoid($2); }
        | '~' expression { $$ = comp($2); }
        | expression LE expression { $$ = leq($1,$3); }
        | expression '<' expression { $$ = lt($1,$3); }
        | expression '+' expression { $$ = plus($1,$3); }
        | expression '-' expression { $$ = sub($1,$3); }
        | expression '*' expression { $$ = mul($1,$3); }
        | expression '/' expression { $$ = divide($1,$3); }
        | NOT expression { $$ = neg($2); }
        | expression '=' expression { $$ = eq($1,$3); }
        | '(' expression ')' { $$ = $2; }
        | INT_CONST { $$ = int_const($1); }
        | STR_CONST { $$ = string_const($1); }
        | BOOL_CONST { $$ = bool_const($1); }
        | OBJECTID { $$ = object($1); }
        ;

let_list: let_var { $$ = new std::vector<LetVar>{$1}; }
        | let_list ',' let_var { $$ = $1; $1->push_back($3); }
        | let_list ',' error { $$ = $1; }
        ;

let_var: OBJECTID ':' TYPEID { $$ = new std::tuple<Symbol, Symbol, Expression>($1, $3, no_expr()); }
        | OBJECTID ':' TYPEID ASSIGN expression { $$ = new std::tuple<Symbol, Symbol, Expression>($1, $3, $5); }
        ;

case_list: case { $$ = single_Cases($1); }
        | case_list case { $$ = append_Cases($1,single_Cases($2)); }
        ;

case: OBJECTID ':' TYPEID DARROW expression ';' { $$ = branch($1,$3,$5); }
        ;

expression_list:        /* empty */
                { $$ = nil_Expressions(); }
        | expression expression_list
                { $$ = append_Expressions(single_Expressions($1),$2); }
        | error expression_list
                { $$ = $2; }
        ;

/* end of grammar */
%%

/* This function is called automatically when Bison detects a parse error. */
void yyerror(const char *s) {
  std::cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " << s
            << " at or near ";
  print_cool_token(std::cerr, yychar, true);
  std::cerr << std::endl;
  omerrs++;

  if (omerrs > 20) {
    if (VERBOSE_ERRORS) {
      std::cerr << "More than 20 errors\n";
    }
    exit(1);
  }
}
