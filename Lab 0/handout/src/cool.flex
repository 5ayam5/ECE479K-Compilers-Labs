/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include "cool_parse.h"
#include "utils.h"
#include <regex>

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
extern FILE *fin; /* we read from this file */
#undef YY_INPUT
#define YY_INPUT(buf, result, max_size)                                        \
  if ((result = fread((char *)buf, sizeof(char), max_size, fin)) < 0)          \
    YY_FATAL_ERROR("read() in flex scanner failed");

extern int curr_lineno;
/*
 *  Add Your own definitions here
 */

namespace cool_flex {
  int index = 0;
  int depth = 0;
  bool eof = false;
}

%}

%option noyywrap

%state COMMENT

/*
 * Define names for regular expressions here.
 */

digit       [0-9]
upper       [A-Z]
lower       [a-z]
letter      {upper}|{lower}|_|{digit}
whitespace  [ \t]
line        \n|\r\n|\r|\f|\v

%%

 /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like syntax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-): The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */
<INITIAL>{
"(*"                      { cool_flex::depth++; BEGIN(COMMENT); } /* start of comment */
"*)"                      { cool_yylval.error_msg = strdup("Unmatched *)"); return ERROR; } /* end of comment */
}
<COMMENT>{
"(*"                      { cool_flex::depth++; } /* nested comment */
"*)"                      { if (!--cool_flex::depth) BEGIN(INITIAL); } /* end of comment */
[^*\n(\r\n)\r\f\v]+       { /* ignore comment */ }
"*"                       { /* ignore comment */ }
{line}                    { curr_lineno++; }
<<EOF>>                   { if (!cool_flex::eof) { cool_flex::eof = true; cool_yylval.error_msg = strdup("EOF in comment"); return ERROR; } else return 0; }
}
"--".*                    { /* ignore comment */ }

(?i:class)                { return CLASS; }
(?i:else)                 { return ELSE; }
(?i:fi)                   { return FI; }
(?i:if)                   { return IF; }
(?i:in)                   { return IN; }
(?i:inherits)             { return INHERITS; }
(?i:isvoid)               { return ISVOID; }
(?i:let)                  { return LET; }
(?i:loop)                 { return LOOP; }
(?i:pool)                 { return POOL; }
(?i:then)                 { return THEN; }
(?i:while)                { return WHILE; }
(?i:case)                 { return CASE; }
(?i:esac)                 { return ESAC; }
(?i:new)                  { return NEW; }
(?i:of)                   { return OF; }
(?i:not)                  { return NOT; }

t(?i:rue)                 { cool_yylval.boolean = true; return BOOL_CONST; }
f(?i:alse)                { cool_yylval.boolean = false; return BOOL_CONST; }

{digit}+                  { cool_yylval.symbol = new IntEntry(std::string(yytext), cool_flex::index++); return INT_CONST; }

\"(\\[^\n]|[^"\n\\])*\"   { std::string str(yytext); str = str.substr(1, str.size() - 2); str = std::regex_replace(str, std::regex("\\\\n"), "\n"); cool_yylval.symbol = new StringEntry(str, cool_flex::index++); return STR_CONST; }

{upper}{letter}*          { cool_yylval.symbol = new IdEntry(std::string(yytext), cool_flex::index++); return TYPEID; }
{lower}{letter}*          { cool_yylval.symbol = new IdEntry(std::string(yytext), cool_flex::index++); return OBJECTID; }

";"                       { return ';'; }
","                       { return ','; }
":"                       { return ':'; }
"("                       { return '('; }
")"                       { return ')'; }
"."                       { return '.'; }
"@"                       { return '@'; }
"~"                       { return '~'; }
"{"                       { return '{'; }
"}"                       { return '}'; }
"="                       { return '='; }
"<="                      { return LE; }
"<"                       { return '<'; }
"+"                       { return '+'; }
"-"                       { return '-'; }
"*"                       { return '*'; }
"/"                       { return '/'; }
"<-"                      { return ASSIGN; }
"=>"                      { return DARROW; }

{line}                    { curr_lineno++; }
{whitespace}+             { /* ignore whitespace */ }
.                         { cool_yylval.error_msg = strdup(yytext); return ERROR; }

%%
