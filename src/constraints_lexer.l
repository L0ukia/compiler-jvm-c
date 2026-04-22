%{ #include <stdio.h> 
#include <string.h> 
#include <stdlib.h> 
#include "types.h" 
#include "constraints.tab.h" 




%} 

%option yylineno 

ws      [ \t]+

newline (\n|\x0A|\x0D\x0A)

%%




{ws} { /* Do nothing */ }
{newline} { /* Do nothing */ }


";" {return ';';} 

":" {return ':';} 

"var" {return T_VAR;} 

"set_of" {return T_SET_OF;} 

"array" {return T_ARRAY;}

 "of" {return T_OF;} 

"bool" {return T_BOOL;} 

"int" {return T_INT;} 

"constraint" {return T_CONSTRAINT;} 

"solve" {return T_SOLVE;} 

"satisfy" {return T_SATISFY;} 

"maximize" {return T_MAXIMIZE;} 

"true" {return T_TRUE;} 
"false" {return T_FALSE;} 

"subset" {return T_SUBSET;} 
"in" {return T_IN;} 

"intersect" {return T_SET_OPS;} 
"union" {return T_SET_OPS;} 
"diff" {return T_SET_OPS;} 

"[" {return'[';} 
"]" {return']';} 
".." {return T_DOTDOT;} 

[0-9]+ {
    yylval.number.value = atoi(yytext);
    yylval.number.type = type_integer;
    return T_NUM;

	}

">" {return T_GT;}
 "<" {return T_LT;} 
"==" {return T_EQ;} 
">=" {return T_GE;} 
"<=" {return T_LE;} 

"#=" {return T_ASSIGNBOOL;} 

"~>" {return T_IMPLIES;}
 "&&" {return T_AND;}
 "not" {return T_NOT;} 

"+" {return T_PLUS;}
 "-" {return T_MINUS;} 
"*" {return T_MULT;} 
"/" {return T_DIV;} 

[a-zA-Z][a-zA-Z0-9]* { yylval.lexical = strdup(yytext); return T_ID; } 

. /*{ printf("Unrecognized character: %s at line %d\n", yytext, yylineno); } */

%% 