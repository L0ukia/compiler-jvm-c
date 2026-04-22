%{

#define T_IF 260
#define T_THEN 261
#define T_ELSE 262
#define T_NUM 270
#define T_ID 271
#define T_RELOP 280
#define ERROR 999

#define GTE 1100
#define LTE 1101
#define EQ  1102

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int idnum=0;
int attr=0;
%}
/* This is for DOS/WINDOWS/LINUX differences in end of line. */
newline \n|\x0A|\x0D\x0A
ws ([ \t]|{newline})+
nzdigit [1-9]
digit (0|{nzdigit}) 
digits {digit}+
letter [A-Za-z]
id     {letter}({letter}|{digit})*

%%
{ws}     {/* do nothing */}
"if"       {attr=0;return(T_IF);}
"then"     {attr=0;return(T_THEN);}
"else"     {attr=0;return(T_ELSE);}
{digits} {attr=atoi(yytext); return(T_NUM);}
{id}	 {attr=idnum++; return(T_ID);}
">="	 {attr=GTE;return(T_RELOP);}
"="	 {attr=EQ;return(T_RELOP);}
"=<"	 {attr=LTE;return(T_RELOP);}
.       {printf("Error in %s \n",yytext);attr=999;return(ERROR);}

%%

int main( int argc, char **argv )
{
	++argv, --argc; /* skip over program name */
	if ( argc > 0 )
		yyin = fopen( argv[0], "r" );
	else
		yyin = stdin;
	int token;
	while ((token=yylex()) != 0)
		{printf("lexeme: %s \t <token,attr> is <%d,%d> \n",yytext,token,attr);}

        fclose(yyin);
        return 0;
}
