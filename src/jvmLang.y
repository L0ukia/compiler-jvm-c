%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Flex Declarations
/* Just for being able to show the line number were the error occurs.*/
extern int line;
extern FILE *yyout;
int yylex();
/* Error Related Functions and Macros*/
int yyerror(const char *);
int no_errors;
/* Error Messages Macros*/
#define ERR_VAR_DECL(VAR,LINE) fprintf(stderr,"Variable :: %s on line %d. ",VAR,LINE); yyerror("Var already defined")
#define ERR_VAR_MISSING(VAR,LINE) fprintf(stderr,"Variable %s NOT declared, n line %d.",VAR,LINE); yyerror("Variable Declation fault")

// Type Definitions and JVM command related Functions
#include "jvmLangTypesFunctions.h"
// Symbol Table definitions and Functions
#include "symbolTable.h"
/* Defining the Symbol table. A simple linked list. */
ST_TABLE_TYPE symbolTable;
#include "codeFacilities.h"

%}
/* Output informative error messages (bison Option) */
%define parse.error verbose


/* Declaring the possible types of Symbols*/
%union{
   char *lexical;
   RelationType relopIndex;
   int lbl;
   ParType tokentype;
   struct {
     NUMBER_LIST_TYPE trueLbl;
     NUMBER_LIST_TYPE falseLbl;
	} condLabels;
   struct {
	   ParType type;
	   char * place;} se;
}

/* Token declarations and their respective types */

%token <lexical> T_num
%token <lexical> T_real
%token <lexical> T_string
%token <lexical> T_id
%token '('
%token ')'
%token ';'
%token '}'
%token '{'

%token <tokentype> T_type
%token ':'
%token T_V_DECL "var"
%token T_begin "begin"
%token T_end "end"
%token T_if "if"
%token T_then "then"
%token T_else "else"
%token T_while "while"
%token T_and "and"
%token T_or "or"
%token T_not "not"
%token T_program "program"
%token T_end_program "end_program"
%token T_print "print"
%token T_for "for"
%token T_true "true"
%token T_false "false"
/* The type of non-terminal symbols*/
%type<se> expr
%type<tokentype> vars
%type<relopIndex> relop
%type<tokentype> printcmd
%type<lbl> label
%type<condLabels> bool

%nonassoc T_assign
%nonassoc '='
%nonassoc '>'
%nonassoc '<'

%left '+' '-'
%left '*' '/'
%left "or"
%left "and"


%%

program: "program" T_id {create_preample($2); symbolTable=NULL; }
			variable_declarations stmts "end_program"
			{insertINSTRUCTION("return");
       insertINSTRUCTION(".end method\n");}
	;

variable_declarations: /* empty */
	| var_decl variable_declarations
	;

var_decl: "var" T_id vars
           {if (!addvar(&symbolTable,$2,$3))
              {ERR_VAR_DECL($2,line);}
           }
	;

vars: ':' T_type ';' {$$ = $2;}
    	| ',' T_id vars  {if (!addvar(&symbolTable,$2,$3))
                          {ERR_VAR_DECL($2,line);} $$ = $3;}
   	 ;

blck: "begin" stmts "end" {/* nothing */}
       |'{' stmts '}'  {/* nothing */}
	;

/* A simple (very) definition of a list of statements.*/
stmts:   stmt  {/* nothing */}
     	|  stmt stmts 	{/* nothing */}
	;

stmt:   cndl	{/* nothing */}
  | asmt	';' {/* nothing */}
	| printcmd {/* nothing */}
	;

printcmd: "print" '(' T_id ')' ';'
	{if (!lookup(symbolTable,$3) ) {ERR_VAR_MISSING($3,line);}
	 $$ = lookup_type(symbolTable,$3);
   insertINSTRUCTION("getstatic java/lang/System/out Ljava/io/PrintStream;");
   insertLOAD($$,lookup_position(symbolTable,$3));
   insertINVOKEVITRUAL("java/io/PrintStream/println",$$,type_void);
	}
  | "print" '(' T_string ')' ';' {
         insertINSTRUCTION("getstatic java/lang/System/out Ljava/io/PrintStream;");
         insertLDC($3);
         insertINVOKEVITRUAL("java/io/PrintStream/println",type_string,type_void) ;
}
	;


cndl:  "while" label '(' bool ')'
		       {backpatch($4.trueLbl,currentLabel());
            insertLabel(Label());}
	 	        blck
		       {insertGOTO($2);
            backpatch($4.falseLbl,currentLabel());
            insertLabel(Label());}
	     |"if" '(' bool ')' "then"
		       {backpatch($3.trueLbl,currentLabel());
            insertLabel(Label());}
	         blck
		       {backpatch($3.falseLbl,currentLabel());
            insertLabel(Label());}
      | "for" '(' asmt ','  label bool ',' label asmt {insertGOTO($5);}
              ')'
					 	   {backpatch($6.trueLbl,currentLabel());
                insertLabel(Label());}
                blck
					  	 {insertGOTO($8);
               backpatch($6.falseLbl,currentLabel());
               insertLabel(Label());}

	;


/* label  is a dummy label that creates an new label and assigns its number to an attrribute. I also inserts the label to the code.*/
label : {$$ = Label(); insertLabel($$);}
	;

asmt: T_id T_assign expr
		{if (!lookup(symbolTable,$1)) {ERR_VAR_MISSING($1,line);}
		typeDefinition(lookup_type(symbolTable,$1), $3.type);
		insertSTORE($3.type,lookup_position(symbolTable,$1));
		}
	;


bool:  "true"    {$$.trueLbl = makelist(nextInstruction());
                  insertGOTO(UNKNOWN);
                  $$.falseLbl = NULL;}
	| "false"      {$$.falseLbl =makelist(nextInstruction());
                  insertGOTO(UNKNOWN);
                  $$.trueLbl = NULL;}
	| '(' bool ')' {$$.trueLbl = $2.trueLbl; $$.falseLbl = $2.falseLbl; }
	| expr relop expr
		{
		   typeDefinition($1.type, $3.type);

		   if ($1.type == type_integer) {
            $$.trueLbl = makelist(nextInstruction());
            insertICMPOP($2,UNKNOWN);
		     }
		   if ($1.type == type_real) {
		        insertINSTRUCTION("fcmpl");
            $$.trueLbl = makelist(nextInstruction());
            insertIFOP($2,UNKNOWN);
		      }
        $$.falseLbl = makelist(nextInstruction());
		    insertGOTO(UNKNOWN);
    }
  | bool "or" {backpatch($1.falseLbl,currentLabel());
               insertLabel(Label());}
        bool
             { $$.falseLbl = $4.falseLbl;
               $$.trueLbl = mergelists($1.trueLbl,$4.trueLbl);
             }

 	| bool "and" {backpatch($1.trueLbl,currentLabel());
                insertLabel(Label());}
        bool
            { $$.falseLbl = mergelists($1.falseLbl,$4.falseLbl);
              $$.trueLbl = $4.trueLbl;}

	;

relop: '>' 	{$$=OP_GT;}
	| '<' {$$=OP_LT;}
	| '=' {$$=OP_EQ;}
	;

expr:  T_num 		{$$.type = type_integer; int x; x = atoi($1); pushInteger(x);}
	| T_real 	{$$.type = type_real;insertLDC($1);}
	| T_id 		{ if (!($$.type = lookup_type(symbolTable,$1)))
                        {ERR_VAR_MISSING($1,line);}
			       insertLOAD($$.type,lookup_position(symbolTable,$1));
			      }
	| '(' expr ')' 	{/* nothing */}
	| expr '+' expr	{$$.type = typeDefinition($1.type, $3.type);
                  insertOPERATION($$.type,"add");}
	| expr '-' expr	{$$.type = typeDefinition($1.type, $3.type);
	                insertOPERATION($$.type,"sub");}
	| expr '*' expr	{$$.type = typeDefinition($1.type, $3.type);
	                insertOPERATION($$.type,"mul");}
	| expr '/' expr	{$$.type = typeDefinition($1.type, $3.type);
	                 insertOPERATION($$.type,"div");}
	;
%%



/* The usual yyerror */
int yyerror (const char * msg)
{
  fprintf(stderr, "PARSE ERROR: %s.on line %d.\n ", msg,line);
  no_errors++;
}

/* Other error Functions*/
/* The lexer... */
#include "jvmLang.lex.c"

/* Main */
int main(int argc, char **argv ){

   ++argv, --argc;  /* skip over program name */
   if ( argc > 0 && (yyin = fopen( argv[0], "r")) == NULL)
    {
      fprintf(stderr,"File %s NOT FOUND in current directory.\n Using stdin.\n",argv[0]);
      yyin = stdin;
    }
   if ( argc > 1) {yyout = fopen(argv[1], "w");}
   else {
      fprintf(stderr,"No second argument defined. Output to screen.\n\n");
      yyout = stdout;
    }

    // Calling the parser
   int result = yyparse();

   fprintf(stderr,"Errors found %d.\n",no_errors);
   if (no_errors == 0)
      {print_int_code(yyout);}
   fclose(yyout);
   /// Need to remove even empty file.
   if (no_errors != 0 && yyout != stdout) {
     remove(argv[1]);
      fprintf(stderr,"No Code Generated.\n");}
   print_symbol_table(symbolTable); /* uncomment for debugging. */

  return result;
}
