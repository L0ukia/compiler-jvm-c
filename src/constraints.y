
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

extern int yylineno;           
int semantic_errors = 0;   

int yylex(void);

void yyerror (const char * msg)
{
   fprintf(stderr,"Error(line %d) : %s\n", yylineno, msg); 
   semantic_errors++;
}

#include "types.h"
/* Required for accessing functions of the symbol Table. */

#include "symbolTable.h"


/* pointer for the symbol table.*/
ST_TABLE_TYPE symbolTable = NULL;


#include "symbolTable.h"
#define type_undef -1   


extern int yylineno;
extern FILE *yyin;



%}



/*Priority*/ 
%right T_NOT
%left T_AND
%left T_OR
%nonassoc T_IMPLIES
%left T_PLUS T_MINUS
%left T_MULT T_DIV


/* Tokens που θα έρθουν από Flex */
%token T_VAR T_SET_OF T_ARRAY T_OF T_BOOL T_INT
%token T_CONSTRAINT T_SOLVE T_SATISFY T_MAXIMIZE
%token T_TRUE T_FALSE
%token T_ID T_NUM
%token T_GT T_LT T_EQ T_GE T_LE
%token T_PLUS T_MINUS T_MULT T_DIV
%token T_SUBSET T_IN T_SET_OPS
%token T_ASSIGNBOOL
%token T_DOTDOT

%define parse.error verbose

/* Union of lexical values defined. 
lexical is for the name of the variables
type is the type defined in the enumerated ParType found in types.h
** You can change the union
*/
%union{
   char *lexical;
   ParType type;
   struct {
        ParType type;
        int value; 
  } number;
}




%union {
    int type_int;    
    int type_bool;   
}



%type <type> Base_type
%type <number> T_NUM
%type <lexical> T_ID
%type <type> Arg
%type <type> BoolExpr NumExpression BoolArg SetExpression Unary



%%

Model : {symbolTable=NULL;} Items
	;

Items
	:Item ';' Items
	| /*κενο*/
	;

Item
	:Var_item
	|Constraint_item
	|Solve_item
	;


Var_item
	:T_VAR Base_type ':' T_ID
	  {/*Δήλωση απλής μεταβλήτής*/
	    if (lookup(symbolTable, $4) ) /*Ελεγχος αν υπάρχει η μεταβλητή στον πίνακα*/  {
	      yyerror("Redeclaration of variable");
	      YYABORT; /*Τερματισμός ανάλυσης*/
	    } else {
	      addvar(&symbolTable, $4, $2); /*Εισαγωγή μεταβλητής με καταλληλο τύπο*/
	      printf("Inserted variable '%s' of type %d\n", $4, $2);
	    }
	  }

	|T_VAR T_SET_OF Base_type ':' T_ID
	  { /*Δήλωση μεταβήτής τύπου set of*/
	    if (lookup(symbolTable, $5) ) {
	      yyerror("Redeclaration of variable");
	      YYABORT;
	    } else { /*Εισαγωγή ως set τύπο*/
	      addvar(&symbolTable, $5, type_int_set); /*type_int_set -> απλοποίηση*/
	      printf("Inserted set variable '%s' of type %d\n", $5, type_int_set);
	    }
	  }

	|T_VAR T_ARRAY '[' T_NUM T_DOTDOT T_NUM ']' T_OF Base_type ':' T_ID
	  { /*Δήλωση  πινακα array[...] of ... */
	    if (lookup(symbolTable, $11) ) {
	      yyerror("Redeclaration of variable");
	      YYABORT;
	    } else { /* Επιλογή τύπου για array*/
	      ParType arrType = ($9 == type_integer) ? type_int_array : type_bool_array;
	      addvar(&symbolTable, $11, arrType);
	      printf("Inserted array '%s' of type %d\n", $11, arrType);
	    }
	  }
	;


Base_type
	:T_BOOL {$$ = type_bool;}
	|T_INT {$$ = type_integer;}
	|T_NUM T_DOTDOT T_NUM {$$ = type_integer;}
	;

Constraint_item
	:T_CONSTRAINT BoolExpr
	|T_CONSTRAINT T_ID T_ASSIGNBOOL BoolExpr
	;

BoolExpr
	:BoolExpr T_IMPLIES BoolExpr 
	{
          if ($1 != type_bool || $3 != type_bool) {
           yyerror("Operands of 'implies' must be boolean.");
           YYABORT;
          }
         $$ = type_bool;
          }
	|BoolExpr T_AND BoolExpr 
	{
         if ($1 != type_bool || $3 != type_bool) {
          yyerror("Operands of 'and' must be boolean.");
          YYABORT;
          }
         $$ = type_bool;
	  }
  	|BoolExpr T_OR BoolExpr 
	{  
	 if ($1 != type_bool || $3 != type_bool) {
          yyerror("Operands of 'or' must be boolean.");
          YYABORT;
          }
         $$ = type_bool;
         }
	|Unary { $$ = $1; }

	;

Unary
	:T_NOT BoolArg 
	{
          if ($2 != type_bool) {
           yyerror("Operand of 'not' must be boolean.");
           YYABORT;
         }
        $$ = type_bool;
         }
	|BoolArg { $$ = $1; }
	;

NumExpression
	:NumExpression T_PLUS NumExpression
	|NumExpression T_MINUS NumExpression
	|NumExpression T_MULT NumExpression
	|NumExpression T_DIV NumExpression 
	{
          if ($1 != type_integer || $3 != type_integer) {
           yyerror("Operands of arithmetic expressions must be integers.");
           YYABORT;
        }
       $$ = type_integer;
        }
	|Arg
	{
         if ($1 != type_integer) {
          yyerror("Expected integer expression.");
          YYABORT;
         }
        $$ = type_integer;
         }
	;

BoolArg
	:NumExpression RelOp NumExpression 
	{
          if ($1 != type_integer || $3 != type_integer) {
           yyerror("Operands of relational operators must be integers.");
           YYABORT;
         }
        $$ = type_bool;
         }
	|SetExpression { $$ = type_bool; }
	|T_TRUE { $$ = type_bool; }
	|T_FALSE { $$ = type_bool; }
	|Arg 
	{
          if ($1 != type_bool) {
           yyerror("Expected boolean expression.");
           YYABORT;
         }
        $$ = type_bool;
         }
	;

RelOp
	:T_GT
	|T_LT
	|T_EQ
	|T_GE
	|T_LE
	;
	
SetExpression
	:Arg T_SUBSET Arg T_SET_OPS Arg 
	{
	if ($1 != type_int_set || $3 != type_int_set || $5 != type_int_set) {
	 yyerror("Set operations require set operands.");
	 YYABORT;
         }
	$$ = type_bool;
	}
	|Arg T_SUBSET Arg 
	{
	if ($1 != type_int_set || $3 != type_int_set) {
	 yyerror("Subset requires set operands.");
	 YYABORT;
	}
	$$ = type_bool;
	}
	|Arg T_IN Arg 
	{
	if ($3 != type_int_set || ($1 != type_integer)) {
	 yyerror("Membership requires element ∈ set.");
	 YYABORT;
	}
	$$ = type_bool;
	}
	;

Arg
	:T_NUM { $$ = type_integer; }
	|T_ID
	{
         ParType t = lookup_type(symbolTable, $1);
         if (t == type_undef) {
          yyerror("Undeclared identifier.");
          YYABORT;
        }
       $$ = t;
        }
	|T_ID '[' Arg ']' 
	{
         ParType t = lookup_type(symbolTable, $1);
         if (t != type_int_array && t != type_bool_array) {
          yyerror("Identifier is not an array.");
          YYABORT;
        }
        if ($3 != type_integer) {
         yyerror("Array index must be integer.");
         YYABORT;
        }
       $$ = (t == type_int_array) ? type_integer : type_bool;
        }
	;

Solve_item
	:T_SOLVE SolutionType
	;

SolutionType
	:T_SATISFY
	|T_MAXIMIZE T_ID
	;




%%




int main(int argc, char **argv ){
  
   ++argv, --argc;  /* skip over program name */
   if ( argc > 0 )
       yyin = fopen( argv[0], "r" );
   else
      yyin = stdin;

   int result = yyparse();

   if (result == 0 && semantic_errors == 0)
      printf("Syntax OK!\n");
   else
      printf("There were %d errors in code. Failure!\n", semantic_errors);
   return result;
}

