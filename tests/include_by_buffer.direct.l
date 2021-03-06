/*
 * This file is part of flex.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
f * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the University nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

%{
/* A scanner file to build "scanner.c".
   Input language is any text.
   "#include <filename>" causes a buffer switch.
 */
#include <stdio.h>
#include <stdlib.h>
#include "config.h"

int error = 0;
%}

%option 8bit prefix="test"
%option nounput nomain noyywrap noinput
%option warn

%x GET_FILENAME
%{

#define MAX_INCLUDE_DEPTH 10
yybuffer include_stack[MAX_INCLUDE_DEPTH];
int include_stack_ptr = 0;
%}
%%

<INITIAL>{
^"#include"[[:blank:]]+"<"  { yybegin(GET_FILENAME); }
.|\n      { yyecho(); }
}

<GET_FILENAME>{
[[:alnum:]_.-]+>  {
     /* recurse */
    yytext[yyleng-1]='\0';
    include_stack[include_stack_ptr++] = yy_current_buffer();
    if((yyin=fopen(yytext,"r"))==NULL) {
        fprintf(stderr,"*** Error: Could not open include file \"%s\".\n",yytext);
        error = 1;
        yyterminate();
    }
    yy_switch_to_buffer( yy_create_buffer( yyin, YY_BUF_SIZE ));
    yybegin(0);
    }
.|\n  {
    fprintf(stderr,"Invalid input \"%s\".\n", yytext);
    error = 1;
    yyterminate();
   }
}

<<EOF>> {
       if ( --include_stack_ptr < 0 ) {
           yyterminate();
       }
       else {
            fclose(yyin);
           yy_delete_buffer( yy_current_buffer() );
           yy_switch_to_buffer( include_stack[include_stack_ptr] );
       }
   }

%%

int main(int argc, char** argv);

int
main ( int argc, char** argv )
{
    FILE * fp;
    if( argc != 2 ) {
        fprintf(stderr,"*** Error: Must specify one filename.\n");
        exit(-1);
    }
    if((fp=fopen(argv[1],"r"))==NULL) {
        fprintf(stderr,"*** Error: fopen(%s) failed.\n",argv[1]);
        exit(-1);
    }
    yyin = fp;
    yyout = stdout;
    yylex();
    if (!error)
        printf("TEST RETURNING OK.\n");
    else
        exit(-1);
    return 0;
}
