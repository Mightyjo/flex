/*
 * This file is part of flex.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
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

#include "config.h"

%}

%option 8bit prefix="test"
%option warn c++
%option skel="../src/flex.cc.skl" header-file="cxx_skel.h"
%option nounput nomain noinput noyywrap 

%%

.              { }

%%

#define yyFlexLexer testFlexLexer
#include "cxx_skel.h"

int main(void);

int
main (void)
{
    yyFlexLexer f;
    f.switch_streams(&std::cin, &std::cout);
    f.yylex();
    std::cout << "TEST RETURNING OK." << std::endl;
    return 0;
}
