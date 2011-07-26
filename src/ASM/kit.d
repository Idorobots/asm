/**********************************************************************************
 * Copyright (c) 2011 Kajetan Rzepecki <kajetan.rzepecki@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *********************/

/***********************************************************************************
 * ASMKit implementations and global values..
 *********************/

module ASM.kit;

import ASM.lexical;
import ASM.AST;         //FIXME: Cyclic imports.

/***********************************************************************************
 * GLOBALS:
 *********************/

Expression FNORD;

static this() {
    FNORD = new Symbol(Keywords.Fnord);
}

/***********************************************************************************
 * DEFINITIONS:
 *********************/

Expression GET(ref Scope s, Expression[] args) {
    //if(args.length != 1)
        //throw new SemanticError("Syntax keyword '"~Keywords.Get~"' requires exactly one argument.");
    if(args[0].type & Type.Symbol) return s.get(args[0].toString);
    //else throw new SemanticError("Undefined symbol '"~args[0].toString~"'.");
    assert(0);
}