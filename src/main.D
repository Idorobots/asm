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

import std.stdio;
import std.conv : to;
import core.memory;

import ASM.interpreter;

/***********************************************************************************
 * GNU Readline.
 *********************/

// extern (C) {
//    char* readline(const char* prompt);
//    void add_history(const char* line);
// }

// string readLine(string prompt) {
//    auto line = readline((prompt~"\0").ptr);
//    auto str = to!string(line);
//    std.c.stdlib.free(line);
//    return str;
// }

// void addHistory(string line) {
//   return add_history((line~"\0").ptr);
// }

int main(string[] args) {
    auto i = new Interpreter();

    i.doString("(import 'imports.core)");

    /***********************************************************************************
     * Files:
     *********************/

    if(args.length != 1) {
        foreach(arg; args[1 .. $]) {
            try i.doFile(arg);
            catch(Exception e) {
                writeln(e);
            }
        }
        return 0;
    }

    /***********************************************************************************
     * REPL:
     *********************/

    while(true) {
        /*auto input = readLine("> ");
        if(input == "q") break;
        addHistory(input);*/
        write("> ");
        auto input = readln();
        if(input == "q\n") break;

        try {
            writeln("\t", i.doString(input.idup));
        }
        catch(Exception e) {
            writeln("\t", e);
        }
        GC.collect();   //FIXME Fix memory management.
        GC.minimize();
    }
    return 0;
}
