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
