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
 * Simple yet functional testing helper for the D programming language.
 *********************/

module utils.testing;

import std.stdio : writeln, writefln;
import std.conv : to;

/***********************************************************************************
 * Error message functions.
 *********************/

string errorMsg(string file, string line, string expected, string got) {
    return file~"("~line~"): Expected `"~expected~"', got `"~got~"'.";
}

string errorMsg(string file, string line) {
    return file~"("~line~").";
}

/***********************************************************************************
 * A test class.
 *********************/

struct TestCase {
    private string name;
    private uint runs;
    private string[] fails;


    this(string name = "") {
        this.name = name;
    }

    /***********************************************************************************
     * Stores the results in TestStats class for later use.
     *********************/

    ~this() {
        TestStats.add(TestResult(name, runs, fails));
    }

    void failure(uint line = __LINE__, string file = __FILE__) {
        runs++;
        fails ~= errorMsg(file, to!string(line));
    }

    void success() {
        runs++;
    }

    /***********************************************************************************
     * DRY my ass. FIXME
     *********************/

    void equals(T)(T got, T expected, uint line = __LINE__, string file = __FILE__) {
        runs++;
        if(!(got == expected))
            fails ~= errorMsg(file, to!string(line), to!string(expected), to!string(got));
    }

    void notEquals(T)(T got, T expected, uint line = __LINE__, string file = __FILE__) {
        runs++;
        if(!(got != expected))
            fails ~= errorMsg(file, to!string(line), to!string(expected), to!string(got));
    }

    void same(T)(T got, T expected, uint line = __LINE__, string file = __FILE__) {
        runs++;
        if(!(got is expected))
            fails ~= errorMsg(file, to!string(line), to!string(expected), to!string(got));
    }

    void truth(bool test, uint line = __LINE__, string file = __FILE__) {
        runs++;
        if(!test) fails ~= errorMsg(file, to!string(line));
    }
}

/***********************************************************************************
 * A struct used to store TestCase results for statistics.
 *********************/

struct TestResult {
    private string name;
    private uint runs;
    private string[] fails;

    this(string name, uint runs, string[] fails) {
        this.name = name;
        this.runs = runs;
        this.fails = fails;
    }
}

/***********************************************************************************
 * A struct that collects all test results and prints stats for them.
 *********************/

struct TestStats {
    static TestResult[] results;
    static uint runs, fails;

    /***********************************************************************************
     * Adds new test results to results.
     *********************/

    static void add(TestResult r) {
        results ~= r;
    }

    /***********************************************************************************
     * A static Dtor that is called after the main has returned, writes the stats.
     *********************/

    static ~this() {
        if(!results.length) return;

        writeln("# TEST RESULTS #\n");
        foreach(r; results) {
            runs += r.runs;
            writefln("%-20s %s", r.name~":", (!r.fails.length) ? "pass" : "");
            if(r.fails.length) {
                fails += r.fails.length;
                writeln("\t", r.runs, " assertions:");
                writeln("\t", r.runs - r.fails.length, " assertions passed");
                writeln("\t", r.fails.length," assertions failed:");
                foreach(f; r.fails) {
                    writeln("\t\t", f);
                }
            }
        }
        writefln("\n%-20s %d", "Total assertions:", runs);
        auto ratio = (cast(double)fails/runs)*100;
        if(fails) writefln("%-20s %d (%.1f%%)", "Total failures:", fails, ratio);
        if(ratio >= 50) writeln("Wow, you suck...");    //What? You know it's true...

        writeln("\n# END RESULTS #");
    }
}