/**********************************************************************************
 * Released under the MIT license <http://www.opensource.org/licenses/mit-license.php>
 * For licensing details see the included LICENSE file.
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