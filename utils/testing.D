/***********************************************************************************
 * Simple yet functional testing helper for the D programming language.
 * Copyright (C) 2011 Kajetan Rzepecki
 *********************/

module utils.testing;

import utils.exception;

/***********************************************************************************
 * Exception thrown in the unittests, it's more convinient than a simple assert.
 *********************/

class AssertionException : MyException {
    private string what;

    this(string type, string file, string line, string expected, string got) {
        what =  "Assertion (\""~type~"\") failure at "~file~"("~line~"):"
                " got: \'"~got~"\', expected: \'"~expected~"\'.";
        super(what);
    }

    this(string file, string line) {
        what = "Assertion failure at "~file~"("~line~").";
        super(what);
    }
}

/***********************************************************************************
 * A test class.
 *********************/

struct TestCase {
    private string name;
    private uint runs;
    private Exception[] fails;

    /***********************************************************************************
     * Default and string ctor.
     *********************/

    this(string name = "") {
        this.name = name;
    }

    /***********************************************************************************
     * This stores the results in TestStats class for later use.
     *********************/

    ~this() {
        TestStats.add(new TestResult(name, runs, fails));
    }

    /***********************************************************************************
     * An empty assertion, always to be a failure.
     *********************/

    public void failure(int line = __LINE__, string file = __FILE__) {
        runs++;
        fails ~= new AssertionException(file, to!string(line));
    }

    /***********************************************************************************
     * An empty assertion, always to be a pass.
     *********************/

    public void success() {
        runs++;
    }

    /***********************************************************************************
     * e.g. test.assertion!("!=")(3, 3.1415);
     *********************/

    import std.conv : to;
    public void assertion(string op, T)(T got, T expected,
                                        int line = __LINE__, string file = __FILE__)
    {
        runs++;
        if(!mixin("got "~op~" expected"))
                fails ~= new AssertionException(op, file, to!string(line),
                                                to!string(expected), to!string(got));
    }

    /***********************************************************************************
     * e. g. test.assertion(2 == 3);
     *********************/

    public void assertion(T : bool)(T got, int line = __LINE__, string file = __FILE__) {
        runs++;
        if(!got)
            fails ~= new AssertionException("boolean", file, to!string(line), "true", "false");
    }
}

/***********************************************************************************
 * A class used to store TestCase results for statistics.
 *********************/

class TestResult {
    private string name;
    private uint runs;
    private Exception[] fails;

    this(string name, uint runs, Exception[] fails) {
        this.name = name;
        this.runs = runs;
        this.fails = fails;
    }
}

/***********************************************************************************
 * A class that collects all test results and prints stats for them.
 *********************/

static class TestStats {
    private static TestResult[] results;
    private static uint runs, fails;

    /***********************************************************************************
     * Adds new test results to results.
     *********************/

    private static void add(TestResult r) {
        results ~= r;
    }

    /***********************************************************************************
     * A static Dtor that is called after the main has returned, writes the stats.
     *********************/

    import std.stdio : writeln, writefln;
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
