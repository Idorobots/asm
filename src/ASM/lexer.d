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

module ASM.lexer;

import std.stdio, std.string, std.regex, std.algorithm;

import utils.testing;
import utils.ctfe : split;

bool lenCompare(string a, string b) {
    return a.length == b.length ? a > b : a.length > b.length;
}


Array[] tokenize(Array)(Array input, Array[] syntax) {
    if(!input.length) return [];
    if(!syntax.length) return [input];

    foreach(i, token; syntax) {
        auto m = match(input, regex(token));
        if(!m.empty)
            return tokenize(m.pre, syntax[i .. $])~
                   (""~'"'~token~'"')~
                   tokenize(m.post, syntax[i .. $]);
    }
    return [input];
}

string[] lex(string input, string[] syntax) {
    sort!lenCompare(syntax);

    string[] tokens;
    foreach(part; split!" \t\n\0"(input)) {
        tokens ~= tokenize(part, syntax);
    }

    return tokens;
}
unittest {
    auto t = TestCase("Lexer.lex");
    auto syntax  = ["\\(", "\\)"];

    void test(int line = __LINE__)(string input, string[] expected) {
        auto got = lex(input, syntax);
        t.assertion!"=="(got, expected, line);
    }

    test("()", ["(", ")"]);
    test("( wat lol )", ["(", "wat", "lol", ")"]);
    test("(wat lol)", ["(", "wat", "lol", ")"]);

    syntax ~= ["\\(>", "\\<\\)"];

    test("(><)", ["(>", "<)"]);
    test("((>)<)", ["(", "(>", ")", "<)"]);
    test("(wat (> lol <) omfg)", ["(", "wat", "(>", "lol", "<)", "omfg", ")"]);
}