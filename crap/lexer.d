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

import  std.regex : match, regex;
import std.algorithm : sort;

import utils.testing;
import utils.ctfe : split, find;

bool lenCompare(string a, string b) {
    return a.length == b.length ? a > b : a.length > b.length;
}

string noMatch(string arg) {
    return arg;
}

Array[] lex(Array)(Array input, func[Array] syntax) {
    if(!input.length) return [];
    if(!syntax.length) return [input];

    auto tokens = syntax.keys;
    sort!"a.length < b.length"(tokens);

    foreach(token; tokens) {
        auto m = match(input, regex(token));
        if(!m.empty)
            return lex(m.pre, syntax)~
                   syntax[token](m.hit)~
                   lex(m.post, syntax);
    }
    return [noMatch(input)];
}

alias string[] delegate (string) func;

unittest {
    auto t = TestCase("Lexer.lex");
    func[string] syntax;

    void test(int line = __LINE__)(string input, string[] expected) {
        auto got = lex(input, syntax);
        t.assertion!"=="(got, expected, line);
    }

    //Whitespaces and symbols:
    syntax["[ \n\r\t\v\f]+"] = (string) {return cast(string[])[];};
    syntax["[^ \n\r\t\v\f]+"] = (string arg) {return ["IDENT", arg];};

    test(" ", cast(string[])[]);
    test("test", ["IDENT", "test"]);
    test(" test ", ["IDENT", "test"]);
    test("test test", ["IDENT", "test", "IDENT", "test"]);
    test("\twat\vlol\r\ntest", ["IDENT", "wat", "IDENT", "lol", "IDENT", "test"]);
    test("IDENT", ["IDENT", "IDENT"]);
    test("(test)", ["IDENT", "(test)"]);
    test("1234567890", ["IDENT", "1234567890"]);
    test("!@#$%%^&*()_-\"}{}]/\\~`',<>.?=:;|[", ["IDENT", "!@#$%%^&*()_-\"}{}]/\\~`',<>.?=:;|["]);

    //Parens:
    syntax["\\("] = (string) {return ["LPAREN"];};
    syntax["\\)"] = (string) {return ["RPAREN"];};

    test("( )", ["LPAREN", "RPAREN"]);
    test("()", ["LPAREN", "RPAREN"]);
    test("( wat lol )", ["LPAREN", "IDENT", "wat", "IDENT", "lol", "RPAREN"]);
    test("(wat lol)", ["LPAREN", "IDENT", "wat", "IDENT", "lol", "RPAREN"]);
    test("(test (test2))", ["LPAREN", "IDENT", "test", "LPAREN", "IDENT", "test2", "RPAREN", "RPAREN"]);

    //Comments:
    syntax["#.*"] = (string arg) {
        if(arg.length == 1) return ["IDENT", "#"];
        if(arg[1] == ' ')   return lex(arg[find!"\n\0"(arg) .. $], syntax);
        return ["IDENT", "#"] ~ lex(arg[1 .. $], syntax);
    };

    test("# comment\n", cast(string[])[]);
    test("#not-a-comment", ["IDENT", "#", "IDENT", "not-a-comment"]);
    test("# comment #not-a-comment\n", cast(string[])[]);
    test("# comment\n other stuff", ["IDENT", "other", "IDENT", "stuff"]);

    //Strings and regexes:
    syntax["\"[^\"]+\""] = (string arg) {return ["STRING", arg];};

    test(`"some string"`, ["STRING", `"some string"`]);
    test(`"whi\ftespa\tce\ns "`, ["STRING", `"whi\ftespa\tce\ns "`]);
}

void main() {

}