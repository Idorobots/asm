/**********************************************************************************
 * Released under the MIT license <http://www.opensource.org/licenses/mit-license.php>
 * For licensing details see the included LICENSE file.
 *********************/

module dasm.lexer;

import std.regex, std.algorithm;

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
        t.equals(got, expected, line);
    }

    test("()", [`"\("`, `"\)"`]);
    test("( wat lol )", [`"\("`, "wat", "lol", `"\)"`]);
    test("(wat lol)", [`"\("`, "wat", "lol", `"\)"`]);

    syntax ~= ["\\(\\>", "\\<\\)"];

    test("(><)", [`"\(\>"`, `"\<\)"`]);
    test("((>)<)", [`"\("`, `"\(\>"`, `"\)"`, `"\<\)"`]);
    test("(wat (> lol <) omfg)", [`"\("`, "wat", `"\(\>"`, "lol", `"\<\)"`, "omfg", `"\)"`]);
}