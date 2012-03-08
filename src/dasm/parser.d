/**********************************************************************************
 * Released under the MIT license <http://www.opensource.org/licenses/mit-license.php>
 * For licensing details see the included LICENSE file.
 *********************/

/***********************************************************************************
 * Code parsing module.
 *********************/

module dasm.parser;

import std.conv : to;
import std.array : front, popFront, appender;

debug import std.stdio;
debug import utils.testing;

import utils.exception : MyException;
import utils.ctfe : contains, ETuple, indexOf, toUpper;

import dasm.lexical;
import dasm.ast;

/***********************************************************************************
 * Exception thrown on a syntactic error.
 *********************/

class SyntacticError : MyException {
    public this(string what, uint line, string file) {
        super(file~"("~to!string(line)~"): "~what);
    }
}

/***********************************************************************************
 * Mismatched token exception..
 *********************/

class MismatchError : SyntacticError {
    public this(string token, uint line, string file) {
        super("Mismatched `"~token~"'.", line, file);
    }
}

/***********************************************************************************
 * A class doing the parsing. Lexes a string into a stream of tokens and then parses
 * them into an AST.
 *********************/

class Parser {
    private:

    /***********************************************************************************
     * Extended syntax tokens.
     *********************/

    enum ESyntax : string {
        Keyword         = "@",          ///Sticks a keyword to an expression.
        Quote           = "'",          ///Quoting: '(#whatever)
        Quasiquote      = "`",          ///Quasiquoting.
        Embed           = "$",          ///'(some expression $with_embedded_symbol
                                        ///    $(* and expression))
                                        ///"A string with an $embedded-variable"
    }
    string[] stringBank;                ///Stores string literals for convinient parsing later on.

    uint lineCount;                     /// For preprocess();
    uint lineNumber;                    /// For parse();
    string fileName;                    /// For parse();

    /***********************************************************************************
     * Removes comments, substitutes syntactic sugars, etc.
     ****************************************/

    string preprocess(in string input)
    body {
        stringBank.clear;

        /***********************************************************************************
         * Adds some metadata - line numbers.
         * TODO: Escape sequences \" etc.
         *********************/

        string addMetadata(in string input) {
            auto spc = Lexical.Space;
            auto c = Lexical.CommentStart;

            auto output = appender!string(""~c~"LINE"~spc~to!string(lineCount++)~spc);
            output.reserve(input.length);

            auto s = (input~Lexical.EndOfFile).ptr;
            bool inAString = false;
            bool addCount = false;

            while(*s) {
                output.put(*s);
                if(*s == Lexical.EndOfLine) {
                    if(!inAString) output.put(""~spc~c~"LINE"~spc~to!string(lineCount++)~spc);
                    else {
                        addCount = true;
                        lineCount++;
                    }
                }
                else if(*s == Syntax.StringDelim) {
                    if(!(inAString && s[-1] && s[-1] == '\\')) {
                        inAString = !inAString;
                    }
                    if(!inAString && addCount) {
                         addCount = false;
                         output.put(""~spc~c~"LINE"~spc~to!string(lineCount-1)~spc);
                    }
                }
                s++;
            }
            return output.data;
        }

        /***********************************************************************************
         * Stores string literals in stringBank to ease parsing.
         *********************/

        string stringCollapse(in string input) {
            auto output = appender!string();
            output.reserve(input.length);

            auto hexes = "0123456789ABCDEF";
            auto escapeSequences = ['\\':'\\', 'n':'\n', 't':'\t', '$':'$', 'v':'\v', 'r':'\r', '"':'"'];
            auto s = (input~Lexical.EndOfFile).ptr;

            while(*s) {
                if(*s == Syntax.StringDelim) {
                    //TODO: StringParser.
                    auto str = appender!string();
                    ++s;
                    while(*s && *s != Syntax.StringDelim) {
                        if(*s == Lexical.EscapeStart) {
                            ++s;
                            // Regular escape sequences.
                            if(*s && *s in escapeSequences) {
                                str.put(escapeSequences[*s++]);
                            }
                            // Hex bytes
                            else if(*s && *s == 'x') {
                                ++s;

                                auto c = 16 * hexes.indexOf(toUpper(s[0])) + hexes.indexOf(toUpper(s[1]));

                                str.put(cast(char) c);
                                s += 2;
                            }
                            else {
                                throw new SyntacticError("Unknown escape sequence `\\" ~ *s ~ "'.",
                                                         lineNumber,
                                                         fileName);
                            }
                        }
                        else str.put(*s++);
                    }
                    ++s;
                    output.put(Syntax.StringDelim~to!string(stringBank.length)~Lexical.Space);
                    stringBank ~= str.data;
                }
                else output.put(*s++);
            }
            return output.data;
        }

        /***********************************************************************************
         * Removes comments.
         *********************/

        string commentCollapse(in string input) {
            auto output = appender!string();
            output.reserve(input.length);

            auto s = (input~Lexical.EndOfFile).ptr;
            while(*s) {
                if(*s == Lexical.CommentStart) {
                    if(s[1] && contains(cast(string) [Lexical.CommentStart,
                                                      Lexical.Space,
                                                      Lexical.Bang],
                                        s[1]))
                        while(*s && *s != Lexical.EndOfLine) s++;
                }
                output.put(*s++);
            }
            return output.data;
        }

        /***********************************************************************************
         * Adds spaces and expands scope brackets into regular ones.
         *********************/

        string syntaxExpand(in string input) {
            auto expanded = appender!string();
            expanded.reserve(input.length);

            auto s = (input~Lexical.EndOfFile).ptr;
            string syntax =  [Syntax.LTuple, Syntax.RTuple,      //Tuple parentesis
                              Syntax.LVector,Syntax.RVector,     //Vector parentesis
                              Syntax.LSet,   Syntax.RSet,        //Set parentesis
                              Syntax.StringDelim];               //String delimiter.
            while(*s) {
                if(contains(syntax, *s)) expanded.put(""~Lexical.Space~s[0 .. 1]~Lexical.Space);
                else expanded.put(s[0 .. 1]);
                s++;
            }

            auto output = appender!string();
            output.reserve(expanded.data.length);

            s = (expanded.data~Lexical.EndOfFile).ptr;
            string prefix = [Lexical.CommentStart,              //Sexp comment.
                             ESyntax.Keyword[0],                //. -> nothing. //TODO
                             ESyntax.Quote[0],                  //' -> (quote )
                             ESyntax.Quasiquote[0],             //` -> (qquote )
                             ESyntax.Embed[0]];                 //$ -> (embed )
            bool jollyRogger = true;
            while(*s) {
                if(*s <= Lexical.Space) {
                    output.put(s[0 .. 1]);
                    jollyRogger = true;
                }
                else if(jollyRogger && contains(prefix, *s))
                    output.put(""~Lexical.Space~s[0 .. 1]~Lexical.Space);
                else {
                    output.put(s[0 .. 1]);
                    jollyRogger = false;
                }
                s++;
            }
            return output.data;
        }

        auto output = appender!string();
        auto s = (syntaxExpand(commentCollapse(stringCollapse(addMetadata(input))))~Lexical.EndOfFile).ptr;

        while(*s && *s < 0x21) s++;                                     //Spaces, spaces everywhere.
        while(*s) {
            switch(*s) {
                case Lexical.Space:                                     //Spaces.
                    output.put(*s++);
                    while(*s && *s <= Lexical.Space) s++;
                    continue;
                case 0x0: .. case 0x1F:                                 //Whitespaces
                    s++;
                    continue;
                default: break;
            }
            output.put(*s++);
        }
        if(output.data.length && output.data[$-1] == Lexical.Space) return output.data[0 .. $-1];
        return output.data;
    }
    unittest {
        auto t = TestCase("Parser.preprocess");
        auto i = new Parser();

        void test(int line = __LINE__)(string input, string expected) {
            auto actual = i.preprocess(input);
            t.equals(actual, expected, line);
        }

        void test1(int line = __LINE__)(string input, string expected) {
            i.preprocess(input);
            t.equals(i.stringBank[0], expected, line);
        }

        //TODO: FIXME: Unittests should be Syntax-independant.
        ///Whitespaces:
        test("   \n\r  Leading whitespaces.",
             "# LINE 0 # LINE 1 Leading whitespaces.");
        test("Trailing whitespaces.  \n\t  \r\n",
             "# LINE 2 Trailing whitespaces. # LINE 3 # LINE 4");
        test("Lots\t\na white\r\nspaces      \nand\t shit.",
             "# LINE 5 Lots # LINE 6 a white # LINE 7 spaces # LINE 8 and shit.");
        test("Lots\t\na white\r\nspaces   \"   \nand a string\".",
             "# LINE 9 Lots # LINE 10 a white # LINE 11 spaces \" 0 # LINE 12 .");
        test("Some multi\nline str\ning without comments.",
             "# LINE 13 Some multi # LINE 14 line str # LINE 15 ing without comments.");
        ///Parenteses:
        test("String (with parenteses (all over)).",
             "# LINE 16 String ( with parenteses ( all over ) ) .");
        test("String (with [different] {parens (all)} over).",
             "# LINE 17 String ( with [ different ] { parens ( all ) } over ) .");
        ///Comments:
        test("Some string with a # line comment\n.",
             "# LINE 18 Some string with a # LINE 19 .");
        test("Some string ##with a line\n comment.",
             "# LINE 20 Some string # LINE 21 comment.");
        test("Some string #(with Expression) comment.",
             "# LINE 22 Some string # ( with Expression ) comment.");
        test("Some string #{with Set} comment.",
             "# LINE 23 Some string # { with Set } comment.");
        test("Some string #(with an (Expression embedded in a) Expression) comment.",
             "# LINE 24 Some string # ( with an ( Expression embedded in a ) Expression ) comment.");
        test("String with a single #word comment.",
             "# LINE 25 String with a single # word comment.");
        test("[Expression with a single word #comment.]",
             "# LINE 26 [ Expression with a single word # comment. ]");
        test("{Expression with (a)#comment rigth #next} to a paren.",
             "# LINE 27 { Expression with ( a ) # comment rigth # next } to a paren.");
        ///Strings and comments:
        test("String with a \"#(comment in a string), lol.\"",
             "# LINE 28 String with a \" 0");
        test("A #\"string commented out\".",
             "# LINE 29 A # \" 0 .");
        test("Some string \"with an \nembeded \tstring\" in it.",
             "# LINE 30 Some string \" 0 # LINE 31 in it.");
        test("\"Comment rigth next to a string.\"#comment",
             "# LINE 32 \" 0 # comment");
        test("#Comment_rigth\"next to a string.\"",
             "# LINE 33 # Comment_rigth \" 0");
        ///Expression embedding and escape sequences:
        test1(`"\tString with\nescape sequences.\r\n"`, "\tString with\nescape sequences.\r\n");
        test1(`"String with an embeded \"string\".`, "String with an embeded \"string\".");
        test1(`"$Strings with simple $embeds in it"`, "$0 with simple $1 in it");
        test1(`"$String-with? more $(complex 3.14 2.71) embeds."`, "$0 more $1 embeds.");
        test1(`"$(Really {complex} embed (with multiple (parens) [and whatnot]))"`, "$0");
        test1(`"${Same deal (but {with another [set of]} parens.)}"`, "$0");
        test1(`"$[Same () thing for [ a {list}]]"`, "$0");
        ///Keywords dispatch:
        //TODO: Shit, those line numbers are a pain in the ass when adding new tests.
        test("String with @keywords in it.",
             "# LINE 41 String with @ keywords in it.");
        test("(Expression with @keywords (and @other expressions)@in it.)",
             "# LINE 42 ( Expression with @ keywords ( and @ other expressions ) @ in it. )");
        test("(@keyword rigth next to)@a paren.",
             "# LINE 43 ( @ keyword rigth next to ) @ a paren.");
        ///General fun:
        test("not@keyword.",
             "# LINE 44 not@keyword.");
        test("not#a#comment#",
             "# LINE 45 not#a#comment#");
        test("not$an$embed$",
             "# LINE 46 not$an$embed$");
        test("not`a`qquote`",
             "# LINE 47 not`a`qquote`");
        test("not'a'quote'",
             "# LINE 48 not'a'quote'");
    }

    /***********************************************************************************
     * Turns a single statement into several tokens.
     *********************/

    string[] tokenize(in string input)
    body {
        auto tokens = appender!(string[]);
        string token;
        string parens = [Syntax.LTuple, Syntax.RTuple,      //Tuple parentesis
                         Syntax.LVector,Syntax.RVector,     //Vector parentesis
                         Syntax.LSet,   Syntax.RSet];       //Set parentesis
        auto s = (input~Lexical.EndOfFile).ptr;
        while(*s) {
            if(*s == ' ') {
                if(token != "") {
                    tokens.put(token);
                    token = "";
                }
            }
            else if(contains(parens, *s)) tokens.put(s[0 .. 1]);
            else token ~= *s;
            s++;
        }
        if(token != "") tokens.put(token);
        return tokens.data;
    }
    unittest {
        auto t = TestCase("Parser.tokenize");
        auto i = new Parser();

        void test(int line = __LINE__)(string input, string[] expected) {
            auto actual = i.tokenize(input);
            t.equals(actual, expected, line);
        }

        test("A simple string.", ["A", "simple", "string."]);
        test("( A simple statement. )", ["(", "A", "simple", "statement.", ")"]);
        test("( Not so ( simple statement ( at ) ) all. )",
            ["(", "Not", "so", "(", "simple", "statement", "(", "at", ")", ")", "all.", ")"]);
        test("( Many [ different { parens } and ] other ) things.",
            ["(", "Many", "[", "different", "{", "parens", "}", "and", "]", "other", ")", "things." ]);
    }

    /***********************************************************************************
     * Parses a stream of tokens into a single AST.
     *********************/

    Expression parse(ref string[] tokens) {
        if(!tokens.length) return null;
        auto token = tokens.front;
        tokens.popFront;

        //All kinds of parenthesis.
        immutable(char)[immutable(char)] parens = [Syntax.LTuple : Syntax.RTuple,
                             Syntax.LVector: Syntax.RVector,
                             Syntax.LSet :   Syntax.RSet];
        immutable(char)[immutable(char)] antyparens = [Syntax.RTuple : Syntax.LTuple,
                                 Syntax.RVector :Syntax.LVector,
                                 Syntax.RSet :   Syntax.LSet];
        //Extended syntax.
        string[string] expandables = [ESyntax.Quote :      Keywords.Quote,
                                      ESyntax.Quasiquote : Keywords.Quasiquote,
                                      ESyntax.Embed :      Keywords.Embed];

        if(token == ""~Syntax.StringDelim) {
            auto index = tokens.front;             //The next token is _always_ the offset in string bank.
            tokens.popFront;
            return new String(stringBank[to!uint(index)], lineNumber, fileName);
        }
        if(token == ESyntax.Keyword) {
            if(tokens.length) {
                auto keyword = parse(tokens);
                if((keyword.type & Type.Symbol) && !(keyword.type & Type.String)) {
                    if(tokens.length) {
                        auto expr = parse(tokens);
                        expr.keywords(keyword.toString);
                        return expr;
                    }
                }
                else throw new SyntacticError("Invalid keyword '"~keyword.toString~"'.", lineNumber, fileName);
            }
        }
        if(token == ""~Lexical.CommentStart) {     //Deletes the following expression from the token stream.
            if(tokens.length) {
                auto str = parse(tokens).toString; //It still does the syntactic analyzis,
                if(str == "LINE") {
                    lineNumber = to!uint(tokens.front);
                    tokens.popFront;
                }
            }
            return null;                           //as this is ment solely for debugging.
        }
        if(contains(expandables.keys, token)) {
            auto symbol = new Symbol(expandables[token], lineNumber, fileName);
            auto expr = parse(tokens);

            return new Tuple([symbol, expr ? expr : new Tuple([], lineNumber, fileName)]);
        }
        if(contains(parens.keys, token[0])) {
            auto delimiter = cast(immutable(char))parens[token[0]];
            Expression[] collection;

            do {
                if(!tokens.length) throw new MismatchError(token, lineNumber, fileName);
                if(tokens.front == ""~delimiter) break;
                if(auto e = parse(tokens)) collection ~= e;
            } while(true);
            tokens.popFront;    //Remove the delimiter from the token stream.

            switch(delimiter) {
                case Syntax.RTuple:
                    return new Tuple(collection, lineNumber, fileName);
                case Syntax.RVector:
                    return new Vector(collection, lineNumber, fileName);
                case Syntax.RSet:
                    return new Set(collection, lineNumber, fileName);
                default: assert(0);
            }
        }
        if(contains(antyparens.keys, token[0])) throw new MismatchError(token, lineNumber, fileName);

        //The last possible case - a symbol or a number.
        try { //BUG FIXME with a rake or something. Seriously?
            auto badShit = ["-", "in", "In", "IN", "iN", "I", "i"]; //Or shall I say "bat-shit".
            if(contains(badShit, token))
                throw new Exception("A Phobos bug workarround.");
            auto value = to!real(token);
            return new Number(value, lineNumber, fileName);
        }
        catch(Exception) return new Symbol(token, lineNumber, fileName);
    }

    public:

    /***********************************************************************************
     * Parses a string into an array of ASTs each representing independant statement.
     *********************/

    Expression[] parse(in string input, in string fileName) {
        this.fileName = fileName;
        this.lineCount = 1;    //TODO: reset it from outside.
        auto output = appender!(Expression[]);
        auto tokens = tokenize(preprocess(input));
        while(tokens.length) {
            if(auto e = parse(tokens)) output.put(e);
        }
        return output.data;
    }
}
