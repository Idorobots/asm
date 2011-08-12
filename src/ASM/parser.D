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
 * Code parsing module.
 *********************/

module ASM.parser;

import std.conv : to;
import std.array : front, popFront;

debug import std.stdio;
debug import utils.testing;

import utils.exception : MyException;
import utils.ctfe : contains, ETuple;

import ASM.lexical;
import ASM.AST;

/***********************************************************************************
 * Exception thrown on a syntactic error.
 *********************/

class SyntacticError : MyException {
    public this(string what) {
        super("Error: "~what);
    }
}


/***********************************************************************************
 * Mismatched token exception..
 *********************/

class MismatchError : SyntacticError {
    public this(string token) {
        super("Mismatched '"~token~"'.");
    }
}
/***********************************************************************************
 * An abstract class for an easy substitution. If you need a faster-than-default
 * parser - feel free to make one.
 *********************/

abstract class Parser {
    /***********************************************************************************
     * Parses a string into an AST.
     *********************/

    public Expression[] parse(in string input);
}

/***********************************************************************************
 * A class doing the parsing. Lexes a string into a stream of tokens and then parses
 * them into an AST.
 *********************/

class DefaultParser : Parser {

    /***********************************************************************************
     * Extended syntax tokens.
     *********************/

    enum ESyntax : string {
        Keyword         = ".",          ///Sticks a keyword to an expression.
        Quote           = "'",          ///Quoting: '(#whatever)
        Quasiquote      = "`",          ///Quasiquoting.
        Embed           = "$",          ///'(some expression $with_embedded_symbol
                                        ///    $(* and expression))
                                        ///"A string with an $embedded-variable"
    }
    string[] stringBank;                ///Stores string literals for convinient parsing later on.

    /***********************************************************************************
     * Removes comments, substitutes syntactic sugars, etc.
     ****************************************/

    string preprocess(in string input)
    body {
        stringBank.clear;

        /***********************************************************************************
         * Stores string literals in stringBank to ease parsing.
         *********************/

        string stringCollapse(in string input) {
            string output;
            auto s = (input~Lexical.EndOfFile).ptr;
            while(*s) {
                if(*s == Syntax.StringDelim) {
                    //TODO: StringParser.
                    string str;
                    s++;
                    while(*s && *s != Syntax.StringDelim) str ~= *s++;
                    s++;
                    output ~= Syntax.StringDelim~to!string(stringBank.length)~Lexical.Space;
                    stringBank ~= str;
                }
                else output ~= *s++;
            }
            return output;
        }

        /***********************************************************************************
         * Removes comments.
         *********************/

        pure string commentCollapse(in string input) {
            string output;
            auto s = (input~Lexical.EndOfFile).ptr;
            while(*s) {
                if(*s == Lexical.CommentStart) {
                    if(s[1] && contains([Lexical.CommentStart, Lexical.Space], s[1]))
                        while(*s && *s != Lexical.EndOfLine) s++;
                }
                output ~= *s++;
            }
            return output;
        }

        /***********************************************************************************
         * Adds spaces and expands scope brackets into regular ones.
         *********************/

        pure string syntaxExpand(in string input) {
            string expanded;
            auto s = (input~Lexical.EndOfFile).ptr;
            auto syntax = [Syntax.LTuple, Syntax.RTuple,      //Tuple parentesis
                           Syntax.LList,  Syntax.RList,       //List parentesis
                           Syntax.LSet,   Syntax.RSet,        //Set parentesis
                           Syntax.StringDelim];               //String delimiter.
            while(*s) {
                if(contains(syntax, *s)) expanded ~= ""~Lexical.Space~s[0 .. 1]~Lexical.Space;
                else expanded ~= s[0 .. 1];
                s++;
            }

            string output;
            s = (expanded~Lexical.EndOfFile).ptr;
            auto prefix = [Lexical.CommentStart,               //Sexp comment.
                           ESyntax.Keyword[0],                //. -> nothing. //TODO
                           ESyntax.Quote[0],                  //' -> (quote )
                           ESyntax.Quasiquote[0],             //` -> (qquote )
                           ESyntax.Embed[0]];                 //$ -> (embed )
            bool jollyRogger = true;
            while(*s) {
                if(*s <= Lexical.Space) {
                    output ~= s[0 .. 1];
                    jollyRogger = true;
                }
                else if(jollyRogger && contains(prefix, *s))
                    output ~= ""~Lexical.Space~s[0 .. 1]~Lexical.Space;
                else {
                    output ~= s[0 .. 1];
                    jollyRogger = false;
                }
                s++;
            }
            return output;
        }

        string output;
        auto s = (syntaxExpand(commentCollapse(stringCollapse(input)))~Lexical.EndOfFile).ptr;

        while(*s && *s < 0x21) s++;                                     //Spaces, spaces everywhere.
        while(*s) {
            switch(*s) {
                case Lexical.Space:                                     //Spaces.
                    output ~= *s++;
                    while(*s && *s <= Lexical.Space) s++;
                    continue;
                case 0x0: .. case 0x1F:                                 //Whitespaces
                    s++;
                    continue;
                default: break;
            }
            output ~= *s++;
        }
        if(output.length && output[$-1] == Lexical.Space) return output[0 .. $-1];
        return output;
    }
    unittest {
        auto t = TestCase("DefaultParser.preprocess");
        auto i = new DefaultParser();

        void test(int line = __LINE__)(string input, string expected) {
            auto actual = i.preprocess(input);
            t.assertion!"=="(actual, expected, line);
        }

        void test1(int line = __LINE__)(string input, string expected) {
            i.preprocess(input);
            t.assertion!"=="(i.stringBank[0], expected);
        }

        //TODO: FIXME: Unittests should be Syntax-independant.
        ///Whitespaces:
        test("   \n\r  Leading whitespaces.", "Leading whitespaces.");
        test("Trailing whitespaces.  \n\t  \r\n", "Trailing whitespaces.");
        test("Lots\t\na white\r\nspaces      \nand\t shit.", "Lotsa whitespaces and shit.");
        test("Lots\t\na white\r\nspaces   \"   \nand a string\".", "Lotsa whitespaces \" 0 .");
        test("Some multi\nline str\ning without comments.", "Some multiline string without comments.");
        ///Parenteses:
        test("String (with parenteses (all over)).", "String ( with parenteses ( all over ) ) .");
        test("String (with [different] {parens (all)} over).",
             "String ( with [ different ] { parens ( all ) } over ) .");
        ///Comments:
        test("Some string with a # line comment\n.", "Some string with a .");
        test("Some string ##with a line\n comment.", "Some string comment.");
        test("Some string #(with Expression) comment.", "Some string # ( with Expression ) comment.");
        test("Some string #{with Set} comment.", "Some string # { with Set } comment.");
        test("Some string #(with an (Expression embedded in a) Expression) comment.",
             "Some string # ( with an ( Expression embedded in a ) Expression ) comment.");
        test("String with a single #word comment.", "String with a single # word comment.");
        test("[Expression with a single word #comment.]", "[ Expression with a single word # comment. ]");
        test("{Expression with (a)#comment rigth #next} to a paren.",
             "{ Expression with ( a ) # comment rigth # next } to a paren.");
        ///Strings and comments:
        test("String with a \"#(comment in a string), lol.\"", "String with a \" 0");
        test("A #\"string commented out\".", "A # \" 0 .");
        test("Some string \"with an \nembeded \tstring\" in it.", "Some string \" 0 in it.");
        test("\"Comment rigth next to a string.\"#comment", "\" 0 # comment");
        test("#Comment_rigth\"next to a string.\"", "# Comment_rigth \" 0");
        ///Expression embedding and escape sequences:
        test1("\"\\tString with\\nescape sequences.\\r\\n\"", "\"String with\nescape sequences.\r\n\"");
        test1("\"$Strings with simple $embeds in it\"", "\"$0 with simple $1 in it\"");
        test1("\"$String-with? more $(complex 3.14 2.71) embeds.\"", "\"$0 more $1 embeds.\"");
        test1("\"$(Really {complex} embed (with multiple (parens) [and whatnot]))\"", "\"$0\"");
        test1("\"${Same deal (but {with another [set of]} parens.)}\"","\"$0\"");
        test1("\"$[Same () thing for [ a {list}]]\"","\"$0\"");
        ///Keywords dispatch:
        test("String with .keywords in it.", "String with . keywords in it.");
        test("(Expression with .keywords (and .other expressions).in it.)",
             "( Expression with . keywords ( and . other expressions ) . in it. )");
        test("(.keyword rigth next to).a paren.", "( . keyword rigth next to ) . a paren.");
        ///General fun:
        test("not.a.keyword.", "not.a.keyword.");
        test("not#a#comment#", "not#a#comment#");
        test("not$an$embed$", "not$an$embed$");
        test("not`a`qquote`", "not`a`qquote`");
        test("not'a'quote'", "not'a'quote'");
    }

    /***********************************************************************************
     * Turns a single statement into several tokens.
     *********************/

    pure string[] tokenize(in string input)
    body {
        string[] tokens;
        string token;
        auto parens = [Syntax.LTuple, Syntax.RTuple,      //Tuple parentesis
                       Syntax.LList,  Syntax.RList,       //List parentesis
                       Syntax.LSet,   Syntax.RSet];       //Set parentesis
        auto s = (input~Lexical.EndOfFile).ptr;
        while(*s) {
            if(*s == ' ') {
                if(token != "") {
                    tokens ~= token;
                    token = "";
                }
            }
            else if(contains(parens, *s)) tokens ~= s[0 .. 1];
            else token ~= *s;
            s++;
        }
        if(token != "") tokens ~= token;
        return tokens;
    }
    unittest {
        auto t = TestCase("DefaultParser.tokenize");
        auto i = new DefaultParser();

        void test(int line = __LINE__)(string input, string[] expected) {
            auto actual = i.tokenize(input);
            t.assertion!"=="(actual, expected, line);
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
        auto parens = [cast(immutable(char))Syntax.LTuple : cast(immutable(char))Syntax.RTuple, //FIXME: Pretier!
                       Syntax.LList :  Syntax.RList,
                       Syntax.LSet :   Syntax.RSet];
        auto antyparens = [cast(immutable(char))Syntax.RTuple : cast(immutable(char))Syntax.LTuple,
                           Syntax.RList :  Syntax.LList,
                           Syntax.RSet :   Syntax.LSet];
        //Extended syntax.
        auto expandables = [cast(string)ESyntax.Quote : cast(string)Keywords.Quote,
                            ESyntax.Quasiquote : Keywords.Quasiquote,
                            ESyntax.Embed :      Keywords.Embed];

        if(token == ""~Syntax.StringDelim) {
            auto index = tokens.front;             //The next token is _always_ the offset in string bank.
            tokens.popFront;
            return new String(stringBank[to!uint(index)]);
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
                else throw new SyntacticError("Invalid keyword '"~keyword.toString~"'.");
            }
        }
        if(token == ""~Lexical.CommentStart) {     //Deletes the following expression from the token stream.
            if(tokens.length) parse(tokens);       //It still does the syntactic analyzis,
            return null;                           //as this is ment solely for debugging.
        }
        if(contains(expandables.keys, token)) {
            return new Tuple([new Symbol(expandables[token]),   //FIXME: Pretier!
                              tokens.length ? parse(tokens) : new Symbol(Keywords.Fnord)]);
        }
        if(contains(parens.keys, token[0])) {
            auto delimiter = parens[token[0]];
            Expression[] collection;

            do {
                if(!tokens.length) throw new MismatchError(token);
                if(tokens.front == ""~delimiter) break;
                if(auto e = parse(tokens)) collection ~= e;
            } while(true);
            tokens.popFront;    //Remove the delimiter from the token stream.

            switch(delimiter) {
                case Syntax.RTuple:
                    if(collection.length) return new Tuple(collection);
                    return new Symbol(Keywords.Fnord);
                case Syntax.RList:
                    return new List(collection);
                case Syntax.RSet:
                    return new Set(collection);
                default: assert(0);
            }
        }
        if(contains(antyparens.keys, token[0])) throw new MismatchError(token);

        //The last possible case - a symbol or a number.
        try { //BUG FIXME with a rake or something. Seriously?
            if(token == "-" || token == "in" || token == "I" || token == "i")
                throw new Exception("A Phobos bug workarround.");
            auto value = to!real(token);
            return new Number(value);
        }
        catch(Exception) return new Symbol(token);
    }

    public:

    /***********************************************************************************
     * Parses a string into an array of ASTs each representing independant statement.
     *********************/

    override Expression[] parse(in string input) {
        Expression[] output;
        auto tokens = tokenize(preprocess(input));
        while(tokens.length) {
            if(auto e = parse(tokens)) output ~= e;
        }
        return output;
    }
}
