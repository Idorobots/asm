/***********************************************************************************
 * Code parsing module.
 * TODO: Refactorisation, rewrite perhaps?
 *********************/

module ASM.parser;

import std.conv : to;
import std.array : front, popFront;

debug import std.stdio;
debug import utils.testing;
import utils.exception : MyException;
import utils.ctfe : contains;

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
        Quote           = "\'",         ///Quoting: '(#whatever)
        Quasiquote      = "`",          ///Quasiquoting.
        Embed           = "$",          ///'(some expression $with_embedded_symbol
                                        ///    $(* and expression))
                                        ///"A string with an $embedded-variable"
        StringPrefix    = "__string",   ///Used for convinient string processing.
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
            auto s = (input~"\0").ptr;
            while(*s) {
                if(*s == Syntax.StringDelim) {
                    //TODO: StringParser.
                    string str;
                    str ~= *s++;
                    while(*s && *s != Syntax.StringDelim) str ~= *s++;
                    str ~= *s++;
                    output ~= " "~Syntax.StringDelim~" "~to!string(stringBank.length)~" ";
                    stringBank ~= str;
                }
                else output ~= *s++;
            }
            return output;
        }

        /***********************************************************************************
         * Adds spaces and expands scope brackets into regular ones.
         *********************/

        pure string syntaxExpand(in string input) {
            string output;
            auto s = (input~"\0").ptr;
            auto syntax = [Syntax.LTuple, Syntax.RTuple,      //Tuple parentesis
                           Syntax.LList,  Syntax.RList,       //List parentesis
                           Syntax.LSet,   Syntax.RSet,        //Set parentesis
                           ESyntax.Quote[0],                  //' -> (quote )
                           ESyntax.Quasiquote[0],             //` -> (quasiquote )
                           ESyntax.Embed[0]];                 //$ -> (embed )

            while(*s) {
                if(contains(syntax, *s)) output ~= " "~s[0 .. 1]~" ";
                else output ~= *s;
                s++;
            }
            return output;
        }

        /***********************************************************************************
         * Extracts a single S-expression from the string pointed to by s.
         * FIXME: Paren regression. This needs to be thrown away.
         *********************/

        string getExpression(Char)(ref Char *s) {
            Char[] expression;
            while(*s && *s == ' ') s++;
            if(*s && *s == Syntax.LTuple) {
                uint parenCount = 0;
                while(*s) {
                    expression ~= *s;
                    if(*s == Syntax.LTuple) parenCount++;
                    else if(*s == Syntax.RTuple) parenCount--;
                    if(parenCount == 0) break;
                    s++;
                }
                s++;
            }
            else while(*s && *s != ' ') expression ~= *s++;
            return expression;
        }

        /***********************************************************************************
         * Removes comments.
         *********************/

        string commentCollapse(in string input) {
            string output;
            auto s = (input~"\0").ptr;
            while(*s) {
                if(*s == Syntax.CommentStart) {
                    s++;
                    if(*s && *s == Syntax.CommentStart)
                    while(*s && *s != Syntax.EndOfLine) s++;       //Line comment.
                    else getExpression(s);                         //Sexp comment.
                }
                output ~= *s++;
            }
            return output;
        }

        string output;
        auto s = (commentCollapse(syntaxExpand(stringCollapse(input)))~"\0").ptr;

        while(*s && *s < 0x21) s++;                                     //Spaces, spaces everywhere.
        while(*s) {
            switch(*s) {
                case ' ':                                               //Spaces.
                    output ~= *s++;
                    while(*s && *s <= ' ') s++;
                    continue;
                case 0x0: .. case 0x1F:                                 //Whitespaces
                    s++;
                    continue;
                default: break;
            }
            output ~= *s++;
        }
        if(output.length && output[$-1] == ' ') return output[0 .. $-1];
        return output;
    }
    unittest {
        auto t = TestCase("DefaultParser.preprocess");
        auto i = new DefaultParser();

        void test(int line = __LINE__)(string input, string expected) {
            auto actual = i.preprocess(input);
            t.assertion!"=="(actual, expected, line);
        }

        test("   \n\r   Whitespaces.", "Whitespaces.");
        test("Trailing whitespaces.  \n\t  \r\n", "Trailing whitespaces.");
        test("Lots\t\na white\r\nspaces      \nand shit\t.", "Lotsa whitespaces and shit.");
        test("Lots\t\na white\r\nspaces   \"   \nand a string\".",
             "Lotsa whitespaces "~Syntax.StringDelim~" 0 .");
        test("Some string without comments.", "Some string without comments.");
        test("String (with parenteses (all over)).", "String ( with parenteses ( all over ) ) .");
        test("Some multi\nline str\ning without comments.", "Some multiline string without comments.");
        test("Some string ##with line\n comment.", "Some string comment.");
        test("Some string #(with Expression) comment.", "Some string comment.");
        test("Some string #(with a (Expression embeded in a) Expression) comment.", "Some string comment.");
        test("Some string \"with an \nembeded \tstring\" in it.",
             "Some string "~Syntax.StringDelim~" 0 in it.");
        test("String with a \"#(comment in a string), lol.\"", "String with a "~Syntax.StringDelim~" 0");
        test("String with a single #word comment.", "String with a single comment.");
        test("(Statement with a single word #comment.)", "( Statement with a single word )");
        // test("(Quotted '(statement bla bla), yeah.)",
        //      "( Quotted ( "~Keyword.Quote~" ( statement bla bla ) ) , yeah. )");
        // test("(Quotted 'word in a statement)", "( Quotted ( "~Keyword.Quote~" word ) in a statement )");
        // test("(Quotted '(statement next to a)\"string.\")",
        //      "( Quotted ( "~Keyword.Quote~" ( statement next to a ) ) "~StringPrefix~"0 )");
        // test("(Quotted 'word\"next to a string.\")",
        //      "( Quotted ( "~Keyword.Quote~" word ) "~StringPrefix~"0 )");
    }

    /***********************************************************************************
     * Cuts a string into several statements, also counts the brackets.
     *********************/

    pure string[] statementize(in string input)
    body {
        string[] statements;
        string statement;
        int parenCount;
        auto s = (input~'\0').ptr;
        while(*s) {
            statement ~= *s;
            if(*s == Syntax.LTuple) parenCount++;
            else if(*s == Syntax.RTuple) {
                parenCount--;
                if(parenCount < 0) throw new SyntacticError("Mismatched \""~Syntax.RTuple~"\"");
                else if(parenCount == 0) {
                    if(statement.length) {
                        statements ~= statement;
                        statement = "";
                    }
                    s++;
                    while(*s && *s == ' ') s++;
                    continue;   //TODO Pretier.
                }
            }
            s++;
        }
        if(statement.length) statements ~= statement;
        if(parenCount > 0) throw new SyntacticError("Mismatched \""~Syntax.LTuple~"\"");
        return statements;
    }
    unittest {
        auto t = TestCase("DefaultParser.statementize");
        auto i = new DefaultParser();

        void test(int line = __LINE__)(string input, string[] expected) {
            auto actual = i.statementize(input);
            t.assertion!"=="(actual, expected, line);
        }

        test("Word.", ["Word."]);
        test("Simple statement.", ["Simple statement."]);
        test("( A statement ( with en embeded ) statement. )",
            ["( A statement ( with en embeded ) statement. )"]);
        test("( Two ) ( statements. )", ["( Two )", "( statements. )"]);
        test("( Three ) ( statements ( with embeded ) ) ( statements. )",
            ["( Three )", "( statements ( with embeded ) )", "( statements. )"]);

        void test1(int line = __LINE__)(string str) {
            try { i.statementize(str); t.success; }
            catch(Exception e) { t.failure(line); }
        }

        test1("Some string with no parens.");
        test1("( ( ( ) ( ( ) ) ( ) ( ( ) ( ( ) ) ) ) )");

        void test2(int line = __LINE__)(string input, string expected) {
            try { i.statementize(input); t.failure(line); }
            catch(Exception e) { t.assertion!("==")(e.toString, expected, line); }
        }

        test2("( ( ) 9 ) ) ) 99 ( ( 00 ) ) ) )", "Error: Mismatched \""~Syntax.RTuple~"\"");
        test2("( ( ( ) )", "Error: Mismatched \""~Syntax.LTuple~"\"");
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
        auto s = (input~'\0').ptr;
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
    }

    /***********************************************************************************
     * Parses a stream of tokens into a single AST.
     *********************/

    Expression parse(ref string[] tokens) {
        auto token = tokens.front;
        tokens.popFront;
        //All kinds of parenthesis.
        auto parens = [cast(immutable(char))Syntax.LTuple : cast(immutable(char))Syntax.RTuple, //TODO: Pretier!
                       Syntax.LList : Syntax.RList,
                       Syntax.LSet : Syntax.RSet];
        //Extended syntax.
        auto expandables = [cast(string)ESyntax.Quote : cast(string)Keywords.Quote,
                               ESyntax.Quasiquote : Keywords.Quasiquote,
                               ESyntax.Embed : Keywords.Embed];

        if(token == ""~Syntax.StringDelim) {
            auto index = tokens.front;
            tokens.popFront;
            return new String(stringBank[to!uint(index)]);
        }
        else if(token in expandables) {
            return new Tuple([new Symbol(expandables[token]), parse(tokens)]);
        }
        else if(token[0] in parens) {
            auto delimiter = parens[token[0]];
            Expression[] collection;

            while(tokens.front != ""~delimiter) {
                collection ~= parse(tokens);
            }
            tokens.popFront();

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
        else {
            try { //BUG FIXME With a rake or something. Seriously?
                if(token == "-" || token == "in") throw new Exception("A Phobos bug workarround.");
                auto value = to!real(token);
                return new Value(value);
            }
            catch(Exception e) {
                return new Symbol(token);
            }
        }
    }

    public:

    /***********************************************************************************
     * Parses a string into an array of ASTs each representing independant statement.
     *********************/

    override Expression[] parse(in string input) {
        Expression[] output;
        auto statements = statementize(preprocess(input));
        foreach(statement; statements) {
            auto tokens = tokenize(statement);
            if(tokens.length) output ~= parse(tokens);
        }
        return output;
    }
}
