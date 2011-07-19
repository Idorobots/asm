/***********************************************************************************
 * Code parsing module. //TODO Some more refactorisation. Rewrite perhapse?
 *********************/

module ASM.parser;

import std.conv : to;
import std.array : front, popFront;

debug import std.stdio;
debug import utils.testing;
import utils.exception;

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
     * Additional syntax tokens.
     *********************/

    enum ExtendedSyntax : string {
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
                if(*s == SyntaxToken.StringDelim) {
                    //TODO: StringParser.
                    string str;
                    str ~= *s++;
                    while(*s && *s != SyntaxToken.StringDelim) str ~= *s++;
                    str ~= *s++;
                    output ~= " "~ExtendedSyntax.StringPrefix~to!string(stringBank.length)~" ";
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
            auto syntax = [SyntaxToken.LTuple, SyntaxToken.RTuple,      //Tuple parentesis
                           SyntaxToken.LList,  SyntaxToken.RList,       //List parentesis
                           SyntaxToken.LSet,   SyntaxToken.RSet,        //Set parentesis
                           ExtendedSyntax.Quote[0],                     //' -> (quote )
                           ExtendedSyntax.Quasiquote[0],                //` -> (quasiquote )
                           ExtendedSyntax.Embed[0]];                    //$ -> (embed )

            bool isIn(LetterT, ArrayT)(LetterT letter, ArrayT  array) {
                foreach(l; array) {
                    if(letter == l) return true;
                }
                return false;
            }

            while(*s) {
                if(isIn(*s, syntax)) output ~= " "~s[0 .. 1]~" ";
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
            if(*s && *s == SyntaxToken.LTuple) {
                uint parenCount = 0;
                while(*s) {
                    expression ~= *s;
                    if(*s == SyntaxToken.LTuple) parenCount++;
                    else if(*s == SyntaxToken.RTuple) parenCount--;
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
                if(*s == SyntaxToken.CommentStart) {
                    s++;
                    if(*s && *s == SyntaxToken.CommentStart)
                    while(*s && *s != SyntaxToken.EndOfLine) s++; //Line comment.
                    else getExpression(s);  //Sexp comment.
                }
                output ~= *s++;
            }
            return output;
        }

        // /***********************************************************************************
        //  * Expands SyntaxToken convinience forms such as ' or $.
        //  * TODO: Move this to stringExpand, or to parse.
        //  *********************/

        // string syntaxExpand(in string input) {
        //     string output;

        //     string tupleUp(string keyword, immutable(char)* s) {
        //         return " "~SyntaxToken.LTuple~" "~keyword~" "~syntaxExpand(getExpression(s))
        //                ~" "~SyntaxToken.RTuple~" ";
        //     }
        //     auto s = (input~"\0").ptr;
        //     while(*s) {
        //         //TODO: Pretier.
        //         if(*s == ExtendedSyntax.Quote) output ~= tupleUp(Keyword.Quote, ++s);
        //         else if(*s == ExtendedSyntax.Quasiquote) output ~= tupleUp(Keyword.Quasiquote,++s);
        //         else if(*s == ExtendedSyntax.Embed) output ~= tupleUp(Keyword.Embed, ++s);
        //         else output ~= *s++;
        //     }
        //     return output;
        // }

        string output;
        //auto s = (syntaxExpand(commentCollapse(parenExpand(stringCollapse(input))))~"\0").ptr;
        auto s = (commentCollapse(syntaxExpand(stringCollapse(input)))~"\0").ptr;

        while(*s && *s < 0x21) s++;     //Spaces, spaces everywhere.
        while(*s) {
            switch(*s) {
                case ' ':               //Spaces.
                    output ~= *s++;
                    while(*s && *s <= ' ') s++;
                    continue;
                case 0x0: .. case 0x1F: //Whitespaces
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
             "Lotsa whitespaces "~ExtendedSyntax.StringPrefix~"0 .");
        test("Some string without comments.", "Some string without comments.");
        test("String (with parenteses (all over)).", "String ( with parenteses ( all over ) ) .");
        test("Some multi\nline str\ning without comments.", "Some multiline string without comments.");
        test("Some string ##with line\n comment.", "Some string comment.");
        test("Some string #(with Expression) comment.", "Some string comment.");
        test("Some string #(with a (Expression embeded in a) Expression) comment.", "Some string comment.");
        test("Some string \"with an \nembeded \tstring\" in it.",
             "Some string "~ExtendedSyntax.StringPrefix~"0 in it.");
        test("String with a \"#(comment in a string), lol.\"", "String with a "~ExtendedSyntax.StringPrefix~"0");
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
            if(*s == SyntaxToken.LTuple) parenCount++;
            else if(*s == SyntaxToken.RTuple) {
                parenCount--;
                if(parenCount < 0) throw new SyntacticError("Mismatched \""~SyntaxToken.RTuple~"\"");
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
        if(parenCount > 0) throw new SyntacticError("Mismatched \""~SyntaxToken.LTuple~"\"");
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

        test2("( ( ) 9 ) ) ) 99 ( ( 00 ) ) ) )", "Error: Mismatched \""~SyntaxToken.RTuple~"\"");
        test2("( ( ( ) )", "Error: Mismatched \""~SyntaxToken.LTuple~"\"");
    }

    /***********************************************************************************
     * Turns a single statement into several tokens.
     *********************/

    pure string[] tokenize(in string input)
    body {
        string[] tokens;
        string token;
        auto parens = [1:SyntaxToken.LTuple, 2:SyntaxToken.RTuple,      //Tuple parentesis
                       3:SyntaxToken.LList,  4:SyntaxToken.RList,       //List parentesis
                       5:SyntaxToken.LSet,   6:SyntaxToken.RSet];       //Set parentesis
        auto s = (input~'\0').ptr;
        while(*s) {
            if(*s == ' ') {
                if(token != "") {
                    tokens ~= token;
                    token = "";
                }
            }
            else if(*s in parens) tokens ~= s[0 .. 1];
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

    // /***********************************************************************************
    //  * String expansion. Each __string## gets substituted for its stringBank equivalent.
    //  *********************/

    // string[] expand(in string[] tokens) {
    //     string[] expanded;
    //     auto len = StringPrefix.length;
    //     foreach(token; tokens) {
    //         if(token.length > len && token[0 .. len] == StringPrefix) {
    //             expanded ~= stringBank[to!uint(token[len .. $])];
    //         }
    //         else expanded ~= token;
    //     }
    //     return expanded;
    // }

    /***********************************************************************************
     * Parses a stream of tokens into a single AST.
     *********************/

    Expression parse(ref string[] tokens) {
        auto token = tokens.front;
        tokens.popFront;
        auto parens = [cast(immutable(char))SyntaxToken.RTuple : cast(immutable(char))SyntaxToken.LTuple,
                       SyntaxToken.RList : SyntaxToken.LList,
                       SyntaxToken.RSet : SyntaxToken.LSet];
        auto extendedSyntax = [cast(string)ExtendedSyntax.Quote : cast(string)Keyword.Quote,
                               ExtendedSyntax.Quasiquote : Keyword.Quasiquote,
                               ExtendedSyntax.Embed : Keyword.Embed];
        if(token in extendedSyntax) {
            return new Tuple([new Symbol(extendedSyntax[token]), parse(tokens)]);
        }
        else if(token[0] in parens) {
            auto delimiter = parens[token[0]];
            Expression[] collection;

            while(tokens.front != ""~delimiter) {
                collection ~= parse(tokens);
            }
            tokens.popFront();

            switch(delimiter) {
                case SyntaxToken.RTuple:
                    if(collection.length) return new Tuple(collection);
                    return new Symbol(Keyword.Fnord);
                case SyntaxToken.RList:
                    return new List(collection);
                case SyntaxToken.RSet:
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
                if(token[0] == SyntaxToken.StringDelim) return new String(token);
                else return new Symbol(token);
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
            //auto tokens = expand(tokenize(statement));
            auto tokens = tokenize(statement);
            if(tokens.length) output ~= parse(tokens);
        }
        return output;
    }
}
