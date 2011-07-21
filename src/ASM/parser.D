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
 * Mismatched token exception..
 *********************/

class Mismatched : SyntacticError {
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
        Quote           = "\'",         ///Quoting: '(#whatever)
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
            auto s = (input~Syntax.EndOfFile).ptr;
            while(*s) {
                if(*s == Syntax.StringDelim) {
                    //TODO: StringParser.
                    string str;
                    str ~= *s++;
                    while(*s && *s != Syntax.StringDelim) str ~= *s++;
                    str ~= *s++;
                    output ~= Syntax.StringDelim~to!string(stringBank.length)~Syntax.Space;
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
            auto s = (input~Syntax.EndOfFile).ptr;
            while(*s) {
                if(*s == Syntax.CommentStart) {
                    s++;
                    if(*s && (*s == Syntax.CommentStart || *s == Syntax.Space))
                       while(*s && *s != Syntax.EndOfLine) s++;
                    else output ~= Syntax.CommentStart; //Sexp comment is thrown away in parse().
                }
                output ~= *s++;
            }
            return output;
        }

        /***********************************************************************************
         * Adds spaces and expands scope brackets into regular ones.
         *********************/

        pure string syntaxExpand(in string input) {
            string output;
            auto s = (input~Syntax.EndOfFile).ptr;
            auto expand = [Syntax.LTuple, Syntax.RTuple,      //Tuple parentesis
                           Syntax.LList,  Syntax.RList,       //List parentesis
                           Syntax.LSet,   Syntax.RSet,        //Set parentesis
                           Syntax.StringDelim,                //String delimiter.
                           Syntax.CommentStart,               //Sexp comment.
                           ESyntax.Quote[0],                  //' -> (quote )
                           ESyntax.Quasiquote[0],             //` -> (qquote )
                           ESyntax.Embed[0]];                 //$ -> (embed )

            while(*s) {
                if(contains(expand, *s)) output ~= ""~Syntax.Space~s[0 .. 1]~Syntax.Space;
                else output ~= *s;
                s++;
            }
            return output;
        }

        string output;
        auto s = (syntaxExpand(commentCollapse(stringCollapse(input)))~Syntax.EndOfFile).ptr;

        while(*s && *s < 0x21) s++;                                     //Spaces, spaces everywhere.
        while(*s) {
            switch(*s) {
                case Syntax.Space:                                      //Spaces.
                    output ~= *s++;
                    while(*s && *s <= Syntax.Space) s++;
                    continue;
                case 0x0: .. case 0x1F:                                 //Whitespaces
                    s++;
                    continue;
                default: break;
            }
            output ~= *s++;
        }
        if(output.length && output[$-1] == Syntax.Space) return output[0 .. $-1];
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
        auto s = (input~Syntax.EndOfFile).ptr;
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
        if(!tokens.length) return null;
        auto token = tokens.front;
        tokens.popFront;

        //All kinds of parenthesis.
        auto parens = [cast(immutable(char))Syntax.LTuple : cast(immutable(char))Syntax.RTuple, //TODO: Pretier!
                       Syntax.LList : Syntax.RList,
                       Syntax.LSet : Syntax.RSet];
        auto antyparens = [cast(immutable(char))Syntax.RTuple : cast(immutable(char))Syntax.LTuple,
                           Syntax.RList : Syntax.LList,
                           Syntax.RSet : Syntax.LSet];
        //Extended syntax.
        auto expandables = [cast(string)ESyntax.Quote : cast(string)Keywords.Quote,
                               ESyntax.Quasiquote : Keywords.Quasiquote,
                               ESyntax.Embed : Keywords.Embed];

        if(token == ""~Syntax.StringDelim) {
            auto index = tokens.front; //The next token is _always_ the offset in string bank.
            tokens.popFront;
            return new String(stringBank[to!uint(index)]);
        }
        if(token == ""~Syntax.CommentStart) { //Deletes the following expression from the token stream.
            if(tokens.length) parse(tokens);       //It still does the syntactic analyzis,
            return null;                           //as this is ment solely for debugging.
        }
        if(token in expandables) {
            return new Tuple([new Symbol(expandables[token]),
                              tokens.length ? parse(tokens) : new Symbol(Keywords.Fnord)]);
        }
        if(token[0] in parens) {
            auto delimiter = parens[token[0]];
            Expression[] collection;

            do {
                if(!tokens.length) throw new Mismatched(token);
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
        if(token[0] in antyparens) throw new Mismatched(token);

        //The last possible case - a symbol or a number.
        try { //BUG FIXME with a rake or something. Seriously?
            if(token == "-" || token == "in") throw new Exception("A Phobos bug workarround.");
            auto value = to!real(token);
            return new Value(value);
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
