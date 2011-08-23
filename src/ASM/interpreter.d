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
 * Interpreter
 ****************************************/

module ASM.interpreter;

import std.stdio;
import std.file : readText, FileException;
import std.utf : UtfException;
import std.random;
import std.regex : match, regex;
import std.algorithm : sort;

import utils.ctfe : tr, ETuple;
import utils.testing : TestCase;

import ASM.lexical;
import ASM.AST;
import ASM.kit;
import ASM.lexer;
import ASM.parser;


/***********************************************************************************
 * Interpreter
 *********************/

class Interpreter {
    Parser parser;          //Parsing unit.
    Scope global;           //Global scope.


    this() {
        this(new DefaultParser());
    }

    this(Parser parser) {
        assert(parser !is null);
        this.parser = parser;
        global = new Scope();

        FNORD = new Symbol(Keywords.Fnord);
        global.define(Keywords.Fnord, FNORD);
        global.define(Keywords.Import, new BuiltinKeyword(&IMPORT));
        global.define(Keywords.Do, new BuiltinKeyword(&DO));
        global.define(Keywords.If, new BuiltinKeyword(&IF));
        global.define(Keywords.Set, new BuiltinKeyword(&SET));
        global.define(Keywords.Get, new BuiltinKeyword(&GET));
        global.define(Keywords.Quote, new BuiltinKeyword(&QUOTE));
        global.define(Keywords.Quasiquote, new BuiltinKeyword(&QQUOTE));
        global.define(Keywords.Embed, new BuiltinKeyword(&EMBED));
        global.define(Keywords.IsEqual, new BuiltinKeyword(&ISEQUAL));
        global.define(Keywords.Mult, new PureBuiltin(&MULT));
        global.define(Keywords.Add, new PureBuiltin(&ADD));
        global.define(Keywords.Var, new BuiltinKeyword(&VAR));
        global.define(Keywords.Lambda, new BuiltinKeyword(&LAMBDA));
        global.define(Keywords.Macro, new BuiltinKeyword(&MACRO));
        global.define(Keywords.Scope, new BuiltinKeyword(&SCOPE));
        global.define(Keywords.Cons, new BuiltinKeyword(&CONS));
        global.define(Keywords.Car, new BuiltinKeyword(&CAR));
        global.define(Keywords.Cdr, new BuiltinKeyword(&CDR));
        global.define(Keywords.Map, new PureBuiltin(&MAP));
        global.define(Keywords.Reduce, new PureBuiltin(&REDUCE));
        global.define(Keywords.TypeOf, new BuiltinKeyword(&TYPEOF));
        global.define(Keywords.KeywordsOf, new BuiltinKeyword(&KEYWORDSOF));
        global.define("call", new BuiltinKeyword(&CALL));
        global.define("defined?", new BuiltinKeyword(&DEFINED));
        global.define("nth", new PureBuiltin(&NTH));
        global.define("select", new PureBuiltin(&SELECT));
        global.define("__setcall", global.get("select"));
        global.define("__listcall", global.get("nth"));
        global.define("__scopecall", global.get(Keywords.Get));
        global.define("__listeval", global.get(Keywords.Lambda));
        global.define("__seteval", global.get(Keywords.Do));
        global.define("tuple", new PureBuiltin(&MAKETUPLE));
        global.define("set", new PureBuiltin(&MAKESET));
        global.define("list", new PureBuiltin(&MAKELIST));
        global.define("tupleof", new PureBuiltin(&TUPLEOF));
        global.define("setof", new PureBuiltin(&SETOF));
        global.define("listof", new PureBuiltin(&LISTOF));
        global.define("stringof", new PureBuiltin(&STRINGOF));
        global.define("random", new PureBuiltin(&RANDOM));
        global.define("range", new PureBuiltin(&RANGE));
        global.define("write", new Builtin(&WRITE));
        global.define("append!", new PureBuiltin(&APPEND));
        global.define("apply", new PureBuiltin(&APPLY));
        global.define("eval", new BuiltinKeyword(&EVAL));
        global.define("read", new BuiltinKeyword(&READ));
        global.define("\\space", new String(" "));
        global.define("\\tab", new String("\t"));
        global.define("\\newline", new String("\n"));

        //New parser/lexer routines:
        global.define("read-from-string", new BuiltinKeyword(&READSTRING));
        global.define("readln", new BuiltinKeyword(&READLN));
        global.define("lex", new BuiltinKeyword(&LEX2));
        global.define("catch", new BuiltinKeyword(&CATCH));
        global.define("error", new BuiltinKeyword(&ERROR));
        global.define("syntax-error", new BuiltinKeyword(&SYNTAXERROR));
        global.define("quit", new BuiltinKeyword(&QUIT));
    }
    unittest {
        auto t = TestCase("Interpreter.builtins");
        auto i = new Interpreter();

        void test(int line = __LINE__)(string input, string expected) {
            string actual;
            try actual = i.doString(input);
            catch(Exception e) actual = e.toString;
            t.assertion!"=="(actual, expected, line);
        }

        //FIXME: Make these Syntax-independant.
        //Quote, Quasiquote and embed:
        test("(tuple () '() fnord 'fnord)", "(fnord fnord fnord fnord)");
        test("'(foo bar)","(foo bar)");
        test("''foo", "(quote foo)");
        test("'`foo","(qquote foo)");
        test("'$wat","(embed wat)");
        test("`($(* 2 2))", "(4)");
        test("`{$(* 2 2)}", "{4}");
        test("`[$(* 2 2)]", "[4]");
        test("(var a 12) (var b 23) (var tmp a) (set! a b) (set! b tmp)", "12");
        test(`(stringof (tupleof "cool"))`, `"cool"`);
        test("(join! 1 1)", "[1 1]");
        test("(join! 1 fnord)", "[1]");
        test("(join! '[1 2 3] fnord)", "[[1 2 3]]");
        test(`(join! "foo" "bar")`, `"foobar"`);
        test("(+ 2 2)", "4");
        test("(+ (* 2 100) (* 1 10))", "210");
        test("(if (> 6 5) (+ 1 1) (+ 2 2))", "2");
        test("(if (< 6 5) (+ 1 1) (+ 2 2))", "4");
        test("(var x 3)", "3");
        test("x", "3");
        test("(+ x x)", "6");
        test("(do (var x 1) (set! x (+ x 1)) (+ x 1))", "3");
        test("((lambda [x] (+ x x)) 5)", "10");
        test("('[1 2 3] 1)", "2");
        test("('[1 2 3] -1)", "3");
        test("(equal? fnord 'fnord () '())", "yup");
    }


    /***********************************************************************************
     * Reads and evaluates a string.
     *********************/

    string doString(in string input, Scope s, string filename) {
        string output;
        auto statements = parser.parse(input, filename);
        foreach(statement; statements) {
            output = statement.eval(s).toString;
        }
        return output;
    }

    string doString(in string input, in string filename = "__main") {  //FIXME
        return doString(input, global, filename);
    }

    /***********************************************************************************
     * Reads and evaluates a file.
     * TODO: InterpretingError
     *********************/

    string doFile(in string filename, Scope s) {    //FIXME
        string input;
        try input = readText(filename);
        catch(FileException e) { //TODO: Change both of these to InterpretingError?
            throw new SemanticError("Unable to read file '"~filename~"'.", 0, "__main"); //FIXME
        }
        catch(UtfException e) {
            throw new SemanticError("Malformed file '"~filename~"'.", 0, "__main"); //FIXME
        }
        return doString(input, s, filename);
    }

    string doFile(in string filename) {
        return doFile(filename, global);
    }

    /***********************************************************************************
     * Defines a new native function
     *********************/

     void define(string name, proc_t proc) {
         global.define(name, new Builtin(proc));
     }

    /***********************************************************************************
     * DEFINITIONS:
     *********************/

    /***********************************************************************************
     * Special fnord keyword - the ony "false" value arround.
     *********************/

    Expression FNORD;

    /***********************************************************************************
     * Performs a call on its arguments.
     *********************/

    Expression CALL(ref Scope s, Expression[] args) {
        if(!args.length)
            throw new SemanticError("Syntax keyword 'call' requires at least one argument.", 0, ":("); //FIXME

        auto op = args[0].eval(s);
        return op.call(s, args[1 .. $]);
    }

    /***********************************************************************************
     * Returns an element at a given index.
     *********************/

    Expression NTH(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Function 'nth' requires exactly two arguments.", 0, ":("); //FIXME

        int index = to!int(args[1].eval(s).value);           //TODO: Idiotproof
        auto coll = args[0].eval(s).range;
        if((index >= 0) && (index < coll.length)) return coll[index];
        if((index < 0) && (coll.length - index >= 0)) return coll[$+index];
        return FNORD;
    }


    /***********************************************************************************
     * Filters a collection returning a new collection containing elements satisfying
     * all the predicates passed to it.
     *********************/

    Expression SELECT(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Function 'select' requires exactly two arguments.", 0, ":("); //FIXME

        Expression predicate = args[1].eval(s);
        Expression coll = args[0].eval(s);
        Expression[] newColl;

        foreach(ref el; coll.range) {
            if(predicate.call(s, [pass(el)]).toString != Keywords.Fnord)        //FIXME: FNORD
                newColl ~= el;
        }
        return coll.factory(newColl);
    }

    /***********************************************************************************
     * Returns wether a symbol is defined in a scope.
     *********************/

     Expression DEFINED(ref Scope s, Expression[] args) {
         if(!args.length)
             throw new SemanticError("Function 'defined?' requires at least one argument.", 0, ":("); //FIXME
         Expression[] argv;
         foreach(arg; args) argv ~= arg.eval(s);
         foreach(arg; argv) if(!s.isDefined(arg.toString)) return FNORD;
         return argv[0];
     }

    /***********************************************************************************
     * Imports and interprets a file, merges Scopes.
     *********************/

    Expression IMPORT(ref Scope s, Expression[] args) {
        foreach(arg; args) {
            arg = arg.eval(s);
            if(arg.type & Type.String) doFile(arg.toString[1 .. $-1], s);
            else if(arg.type & Type.Symbol) doFile(tr!(".", "/")(arg.toString)~".asm", s);
            else if(arg.type & Type.Scope) assert(0, "Not yet implemented.");
            else throw new ObjectNotAppError(args[0].toString, args[0].line, args[0].file);
        }
         return FNORD;
    }

    /***********************************************************************************
     * Sets a settable location with a new value.
     *********************/

    Expression SET(ref Scope s, Expression[] args) {
        auto location = args[0].eval(s);
        auto value = args[1].eval(s);

        if(location.type & Type.Settable) {
            auto reference = cast(Reference)location;
            reference.set(value);
        }
        else throw new ObjectNotAppError(args[0].toString, args[0].line, args[0].file);
        return value;
    }

    /***********************************************************************************
     * Returns an object bound to a symbol.
     *********************/

    Expression GET(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Syntax keyword '"~Keywords.Get~"' requires exactly one argument.", 0, ":("); //FIXME
        auto arg = args[0].eval(s);
        if(arg.type & Type.Symbol) return new Reference(s.getRef(arg.toString));
        throw new ObjectNotAppError(args[0].toString, args[0].line, args[0].file);
    }

    /***********************************************************************************
     * Quote
     *********************/

    Expression QUOTE(ref Scope, Expression[] args) {
        return args.length ? args[0] : FNORD;
    }

    /***********************************************************************************
     * Quasiquote - quotes an expression embedding any embed expressions in it.
     * Embeds are coupled with the innermost quasiquote.
     * FIXME: StackOverflow when tryEvaling strings.
     *********************/

    Expression QQUOTE(ref Scope s, Expression[] args) {
        if(!args.length) return FNORD;
        Expression tryEval(Expression arg) {
            if(arg.type & Type.Collection) {
                if(arg.type & Type.Tuple) {
                    foreach(e; arg.range) {
                        if(e.toString == Keywords.Embed) return arg.eval(s); //FIXME: Quickfix.
                        if(e.toString == Keywords.Quasiquote) return arg;
                    }
                }
                else if(arg.type & Type.String) {
                    return arg;
                }
                //Not embedding:
                Expression[] collection;
                foreach(a; arg.range) collection ~= tryEval(a);
                return arg.factory(collection);
            }
            return arg;
        }
        return tryEval(args[0]);
    }

    /***********************************************************************************
     * Embeds an expression into Quasiquote expression, etc.
     *********************/

    Expression EMBED(ref Scope s, Expression[] args) {
        if(!args.length) return FNORD;
        if(args[0].type & Type.String) {                           //FIXME: $$"string" != (embed (embed "string"))
            return new Reference(s.getRef(args[0].toString));      //FIXME: Same thing with quote
        }
        return args[0].eval(s);
    }

    /***********************************************************************************
     * Returns wether two objects are equal.
     *********************/

    Expression ISEQUAL(ref Scope s, Expression[] args) {
        if(args.length < 2)
            throw new SemanticError("Syntax keyword '"~Keywords.IsEqual~"' requires at least two arguments.", 0, ":("); //FIXME
        auto first = args[0].eval(s);
        foreach(arg; args[1 .. $]) {
            if(arg.eval(s).toString != first.toString) return FNORD;
        }
        return first.toString != Keywords.Fnord ? first : new Symbol("yup");    //TODO: FNORD
    }

    /***********************************************************************************
     * Multiplies two values.
     * FIXME: Needs only two args.
     *********************/

    Expression MULT(ref Scope s, Expression[] args) {
        if(!args.length)
            throw new SemanticError("Function '"~Keywords.Mult~"' requires at least one argument.", 0, ":("); //FIXME
        auto accumulator = args[0].eval(s).value;
        foreach(arg; args[1 .. $]) accumulator *= arg.eval(s).value;
        return new Number(accumulator);
    }

    /***********************************************************************************
     * Adds two values.
     * FIXME: Needs only two args.
     *********************/

    Expression ADD(ref Scope s, Expression[] args) {
        if(!args.length)
            throw new SemanticError("Function '"~Keywords.Add~"' requires at least one argument.", 0, ":("); //FIXME
        auto accumulator = args[0].eval(s).value;
        foreach(arg; args[1 .. $]) accumulator += arg.eval(s).value;
        return new Number(accumulator);
    }

    /***********************************************************************************
     * Binds symbols to other objects.
     *********************/

    Expression VAR(ref Scope s, Expression[] args) {
        if(!args.length || args.length > 2)
            throw new SemanticError("Syntax keyword '"~Keywords.Var~"' requires one or two arguments.", 0, ":("); //FIXME
        if(!(args[0].type & Type.Symbol))
            throw new ObjectNotAppError(args[0].toString, args[0].line, args[0].file);
        auto value = args.length == 2 ? args[1].eval(s).deref : FNORD;
        s.define(args[0].toString, value);
        return value;
    }

    /***********************************************************************************
     * Returns an anonymous closure.
     *********************/

    Expression LAMBDA(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Syntax keyword '"~Keywords.Lambda~"' requires exactly two arguments.", 0, ":("); //FIXME

        Expression argList = args[0];
        Expression functionBody = args[1];
        auto tmp = s; //FIXME: OUT

        Expression foo;
        foo = new Function(delegate Expression (ref Scope callScope, Expression[] callArgs) {
            auto closureScope = new Scope(tmp);
            closureScope.define(Keywords.Self, foo);

            if(callArgs.length != argList.range.length)
                throw new SemanticError(format("Expected %s arguments instead of %s.",
                                               argList.range.length, callArgs.length), 0, ":("); //FIXME

            foreach(i, arg; argList.range) {
                closureScope.define(arg.toString, callArgs[i].eval(callScope));
            }
            return functionBody.eval(closureScope);
        });
        return foo;
    }

    /***********************************************************************************
     * Returns a new syntax keyword.
     *********************/

    Expression MACRO(ref Scope s, Expression[] args) {
        if(args.length != 3)
            throw new SemanticError("Syntax keyword '"~Keywords.Macro~"' requires exactly three argements.", 0, ":("); //FIXME

        Expression macroName = args[0];
        Expression argList   = args[1];
        Expression macroBody = args[2];

        auto foo = new Keyword(delegate Expression (ref Scope callScope, Expression[] callArgs) {
            // if(callArgs.length != argList.range.length)
            //     throw new SemanticError(format("Expected %s arguments instead of %s.",
            //                                    argList.range.length, callArgs.length));

            auto macroScope = new Scope(callScope);
            foreach(i, arg; argList.range) {
                if(contains(arg.keywords, "tuple")) {
                    macroScope.define(arg.toString, new Tuple(callArgs[i .. $])); //TODO: Keyword.Dots?
                    break;
                }
                else macroScope.define(arg.toString, callArgs[i]);
            }
            return macroBody.eval(macroScope).eval(callScope);
        });

        s.define(macroName.toString, foo);
        return macroName;
    }

    /***********************************************************************************
     * Returns a new Scope evaluating all the arguments in it.
     *********************/

    Expression SCOPE(ref Scope s, Expression[] args) {
        auto newScope = new Scope(s);
        foreach(arg; args) arg.eval(newScope);
        return newScope;
    }

    /***********************************************************************************
     * Joins two objects into a collection according to these rules:
     * foo, bar --> [foo bar]
     * foo, (bar) --> (foo bar)
     * (foo), bar --> ((foo) bar)
     * (foo), (bar) --> ((foo) bar)
     *********************/

    Expression CONS(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Function '"~Keywords.Cons~"' requires exactly two arguments.", 0, ":("); //FIXME
        auto arg0 = args[0].eval(s).deref;
        auto arg1 = args[1].eval(s).deref;

        if(arg1.type & Type.Collection) return arg1.factory([arg0]~arg1.range);
        if(arg1.toString == Keywords.Fnord) return new List([arg0]); //FIXME: FNORD
        return new List([arg0, arg1]);
    }

    /***********************************************************************************
     * Returns a Reference to the head of a mutable collection,
     * or value of the head of an immutable collection.
     *********************/

    Expression CAR(ref Scope s, Expression[] args) {
        auto collection = args[0].eval(s);
        if(!collection.range.length) return FNORD;
        if(collection.type & Type.Immutable) return collection.range[0];
        else return new Reference(&(collection.range[0]));
    }

    /***********************************************************************************
     * Returns a reference to the tail of a collection.
     *********************/

    Expression CDR(ref Scope s, Expression[] args) {
        auto collection = args[0].eval(s);
        if(collection.range.length < 2) return FNORD;
        return collection.factory(collection.range[1 .. $]);
    }

    /***********************************************************************************
     * Maps an operation to a collection collecting the result. Type generic.
     *********************/

    Expression MAP(ref Scope s, Expression[] args) {
        if(args.length != 2)
           throw new SemanticError("Function 'map' requires exactly two arguments.", 0, ":("); //FIXME
        Expression[] coll;
        auto func = args[0].eval(s);
        auto collection = args[1].eval(s);
        foreach(ref value; collection.range)
            coll ~= func.call(s, [pass(value)]);
        return collection.factory(coll);
    }

    /***********************************************************************************
     * Maps an operation to a collection reducing it to a single value. Type generic.
     *********************/

    Expression REDUCE(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Function 'reduce' requires exactly two arguments.", 0, ":("); //FIXME
        auto func = args[0].eval(s);
        auto collection = args[1].eval(s);
        auto result = collection.range[0];
        foreach(ref value; collection.range[1 .. $])
            result = func.call(s, [pass(result), pass(value)]);
        return result;
    }

    /***********************************************************************************
     * (if condition then else/fnord)
     *********************/

    Expression IF(ref Scope s, Expression[] args) {
        if(args.length != 2 && args.length != 3)
            throw new SemanticError("Syntax keyword '"~Keywords.If~"' requires two or three arguments.", 0, ":("); //FIXME
        if(args[0].eval(s).toString != Keywords.Fnord) //FIXME: FNORD
            return args[1].eval(s);
        else return args.length == 3 ? args[2].eval(s) : FNORD;
    }

    /***********************************************************************************
     * Evaluates arguments and returns value of the last one.
     *********************/

     Expression DO(ref Scope s, Expression[] args) {
         if(!args.length) return FNORD;

         auto keywords = args[0].keywords;
         Expression output = FNORD;

         if(!keywords.length) {
             foreach(arg; args) output = arg.eval(s);
         }
         else {
             void loop(bool delegate () condition) {
                 while(condition())
                     foreach(arg; args[1 .. $]) output = arg.eval(s);
             }

             if(contains(keywords, "while"))
                 loop({return args[0].eval(s).toString != Keywords.Fnord;});
             else if(contains(keywords, "until"))
                 loop({return args[0].eval(s).toString == Keywords.Fnord;});
         }
         return output;
     }

    /***********************************************************************************
     * Returns a type tuple of an object.
     *********************/

    Expression TYPEOF(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Function '"~Keywords.TypeOf~"' requires exactly one argument.", 0, ":("); //FIXME
        auto argType = args[0].eval(s).type;
        Expression[] typeTuple;

        alias ETuple!(Type.Immutable, Type.Settable, Type.Pure, Type.Builtin, Type.Atom, Type.Callable,
                      Type.Collection, Type.Number, Type.Symbol, Type.String, Type.Function, Type.Keyword,
                      Type.Scope, Type.Set, Type.Tuple, Type.List)
              types;
        alias ETuple!("immutable", "settable", "pure", "builtin", "atom", "callable", "collection", "number",
                      "symbol", "string", "function", "keyword", "scope", "set", "tuple", "list")
              typeNames;

        /*static*/ foreach(i, type; types) if(argType & type) typeTuple ~= new Symbol(typeNames[i]);
        return new Tuple(typeTuple);
    }


    /***********************************************************************************
     * Returns a keyword tuple of an object.
     *********************/

    Expression KEYWORDSOF(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Function '"~Keywords.KeywordsOf~"' requires exactly one argument.", 0, ":("); //FIXME
        auto keywords = args[0].eval(s).keywords;
        if(!keywords.length) return FNORD;

        Expression[] tuple;
        foreach(keyword; keywords) tuple ~= new Symbol(keyword);
        return new Tuple(tuple);
    }

    /***********************************************************************************
     * Creates a set of its arguments.
     *********************/

    Expression MAKECOLL(T)(ref Scope s, Expression[] args) {
        Expression[] coll;
        foreach(arg; args) coll ~= arg.eval(s).deref;
        static if(is(T : Tuple)) {
           if(!coll.length) return FNORD;
        }
        return new T(coll);
    }

    alias MAKECOLL!(Set)        MAKESET;
    alias MAKECOLL!(List)       MAKELIST;
    alias MAKECOLL!(Tuple)      MAKETUPLE;

    /***********************************************************************************
     * Returns a set representation of a collection.
     *********************/

    Expression SETOF(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Function 'setof' requires exactly one argument.", 0, ":("); //FIXME
        return new Set(args[0].eval(s).range.dup);
    }

    /***********************************************************************************
     * Returns a list representation of a collection.
     *********************/

    Expression LISTOF(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Function 'listof' requires exactly one argument.", 0, ":("); //FIXME
        return new List(args[0].eval(s).range.dup);
    }

    /***********************************************************************************
     * Retruns a tuple representation of a collection.
     *********************/

    Expression TUPLEOF(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Function 'tupleof' requires exactly one argument.", 0, ":("); //FIXME
        auto arg = args[0].eval(s);
        if(arg.toString == Keywords.Fnord) return FNORD;        //TODO FNORD, OUT
        auto range = arg.range;
        if(!range.length) return FNORD;
        return new Tuple(range);
    }

    /***********************************************************************************
     * Retruns a string representation of an argument.
     *********************/

    Expression STRINGOF(ref Scope s, Expression [] args) {
        if(args.length != 1)
            throw new SemanticError("Function 'stringof' requires exactly one argument.", 0, ":("); //FIXME
        auto arg = args[0].eval(s);
        if(arg.type & Type.Collection) return new String(arg.range);
        return new String(args[0].eval(s).toString);
    }

    /***********************************************************************************
     * Returns a range of values as a tuple, mainly for iteration purposes.
     *********************/

    Expression RANGE(ref Scope s, Expression[] args) {
        if(args.length != 2 && args.length != 3)
            throw new SemanticError("Function 'range' requires two or three arguments.", 0, ":("); //FIXME
        Number[] tuple;
        auto left = args[0].eval(s).value;
        auto right = args[1].eval(s).value;
        auto increment = (args.length == 3) ? args[2].eval(s).value : 1;
        for(auto i = left; i < right; i += increment) tuple ~= new Number(i);
        return new Tuple(tuple);
    }

    /***********************************************************************************
     * Returns a uniformely distributed value from a 0 .. arg range, or a random
     * collection element from arg.
     * Returns a set of values when called with multiple parameters.
     *********************/

    Expression RANDOM(ref Scope s, Expression[] args) {
        if(!args.length)
            throw new SemanticError("Function 'random' requires at least one argument.", 0, ":("); //FIXME

        auto randomGenerator = MinstdRand(unpredictableSeed);
        Expression randomImpl(Expression arg) {
            auto r = arg.eval(s);
            if(r.type & Type.Number) {
                auto v = r.value;
                return new Number(uniform(0, v));
            }
            //The other use case has to be a collection.
            auto len = r.range.length;
            return r.range[randomGenerator.front % len];
        }

        if(args.length == 1) return randomImpl(args[0]);
        else {
            Expression[] set;
            foreach(arg; args) set ~= randomImpl(arg);
            return new Set(set);
        }
    }

    ///////////////////////
    //TODO:

    Expression WRITE(ref Scope s, Expression[] args) {
        auto output = FNORD;
        foreach(arg; args) {
            output = arg.eval(s);
            if(output.type & Type.String) write(output.toString[1 .. $-1]);
            else write(output);
        }
        return output;
    }

    Expression APPLY(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Function 'apply' requires exactly two arguments.", 0, ":("); //FIXME
        auto func = args[0].eval(s);
        Expression[] callArgs;
        foreach(a; args[1].eval(s).range) callArgs ~= pass(a);
        return func.call(s, callArgs);
    }

    Expression APPEND(ref Scope s, Expression[] args) {
        if(!args.length) return FNORD;
        auto coll = args[0].eval(s);

        Expression[] newColl = coll.range;
        foreach(arg; args[1 .. $]) {
            auto earg = arg.eval(s);
            if(earg.toString != Keywords.Fnord) newColl ~= earg.range;        //TODO: Atoms!
        }

        return coll.factory(newColl);
    }

    Expression EVAL(ref Scope s, Expression[] args) {
        if(!args.length) return FNORD;
        return args[0].eval(s).eval(s);
    }

    Expression READ(ref Scope s, Expression[] args) {
        string prompt = "";
        if(args.length) prompt = args[0].eval(s).toString;
        auto input = stdin.readln;
        auto output = parser.parse(input[0 .. $-1], "__stdin");
        if(!output.length) return FNORD;
        if(output.length != 1) return new List(output);
        return output[0];
    }

    /////////////////////////////////////////////////////////////////////
    // New parser:

    Expression READSTRING(ref Scope s, Expression[] args) {
        auto input = args[0].eval(s).toString[1 .. $-1];
        auto output = parser.parse(input, "__string");
        if(!output.length) return FNORD;
        if(output.length != 1) return new List(output);
        return output[0];
    }

    Expression READLN(ref Scope s, Expression[] args) {
        string prompt = "";
        if(args.length) prompt = args[0].eval(s).toString;
        auto input = stdin.readln;
        return new String(input[0 .. $-1]);
    }

    //TODO: Numbers and strings!

    Expression LEX(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Syntax keyword 'lex' requires exactly two arguments.", 0, ":("); //FIXME
        auto input = args[0].eval(s).toString[1 .. $-1];
        auto expressionTable = args[1].eval(s).range;
        string[] syntaxTable;
        foreach(e; expressionTable) syntaxTable ~= e.toString[1 .. $-1];

        auto tokens = ASM.lexer.lex(input, syntaxTable);
        Expression[] list;

        foreach(token; tokens) list ~= new Symbol(token);
        return new List(list);
    }

    Expression LEX2(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Syntax keyword 'lex' requires exactly two arguments.", 0, ":("); //FIXME
        auto input = args[0].eval(s).toString[1 .. $-1];
        auto expressionTable = args[1].eval(s).range;
        string[] syntaxTable;
        foreach(e; expressionTable) syntaxTable ~= e.toString[1 .. $-1];


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

        auto tokens = lex(input, syntaxTable);
        Expression[] list;

        foreach(token; tokens) list ~= new Symbol(token);
        return new List(list);
    }

    Expression CATCH(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("SyntaxKeyword 'catch' requires exactly two arguments", 0, ":("); //FIXME
        auto output = FNORD;

        try output = args[0].eval(s);
        catch(utils.exception.MyException e)
            return args[1].eval(s).call(s, [pass(new String(e.toString))]);
        return output;
    }

    Expression QUIT(ref Scope s, Expression[] args) {
        throw new Exception("Bye, bye.");
    }

    //TODO: ParsingError(Expression) ?
    //TODO metadata
    Expression SYNTAXERROR(ref Scope s, Expression[] args) {
        throw new SyntacticError(args[0].eval(s).toString[1 .. $-1], 0, "nope");
    }

    Expression ERROR(ref Scope s, Expression[] args) {
        throw new SemanticError(args[0].eval(s).toString[1 .. $-1], args[0].line, args[0].file);
    }
}
