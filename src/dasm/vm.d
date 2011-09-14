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
 * ASM Virtual Machine
 ****************************************/

module dasm.vm;

import std.stdio;
import std.file : readText, FileException;
import std.utf : UtfException;
import std.random;

import utils.ctfe : tr, ETuple;
import utils.testing : TestCase;

import dasm.lexical;
import dasm.ast;
import dasm.kit;
import dasm.lexer;
import dasm.parser;


/***********************************************************************************
 * Interpreter
 *********************/

class VM {
    Parser parser;          //Parsing unit.
    Scope global;           //Global scope.

    this() {
        this.parser = new Parser();
        global = new Scope();

        FNORD = new Tuple([]);
        define(Keywords.Fnord, FNORD);
        define(Keywords.Import, new BuiltinKeyword(&IMPORT, 1, INF_ARITY));
        auto DO = new BuiltinKeyword(&DO, 1, INF_ARITY);
        define(Keywords.Do, DO);
        define(Keywords.If, new BuiltinKeyword(&IF, 2, 3));
        define(Keywords.Set, new BuiltinKeyword(&SET, 2));
        auto GET = new BuiltinKeyword(&GET, 1);
        define(Keywords.Get, GET);
        define(Keywords.Quote, new BuiltinKeyword(&QUOTE, 1));
        define(Keywords.Quasiquote, new BuiltinKeyword(&QQUOTE, 1));
        define(Keywords.Embed, new BuiltinKeyword(&EMBED, 1));
        define(Keywords.IsEqual, new BuiltinKeyword(&ISEQUAL, 1, INF_ARITY));
        define(Keywords.Mult, new PureBuiltin(&MULT, 1, INF_ARITY));
        define(Keywords.Div, new PureBuiltin(&DIV, 2, INF_ARITY));
        define(Keywords.Add, new PureBuiltin(&ADD, 1, INF_ARITY));
        define(Keywords.Sub, new PureBuiltin(&SUB, 1, INF_ARITY));
        define(Keywords.Var, new BuiltinKeyword(&VAR, 1, INF_ARITY));
        auto LAMBDA = new BuiltinKeyword(&LAMBDA, 2);
        define(Keywords.Lambda, LAMBDA);
        define(Keywords.Macro, new BuiltinKeyword(&MACRO, 3));
        define(Keywords.Scope, new BuiltinKeyword(&SCOPE, 1, INF_ARITY));
        define(Keywords.Cons, new BuiltinKeyword(&CONS, 2));
        define(Keywords.Car, new BuiltinKeyword(&CAR, 1));
        define(Keywords.Cdr, new BuiltinKeyword(&CDR, 1));
        define(Keywords.Map, new PureBuiltin(&MAP, 2));
        define(Keywords.Reduce, new PureBuiltin(&REDUCE, 2));
        define(Keywords.TypeOf, new BuiltinKeyword(&TYPEOF, 1));
        define(Keywords.KeywordsOf, new BuiltinKeyword(&KEYWORDSOF, 1));
        define("call", new BuiltinKeyword(&CALL, 1, INF_ARITY));
        define("defined?", new BuiltinKeyword(&DEFINED, 1));
        auto NTH = new PureBuiltin(&NTH, 2);
        define("nth", NTH);
        auto SELECT = new PureBuiltin(&SELECT, 2);
        define("select", SELECT);
        define("__setcall", SELECT);
        define("__vectorcall", NTH);
        define("__scopecall", GET);
        define("__vectoreval", LAMBDA);
        define("__seteval", DO);
        define("tuple", new PureBuiltin(&MAKETUPLE, 0, INF_ARITY));
        define("set", new PureBuiltin(&MAKESET, 0, INF_ARITY));
        define("vector", new PureBuiltin(&MAKEVECTOR, 0, INF_ARITY));
        define("tupleof", new PureBuiltin(&TUPLEOF, 1));
        define("setof", new PureBuiltin(&SETOF, 1));
        define("vectorof", new PureBuiltin(&VECTOROF, 1));
        define("stringof", new PureBuiltin(&STRINGOF, 1));
        define("random", new PureBuiltin(&RANDOM, 1, INF_ARITY));
        define("range", new PureBuiltin(&RANGE, 2, 3));
        define("write", new Builtin(&WRITE, 1, INF_ARITY));
        define("append", new PureBuiltin(&APPEND, 0, INF_ARITY));
        define("apply", new PureBuiltin(&APPLY, 2));
        define("eval", new BuiltinKeyword(&EVAL, 1));
        define("read", new BuiltinKeyword(&READ));
        define("\\space", new String(" "));
        define("\\tab", new String("\t"));
        define("\\newline", new String("\n"));
        define("lazy", new BuiltinKeyword(&LAZY, 1));

        //New parser/lexer routines:
        define("lex", new BuiltinKeyword(&LEX, 2));
        define("read-from-string", new BuiltinKeyword(&READSTRING, 1));
        define("readln", new BuiltinKeyword(&READLN));
        define("catch", new BuiltinKeyword(&CATCH, 2));
        define("error", new BuiltinKeyword(&ERROR, 1));
        define("syntax-error", new BuiltinKeyword(&SYNTAXERROR, 1));
        define("quit", new BuiltinKeyword(&QUIT));
    }
    unittest {
        auto t = TestCase("Interpreter.builtins");
        auto i = new VM();

        void test(int line = __LINE__)(string input, string expected) {
            string actual;
            try actual = i.doString(input, null, "__unittest");
            catch(Exception e) actual = e.toString;
            t.equals(actual, expected, line);
        }

        //Sanity checks:
        //TODO: Move those to an apropriate context in this file.
        test("(tuple () '() fnord 'fnord)", "(() () () fnord)");
        test("'(foo bar)","(foo bar)");
        test("''foo", "(quote foo)");
        test("'`foo","(qquote foo)");
        test("'$wat","(embed wat)");
        test("`($(* 2 2))", "(4)");
        test("`{$(* 2 2)}", "{4}");
        test("`[$(* 2 2)]", "[4]");
        test("(var a 12) (var b 23) (var tmp a) (set! a b) (set! b tmp)", "12");
        test(`(stringof (tupleof "cool"))`, `"cool"`);
        test("(join 1 1)", "(1 1)");
        test("(join 1 fnord)", "(1)");
        test("(join 1 '())", "(1)");
        test("(join 1 ())", "(1)");
        test("(join '[1 2 3] fnord)", "([1 2 3])");
        test(`(join "foo" "bar")`, `"foobar"`);
        test("(+ 2 2)", "4");
        test("(+ (* 2 100) (* 1 10))", "210");
        test("(if (> 6 5) (+ 1 1) (+ 2 2))", "2");
        test("(if (< 6 5) (+ 1 1) (+ 2 2))", "4");
        test("(var x 3)", "3");
        test("x", "3");
        test("(+ x x)", "6");
        test("(var x 2 3 4)", "(2 3 4)");
        test("x", "(2 3 4)");
        test("(var bar (* 1 1) (* 2 2) (* 3 3))", "(1 4 9)");
        test("bar", "(1 4 9)");
        test("(var (x y) '(1 2))", "(1 2)");
        test("x", "1");
        test("y", "2");
        test("(do (var x 1) (set! x (+ x 1)) (+ x 1))", "3");
        test("((lambda [x] (+ x x)) 5)", "10");
        test("('[1 2 3] 1)", "2");
        test("('[1 2 3] -1)", "3");
        test("(equal? fnord () '())", "yup");
        test("(equal? 3.14159265 3.141592)", "()");
    }


    /***********************************************************************************
     * Reads and evaluates a string.
     *********************/

    string doString(in string input, Scope s = null, string filename = "__repl") {
        if(!s) s = global;    //FIXME

        string output;
        auto statements = parser.parse(input, filename);
        foreach(statement; statements) {
            output = statement.eval(s).toString;
        }
        return output;
    }

    /***********************************************************************************
     * Reads and evaluates a file.
     *********************/

    string doFile(in string filename, Scope s = null) {
        if(!s) s = global;  //FIXME

        string input;
        try input = readText(filename);
        catch(FileException e) { //TODO: Change both of these to InterpretingError?
            throw new SemanticError("Unable to read file '"~filename~"'.", 0, "__interpreter"); //FIXME
        }
        catch(UtfException e) {
            throw new SemanticError("Malformed file '"~filename~"'.", 0, "__interpreter"); //FIXME
        }
        return doString(input, s, filename);
    }

    /***********************************************************************************
     * Defines a new native function
     *********************/

     void define(string name, proc_t proc, uint minArity = 0, uint maxArity = 0) {
         global.define(name, new Builtin(proc, minArity, maxArity));
     }

     void define(string name, Expression e) {
         global.define(name, e);
     }

    /***********************************************************************************
     * DEFINITIONS:
     *********************/

    /***********************************************************************************
     * UnitType - the ony "false" value arround.
     *********************/

    Expression FNORD;

    /***********************************************************************************
     * Performs a call on its arguments.
     *********************/

    Expression CALL(ref Scope s, Expression[] args) {
        auto op = args[0].eval(s);
        return op.call(s, args[1 .. $]);
    }

    /***********************************************************************************
     * Returns an element at a given index.
     *********************/

    Expression NTH(ref Scope s, Expression[] args) {
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
        Expression predicate = args[1].eval(s);
        Expression coll = args[0].eval(s);
        Expression[] newColl;

        foreach(ref el; coll.range) {
            if(predicate.call(s, [pass(el)]) != FNORD)
                newColl ~= el;
        }
        return coll.factory(newColl);
    }

    /***********************************************************************************
     * Returns wether a symbol is defined in a scope.
     *********************/

     Expression DEFINED(ref Scope s, Expression[] args) {
         Expression[] argv;
         foreach(arg; args) argv ~= arg.eval(s);
         foreach(arg; argv) if(!s.isDefined(arg.toString)) return FNORD;
         return argv[0];
     }

    /***********************************************************************************
     * Imports and interprets a file, merges Scopes.
     *********************/

    Expression IMPORT(ref Scope s, Expression[] args) {
        foreach(a; args) {
            auto arg = a.eval(s);
            if(isString(arg)) doFile(arg.toString[1 .. $-1], s);
            else if(isSymbol(arg)) doFile(tr!(".", "/")(arg.toString)~".asm", s);
            else throw new ObjectNotAppError(args[0]);
        }
         return new Symbol("done");
    }

    /***********************************************************************************
     * Sets a settable location with a new value.
     *********************/

    Expression SET(ref Scope s, Expression[] args) {
        auto location = args[0].eval(s);
        auto value = args[1].eval(s);

        if(isSettable(location)) {
            auto reference = cast(Reference)location;
            reference.set(value);
        }
        else throw new ObjectNotAppError(args[0]);
        return value;
    }

    /***********************************************************************************
     * Returns an object bound to a symbol.
     *********************/

    Expression GET(ref Scope s, Expression[] args) {
        auto arg = args[0].eval(s);
        if(isSymbol(arg)) return s.getRef(arg);
        throw new ObjectNotAppError(args[0]);
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
            if(isCollection(arg)) {
                if(isTuple(arg)) {
                    foreach(e; arg.range) {
                        if(e.toString == Keywords.Embed) return arg.eval(s); //FIXME: Can't rely on the toString method.
                        if(e.toString == Keywords.Quasiquote) return arg;    //FIXME: Use opEquals.
                    }
                }
                else if(isString(arg)) { //TODO: hotfix
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
        if(isString(args[0])) return s.getRef(args[0]);    //FIXME: $$"string" != (embed (embed "string"))
        return args[0].eval(s);                                     //FIXME: Same thing with quote
    }

    /***********************************************************************************
     * Returns wether two objects are equal.
     *********************/

    Expression ISEQUAL(ref Scope s, Expression[] args) {
        auto first = args[0].eval(s);
        foreach(arg; args[1 .. $]) {
            if(arg.eval(s) != first) return FNORD;
        }
        return first != FNORD ? first : new Symbol("yup");
    }

    /***********************************************************************************
     * Multiplies two values.
     * FIXME: Needs only two args.
     *********************/

    Expression MULT(ref Scope s, Expression[] args) {
        auto accumulator = args[0].eval(s).value;
        foreach(arg; args[1 .. $]) accumulator *= arg.eval(s).value;
        return new Number(accumulator);
    }

    /***********************************************************************************
     * Adds two values.
     * FIXME: Needs only two args.
     *********************/

    Expression ADD(ref Scope s, Expression[] args) {
        auto accumulator = args[0].eval(s).value;
        foreach(arg; args[1 .. $]) accumulator += arg.eval(s).value;
        return new Number(accumulator);
    }


    /***********************************************************************************
     * Substracts values.
     * FIXME: Needs only two args.
     *********************/

    Expression SUB(ref Scope s, Expression[] args) {
        auto accumulator = args[0].eval(s).value;
        if(args.length < 2) return new Number(-accumulator);

        foreach(arg; args[1 .. $]) accumulator -= arg.eval(s).value;
        return new Number(accumulator);
    }

    /***********************************************************************************
     * Divides values.
     * FIXME: Needs only two args.
     *********************/

    Expression DIV(ref Scope s, Expression[] args) {
        auto accumulator = args[0].eval(s).value;
        foreach(arg; args[1 .. $]) accumulator /= arg.eval(s).value;
        return new Number(accumulator);
    }

    /***********************************************************************************
     * Binds symbols to other objects.
     * TODO: Tuple packing and unpacking.
     *********************/

    Expression VAR(ref Scope s, Expression[] args) {
        Expression value;

        if(isSymbol(args[0])) {
            if(args.length == 1)
                value =  FNORD;
            else if(args.length == 2)
                value = args[1].eval(s).deref;
            else {
                Expression[] tuple;
                foreach(arg; args[1 .. $])
                    tuple ~= arg.eval(s);
                value = new Tuple(tuple);
            }

            s.define(args[0].toString, value);
        }
        else if(isTuple(args[0])) {
            foreach(arg; args[0].range) {
                //TODO
            }
            return FNORD;
        }
        else throw new ObjectNotAppError(args[0]);

        return value;
    }

    /***********************************************************************************
     * Returns an anonymous closure.
     *********************/

    Expression LAMBDA(ref Scope s, Expression[] args) {
        return new Closure(s, args[0], args[1]);
    }

    /***********************************************************************************
     * Returns a new syntax keyword.
     * TODO: Move to a separate AST class.
     *********************/

    Expression MACRO(ref Scope s, Expression[] args) {
        Expression macroName = args[0];
        Expression argList   = args[1];
        Expression macroBody = args[2];

        uint minArity = argList.range.length;
        uint maxArity = argList.range.length;

        //TODO This shall be outta here.
        foreach(i, arg; argList.range) {
            if(contains(arg.keywords, "tuple")) {
                maxArity = INF_ARITY;
                break;
            }
        }

        auto foo = new Keyword(delegate Expression (ref Scope callScope, Expression[] callArgs) {
            auto macroScope = new Scope(callScope);
            foreach(i, arg; argList.range) {
                if(contains(arg.keywords, "tuple")) {
                    macroScope.define(arg.toString, new Tuple(callArgs[i .. $])); //TODO: Keyword.Dots?
                    break;
                }
                else macroScope.define(arg.toString, callArgs[i]);
            }
            return macroBody.eval(macroScope).eval(callScope);
        }, minArity, maxArity);

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
        auto arg0 = args[0].eval(s).deref;
        auto arg1 = args[1].eval(s).deref;

        if(isCollection(arg1)) return arg1.factory([arg0]~arg1.range);
        return new Tuple([arg0, arg1]);
    }

    /***********************************************************************************
     * Returns a Reference to the head of a mutable collection,
     * or value of the head of an immutable collection.
     *********************/

    Expression CAR(ref Scope s, Expression[] args) {
        auto collection = args[0].eval(s);
        if(!collection.range.length) return FNORD;
        if(isImmutable(collection)) return collection.range[0];
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
        auto func = args[0].eval(s);
        auto collection = args[1].eval(s);
        if(!collection.range.length) return FNORD;

        auto result = collection.range[0];

        foreach(ref value; collection.range[1 .. $])
            result = func.call(s, [pass(result), pass(value)]);
        return result;
    }

    /***********************************************************************************
     * (if condition then else/fnord)
     *********************/

    Expression IF(ref Scope s, Expression[] args) {
        if(args[0].eval(s) != FNORD)
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
             foreach(arg; args)
                 output = arg.eval(s);
         }
         else {
             void loop(bool delegate () condition) {
                 while(condition())
                     foreach(arg; args[1 .. $])
                         output = arg.eval(s);
             }

             if(contains(keywords, "while"))
                 loop({return args[0].eval(s) != FNORD;});
             else if(contains(keywords, "until"))
                 loop({return args[0].eval(s) == FNORD;});
         }
         return output;
     }

    /***********************************************************************************
     * Returns a type tuple of an object.
     *********************/

    Expression TYPEOF(ref Scope s, Expression[] args) {
        auto argType = args[0].eval(s).type;
        Expression[] typeTuple;

        alias ETuple!(Type.Immutable, Type.Settable, Type.Pure, Type.Builtin, Type.Atom, Type.Callable,
                      Type.Collection, Type.Number, Type.Symbol, Type.String, Type.Function, Type.Keyword,
                      Type.Scope, Type.Set, Type.Tuple, Type.Vector)
              types;
        alias ETuple!("immutable", "settable", "pure", "builtin", "atom", "callable", "collection", "number",
                      "symbol", "string", "function", "keyword", "scope", "set", "tuple", "vector")
              typeNames;

        /*static*/ foreach(i, type; types)
            if(argType & type)
                typeTuple ~= new Symbol(typeNames[i]);
        return new Tuple(typeTuple);
    }


    /***********************************************************************************
     * Returns a keyword tuple of an object.
     *********************/

    Expression KEYWORDSOF(ref Scope s, Expression[] args) {
        auto keywords = args[0].eval(s).keywords;
        if(!keywords.length) return FNORD;

        Expression[] tuple;
        foreach(keyword; keywords) tuple ~= new Symbol(keyword);
        return new Tuple(tuple);
    }

    /***********************************************************************************
     * Creates a set of its arguments.
     *********************/

    Expression MAKE(T)(ref Scope s, Expression[] args) {
        Expression[] coll;
        foreach(arg; args)
            coll ~= arg.eval(s).deref;
        return new T(coll);
    }

    alias MAKE!Set        MAKESET;
    alias MAKE!Vector     MAKEVECTOR;
    alias MAKE!Tuple      MAKETUPLE;

    /***********************************************************************************
     * Returns another representation of a collection.
     *********************/

    Expression TO(T)(ref Scope s, Expression[] args) {
        return new T(args[0].eval(s).range);
    }

    alias TO!Set          SETOF;
    alias TO!Vector       VECTOROF;
    alias TO!Tuple        TUPLEOF;

    /***********************************************************************************
     * Retruns a string representation of an argument.
     *********************/

    Expression STRINGOF(ref Scope s, Expression [] args) {
        auto arg = args[0].eval(s);
        if(isCollection(arg)) return new String(arg.range);
        return new String(args[0].eval(s).toString);
    }

    /***********************************************************************************
     * Returns a range of values as a tuple, mainly for iteration purposes.
     *********************/

    Expression RANGE(ref Scope s, Expression[] args) {
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
        auto randomGenerator = MinstdRand(unpredictableSeed);
        Expression randomImpl(Expression arg) {
            auto r = arg.eval(s);
            if(isNumber(r)) {
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
            if(isString(output)) write(output.toString[1 .. $-1]);
            else write(output);
        }
        return output;
    }

    Expression APPLY(ref Scope s, Expression[] args) {
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
            if(earg != FNORD) newColl ~= earg.range;
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
        if(output.length != 1) return new Tuple(output);
        return output[0];
    }

    /////////////////////////////////////////////////////////////////////
    // New parser:

    Expression READSTRING(ref Scope s, Expression[] args) {
        auto input = args[0].eval(s).toString[1 .. $-1];
        auto output = parser.parse(input, "__string");
        if(!output.length) return FNORD;
        if(output.length != 1) return new Vector(output);
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
        auto input = args[0].eval(s).toString[1 .. $-1];
        auto expressionTable = args[1].eval(s).range;
        string[] syntaxTable;
        foreach(e; expressionTable) syntaxTable ~= e.toString[1 .. $-1];

        auto tokens = dasm.lexer.lex(input, syntaxTable);
        Expression[] tokenList;

        foreach(token; tokens) {
            if(token.length && token[0] == '"') tokenList ~= new String(token[1 .. $-1]);
            else tokenList ~= new Symbol(token);
        }
        return new Vector(tokenList);
    }

    Expression CATCH(ref Scope s, Expression[] args) {
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
    Expression SYNTAXERROR(ref Scope s, Expression[] args) {
        throw new SyntacticError(args[0].eval(s).toString[1 .. $-1], args[0].line, args[0].file);
    }

    Expression ERROR(ref Scope s, Expression[] args) {
        throw new SemanticError(args[0].eval(s).toString[1 .. $-1], args[0].line, args[0].file);
    }

    Expression LAZY(ref Scope s, Expression[] args) {
        return new Lazy(args[0], s);
    }
}
