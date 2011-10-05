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
    enum ASMVersion    = 0;            ///ASM language version supported.
    enum Vendor        = "DASM";       ///Vendor name.
    enum MajorRevision = 0;            ///Major revision.
    enum MinorRevision = 0;            ///Minor revision.

    private Parser parser;          //Parsing unit.
    private Scope global;           //Global scope.

    this() {
        this.parser = new Parser();
        global = new Scope();

        //Some version and vendor info:
        //TODO: Immutable
        define("*vendor*", new String(Vendor));
        define("*major-revision*", new Number(MajorRevision));
        define("*minor-revision*", new Number(MinorRevision));
        define("*asm-version*", new Number(ASMVersion));

        FNORD = new Tuple([]);
        define(Keywords.Fnord, FNORD);

        define(Keywords.Import, new BuiltinKeyword(&IMPORT, 1, INF_ARITY));

        auto DO = new BuiltinKeyword(&DO, 1, INF_ARITY);
        define(Keywords.Do, DO);
        define(Keywords.If, new BuiltinKeyword(&IF, 1, INF_ARITY));

        define(Keywords.Set, new BuiltinKeyword(&SET, 2));
        auto GET = new BuiltinKeyword(&GET, 1);
        define(Keywords.Get, GET);

        define(Keywords.Quote, new BuiltinKeyword(&QUOTE, 1));
        define(Keywords.Quasiquote, new BuiltinKeyword(&QQUOTE, 1));
        define(Keywords.Embed, new BuiltinKeyword(&EMBED, 1));

        define(Keywords.IsEqual, new BuiltinKeyword(&ISEQUAL, 1, INF_ARITY));

        define(Keywords.Mult, new PureBuiltin(&MULT, 2));
        define(Keywords.Div, new PureBuiltin(&DIV, 2));
        define(Keywords.Add, new PureBuiltin(&ADD, 2));
        define(Keywords.Sub, new PureBuiltin(&SUB, 2));
        define(Keywords.Mod, new PureBuiltin(&MOD, 2));
        define("pow", new PureBuiltin(&POW, 2));

        define(Keywords.Var, new BuiltinKeyword(&VAR, 1, INF_ARITY));
        auto LAMBDA = new BuiltinKeyword(&LAMBDA, 2);
        define(Keywords.Lambda, LAMBDA);
        define(Keywords.Macro, new BuiltinKeyword(&MACRO, 3));
        define(Keywords.Scope, new BuiltinKeyword(&SCOPE, 1, INF_ARITY));

        define(Keywords.Cons, new BuiltinKeyword(&CONS, 2));
        define(Keywords.Car, new BuiltinKeyword(&CAR, 1));
        define(Keywords.Cdr, new BuiltinKeyword(&CDR, 1));

        define(Keywords.Map, new PureBuiltin(&MAP, 2));
        define(Keywords.Reduce, new PureBuiltin(&REDUCE, 2, 3));

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
        define("lazy", new BuiltinKeyword(&LAZY, 1));

        //New parser/lexer routines:
        define("lex", new BuiltinKeyword(&LEX, 2));
        define("read-from-string", new BuiltinKeyword(&READSTRING, 1));
        define("readln", new BuiltinKeyword(&READLN));
        define("catch", new BuiltinKeyword(&CATCH, 2));
        define("error", new BuiltinKeyword(&ERROR, 1));
        define("syntax-error", new BuiltinKeyword(&SYNTAXERROR, 1));
        define("quit", new BuiltinKeyword(&QUIT));

        //Other:
        define("is?", new PureBuiltin(&IS, 2));
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
        test("`($(mult 2 2))", "(4)");
        test("`{$(mult 2 2)}", "{4}");
        test("`[$(mult 2 2)]", "[4]");
        test("(var a 12) (var b 23) (var tmp a) (set! a b) (set! b tmp)", "12");
        test(`(stringof (tupleof "cool"))`, `"cool"`);
        test("(join 1 1)", "(1 1)");
        test("(join 1 fnord)", "(1)");
        test("(join 1 '())", "(1)");
        test("(join 1 ())", "(1)");
        test("(join '[1 2 3] fnord)", "([1 2 3])");
        test(`(join "foo" "bar")`, `"foobar"`);
        test("(add 2 2)", "4");
        test("(add (mult 2 100) (mult 1 10))", "210");
        test("(if ((> 6 5) (add 1 1)) ('else (add 2 2)))", "2");
        test("(if ((< 6 5) (add 1 1)) ('else (add 2 2)))", "4");
        test("(var x)", "()");
        test("x", "()");
        test("(var x 3)", "3");
        test("x", "3");
        test("(add x x)", "6");
        test("(var x 2 3 4)", "(2 3 4)");
        test("x", "(2 3 4)");
        test("(var bar (mult 1 1) (mult 2 2) (mult 3 3))", "(1 4 9)");
        test("bar", "(1 4 9)");
        test("(var (x y) '(1 2))", "(1 2)");
        test("x", "1");
        test("y", "2");
        test("(var (a b))", "()");
        test("a", "()");
        test("b", "()");
        test("(var bar ((lambda () '(1 2 3))))", "(1 2 3)");
        test("bar", "(1 2 3)");
        test("(var (foo bar baz) ((lambda () '(1 2 3))))", "(1 2 3)");
        test("bar", "2");
        test("(do (var x 1) (set! x (add x 1)) (add x 1))", "3");
        test("((lambda [x] (add x x)) 5)", "10");
        test("(equal? fnord () '())", "yup");
        test("(equal? 3.14159265 3.141592)", "()");
        test("(equal? '(1 2 3) (tuple (add 0 1) (add 1 1) (add 2 1)))", "(1 2 3)");
        test(`(equal? "string" (stringof 'string))`, `"string"`);
        test("(if (1 2) (3 4))", "2");
        test("(if (() 1) (2 3))", "3");
        test("(if ('foo 'bar))", "bar");
        test("(if (fnord 'bar))", "()");
        test("(if (fnord 1 2 3) (() 2 3 4) ('() 3 4 5) ('fnord 4 5 6))", "6");
        test("('[1 2 3] 1)", "2");
        test("('[1 2 3] -1)", "3");
        test("(var bar '[1 2 3])", "[1 2 3]");
        test("(bar -1)", "3");
        test("(bar 1)", "2");
        test("(bar 23)", "()");
        test("(bar -3)", "1");
        test("(bar -23)", "()");
    }

    /***********************************************************************************
     * Reads and evaluates a string.
     *********************/

    string doString(in string input, Scope s = null, string filename = "__repl") {
        if(!s) s = global;

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
        if(!s) s = global;

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
        auto indexArg = args[1].eval(s).value;
        int index;
        try {
            index = to!int(indexArg);
        }
        catch(Exception e) {
            return FNORD;
        }
        auto collection = args[0].eval(s);
        auto coll = collection.range;
        if(index >= 0) {
            if(index < coll.length) {
                if(isImmutable(collection)) {
                    return coll[index];
                }
                else return new Reference(&coll[index]);
            }
        }
        else if(index < 0) {
            index = -index;
            if(index <= coll.length) {
                if(isImmutable(collection)) {
                    return coll[$-index];
                }
                else return new Reference(&coll[$-index]);
            }
        }
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
     * Defines a binary operator rutine.
     *********************/

    Expression mathOp(string op)(ref Scope s, Expression[] args) {
        auto a = args[0].eval(s).value;
        auto b = args[1].eval(s).value;
        return new Number(mixin("a "~op~" b"));
    }

    alias mathOp!"*"  MULT;
    alias mathOp!"/"  DIV;
    alias mathOp!"+"  ADD;
    alias mathOp!"-"  SUB;
    alias mathOp!"%"  MOD;
    alias mathOp!"^^" POW;

    /***********************************************************************************
     * Binds symbols to other objects.
     * TODO: Tweak its semantics.
     * TODO: Tidy this up.
     *********************/

    Expression VAR(ref Scope s, Expression[] args) {
        Expression value = FNORD;

        if(isSymbol(args[0])) {
            //Simple variable declaration.
            if(args.length == 1)
                value =  FNORD;
            //Variable declaration with initialisation.
            else if(args.length == 2)
                value = args[1].eval(s).deref;
            //Tuple packing.
            else {
                Expression[] tuple;
                foreach(arg; args[1 .. $])
                    tuple ~= arg.eval(s);
                value = new Tuple(tuple);
            }
            s.define(args[0].toString, value);
        }
        else if(isTuple(args[0])) {
            auto vars = args[0].range;

            //Multiple variable declaration.
            if(args.length == 1)
                foreach(var; vars)
                    s.define(var.toString, FNORD);
            //Tuple unpacking.
            else if(args.length == 2) {
                value = args[1].eval(s);

                if(vars.length != value.range.length)
                    throw new ObjectNotAppError(args[1]);

                foreach(i, var; vars)
                    s.define(var.toString, value.range[i].deref);
            }
            else throw new ObjectNotAppError(args[2]); //FIXME: Inconsistent.

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
     *********************/

    Expression MACRO(ref Scope s, Expression[] args) {
        s.define(args[0].toString, new Macro(args[1], args[2]));
        return args[0];
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

        Expression result;
        Expression[] range;

        if(args.length == 3) {
            result = args[2].eval(s);
            range = collection.range;
        }
        else {
            result = collection.range[0];
            range = collection.range[1 .. $];
        }

        foreach(ref value; range)
            result = func.call(s, [pass(result), pass(value)]);

        return result;
    }

    /***********************************************************************************
     * Evaluates every car of its arguments and if it's a non-fnord return evaluated cdr
     * of that arg.
     *
     * (if (c1 e1 ...)
     *     (c2 e2 ...)
     *     ...)
     * TODO: This should be scoped.
     *********************/

    Expression IF(ref Scope s, Expression[] args) {
        foreach(arg; args) {
            if(arg.range.length < 2) throw new ObjectNotAppError(arg);
            auto condition = arg.range[0];
            if(condition.eval(s) != FNORD) {
                Expression result;
                foreach(expr; arg.range[1 .. $]) {
                    result = expr.eval(s);
                }
                return result;
            }
        }
        return FNORD;
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

    Expression make(T)(ref Scope s, Expression[] args)
        if(is(T : Expression))
    {
        Expression[] coll;
        foreach(arg; args)
            coll ~= arg.eval(s).deref;
        return new T(coll);
    }

    alias make!Set       MAKESET;
    alias make!Vector    MAKEVECTOR;
    alias make!Tuple     MAKETUPLE;

    /***********************************************************************************
     * Returns another representation of a collection.
     *********************/

    Expression convTo(T)(ref Scope s, Expression[] args)
        if(is(T : Expression))
    {
        return new T(args[0].eval(s).range);
    }

    alias convTo!Set     SETOF;
    alias convTo!Vector  VECTOROF;
    alias convTo!Tuple   TUPLEOF;

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
            return len > 0 ? r.range[randomGenerator.front % len] : FNORD;
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

    Expression IS(ref Scope s, Expression[] args) {
        auto arg0 = args[0].eval(s).deref;
        auto arg1 = args[1].eval(s).deref;

        if(arg0 is arg1) return arg0;
        return FNORD;
    }
}
