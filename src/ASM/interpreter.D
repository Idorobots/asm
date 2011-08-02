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

debug import std.stdio;
debug import utils.testing;


//import std.parallelism; //TODO: Parallel!
import std.file : readText, FileException;
import std.utf : UtfException;
import std.random;

import utils.ctfe : tr, ETuple;

import ASM.lexical;
import ASM.AST;
import ASM.kit;
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
        global.defineSyntax(Keywords.Fnord, FNORD);
        global.defineSyntax(Keywords.Import, new BuiltinKeyword(&IMPORT));
        global.defineSyntax(Keywords.Do, new BuiltinKeyword(&DO));
        global.defineSyntax(Keywords.If, new BuiltinKeyword(&IF));
        global.defineSyntax(Keywords.While, new BuiltinKeyword(&WHILE));
        global.defineSyntax(Keywords.Set, new BuiltinKeyword(&SET));
        global.defineSyntax(Keywords.Get, new BuiltinKeyword(&GET));
        global.defineSyntax(Keywords.Quote, new BuiltinKeyword(&QUOTE));
        global.defineSyntax(Keywords.Quasiquote, new BuiltinKeyword(&QQUOTE));
        global.defineSyntax(Keywords.Embed, new BuiltinKeyword(&EMBED));
        global.defineSyntax(Keywords.IsEqual, new BuiltinKeyword(&ISEQUAL));
        global.defineSymbol(Keywords.Mult, new PureBuiltin(&MULT));
        global.defineSymbol(Keywords.Add, new PureBuiltin(&ADD));
        global.defineSyntax(Keywords.Var, new BuiltinKeyword(&VAR));
        global.defineSyntax(Keywords.Function, new BuiltinKeyword(&FUNCTION));
        global.defineSyntax(Keywords.Macro, new BuiltinKeyword(&MACRO));
        global.defineSyntax(Keywords.Scope, new BuiltinKeyword(&SCOPE));
        global.defineSyntax(Keywords.Cons, new BuiltinKeyword(&CONS));
        global.defineSyntax(Keywords.Car, new BuiltinKeyword(&CAR));
        global.defineSyntax(Keywords.Cdr, new BuiltinKeyword(&CDR));
        global.defineSymbol(Keywords.Map, new PureBuiltin(&MAP));
        global.defineSymbol(Keywords.Reduce, new PureBuiltin(&REDUCE));
        global.defineSyntax(Keywords.TypeOf, new BuiltinKeyword(&TYPEOF));
        global.defineSyntax(Keywords.KeywordsOf, new BuiltinKeyword(&KEYWORDSOF));
//        global.defineSyntax("__defaultArgDispatcher", new BuiltinKeyword(&DEFAULTARGDISPATCHER));
        global.defineSyntax("call", new BuiltinKeyword(&CALL));
        global.defineSyntax("defined?", new BuiltinKeyword(&DEFINED));
        global.defineSymbol("at", new PureBuiltin(&AT));
        global.defineSymbol("filter", new PureBuiltin(&FILTER));
        global.defineSymbol("__setcall", global.get("filter"));
        global.defineSymbol("__listcall", global.get("at"));
        global.defineSymbol("__scopecall", global.get(Keywords.Get));
        global.defineSymbol("__listeval", global.get(Keywords.Function));
        global.defineSymbol("__seteval", global.get(Keywords.Do));
        global.defineSymbol("__tupleeval", global.get("call"));
        global.defineSymbol("tuple", new PureBuiltin(&TUPLE));
        global.defineSymbol("tupleof", new PureBuiltin(&TUPLEOF));
        global.defineSymbol("setof", new PureBuiltin(&SETOF));
        global.defineSymbol("listof", new PureBuiltin(&LISTOF));
        global.defineSymbol("stringof", new PureBuiltin(&STRINGOF));
        global.defineSymbol("random", new PureBuiltin(&RANDOM));
        global.defineSymbol("range", new PureBuiltin(&RANGE));
    }
    unittest {
        auto t = TestCase("Interpreter.implementations");
        auto i = new Interpreter();

        void test(int line = __LINE__)(string input, string expected) {
            string actual;
            try actual = i.doString(input);
            catch(Exception e) actual = e.toString;
            t.assertion!"=="(actual, expected, line);
        }

        //FIXME: Make these Syntax-independant.
        //Quote, Quasiquote and embed:
        test("'(foo bar)","(foo bar)");
        test("''foo", "(quote foo)");
        test("'`foo","(qquote foo)");
        test("'$wat","(embed wat)");
        test("`($(* 2 2))", "(4)");
        test("`{$(* 2 2)}", "{4}");
        test("`[$(* 2 2)]", "[4]");
        test("(var a 12) (var b 23) (var tmp a) (set! a b) (set! b tmp)", "12");
    }


    /***********************************************************************************
     * Reads and evaluates a string.
     *********************/

    string doString(in string input, Scope s) {
        string output;
        auto statements = parser.parse(input);
        foreach(statement; statements) {
            output = statement.eval(s).toString;
        }
        return output;
    }

    string doString(in string input) {  //FIXME
        return doString(input, global);
    }

    /***********************************************************************************
     * Reads and evaluates a file.
     * TODO: InterpretingError
     *********************/

    string doFile(in string filename, Scope s) {    //FIXME
        string input;
        try input = readText(filename);
        catch(FileException e) {
            throw new SemanticError("Unable to read file '"~filename~"'.");
        }
        catch(UtfException e) {
            throw new SemanticError("Malformed file '"~filename~"'.");
        }
        return doString(input, s);
    }

    string doFile(in string filename) {
        return doFile(filename, global);
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
            throw new SemanticError("Syntax keyword 'call' requires at least one argument.");

        foreach(i, el; args) {
            auto op = el.eval(s); //FIXME TODO No evaluation before the call.
            if(op.type & Type.Callable) return op.call(s, args[0 .. i]~args[i+1 .. $]);
        }

        throw new SemanticError("No applicable object found.");
    }

    /***********************************************************************************
     * Returns a subcollection containing values at given ideces.
     *********************/

    Expression AT(ref Scope s, Expression[] args) {
        if(!args.length)
            throw new SemanticError("Function 'at' requires at least one argument.");

        auto coll = args[0].eval(s);
        auto max = coll.range.length;
        Expression[] newColl;

        foreach(arg; args[1 .. $]) {
            uint index;
            try index = to!uint(arg.eval(s).value);
            catch (Exception) { throw new ObjectNotAppError(arg.toString); }

            if(index < max) newColl ~= coll.range[index];
            else throw new SemanticError("Index out of bounds: '"~arg.toString~"'.");
        }

        if(newColl.length == 1) return newColl[0];
        else return coll.factory(newColl);
    }


    /***********************************************************************************
     * Filters a collection returning a new collection containing elements satisfying
     * all the predicates passed to it.
     *********************/

    Expression FILTER(ref Scope s, Expression[] args) {
        if(args.length < 2)
            throw new SemanticError("Function 'filter' requires at least two arguments.");

        Expression[] predicates;
        foreach(arg; args[1 .. $]) predicates ~= arg.eval(s);

        Expression coll = args[0].eval(s);
        Expression[] newColl;

        foreach(ref el; coll.range) {
            bool jollyRogger = true;    //A flag to tell if an element satysfies all the predicates.
            foreach(predicate; predicates) {
                if(predicate.call(s, [pass(el)]).toString == Keywords.Fnord) {
                    jollyRogger = false;
                    break;
                }
            }
            if(jollyRogger) newColl ~= el;
        }
        return coll.factory(newColl);
    }

    /***********************************************************************************
     * Returns wether a symbol is defined in a scope.
     *********************/

     Expression DEFINED(ref Scope s, Expression[] args) {
         if(!args.length)
             throw new SemanticError("Function 'defined?' requires at least one argument.");
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
            else throw new ObjectNotAppError(args[0].toString);
        }
         return s;
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
        else throw new ObjectNotAppError(args[0].toString);
        return value;
    }

    /***********************************************************************************
     * Returns an object bound to a symbol.
     *********************/

    Expression GET(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Syntax keyword '"~Keywords.Get~"' requires exactly one argument.");
        auto arg = args[0].eval(s);
        if(arg.type & Type.Symbol) return new Reference(s.getRef(arg.toString));
        throw new ObjectNotAppError(args[0].toString);
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
            if(arg.type & Type.Collection) {
                if(arg.type & Type.Tuple) {
                    foreach(e; arg.range) {
                        if(e.toString == Keywords.Embed) return arg.eval(s); //FIXME: Quickfix.
                        if(e.toString == Keywords.Quasiquote) return arg;
                    }
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
            throw new SemanticError("Syntax keyword '"~Keywords.IsEqual~"' requires at least two arguments.");
        auto first = args[0].eval(s);
        foreach(arg; args[1 .. $]) {
            if(arg.eval(s).toString != first.toString) return FNORD;
        }
        return first;
    }

    /***********************************************************************************
     * Multiplies two values.
     * FIXME: Needs only two args.
     *********************/

    Expression MULT(ref Scope s, Expression[] args) {
        if(!args.length) throw new SemanticError("Function '"~Keywords.Mult~"' requires at least one argument.");
        auto accumulator = args[0].eval(s).value;
        foreach(arg; args[1 .. $]) accumulator *= arg.eval(s).value;
        return new Number(accumulator);
    }

    /***********************************************************************************
     * Adds two values.
     * FIXME: Needs only two args.
     *********************/

    Expression ADD(ref Scope s, Expression[] args) {
        if(!args.length) throw new SemanticError("Function '"~Keywords.Add~"' requires at least one argument.");
        auto accumulator = args[0].eval(s).value;
        foreach(arg; args[1 .. $]) accumulator += arg.eval(s).value;
        return new Number(accumulator);
    }

    /***********************************************************************************
     * Binds symbols to other objects.
     *********************/

    Expression VAR(ref Scope s, Expression[] args) {
        if(!args.length || args.length > 2)
            throw new SemanticError("Syntax keyword '"~Keywords.Var~"' requires one or two arguments.");
        if(!(args[0].type & Type.Symbol))
            throw new ObjectNotAppError(args[0].toString);
        auto value = args.length == 2 ? args[1].eval(s) : FNORD;
        if(value.type & Type.Settable) value = (cast(Reference)value).get;
        s.defineSymbol(args[0].toString, value);
        return value;
    }

    /***********************************************************************************
     * Returns an anonymous closure
     *********************/

    // Expression DEFAULTARGDISPATCHER(ref Scope s, Expression[] args) { //TODO: OUT
    //     if(args.length != 2)
    //         throw new SemanticError("Syntax keyword '__defaultArgDispatcher' requires exactly two arguments.");

    //     auto argList = args[0].range;
    //     auto callArgs = args[1].range;

    //     if(callArgs.length != argList.length)
    //         throw new SemanticError(format("Expected %s arguments instead of %s.",
    //                                        argList.length, callArgs.length));

    //     Expression[] tuple = [new Symbol(Keywords.Begin)];
    //     foreach(i, arg; argList) {
    //         tuple ~= new Tuple([new Symbol(Keywords.Var), arg, callArgs[i].eval(s)]);
    //     }
    //     return new Tuple(tuple);
    // }

    Expression FUNCTION(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Syntax keyword '"~Keywords.Function~"' requires exactly two arguments.");

        Expression argList = args[0];
        Expression functionBody = args[1];
        auto tmp = s; //FIXME: OUT

        return new Function(delegate Expression (ref Scope callScope, Expression[] callArgs) {
            auto closureScope = new Scope(tmp);

            if(callArgs.length != argList.range.length)
                throw new SemanticError(format("Expected %s arguments instead of %s.",
                                               argList.range.length, callArgs.length));

            foreach(i, arg; argList.range) {
                closureScope.defineSymbol(arg.toString, callArgs[i].eval(callScope));
            }
            // auto dispatchedArgs = callScope.get("__argDispatcher").call(closureScope, [argList, new List(callArgs)]);
            // writeln("Dispatched args: ", dispatchedArgs);
            // dispatchedArgs.eval(closureScope);
            return functionBody.eval(closureScope);
        });
    }

    /***********************************************************************************
     * Returns a new syntax keyword.
     *********************/

    Expression MACRO(ref Scope s, Expression[] args) {
        if(args.length != 3)
            throw new SemanticError("Syntax keyword '"~Keywords.Macro~"' requires exactly three argements.");

        Expression macroName = args[0];
        Expression argList   = args[1];
        Expression macroBody = args[2];

        auto foo = new Keyword(delegate Expression (ref Scope callScope, Expression[] callArgs) {
            // if(callArgs.length != argList.range.length)
            //     throw new SemanticError(format("Expected %s arguments instead of %s.",
            //                                    argList.range.length, callArgs.length));

            auto macroScope = new Scope(callScope);
            foreach(i, arg; argList.range) {
                if(arg.toString == "@tuple") {
                    macroScope.defineSymbol("@tuple", new Tuple(callArgs[i .. $])); //TODO: Keyword.Dots?
                    break;
                }
                else macroScope.defineSymbol(arg.toString, callArgs[i]);
            }
            return macroBody.eval(macroScope).eval(callScope);
        });

        s.defineSyntax(macroName.toString, foo);
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
     * foo, bar --> (foo bar)
     * foo, (bar) --> (foo bar)
     * (foo), bar --> ((foo) bar)
     * (foo), (bar) --> ((foo) bar)
     *********************/

    Expression CONS(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Function '"~Keywords.Cons~"' requires exactly two arguments.");
        auto arg0 = args[0].eval(s);
        auto arg1 = args[1].eval(s);

        if(arg1.type & Type.Collection) return arg1.factory([arg0]~arg1.range);
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
        if(args.length != 2) throw new SemanticError("Function 'map' requires exactly two arguments.");
        Expression[] coll;
        auto func = args[0].eval(s);
        auto collection = args[1].eval(s);
        foreach(ref value; collection.range) coll ~= func.call(s, [pass(value)]);
        return collection.factory(coll);
    }

    /***********************************************************************************
     * Maps an operation to a collection reducing it to a single value. Type generic.
     *********************/

    Expression REDUCE(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Function 'reduce' requires exactly two arguments.");
        auto func = args[0].eval(s);
        auto collection = args[1].eval(s);
        auto result = collection.range[0];
        foreach(ref value; collection.range) result = func.call(s, [pass(result), pass(value)]);
        return result;
    }

    /***********************************************************************************
     * (if condition then else/fnord)
     *********************/

    Expression IF(ref Scope s, Expression[] args) {
        if(args.length != 2 && args.length != 3)
            throw new SemanticError("Syntax keyword '"~Keywords.If~"' requires two or three arguments.");
        if(args[0].eval(s).toString != Keywords.Fnord) //FIXME: FNORD
            return args[1].eval(s);
        else return args.length == 3 ? args[2].eval(s) : FNORD;
    }

    /***********************************************************************************
     * Evaluates arguments and returns value of the last one.
     *********************/

     Expression DO(ref Scope s, Expression[] args) {
         Expression output = FNORD;
         foreach(el; args) output = el.eval(s);
         return output;
     }


    /***********************************************************************************
     * While loop. Repeats it's contents while condition is not fnord.
     *********************/

    Expression WHILE(ref Scope s, Expression[] args) {
        if(args.length != 2)
            throw new SemanticError("Syntax keyword '"~Keywords.While~"' requires two or three arguments.");

        auto condition = args[0];
        auto body_ = args[1];
        Expression output = FNORD;

        writeln(condition.toString, " ", body_.toString);

        while(condition.eval(s).toString != Keywords.Fnord) {
            output = body_.eval(s);
        }
        return output;
    }

    /***********************************************************************************
     * Returns a type tuple of an object.
     *********************/

    Expression TYPEOF(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Function '"~Keywords.TypeOf~"' requires exactly one argument.");
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
            throw new SemanticError("Function '"~Keywords.KeywordsOf~"' requires exactly one argument.");
        auto keywords = args[0].eval(s).keywords;
        if(!keywords.length) return FNORD;

        Expression[] tuple;
        foreach(keyword; keywords) tuple ~= new Symbol(keyword);
        return new Tuple(tuple);
    }

    /***********************************************************************************
     * Creates a tuple of its arguments.
     *********************/

    Expression TUPLE(ref Scope s, Expression[] args) {
        Expression[] tuple;
        foreach(arg; args) tuple ~= arg.eval(s);
        if(tuple.length) return new Tuple(tuple);
        return FNORD;
    }

    /***********************************************************************************
     * Returns a set representation of a collection.
     * TODO: Change args type in place if possible?
     *********************/

    Expression SETOF(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Function 'setof' requires exactly one argument.");
        return new Set(args[0].eval(s).range.dup);
    }

    /***********************************************************************************
     * Returns a list representation of a collection.
     * TODO: Same as above.
     *********************/

    Expression LISTOF(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Function 'listof' requires exactly one argument.");
        return new List(args[0].eval(s).range.dup);
    }

    /***********************************************************************************
     * Retruns a tuple representation of a collection.
     * TODO: Same as above?
     *********************/

    Expression TUPLEOF(ref Scope s, Expression[] args) {
        if(args.length != 1)
            throw new SemanticError("Function 'tupleof' requires exactly one argument.");
        return new Tuple(args[0].eval(s).range);
    }

    /***********************************************************************************
     * Retruns a string representation of an argument.
     *********************/

    Expression STRINGOF(ref Scope s, Expression [] args) {
        if(args.length != 1)
            throw new SemanticError("Function 'stringof' requires exactly one argument.");
        return new String("\""~args[0].eval(s).toString~"\"");
    }

    /***********************************************************************************
     * Returns a range of values as a tuple, mainly for iteration purposes.
     *********************/

    Expression RANGE(ref Scope s, Expression[] args) {
        if(args.length != 2 && args.length != 3)
            throw new SemanticError("Function 'range' requires two or three arguments.");
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
        if(!args.length) throw new SemanticError("Function 'random' requires at least one argument.");

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
}
