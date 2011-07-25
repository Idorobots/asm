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

//import std.parallelism; //TODO: Parallel!
import std.file : readText, FileException;
import std.utf : UtfException;
import std.random;
import utils.ctfe : tr;

import ASM.lexical;
import ASM.AST;
import ASM.parser;

/***********************************************************************************
 * Interpreter
 *********************/

class Interpreter {
    Parser parser;          //Parsing unit.
    Scope global;           //Global scope.

    /***********************************************************************************
     * Special fnord keyword - the ony "false" value arround.
     *********************/

    static Expression FNORD;

    this() {
        this(new DefaultParser());
    }

    this(Parser parser) {
        assert(parser !is null);
        this.parser = parser;
        global = new Scope();
        FNORD = new Symbol(Keywords.Fnord);     //NOTE: This is only for return values. Never compare against this.
                                                //NOTE: Well, actually untill I fix this.

        global.defineSyntax(Keywords.Fnord, FNORD);

        global.defineSyntax(Keywords.IsEqual, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length < 2)
                throw new SemanticError("Syntax keyword '"~Keywords.IsEqual~"' requires at least two arguments.");
            auto first = args[0].eval(s);
            foreach(arg; args[1 .. $]) {
                if(arg.eval(s).toString != first.toString) return FNORD;
            }
            return first;
        }));

        global.defineSyntax(Keywords.Quasiquote, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length) return FNORD;
            Expression tryEval(Expression arg) {
                if(arg.type & Type.Tuple) {
                    foreach(e; arg.range) if(e.toString == Keywords.Embed) return arg.eval(s); //FIXME: Quickfix.
                    //Nothing embedded:
                    Expression[] tuple;
                    foreach(a; arg.range) tuple ~= tryEval(a);
                    return new Tuple(tuple);
                }
                return arg;
            }
            return tryEval(args[0]);
        }));

        global.defineSyntax(Keywords.Quote, new Keyword(delegate Expression (ref Scope, Expression[] args) {
            return args.length ? args[0] : FNORD;
        }));

        global.defineSyntax(Keywords.Embed, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length) return FNORD;
            if(args[0].type & Type.String) {                           //FIXME: $$"string" != (embed (embed "string"))
                return new Reference(s.getRef(args[0].toString));      //FIXME: Same thing with quote
            }
            return args[0].eval(s);
        }));

        global.definePure("*", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length)
                throw new SemanticError("Function '*' requires at least one argument.");
            auto accumulator = args[0].eval(s).value;
            foreach(arg; args[1 .. $]) {
                accumulator *= arg.eval(s).value;
            }
            return new Value(accumulator);
        }));

        global.definePure("+", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length)
                throw new SemanticError("Function '+' requires at least one argument.");
            auto accumulator = args[0].eval(s).value;
            foreach(arg; args[1 .. $]) {
                accumulator += arg.eval(s).value;
            }
            return new Value(accumulator);
        }));

        global.defineSyntax(Keywords.Function, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2 && args.length != 3)
                throw new SemanticError("Syntax keyword '"~Keywords.Function~"' requires exactly two argements.");

            Expression argTuple = args[0];
            Expression functionBody = args[1];
            Expression foo;
            auto tmp = s;       //TODO: OUT

            if(argTuple.toString == Keywords.Fnord)
                foo = new Function(delegate Expression (ref Scope, Expression[] callArgs) {
                //FIXME Quickfix
                auto lambdaScope = new Scope(tmp);
                if(callArgs.length)
                    throw new SemanticError(format("Expected &s arguments instead of %s",
                                                   0, callArgs.length));
                return functionBody.eval(lambdaScope);
            });
            else foo = new Function(delegate Expression (ref Scope callScope, Expression[] callArgs) {
                if(callArgs.length != argTuple.range.length)
                    throw new SemanticError(format("Expected %s arguments instead of %s.",
                                                   argTuple.range.length, callArgs.length));

                auto lambdaScope = new Scope(tmp);
                foreach(i, argName; argTuple.range) {
                    lambdaScope.defineSymbol(argName.toString, callArgs[i].eval(callScope));
                }
                return functionBody.eval(lambdaScope);
            });
            return foo;
        }));

        global.defineSyntax(Keywords.Macro, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 3)
                throw new SemanticError("Syntax keyword '"~Keywords.Macro~"' requires exactly three argements.");

            Expression macroName = args[0];
            Expression argTuple   = args[1];
            Expression macroBody = args[2];

            auto foo = new Keyword(delegate Expression (ref Scope callScope, Expression[] callArgs) {
                if(callArgs.length != argTuple.range.length)
                    throw new SemanticError(format("Expected %s arguments instead of %s.",
                                                   argTuple.range.length, callArgs.length));

                auto macroScope = new Scope(callScope);
                foreach(i, argName; argTuple.range) {
                    macroScope.defineSymbol(argName.toString, callArgs[i]);
                }
                return macroBody.eval(macroScope).eval(callScope);
            });

            s.defineSyntax(macroName.toString, foo);
            return macroName;
        }));

        global.defineSyntax(Keywords.Var, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(!(args[0].type & Type.Symbol))
                throw new SemanticError("The object "~args[0].toString~" is not applicable.");
            auto value = args.length == 2 ? args[1].eval(s) : FNORD;
            s.defineSymbol(args[0].toString, value);
            return value;
        }));

        global.defineSyntax(Keywords.Set, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            auto expr = args[0].eval(s);
            if(expr.type & Type.Reference) {
                *((cast(Reference)expr).referee) = args[1].eval(s);
            }
            else throw new SemanticError("The object "~args[0].toString~" is not applicable.");
            return expr;
        }));

        global.defineSymbol(Keywords.Cons, new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2)
                throw new SemanticError("Function '"~Keywords.Cons~"' requires exactly two arguments.");
            auto arg0 = args[0].eval(s);
            auto arg1 = args[1].eval(s);

            if(arg1.type & Type.Collection) {
                if(arg1.type & Type.Tuple) return new Tuple([arg0]~arg1.range); //TODO: Mixins not to repeat so much.
                if(arg1.type & Type.List) return new List([arg0]~arg1.range);
                if(arg1.type & Type.Set) return new List([arg0]~arg1.range);
            }
            return new List([arg0, arg1]);
        }));

        global.defineSymbol(Keywords.Car, new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            auto collection = args[0].eval(s);
            if(collection.type & Type.Immutable) return collection.range[0];
            else return new Reference(&(collection.range[0]));
        }));

        global.defineSymbol(Keywords.Cdr, new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            auto collection = args[0].eval(s);
            if(collection.type & Type.Tuple) //TODO: Mixins!
                return collection.range.length > 1 ? new Tuple(collection.range[1 .. $]) : FNORD;
            if(collection.type & Type.List)
                return collection.range.length > 1 ? new List(collection.range[1 .. $]) : FNORD;
            if(collection.type & Type.Set)
                return collection.range.length > 1 ? new Set(collection.range[1 .. $]) : FNORD;
            return FNORD;
        }));

        global.defineSymbol("global-scope", new Builtin(delegate Expression (ref Scope s, Expression[] args) {
            return global;
        }));

        global.defineSymbol("this-scope", new Builtin(delegate Expression (ref Scope s, Expression[] args) {
            return s;
        }));

        global.definePure("map", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2) throw new SemanticError("Function 'map' requires exactly two arguments.");
            Expression[] tuple;
            auto func = args[0].eval(s);
            auto collection = args[1].eval(s);
            foreach(ref value; collection.range) {
                tuple ~= func.call(s, [value]);
            }
            return new Tuple(tuple);
        }));

        global.definePure("reduce", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2) throw new SemanticError("Function 'reduce' requires exactly two arguments.");
            auto func = args[0].eval(s);
            auto collection = args[1].eval(s);
            auto result = collection.range[0];
            foreach(value; collection.range) {
                result = func.call(s, [result, value]);
            }
            return result;
        }));

        global.definePure("random", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length) throw new SemanticError("Function 'random' requires at least one argument.");
            auto randomGenerator = MinstdRand(unpredictableSeed);
            Expression randomImpl(Expression arg) {
                auto r = arg.eval(s);
                if(r.type & Type.Value) {
                    auto v = r.value;
                    return new Value(uniform(0, v));
                }
                //The other use case has to be a collection.
                auto len = r.range.length;
                return r.range[randomGenerator.front % len];
            }
            if(args.length == 1) return randomImpl(args[0]);
            else {
                Expression[] tuple;
                foreach(arg; args) {
                    tuple ~= randomImpl(arg);
                }
                return new Tuple(tuple);
            }
        }));

        global.definePure("tuple", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            Expression[] tuple;
            foreach(arg; args) {
                tuple ~= arg.eval(s);
            }
            if(tuple.length) return new Tuple(tuple);
            return FNORD;
        }));

        global.definePure("tuple?", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 1) throw new SemanticError("Function 'touple?' requires exactly one argument.");
            auto arg = args[0].eval(s);
            if(arg.type & Type.Tuple) return arg;
            return FNORD;
        }));

        global.defineSyntax("doWhatSetDoes", new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length) return FNORD;
            Expression expr;
            foreach(arg; args[0 .. $]) {
                expr = arg.eval(s);
            }
            return expr;
        }));
        global.defineSyntax(Keywords.If, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2 && args.length != 3)
                throw new SemanticError("Syntax keyword '"~Keywords.If~"' requires two or three arguments.");
            if(args[0].eval(s).toString != Keywords.Fnord)
                return args[1].eval(s);
            else return args.length == 3 ? args[2].eval(s) : FNORD;
        }));

        global.definePure("range", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2 && args.length != 3)
                throw new SemanticError("Function 'range' requires two or three arguments.");
            Value[] tuple;
            auto left = args[0].eval(s).value;
            auto right = args[1].eval(s).value;
            auto increment = (args.length == 3) ? args[2].eval(s).value : 1;
            for(auto i = left; i < right; i += increment) {
                tuple ~= new Value(i);
            }
            if(tuple.length) return new Tuple(tuple);
            return FNORD;
        }));

        global.defineSyntax("import", new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 1) throw new SemanticError("Function 'import' requires exactly one argument.");
            auto arg = args[0].eval(s);
            if(arg.type & Type.String) {
                doFile(arg.toString[1 .. $-1], s);
                return s;
            }
            else if(arg.type & Type.Symbol) {
                 doFile(tr!(".", "/")(arg.toString)~".asm", s);
                 return s;
            }
            else if(arg.type & Type.Scope) {
                //s.merge(args[0]); //TODO
                return FNORD;
            }
            throw new SemanticError("The object '"~args[0].toString~"' is not applicable.");
        }));

        global.defineSyntax(Keywords.Scope, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            auto ns = new Scope(s);
            foreach(arg; args) {
                arg.eval(ns);
            }
            return ns;
        }));
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
}
