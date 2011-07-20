/***********************************************************************************
 * Interpreter
 ****************************************/

module ASM.interpreter;

debug import std.stdio;

//import std.parallelism;
import std.file : readText, FileException;
import std.utf : UtfException;

import std.random;

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
        FNORD = new Symbol(Keywords.Fnord);

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
                    auto l = cast(Tuple)arg;
                    if(l.range[0].toString == Keywords.Embed) {
                        return l.eval(s);
                    }
                    else {
                        Expression[] tuple;
                        foreach(a; l.range) {
                           tuple ~= tryEval(a);
                        }
                        return new Tuple(tuple);
                    }
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
            if(args[0].type & Type.String) {
                auto str = cast(String) args[0];                //FIXME: $$"string" != (embeed (embeed "string"))
                return new Reference(s.getRef(str.value));      //FIXME: Same thing with quote
            }
            return args[0].eval(s);
        }));

        global.definePure("*", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length)
                throw new SemanticError("Function '*' requires at least one argument.");
            auto accumulator = (cast(Value)args[0].eval(s)).value;
            foreach(arg; args[1 .. $]) {
                accumulator *= (cast(Value)arg.eval(s)).value;
            }
            return new Value(accumulator);
        }));

        global.definePure("+", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length)
                throw new SemanticError("Function '+' requires at least one argument.");
            auto accumulator = (cast(Value)args[0].eval(s)).value;
            foreach(arg; args[1 .. $]) {
                accumulator += (cast(Value)arg.eval(s)).value;
            }
            return new Value(accumulator);
        }));

        global.defineSyntax(Keywords.Function, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2 && args.length != 3)
                throw new SemanticError("Syntax keyword '"~Keywords.Function~"' requires exactly two argements.");

            Expression argTuple;
            Expression functionBody;

            if(args.length == 2) {
                argTuple = args[0];
                functionBody = args[1];
            }
            auto tmp = s;   //WUT FIXME
            Expression foo;
            if(argTuple.type & Type.Symbol) foo = new Function(delegate Expression (ref Scope, Expression[] callArgs) {
                //FIXME Quickfix
                auto lambdaScope = new Scope(tmp);
                if(callArgs.length)
                    throw new SemanticError(format("Expected &s arguments instead of %s",
                                                   0, callArgs.length));
                return functionBody.eval(lambdaScope);
            });
            else foo = new Function(delegate Expression (ref Scope callScope, Expression[] callArgs) {
                auto argTupleRange = (cast(Tuple)argTuple).range;
                if(callArgs.length != argTupleRange.length)
                    throw new SemanticError(format("Expected %s arguments instead of %s.",
                                                   argTupleRange.length, callArgs.length));

                auto lambdaScope = new Scope(tmp);
                foreach(i, argName; argTupleRange) {
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
                auto argTupleRange = (cast(Tuple)argTuple).range;
                if(callArgs.length != argTupleRange.length)
                    throw new SemanticError(format("Expected %s arguments instead of %s.",
                                                   argTupleRange.length, callArgs.length));

                auto macroScope = new Scope(callScope);
                foreach(i, argName; argTupleRange) {
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
            s.defineSymbol(args[0].toString, args.length == 2 ? args[1].eval(s) : FNORD);
            return args[0];
        }));

        global.defineSyntax(Keywords.Set, new Keyword(delegate Expression (ref Scope s, Expression[] args) {
            auto expr = args[0].eval(s);
            if(expr.type & Type.Reference) {
                *((cast(Reference)expr).referee) = args[1].eval(s);
            }
            else throw new SemanticError("The object "~args[0].toString~" is not applicable.");
            return expr;
        }));

        global.defineSymbol(Keywords.Car, new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            auto collection = args[0].eval(s);
            if(collection.type & Type.Collection) {
                return new Reference(&(cast(Collection)collection).range()[0]);
            }
            else throw new SemanticError("The object "~args[0].toString~" is not applicable.");
        }));

        global.defineSymbol("global-scope", new Builtin(delegate Expression (ref Scope s, Expression[] args) {
            return global;
        }));

        global.defineSymbol("this-scope", new Builtin(delegate Expression (ref Scope s, Expression[] args) {
            return s;
        }));

        global.definePure("map", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2) throw new SemanticError("Function 'map' requires exactly two arguments.");
            Expression[] list;
            auto func = args[0].eval(s);
            auto collection = args[1].eval(s);
            if(func.type & Type.Callable) {
//                if((collection.type & Type.Collection) && (func.type & Type.Pure)) {
//                    auto r = (cast(Collection) collection).range.dup;
//                    foreach(ref value; taskPool.parallel(r)) {
//                        value = (cast(Callable) func).call(s, [value]);
//                    }
//                    return new List(r);
//                }
//                else
                if(collection.type & Type.Collection) {
                    foreach(ref value; (cast(Collection) collection).range) {
                        list ~= (cast(Callable) func).call(s, [value]);
                    }
                }
                else throw new SemanticError("The object "~args[1].toString~" is not applicable.");
            }
            else throw new SemanticError("The object "~args[0].toString~" is not applicable.");
            return new List(list);
        }));

        global.definePure("reduce", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2) throw new SemanticError("Function 'reduce' requires exactly two arguments.");
            auto func = args[0].eval(s);
            auto collection = args[1].eval(s);
            if(func.type & Type.Callable) {
                if(collection.type & Type.Collection) {
                    auto result = (cast(Collection) collection).range[0];
                    foreach(ref value; (cast(Collection) collection).range[1 .. $]) {
                        result = (cast(Callable) func).call(s, [result, value]);
                    }
                    return result;
                }
                else throw new SemanticError("The object "~args[1].toString~" is not applicable.");
            }
            else throw new SemanticError("The object "~args[0].toString~" is not applicable.");
        }));

        global.definePure("random", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length) throw new SemanticError("Function 'random' requires at least one argument.");
            auto randomGenerator = MinstdRand(unpredictableSeed);
            Expression randomImpl(Expression arg) {
                auto range = arg.eval(s);
                if(range.type & Type.Value) {
                    auto r = (cast(Value)range).value;
                    return new Value(uniform(0, r));
                }
                else if(range.type & Type.Collection) {
                    auto r = (cast(Collection)range).range.length;
                    return (cast(Collection)range).range[randomGenerator.front % r];
                }
                else throw new SemanticError("The object "~arg.toString~" is not applicable.");
            }
            if(args.length == 1) return randomImpl(args[0]);
            else {
                Expression[] list;
                foreach(arg; args) {
                    list ~= randomImpl(arg);
                }
                return new List(list);
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
            if(args[0].eval(s) !is FNORD)
                return args[1].eval(s);
            else return args.length == 3 ? args[2].eval(s) : FNORD;
        }));

        global.definePure("range", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2 && args.length != 3)
                throw new SemanticError("Function 'range' requires two or three arguments.");
            Value[] tuple;
            auto left = (cast(Value) args[0].eval(s)).value;
            auto right = (cast(Value) args[1].eval(s)).value;
            auto increment = (args.length == 3) ? (cast(Value) args[2].eval(s)).value : 1;
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
                 doFile(arg.toString, s);
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
