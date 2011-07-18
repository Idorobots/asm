/***********************************************************************************
 * Interpreter
 ****************************************/

module ASM.interpreter;

debug import std.stdio;

//import std.parallelism;
import std.file : readText, FileException;
import std.utf : UtfException;

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
        FNORD = new Symbol(Keyword.Fnord);

        global.defineSyntax(Keyword.Fnord, FNORD);

        global.defineSyntax(Keyword.IsEqual, new Syntax(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length < 2)
                throw new SemanticError("Syntax keyword '"~Keyword.IsEqual~"' requires at least two arguments.");
            auto first = args[0].eval(s);
            foreach(arg; args[1 .. $]) {
                if(arg.eval(s).toString != first.toString) return FNORD;
            }
            return first;
        }));

        global.defineSyntax(Keyword.Quote, new Syntax(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length) return FNORD;

            Expression tryEval(Expression arg) {
                if(arg.type & Type.List) {
                    auto l = (cast(List)arg);
                    if(l.range[0].toString == Keyword.Embeed) {
                        return l.eval(s);
                    }
                    else {
                        Expression[] list;
                        foreach(a; l.range) {
                           list ~= tryEval(a);
                        }
                        return new List(list);
                    }
                }
                return arg;
            }
            return tryEval(args[0]);
        }));

        global.defineSyntax(Keyword.Embeed, new Syntax(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length) return FNORD;
            if(args[0].type & Type.String) {
                auto str = cast(String) args[0];                //FIXME: $$"string" != (embeed (embeed "string"))
                return new Reference(s.getRef(str.value));      //FIXME: Same thing with quote
            }
            return args[0].eval(s);
        }));

        global.definePure("*", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length)
                throw new SemanticError("Function '"~Keyword.Mult~"' requires at least one argument.");
            auto accumulator = (cast(Value)args[0].eval(s)).value;
            foreach(arg; args[1 .. $]) {
                accumulator *= (cast(Value)arg.eval(s)).value;
            }
            return new Value(accumulator);
        }));

        global.definePure("+", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length)
                throw new SemanticError("Function '"~Keyword.Add~"' requires at least one argument.");
            auto accumulator = (cast(Value)args[0].eval(s)).value;
            foreach(arg; args[1 .. $]) {
                accumulator += (cast(Value)arg.eval(s)).value;
            }
            return new Value(accumulator);
        }));

        global.defineSyntax(Keyword.Function, new Syntax(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2 && args.length != 3)
                throw new SemanticError("Syntax keyword 'function' requires two or three argements.");

            Expression argList;
            Expression functionBody;

            if(args.length == 2) {
                argList = args[0];
                functionBody = args[1];
            }
            else {
                argList = args[1];
                functionBody = args[2];
            }
            auto tmp = s;   //WUT FIXME
            auto foo = new Function(delegate Expression (ref Scope callScope, Expression[] callArgs) {
                auto lambdaScope = new Scope(tmp);
                foreach(i, argName; (cast(List) argList).range) {
                    lambdaScope.defineSymbol(argName.toString, callArgs[i].eval(callScope));
                }
                return functionBody.eval(lambdaScope);
            });

            if(args.length == 2) return foo;
            else {
                s.defineSymbol(args[0].toString, foo);
                return args[0];
            }
        }));

        global.defineSyntax(Keyword.Macro, new Syntax(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 3)
                throw new SemanticError("Syntax keyword 'macro' requires exactly three argements.");

            Expression macroName = args[0];
            Expression argList   = args[1];
            Expression macroBody = args[2];

            auto foo = new Syntax(delegate Expression (ref Scope callScope, Expression[] callArgs) {
                auto macroScope = new Scope(callScope);
                foreach(i, argName; (cast(List) argList).range) {
                    macroScope.defineSymbol(argName.toString, callArgs[i]);
                }
                return macroBody.eval(macroScope).eval(callScope);
            });

            s.defineSyntax(macroName.toString, foo);
            return macroName;
        }));

        global.defineSyntax(Keyword.Var, new Syntax(delegate Expression (ref Scope s, Expression[] args) {
            if(!(args[0].type & Type.Symbol))
                throw new SemanticError("The object "~args[0].toString~" is not applicable.");
            s.defineSymbol(args[0].toString, args.length == 2 ? args[1].eval(s) : FNORD);
            return args[0];
        }));

        global.defineSyntax(Keyword.Set, new Syntax(delegate Expression (ref Scope s, Expression[] args) {
            auto expr = args[0].eval(s);
            if(expr.type & Type.Reference) {
                *((cast(Reference)expr).referee) = args[1].eval(s);
            }
            else throw new SemanticError("The object "~args[0].toString~" is not applicable.");
            return expr;
        }));

        global.defineSymbol(Keyword.Car, new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
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

        global.defineSyntax(Keyword.Body, new Syntax(delegate Expression (ref Scope s, Expression[] args) {
            if(!args.length) return FNORD;
            Expression Expression;
            foreach(arg; args[0 .. $]) {
                Expression = arg.eval(s);
            }
            return Expression;
        }));

        global.definePure("range", new PureBuiltin(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 2 && args.length != 3)
                throw new SemanticError("Function 'range' requires two or three arguments.");
            Value[] list;
            auto left = (cast(Value) args[0].eval(s)).value;
            auto right = (cast(Value) args[1].eval(s)).value;
            auto increment = (args.length == 3) ? (cast(Value) args[2].eval(s)).value : 1;
            for(auto i = left; i < right; i += increment) {
                list ~= new Value(i);
            }
            if(list.length) return new List(list);
            return FNORD;
        }));

        global.defineSyntax("import", new Syntax(delegate Expression (ref Scope s, Expression[] args) {
            if(args.length != 1) throw new SemanticError("Function 'import' requires exactly one argument.");
            if(args[0].type & Type.String) {
                auto filename = args[0].toString[1 .. $-1];
                doFile(filename, s);
                return s;
            }
            else {
                auto arg = args[0].eval(s);
                if(arg.type & Type.Symbol) {
                    doFile(arg.toString, s);
                    return s;
                }
                else if(arg.type & Type.Scope) {
                    //s.merge(args[0]); //TODO
                    return FNORD;
                }
                throw new SemanticError("The object '"~args[0].toString~"' is not applicable.");
            }
        }));

        global.defineSyntax(Keyword.Scope, new Syntax(delegate Expression (ref Scope s, Expression[] args) {
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
