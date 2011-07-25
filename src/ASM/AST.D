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
 * AST primitives for the language.
 * TODO: version(safe), recursion checking etc.
 *********************/

module ASM.AST;

import std.string : format;
import std.conv : to;

import utils.exception : MyException;
import utils.ctfe;

import ASM.lexical;

/***********************************************************************************
 * Exception thrown on a semantic error.
 *********************/

class SemanticError : MyException {
    public this(string what) {
        super("Error: "~what);
    }
}

class ObjectNotApplicable : SemanticError {
    public this(string obj) {
        super("The object '"~obj~"' is not applicable.");
    }
}

/***********************************************************************************
 * Types of the AST primitives.
 *********************/

enum Type {
    //Type attributes:
    Immutable   = 1,
    Reference   = 2,
    Pure        = 4,
    Builtin     = 8,
    //Root types:
    Atom        = 16,
    Callable    = 32,
    Collection  = 64,
    //Atomic types:
    Value       = 128,
    Symbol      = 256,
    String      = 512,
    //Callable types:
    Function    = 1024,
    Keyword     = 2048,
    Scope       = 4096,
    //Set       = 8192 //???
    //Listn     = 32768,
    //Collection types:
    Set         = 8192,
    Tuple       = 16384,
    List        = 32768,
    //Scope       = 4096,
    //String      = 512,
}

/***********************************************************************************
 * An abstract S-Expression primitive, the root of the hierarchy.
 * TODO doc string?
 *********************/

abstract class Expression {

    /***********************************************************************************
     * Returns the evaluated S-expression. Defaults to self.
     *********************/

    Expression eval(ref Scope s, uint depth = 0) {
        return this;
    }

    uint type();                    ///Returns the type of the S-expression.
    override string toString();     ///Returns the string representation of an S-expression.

    /***********************************************************************************
     * Returns the underlying collection. Only for collections.
     * TODO: opApply().
     *********************/

    Expression[] range() {
        throw new ObjectNotApplicable(this.toString);
    }

    /***********************************************************************************
     * Returns the evaluated call to a callable object. Only for callables..
     *********************/

    Expression call(ref Scope s, Expression[] args) {
        throw new ObjectNotApplicable(this.toString);
    }

    //TODO: T value(T)();
    real value() {
        throw new ObjectNotApplicable(this.toString);
    }
}

/***********************************************************************************
 * Reference type used for setting.
 *********************/

class Reference : Expression {
    Expression* referee;

    this(Expression* expr) {
        if(expr.type & Type.Reference) referee = (cast(Reference)*expr).referee;
        else referee = expr;
    }

    override Expression eval(ref Scope s, uint depth = 0) {
        return referee.eval(s, depth + 1);
    }

    override uint type() {
        return Type.Reference|referee.type;
    }

    override string toString() {
        return referee.toString();
    }

    override Expression[] range() {
        return referee.range();
    }

    override Expression call(ref Scope s, Expression[] args) {
        return referee.call(s, args);
    }

    //TODO: T value(T)();
    override real value() {
        return referee.value;
    }
}

/***********************************************************************************
 * Represents numbers, symbols and string (aka WYSIWYG symbols).
 * TODO: Take the string away from here. Make it a proper immutable collection
 * and make it callable with things like append, split, etc?
 *********************/

class Atom(T, uint atomType) : Expression {
    private T val;

    this(T val) {
        this.val = val;
    }

    /***********************************************************************************
     * Returns its definition in the scope s or its value.
     *********************/

    override Expression eval(ref Scope s, uint depth = 0) {
         static if(atomType & Type.String) return this;
         else static if(atomType & Type.Symbol) return new Reference(s.getRef(val));
         else return this;
    }

    override uint type() {
        return Type.Atom|atomType;
    }

    override string toString() {
        return format("%s", val);
    }

    //TODO: string value() for symbols too.
    override real value() {
        static if(atomType & Type.Value) {
            return this.val;
        }
        else throw new ObjectNotApplicable(val);
    }
}

alias Atom!(real, Type.Value)                                  Value;      ///A number.
alias Atom!(string, Type.Symbol)                               Symbol;     ///Regular symbol.
alias Atom!(string, Type.Immutable|Type.Symbol|Type.String)    String;     ///WYSIWYG symbol.

/***********************************************************************************
 * Collection class representing lisp-like and python-like sequences.
 *********************/

class Collection(uint collectionType, char LParen, char RParen) : Expression {
    private Expression[] coll;

    this(Expression[] coll) {
        this.coll = coll;
    }

    override Expression[] range() {
        return coll;
    }

    override Expression call(ref Scope s, Expression[] args) {
        static if(collectionType & Type.Set) {
            Expression pass(ref Expression el) { //TODO: Move to ASM.utils
                Expression quote[];
                return new Tuple([cast(Expression)new Symbol(Keywords.Quote), el]);
            }

            Expression[] argv;
            foreach(arg; args) argv ~= arg.eval(s);

            Expression[] newSet;

            foreach(ref el; coll) {
                bool jollyRogger = true;    //A flag to tell if an element satysfies all the predicates.
                foreach(arg; argv) {
                    if(arg.call(s, [pass(el)]).toString == Keywords.Fnord) {
                        jollyRogger = false;
                        break;
                    }
                }
                if(jollyRogger) newSet ~= new Reference(&el);
            }
            return new Set(newSet);
        }
        else static if(collectionType & Type.List) {
            Expression[] newList;

            foreach(arg; args) {
                uint index = to!uint(arg.eval(s).value);
                if(index < coll.length) {
                    newList ~= new Reference(&coll[index]);
                }
                else throw new SemanticError("Index out of bounds: '"~arg.toString~"'.");
            }
            if(newList.length == 1) return newList[0];
            else return new List(newList);
        }
        assert(0);
    }

    /***********************************************************************************
     * Evaluates as a form for the tuple, one by one for the set, and creates
     * an anonymous function for the list. (TODO)
     *********************/

    override Expression eval(ref Scope s, uint depth = 0) {
        static if(collectionType & Type.Tuple) {
            foreach(i, el; coll) {
                auto op = el.eval(s, depth + 1); //FIXME TODO No evaluation before the call.
                if(op.type & Type.Callable) return op.call(s, coll[0 .. i]~coll[i+1 .. $]);
            }
            throw new SemanticError("No applicable object in tuple "~this.toString~".");
       }
       else static if(collectionType & Type.Set) {
           Expression output;
           foreach(el; coll) output = el.eval(s);
           return output ? output : new Symbol(Keywords.Fnord); //FIXME: Return FNORD on an empty set.
       }
       else static if(collectionType & Type.List) {
           //TODO
           return this;
       }
    }

    override uint type() {
        return Type.Collection|collectionType;
    }

    override string toString() {
        if(!coll.length) return ""~LParen~RParen;
        auto output = ""~LParen;
        foreach(el; coll) {
            if(el !is this) output ~= el.toString~" ";
            else output ~= Keywords.Self~" ";
        }
        return output[0 .. $-1]~RParen;
    }
}

alias Collection!(Type.Tuple|Type.Immutable, Syntax.LTuple, Syntax.RTuple)      Tuple;
alias Collection!(Type.Callable|Type.List, Syntax.LList, Syntax.RList)          List;
alias Collection!(Type.Callable|Type.Set, Syntax.LSet, Syntax.RSet)             Set;

/***********************************************************************************
 * Procedure type for the Callable datatype.
 *********************/

alias Expression delegate(ref Scope s, Expression[] args) proc_t;   //TODO += depth

/***********************************************************************************
 * Callable object primitive.
 *********************/

class Callable(uint procType) : Expression {
    enum string stringof = getName(procType);
    proc_t procedure;

    /***********************************************************************************
     * Returns the string representation of this Callable at compile time.
     *********************/

    private static string getName(uint procType) {
        string output;
        if(procType & Type.Builtin)  output ~= "compiled ";
        if(procType & Type.Pure)     output ~= "pure ";
        if(procType & Type.Function) output ~= "function ";
        if(procType & Type.Keyword)  output ~= "syntax keyword ";
        return utils.ctfe.capitalize(output);
    }

    this(proc_t procedure) {
        this.procedure = procedure;
    }

    override Expression call(ref Scope s, Expression[] args) {
        return procedure(s, args);
    }

    override uint type() {
        return Type.Callable|procType;
    }

    override string toString() {
        return format(""~Syntax.StringDelim~stringof~"at 0x%d."~Syntax.StringDelim, cast(void*) this);
    }
}

alias Callable!(Type.Function)                          Function;       ///Function.
alias Callable!(Type.Pure|Type.Function)                Pure;           ///Pure function.
alias Callable!(Type.Builtin|Type.Function)             Builtin;        ///Builtin function.
alias Callable!(Type.Pure|Type.Builtin|Type.Function)   PureBuiltin;    ///Builtin function.
alias Callable!(Type.Keyword)                           Keyword;        ///Syntax keyword.

/***********************************************************************************
 * A variable scope for soring symbols.
 *********************/

class Scope : Expression {
    Expression[] defines;       ///Object definitions.
    uint[string] syntax;        ///Syntax scope.
    uint[string] symbols;       ///Other symbols.

    Scope outter;               ///Outter scope.

    this(Scope outter = null) {
        this.outter = outter;
        //this.defineSyntax(Keywords.Self, this);
    }

    /***********************************************************************************
     * Defines sym in a particular namespace.
     * Eg. use: scope.definePure(somePure);
     *********************/

    private void define(string where)(string sym, Expression expression) {
        mixin(where~"[sym] = defines.length;");
        defines ~= expression;
    }

    alias define!"syntax"   defineSyntax;
    alias define!"symbols"  defineSymbol;
    alias define!"symbols"  definePure;

    /***********************************************************************************
     * Returns the definition of a symbol or throws "Undefined symbol" exception
     * if a symbol isn't found in this or parents scope.
     *********************/

    Expression get(string sym) {
        if(auto s = sym in syntax)   return defines[*s];
        if(auto s = sym in symbols)  return defines[*s];
        if(outter)                   return outter.get(sym);
        else throw new SemanticError("Undefined symbol '"~sym~"'.");
    }

    Expression* getRef(string sym) {
        if(auto s = sym in syntax)  return &defines[*s];
        if(auto s = sym in symbols) return &defines[*s];
        if(outter)                  return outter.getRef(sym);
        else throw new SemanticError("Undefined symbol '"~sym~"'.");
    }

    /***********************************************************************************
     * "Outside call", returns values from this scope to the outside world.
     *********************/

    override Expression call(ref Scope s, Expression[] args) {
        if(args.length != 1) throw new SemanticError("Scope call requires exactly one argument.");

        if(args[0].type & Type.Symbol) return new Reference(getRef(args[0].toString));
        throw new SemanticError("The object '"~args[0].toString~"' is not applicable.");
    }

    /***********************************************************************************
     * For iteration.
     *********************/

    override Expression[] range() {
        return defines;
    }

    override string toString() {
        return format(""~Syntax.StringDelim~"Scope at 0x%d."~Syntax.StringDelim, cast(void*) this);
    }

    override uint type() {
        return Type.Collection|Type.Scope|Type.Callable;
    }
}