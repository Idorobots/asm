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
import ASM.kit;

/***********************************************************************************
 * Exception thrown on a semantic error.
 *********************/

class SemanticError : MyException {
    public this(string what) {
        super("Error: "~what);
    }
}

class ObjectNotAppError : SemanticError {
    public this(string obj) {
        super("The object '"~obj~"' is not applicable.");
    }
}

class UndefinedSymError : SemanticError {
    public this(string sym) {
        super("Undefined symbol '"~sym~"'.");
    }
}

/***********************************************************************************
 * Types of the AST primitives.
 *********************/

enum Type {
    //Type attributes:
    Immutable   = 1,
    Settable    = 2,
    Pure        = 4,
    Builtin     = 8,
    //Root types:
    Atom        = 16,
    Callable    = 32,
    Collection  = 64,
    //Atomic types:
    Number      = 128,
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
     * Meta- and semantic data:
     *********************/

    //string filename;
    //uint line;
    private string[] keys;

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
        throw new ObjectNotAppError(this.toString);
    }

    /***********************************************************************************
     * Returns the evaluated call to a callable object. Only for callables..
     *********************/

    Expression call(ref Scope s, Expression[] args) {
        throw new ObjectNotAppError(this.toString);
    }

    //TODO: T value(T)();
    real value() {
        throw new ObjectNotAppError(this.toString);
    }

    /***********************************************************************************
     * Returns an expression of the same type as this.
     *********************/

     Expression factory(Expression[] vals) {
        throw new ObjectNotAppError(this.toString);
     }

    /***********************************************************************************
     * Returns keywords of this expression.
     *********************/

     @property string[] keywords() {
         return keys;
     }

     @property void keywords(string key) {
         keys ~= key;
     }
}

/***********************************************************************************
 * Reference type used for setting.
 *********************/

class Reference : Expression {
    Expression* referee;

    this(Expression* expr) {
        if(expr.type & Type.Settable) referee = (cast(Reference)*expr).referee;
        else referee = expr;
    }

    /***********************************************************************************
     * Sets the reference to an object.
     *********************/

    void set(Expression expr) {
        if(expr.type & Type.Settable) *referee = (cast(Reference)expr).get;
        else *referee = expr;
    }

    Expression get() {
        return *referee;
    }

    override Expression eval(ref Scope s, uint depth = 0) {
        return referee.eval(s, depth + 1);
    }

    override uint type() {
        return Type.Settable|referee.type;
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

     override Expression factory(Expression[] vals) {
        return referee.factory(vals);
     }

     override string[] keywords() {
         return referee.keywords;
     }

     override void keywords(string key) {
         referee.keywords(key);
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
         static if(atomType & Type.Symbol) return new Reference(s.getRef(val));
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
        static if(atomType & Type.Number) return this.val;
        else throw new ObjectNotAppError(this.toString);
    }
}

alias Atom!(real, Type.Number)          Number;     ///A number.
alias Atom!(string, Type.Symbol)        Symbol;     ///Regular symbol.

/***********************************************************************************
 * Collection class representing lisp-like and python-like sequences.
 * TODO: Take the tuple away from here.
 *********************/

class Collection(uint collectionType,
                 char LParen, char RParen,
                 string evalKeyword = Keywords.Fnord,
                 string callKeyword = Keywords.Fnord) : Expression {

    private Expression[] coll;

    this(Expression[] coll) {
        this.coll = coll;
    }

    override Expression[] range() {
        return coll;
    }

    override Expression call(ref Scope s, Expression[] args) {
        return s.get(callKeyword).call(s, [pass(this)]~args);
    }

    override Expression eval(ref Scope s, uint depth = 0) {
        static if(collectionType & Type.Tuple) return s.get("call").call(s, coll);              //FIXME: Make in unredefineable.
        else return s.get(evalKeyword).call(s, coll);
    }

    override Expression factory(Expression[] vals) {
        return new typeof(this)(vals);
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

/***********************************************************************************
 * Collection specialisation for the string datatype.
 *********************/

class Collection(T, uint stringType) : Expression {
    private T letters;

    this(T letters) {
        this.letters = letters;
    }

    this(Expression[] collection) {
        foreach(el; collection) {
            if(el.type & Type.String) letters ~= el.toString[1 .. $-1]; //TODO: Generic!
            else letters ~= el.toString;
        }
    }

    override Expression[] range() {
        Expression[] collection;
        foreach(c; letters)
            collection ~= new typeof(this)(""~c);
        return collection;
    }

    override Expression factory(Expression[] vals) {
        return new typeof(this)(vals);
    }

    override uint type() {
        return Type.Collection|stringType;
    }

    override string toString() {
        return format("\"%s\"", letters);
    }
}

alias Collection!(string, Type.Immutable|Type.Symbol|Type.String)       String;         ///WYSIWYG symbol.
alias Collection!(Type.Tuple|Type.Immutable,
                  Syntax.LTuple, Syntax.RTuple,
                  "__tupleeval")                                        Tuple;          ///Immutable tuple. //TODO: Clean this up!
alias Collection!(Type.Callable|Type.List,
                  Syntax.LList, Syntax.RList,
                  "__listeval", "__listcall")                           List;           ///Mutable list.
alias Collection!(Type.Callable|Type.Set,
                  Syntax.LSet, Syntax.RSet,
                  "__seteval", "__setcall")                             Set;            ///Mutable set.

/***********************************************************************************
 * Procedure type for the Callable datatype.
 *********************/

alias Expression delegate(ref Scope s, Expression[] args) proc_t;   //TODO += depth

/***********************************************************************************
 * Callable object primitive.
 *********************/

class Callable(uint procType) : Expression {
    proc_t procedure;

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
        //Make this actually useful.
        string getName() {
            string output;
            if(procType & Type.Builtin)  output ~= "compiled ";
            if(procType & Type.Pure)     output ~= "pure ";
            if(procType & Type.Function) output ~= "function ";
            if(procType & Type.Keyword)  output ~= "syntax keyword ";
            return utils.ctfe.capitalize(output);
        }
        enum stringof = getName();
        return format(""~Syntax.StringDelim~stringof~"at 0x%d."~Syntax.StringDelim, cast(void*) this);
    }
}

alias Callable!(Type.Function)                          Function;       ///Function.
alias Callable!(Type.Pure|Type.Function)                Pure;           ///Pure function.
alias Callable!(Type.Builtin|Type.Function)             Builtin;        ///Builtin function.
alias Callable!(Type.Pure|Type.Builtin|Type.Function)   PureBuiltin;    ///Builtin function.
alias Callable!(Type.Keyword)                           Keyword;        ///Syntax keyword.
alias Callable!(Type.Builtin|Type.Keyword)              BuiltinKeyword; ///Syntax keyword.

/***********************************************************************************
 * A variable scope for soring symbols.
 *********************/

class Scope : Expression {
    Expression[] defines;       ///Object definitions.
    uint[string] symbols;       ///Other symbols.
    Scope outter;               ///Outter scope.

    this(Scope outter = null) {
        this.outter = outter;
    }

    /***********************************************************************************
     * Defines sym in a particular namespace.
     * Eg. use: scope.definePure(somePure);
     *********************/

    void define(string sym, Expression expression) {
        if(auto s = sym in symbols) defines[*s] = expression;   //FIXME: throw ShadowingDeclarationError?
        else {
            symbols[sym] = defines.length;
            defines ~= expression;
        }
    }

    /***********************************************************************************
     * Returns the definition of a symbol or throws "Undefined symbol" exception
     * if a symbol isn't found in this or parents scope.
     *********************/

    Expression get(string sym) {
        if(auto s = sym in symbols)  return defines[*s];
        if(outter)                   return outter.get(sym);
        else throw new UndefinedSymError(sym);
    }

    Expression* getRef(string sym) {
        if(auto s = sym in symbols) return &defines[*s];
        if(outter)                  return outter.getRef(sym);
        else throw new UndefinedSymError(sym);
    }

    /***********************************************************************************
     * Returns true if a symbol is defined in this scope.
     *********************/

    bool isDefined(string sym) {
        return (sym in symbols) != null;
    }

    /***********************************************************************************
     * Scope call - defaults to this.get(sym);
     *********************/

    override Expression call(ref Scope s, Expression[] args) {
        return s.get("__scopecall").call(this, args);  //TODO: Add a keyword?
    }

    override Expression[] range() {
        return defines;
    }

    override string toString() {
        string output = format(""~Syntax.LTuple~Lexical.CommentStart~"scope@0x%d ", cast(void*) this);
        foreach(sym; symbols.keys.sort) output ~= sym~Lexical.Space;
        return output[0 .. $-1]~Syntax.RTuple;
    }

    override uint type() {
        return Type.Collection|Type.Scope|Type.Callable;
    }
}