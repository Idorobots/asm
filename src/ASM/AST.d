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
    public this(string what, uint line, string file) {
        super(format("%s(%s): ", file, line, what));
    }
}

class ObjectNotAppError : SemanticError {
    public this(Expression obj) {
        super("The object `"~obj.toString~"' is not applicable.", obj.line, obj.file);
    }
}

class UndefinedSymError : SemanticError {
    public this(Expression sym) {
        super("Undefined symbol `"~sym.toString~"'.", sym.line, sym.file);
    }
}

//TODO Throw this away?
class CalledHereError : MyException {
    public this(MyException e, Expression ex) {
        super(e.toString~format("\n%s(%s): Issued here: %s.", ex.file, ex.line, ex));
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

    private string fileName;
    private uint lineNumber;
    private string[] keys;

    ~this() {
        this.keys = null;
    }

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
        throw new ObjectNotAppError(this);
    }

    /***********************************************************************************
     * Returns the evaluated call to a callable object. Only for callables..
     *********************/

    Expression call(ref Scope s, Expression[] args) {
        throw new ObjectNotAppError(this);
    }

    //TODO: T value(T)();
    real value() {
        throw new ObjectNotAppError(this);
    }

    /***********************************************************************************
     * Returns an expression of the same type as this.
     *********************/

    Expression factory(Expression[] vals) {
        throw new ObjectNotAppError(this);
    }

    /***********************************************************************************
     * Returns this. For cleaner reference treating in the interpreter.
     *********************/

    Expression deref() {
        return this;
    }

    /***********************************************************************************
     * Metadata:
     *********************/

    @property {
        string[] keywords() {
            return keys;
        }

        //FIXME: Ugly as fuck.
        string[] keywords(string key) {
            return keys ~= key;
        }

        string file() {
            return fileName;
        }

        string file(string name) {
            return fileName = name;
        }

        uint line() {
            return lineNumber;
        }

        uint line(int number) {
            return lineNumber = number;
        }
    }
}

/***********************************************************************************
 * Reference type used for setting.
 *********************/

class Reference : Expression {
    Expression* referee;

    this(Expression* expr, uint line = 0, string file = "fnord") {
        if(expr.type & Type.Settable) referee = (cast(Reference)*expr).referee;
        else referee = expr;
        this.line = line;
        this.file = file;
    }

    ~this() {
        this.referee = null;
    }

    /***********************************************************************************
     * Sets the reference to an object.
     *********************/

    void set(Expression expr) {
        *referee = expr.deref;
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

    /***********************************************************************************
     * Returns referee.
     *********************/

    override Expression deref() {
        return *referee;
    }

    @property {
        override string[] keywords() {
            return referee.keywords;
        }

        override string[] keywords(string key) {
            return referee.keywords = key;
        }

        override string file() {
            return referee.file;
        }

        override string file(string name) {
            return referee.file = name;
        }

        override uint line() {
            return referee.line;
        }

        override uint line(int number) {
            return referee.line = number;
        }
    }
}

/***********************************************************************************
 * Represents numbers, symbols and string (aka WYSIWYG symbols).
 * TODO: Take the string away from here. Make it a proper immutable collection
 * and make it callable with things like append, split, etc?
 *********************/

class Atom(T, uint atomType) : Expression {
    private T val;

    this(T val, uint line = 0, string file = "fnord") {
        this.line = line;
        this.file = file;
        this.val = val;
    }

    /***********************************************************************************
     * Returns its definition in the scope s or its value.
     *********************/

    override Expression eval(ref Scope s, uint depth = 0) {
         static if(atomType & Type.Symbol) {
             auto r = s.getRef(this);
             r.line = line;     //FIXME: Ugly and sooo many levels of wrong I actually did it.
             r.file = file;
             return r;
         }
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
        else throw new ObjectNotAppError(this);
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

    this(Expression[] coll, uint line = 0, string file = "fnord") {
        this.line = line;
        this.file = file;
        this.coll = coll;
    }

    ~this() {
        this.coll = null;
    }

    override Expression[] range() {
        return coll;
    }

    override Expression call(ref Scope s, Expression[] args) {
        return s.get(new Symbol(callKeyword)).call(s, [pass(this)]~args); //FIXME: Loose the 'new Symbol'
    }

    override Expression eval(ref Scope s, uint depth = 0) {
        static if(collectionType & Type.Tuple) {
            try {
                if(!coll.length) return this;
                auto op = coll[0].eval(s);
                return op.call(s, coll[1 .. $]);
            }
            catch(SemanticError e) {
                throw new CalledHereError(e, this);
            }
        }
        else return s.get(new Symbol(evalKeyword)).call(s, coll); //FIXME: Loose the 'new Symbol'
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

    this(T letters, uint line = 0, string file = "fnord") {
        this.line = line;
        this.file = file;
        this.letters = letters;
    }

    ~this() {
        this.letters = null;
    }

    this(Expression[] collection) {
        foreach(el; collection) {
            if(el.type & Type.String) letters ~= el.toString[1 .. $-1]; //TODO: Generic!
            else letters ~= el.toString;
        }
    }

    override Expression[] range() {
        Expression[] collection;
        foreach(dchar c; letters)
            collection ~= new typeof(this)(""~to!string(c));
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
                  "NOPE")                                               Tuple;          ///Immutable tuple. //TODO: Clean this up!
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

enum INF_ARITY = uint.max;

/***********************************************************************************
 * Callable object primitive.
 *********************/

class Callable(uint procType) : Expression {
    uint minArity, maxArity;
    string argMismatchString;
    proc_t procedure;

    this(proc_t procedure, uint minArity = 0, uint maxArity = 0, uint line = 0, string file = "fnord") {
        this.procedure = procedure;
        this.minArity = minArity;
        this.maxArity = max(minArity, maxArity);

        if(this.maxArity == INF_ARITY) {
            argMismatchString = format("Expected at least %s argument%s instead of %%s.",
                                       minArity, minArity > 1 ? "s" : "");
        }
        else if(this.maxArity != this.minArity) {
            argMismatchString = format("Expected %s to %s argument%s instead of %%s.",
                                       minArity, maxArity, maxArity > 1 ? "s" : "");
        }
        else argMismatchString = format("Expected exactly %s argument%s instead of %%s.",
                                        minArity, minArity > 1 ? "s" : "");

        this.line = line;
        this.file = file;
    }

    ~this() {
        this.procedure = null;
    }

    override Expression call(ref Scope s, Expression[] args) {
        if(args.length < minArity || args.length > maxArity)
            throw new SemanticError(format(argMismatchString, args.length), line, file);
        //TODO: Preconditions.
        auto result = procedure(s, args);
        //TODO: Postconditions.
        result.line = line;
        result.file = file;
        return result;
    }

    override uint type() {
        return Type.Callable|procType;
    }

    override string toString() {
        //TODO: Make this actually useful.
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

    this(Scope outter = null, uint line = 0, string file = "fnord") {
        this.line = line;
        this.file = file;
        this.outter = outter;
    }

    ~this() {
        defines = null;
        symbols = null;
    }

    /***********************************************************************************
     * Defines sym in a particular namespace.
     * Eg. use: scope.definePure(somePure);
     *********************/

    void define(string sym, Expression expression) {
        if(auto s = sym in symbols) {
            defines[*s] = expression;   //FIXME: throw ShadowingDeclarationError?
        }
        else {
            symbols[sym] = defines.length;
            defines ~= expression;
        }
    }

    /***********************************************************************************
     * Returns the definition of a symbol or throws "Undefined symbol" exception
     * if a symbol isn't found in this or parents scope.
     *********************/

    Expression get(Expression symbol) {
        if(!(symbol.type & Type.Symbol))
            throw new ObjectNotAppError(symbol);

        auto sym = symbol.toString;
        if(auto s = sym in symbols)  return defines[*s];
        if(outter)                   return outter.get(symbol);
        else throw new UndefinedSymError(symbol);
    }

    Reference getRef(Expression symbol) {
        if(!(symbol.type & Type.Symbol))
            throw new ObjectNotAppError(symbol);

        auto sym = symbol.toString;
        if(auto s = sym in symbols) return new Reference(&defines[*s], line, file);
        if(outter)                  return outter.getRef(symbol);
        else throw new UndefinedSymError(symbol);
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
        return s.get(new Symbol("__scopecall")).call(this, args);  //FIXME: Loose the 'new Symbol( * )'
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