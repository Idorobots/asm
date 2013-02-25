/**********************************************************************************
 * Released under the MIT license <http://www.opensource.org/licenses/mit-license.php>
 * For licensing details see the included LICENSE file.
 *********************/

/***********************************************************************************
 * AST primitives for the language.
 * TODO: version(safe), recursion checking etc.
 *********************/

module dasm.ast;

import std.string : format;
import std.conv : to;

import utils.exception : MyException;
import utils.ctfe;

import dasm.lexical;
import dasm.kit;

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
        super(e.toString~format("\n%s(%s): Issued here: `%s'.", ex.file, ex.line, ex));
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
    Lazy        = 65536,
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
    Fexpr       = 131072,
    Scope       = 4096,
    //Set         = 8192 //???
    //Vector      = 32768,
    //Collection types:
    Set         = 8192,
    Tuple       = 16384,
    Vector      = 32768,
    //Scope       = 4096,
    //String      = 512,
}

/***********************************************************************************
 * Convinience typechecking functions.
 *********************/

bool isT(uint type)(Expression e) {
    return (e.type & type) != 0;
}

alias isT!(Type.Immutable)  isImmutable;
alias isT!(Type.Settable)   isSettable;
alias isT!(Type.Pure)       isPure;
alias isT!(Type.Builtin)    isBuiltin;
alias isT!(Type.Lazy)       isLazy;
alias isT!(Type.Atom)       isAtom;
alias isT!(Type.Callable)   isCallable;
alias isT!(Type.Collection) isCollection;
alias isT!(Type.Number)     isNumber;
alias isT!(Type.Symbol)     isSymbol;
alias isT!(Type.String)     isString;
alias isT!(Type.Function)   isFunction;
alias isT!(Type.Keyword)    isKeyword;
alias isT!(Type.Fexpr)      isFexpr;
alias isT!(Type.Scope)      isScope;
alias isT!(Type.Set)        isSet;
alias isT!(Type.Tuple)      isTuple;
alias isT!(Type.Vector)     isVector;

/***********************************************************************************
 * An abstract S-Expression primitive, the root of the hierarchy.
 *********************/

abstract class Expression {
    /***********************************************************************************
     * Metadata:
     *********************/

    private string fileName;
    private uint lineNumber;
    private string[] keys;

    ~this() {
        this.fileName = null;
        this.lineNumber = 0;
        this.keys = null;
    }

    /***********************************************************************************
     * Returns the evaluated S-expression. Defaults to self.
     *********************/

    Expression eval(ref Scope s, uint depth = 0) {
        return this;
    }

    uint type();                         ///Returns the type of the S-expression.
    override string toString();          ///Returns the string representation of an S-expression.
    override bool opEquals(Object o);    ///Equality check, du'uh.

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

    this(Expression* expr, uint line = __LINE__, string file = __FILE__) {
        if(isSettable(*expr)) {
            referee = (cast(Reference)*expr).referee;
        }
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

    override bool opEquals(Object o) {
        return referee.opEquals(o);
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

    this(T val, uint line = __LINE__, string file = __FILE__) {
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
             r.line = this.line;        //FIXME: Tis' ugly!
             r.file = this.file;
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

    override bool opEquals(Object o) {
        auto e = cast(Expression) o;
        if(!e) return false;

        auto a = cast(typeof(this)) e.deref;
        if(!a) return false;

        return this.val == a.val;
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
    private string toStringCache;

    this(Expression[] coll, uint line = __LINE__, string file = __FILE__) {
        this.line = line;
        this.file = file;
        static if(!(collectionType & Type.Tuple)) {     //FIXME: Quickfix
            this.coll = coll.dup;
        }
        else this.coll = coll;
    }

    ~this() {
        this.coll = null;
    }

    override Expression[] range() {
        return coll;
    }

    override Expression call(ref Scope s, Expression[] args) {
        static if(collectionType & Type.Callable) {
            return s.get(new Symbol(callKeyword)).call(s, [pass(this)]~args); //FIXME: Loose the 'new Symbol'
        }
        else {
            throw new ObjectNotAppError(this);
        }
    }

    override Expression eval(ref Scope s, uint depth = 0) {
        static if(collectionType & Type.Tuple) {
            try {
                if(!coll.length) return this;
                auto c = coll[0];
                auto ce = c.eval(s);
                auto crest = coll[1 .. $];
                auto cc = ce.call(s, crest);
                return cc;
                //return coll[0].eval(s).call(s, coll[1 .. $]); //TODO Signalize not evaluated OP
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
        bool cacheSet = toStringCache !is null;
        if(cacheSet) return toStringCache;

        if(!coll.length) return ""~LParen~RParen;

        auto output = ""~LParen;
        toStringCache = "...";

        foreach(el; coll) {
            output ~= el.toString~" ";
        }

        toStringCache = output[0 .. $-1]~RParen;

        if(!cacheSet) {
            auto o = toStringCache;
            toStringCache = null;
            return o;
        }
        return toStringCache;
    }

    override bool opEquals(Object o) {
        auto e = cast(Expression) o;
        if(!e) return false;

        auto c = cast(typeof(this)) e.deref;
        if(!c || (this.coll.length != c.coll.length)) return false;

        foreach(i, el; this.coll)
            if(el != c.coll[i]) return false;
        return true;
    }
}

/***********************************************************************************
 * Collection specialisation for the string datatype.
 *********************/

class Collection(T, uint stringType) : Expression {
    private T letters;

    this(T letters, uint line = __LINE__, string file = __FILE__) {
        this.line = line;
        this.file = file;
        this.letters = letters;
    }

    ~this() {
        this.letters = null;
    }

    this(Expression[] collection) {
        foreach(el; collection) {
            if(isString(el)) letters ~= el.toString[1 .. $-1]; //TODO: Generic!
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

    override bool opEquals(Object o) {
        auto e = cast(Expression) o;
        if(!e) return false;

        auto s = cast(typeof(this)) e.deref;
        if(!s) return false;

        return this.letters == s.letters;
    }
}

alias Collection!(string, Type.Immutable|Type.Symbol|Type.String)       String;         ///WYSIWYG symbol.
alias Collection!(Type.Tuple|Type.Immutable,
                  Syntax.LTuple, Syntax.RTuple, "NOPE")                 Tuple;          ///Immutable tuple. //TODO: Clean this up!
alias Collection!(Type.Callable|Type.Vector,
                  Syntax.LVector, Syntax.RVector,
                  "__vectoreval", "__vectorcall")                       Vector;         ///Mutable vector.
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
 * TODO: Rename to Builtin
 *********************/

class Callable(uint procType) : Expression {
    uint minArity, maxArity;
    string argMismatchString;
    proc_t procedure;

    this(proc_t procedure, uint minArity = 0, uint maxArity = 0, uint line = __LINE__, string file = __FILE__) {
        this.procedure = procedure;
        this.minArity = minArity;
        this.maxArity = max(minArity, maxArity);

        if(this.maxArity == INF_ARITY) {
            argMismatchString = format("Expected at least %s argument%s instead of %%s.",
                                       minArity, minArity != 1 ? "s" : "");
        }
        else if(this.maxArity != this.minArity) {
            argMismatchString = format("Expected %s to %s argument%s instead of %%s.",
                                       minArity, maxArity, maxArity > 1 ? "s" : "");
        }
        else argMismatchString = format("Expected exactly %s argument%s instead of %%s.",
                                        minArity, minArity != 1 ? "s" : "");

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

    override bool opEquals(Object o) {
        auto e = cast(Expression) o;
        if(!e) return false;

        auto c = cast(typeof(this)) e.deref;
        if(!c) return false;

        return this.procedure == c.procedure;
    }
}

class Closure : Expression {
    Scope definitionScope;
    Expression argList;
    Expression functionBody;

    this(Scope s, Expression argList, Expression functionBody,
        uint line = __LINE__, string file = __FILE__)
    {
        this.definitionScope = s;
        this.argList = argList;
        this.functionBody = functionBody;
        this.line = line;
        this.file = file;
    }

    ~this() {
        this.definitionScope = null;
        this.argList = null;
        this.functionBody = null;
    }

    override Expression call(ref Scope callScope, Expression[] callArgs) {
        auto closureScope = new Scope(definitionScope);
        closureScope.define(Keywords.Self, this);

        if(argList.isTuple || argList.isVector) {
            auto len = argList.range.length;

            if(callArgs.length != len) {
                auto arity = len;
                throw new SemanticError(format("Expected exactly %s argument%s instead of %s.",
                                               arity, arity != 1 ? "s" : "", callArgs.length),
                                        line, file);
            }


            foreach(i, arg; argList.range) {
                closureScope.define(arg.toString, callArgs[i].eval(callScope));
            }
        } else if (argList.isSymbol) {
            Expression[] args;
            args.length = callArgs.length;

            foreach(i, arg; callArgs) {
                args[i] = arg.eval(callScope);
            }
            closureScope.define(argList.toString, new Tuple(args));
        }

        return functionBody.eval(closureScope);
    }

    override uint type() {
        return Type.Callable|Type.Function;
    }

    override string toString() {
        //FIXME: Syntax independant.
        return "("~Keywords.Lambda~" "~argList.toString~" "~functionBody.toString~")";
    }

    override bool opEquals(Object o) {
        auto e = cast(Expression) o;
        if(!e) return false;

        auto c = cast(Closure) e.deref;
        if(!c) return false;

        if(this.argList != c.argList) return false;
        if(this.functionBody != c.functionBody) return false;

        return true;
    }
}

class Macro : Expression {
    Expression argList;
    Expression macroBody;

    this(Expression argList, Expression macroBody, uint line = __LINE__, string file = __FILE__) {
        this.argList = argList;
        this.macroBody = macroBody;
        this.line = line;
        this.file = file;
    }

    ~this() {
        this.argList = null;
        this.macroBody = null;
    }

    override Expression call(ref Scope callScope, Expression[] callArgs) {
        auto macroScope = new Scope(callScope);

        foreach(i, arg; argList.range) {
            if(contains(arg.keywords, "tuple")) {
                macroScope.define(arg.toString, new Tuple(callArgs[i .. $]));
                break;
            }
            else macroScope.define(arg.toString, callArgs[i]);
        }
        return macroBody.eval(macroScope).eval(callScope);
    }

    override uint type() {
        return Type.Callable|Type.Keyword;
    }

    override string toString() {
        return "("~Keywords.Macro~" #gensymed-name "~argList.toString~" "~macroBody.toString~")";
    }

    override bool opEquals(Object o) {
        auto e = cast(Expression) o;
        if(!e) return false;

        auto m = cast(Macro) e.deref;
        if(!m) return false;

        if(this.argList != m.argList) return false;
        if(this.macroBody != m.macroBody) return false;

        return true;
    }
}

class Fexpr : Expression {
    Scope fexprScope;
    Expression argList;
    Expression fexprBody;

    this(Scope s, Expression argList, Expression fexprBody, uint line = __LINE__, string file = __FILE__) {
        this.fexprScope = s;
        this.argList = argList;
        this.fexprBody = fexprBody;
        this.line = line;
        this.file = file;
    }

    override Expression call(ref Scope callScope, Expression[] callArgs) {
        auto staticScope = new Scope(fexprScope);

        staticScope.define(argList.range[$-1].toString, callScope); // Dynamic scope.

        foreach(i, arg; argList.range[0..$-1]) {
            if(contains(arg.keywords, "tuple")) {
                staticScope.define(arg.toString, new Tuple(callArgs[i .. $]));
                break;
            }
            else staticScope.define(arg.toString, callArgs[i]);
        }
        return fexprBody.eval(staticScope);//.eval(callScope); // TODO
    }

    override uint type() {
        return Type.Callable|Type.Fexpr;
    }

    override string toString() {
        return "("~Keywords.Fexpr~" "~argList.toString~" "~fexprBody.toString~")";
    }

    override bool opEquals(Object o) {
        auto e = cast(Expression) o;
        if(!e) return false;

        auto m = cast(Fexpr) e.deref;
        if(!m) return false;

        if(this.argList != m.argList) return false;
        if(this.fexprBody != m.fexprBody) return false;
        if(this.fexprScope != m.fexprScope) return false;

        return true;
    }
}

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
    size_t[string] symbols;       ///Other symbols.
    Scope outter;               ///Outter scope.

    this(Scope outter = null, uint line = __LINE__, string file = __FILE__) {
        this.line = line;
        this.file = file;
        this.outter = outter;

        this.define(Keywords.Self, this);
    }

    ~this() {
        defines = null;
        symbols = null;
    }

    /***********************************************************************************
     * Defines sym in a particular namespace.
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
        if(!isSymbol(symbol))
            throw new ObjectNotAppError(symbol);

        auto sym = symbol.toString;
        if(auto s = sym in symbols)  return defines[*s];
        if(outter)                   return outter.get(symbol);
        else throw new UndefinedSymError(symbol);
    }

    Reference getRef(Expression symbol) {
        if(!isSymbol(symbol))
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

    //TODO: Think this through.
    override bool opEquals(Object o) {
        auto e = cast(Expression) o;
        if(!e) return false;

        auto s = cast(typeof(this)) e.deref;
        if(!s) return false;

        if(this.outter != s.outter) return false;
        if(this.defines.length != s.defines.length) return false;
        if(this.symbols.keys.sort != s.symbols.keys.sort) return false;
        return true;
    }
}

class Lazy : Expression {
    Scope s;
    Expression e;
    bool forced = false;

    this(Expression e, Scope s, uint line = __LINE__, string file = __FILE__) {
        this.s = s;
        this.e = e;
        this.line = line;
        this.file = file;
    }

    ~this() {
        s = null;
        e = null;
        forced = true;
    }

    override Expression call(ref Scope, Expression[] args) {
        if(args.length) throw new SemanticError("Expected exactly 0 arguments.", this.line, this.file);

        if(!forced) {
            forced = true;
            e = e.eval(s);
        }

        return e;
    }

    override uint type() {
        return Type.Callable|Type.Lazy;
    }

    override string toString() {
        return "#lazy";
    }

    override bool opEquals(Object o) {
        auto e = cast(Expression) o;
        if(!e) return false;

        auto l = cast(Lazy) e.deref;
        if(!l) return false;

        if((l.s != this.s) || (l.e != this.e)) return false;
        return true;
    }
}