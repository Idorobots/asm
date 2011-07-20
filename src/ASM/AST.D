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
    //Collection types:
    Set         = 8192,
    Tuple       = 16384,
    List        = 32768,
    //Scope       = 4096,
    //String      = 512,
}

/***********************************************************************************
 * Procedure type for builtins.
 *********************/

alias Expression delegate(ref Scope s, Expression[] args) proc_t;   //TODO += depth

/***********************************************************************************
 * An interface for all callable objects.
 *********************/

interface Callable {
    Expression call(ref Scope s, Expression[] args);
}

/***********************************************************************************
 * An interface for all iterable collection types.
 * TODO: void opApply()
 *********************/

interface Collection {
    Expression[] range();
}

/***********************************************************************************
 * An abstract S-Expression primitive, the root of the hierarchy.
 * TODO doc string
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
}

/***********************************************************************************
 * Reference type used for setting.
 *********************/

class Reference : Expression {
      Expression* referee;

      this(Expression* expr) {
          referee = expr;
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
}

/***********************************************************************************
 * Represents numbers, symbols and string (aka WYSIWYG symbols).
 *********************/

class Atom(T, uint atomType) : Expression {
    T value;

    this(T value) {
        this.value = value;
    }

    /***********************************************************************************
     * Returns its definition in the scope s or its value.
     *********************/

    override Expression eval(ref Scope s, uint depth = 0) {
        static if(atomType & Type.Value) return this;
        else static if(atomType & Type.String) return this;
       // else return new Reference(s.getRef(value)); //TODO !!!
          else return s.get(value);
    }

    override uint type() {
        return Type.Atom|atomType;
    }

    override string toString() {
        return format("%s", value);
    }
}

/***********************************************************************************
 * Convinience aliases:
 *********************/

alias Atom!(real, Type.Value)                                  Value;      ///A number.
alias Atom!(string, Type.Symbol)                               Symbol;     ///Regular symbol.
alias Atom!(string, Type.Immutable|Type.Symbol|Type.String)    String;     ///WYSIWYG symbol.

/***********************************************************************************
 * Tuple class representing lisp-like forms - tuples.
 *********************/

class Tuple : Expression, Collection {
    private Expression[] tuple;

    this(Expression[] tuple) {
        this.tuple = tuple;
    }

    /***********************************************************************************
     * For iteration.
     *********************/

    Expression[] range() {
        return tuple;
    }

    /***********************************************************************************
     * Evaluates as a form.
     *********************/

    override Expression eval(ref Scope s, uint depth = 0) {
        foreach(i, el; tuple) {
            auto op = el.eval(s, depth + 1); //FIXME TODO No evaluation before the call.
            if(op.type & Type.Callable) return (cast(Callable)op).call(s, tuple[0 .. i]~tuple[i+1 .. $]);
        }
        throw new SemanticError("No applicable object in tuple "~this.toString~".");
    }

    override uint type() {
        return Type.Immutable|Type.Collection|Type.Tuple;
    }

    override string toString() {
        auto output = ""~Syntax.LTuple;
        foreach(expression; tuple) {
            if(expression !is this) output ~= expression.toString~" ";
            else output ~= Keywords.Self~" ";
        }
        return output[0 .. $-1]~Syntax.RTuple;
    }
}

alias Tuple List;
alias Tuple Set;

/***********************************************************************************
 * Callable object primitive.
 * TODO: Rename to Callable, and Callable to ICallable/Call?
 *********************/

class Procedure(uint procType) : Expression, Callable {
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

    Expression call(ref Scope s, Expression[] args) {
        return procedure(s, args);
    }

    override uint type() {
        return Type.Callable|procType;
    }

    override string toString() {
        return format(""~Syntax.StringDelim~stringof~"at 0x%d."~Syntax.StringDelim, cast(void*) this);
    }
}

/***********************************************************************************
 * Convinience templates.
 *********************/

alias Procedure!(Type.Function)                          Function;       ///Function.
alias Procedure!(Type.Pure|Type.Function)                Pure;           ///Pure function.
alias Procedure!(Type.Builtin|Type.Function)             Builtin;        ///Builtin function.
alias Procedure!(Type.Pure|Type.Builtin|Type.Function)   PureBuiltin;    ///Builtin function.
alias Procedure!(Type.Keyword)                           Keyword;        ///Syntax keyword.

/***********************************************************************************
 * A variable scope for soring symbols.
 *********************/

class Scope : Expression, Callable, Collection {
    Expression[string] syntax;  ///Syntax scope.
    Expression[string] pures;   ///Pure scope.
    Expression[string] symbols; ///Other symbols.

    Scope outter;               ///Outter scope.

    this(Scope outter = null) {
        this.outter = outter;
        this.defineSyntax(Keywords.Self, this);
    }

    /***********************************************************************************
     * Defines sym in a particular namespace.
     * Eg. use: scope.definePure(somePure);
     *********************/

    private void define(string where)(string sym, Expression expression) {
        mixin(where~"[sym] = expression;");
    }

    alias define!"syntax"   defineSyntax;
    alias define!"symbols"  defineSymbol;
    alias define!"pures"    definePure;

    /***********************************************************************************
     * Sets the value of an expression
     * TODO: OUT
     *********************/

    private void set(string where)(string sym, Expression expression) {
        mixin("if(sym in "~where~") "~where~"[sym] = expression;");
    }

    alias set!"syntax"      setSyntax;
    alias set!"symbols"     setSymbol;
    alias set!"pures"       setPure;

    /***********************************************************************************
     * Returns the definition of a symbol or throws "Undefined symbol" exception
     * if a symbol isn't found in this or parents scope.
     *********************/

    ref Expression get(string sym) {
        if(auto s = sym in syntax)   return *s;
        if(auto s = sym in pures)    return *s;
        if(auto s = sym in symbols)  return *s;
        if(outter)                   return outter.get(sym);
        else throw new SemanticError("Undefined symbol '"~sym~"'.");
    }

    Expression* getRef(string sym) {
        if(auto s = sym in syntax)  return s;
        if(auto s = sym in pures)   return s;
        if(auto s = sym in symbols) return s;
        if(outter)                  return outter.getRef(sym);
        else throw new SemanticError("Undefined symbol '"~sym~"'.");
    }

    /***********************************************************************************
     * "Outside call", returns values from this scope to the outside world.
     *********************/

    Expression call(ref Scope s, Expression[] args) {
        if(args.length != 1) throw new SemanticError("Scope call requires exactly one argument.");

        if(args[0].type & Type.Symbol) return get(args[0].toString);
        throw new SemanticError("The object '"~args[0].toString~"' is not applicable.");
    }

    /***********************************************************************************
     * For iteration.
     *********************/

    Expression[] range() {
        auto list = syntax.values;
        list ~= pures.values;
        list ~= symbols.values;
        return list;
    }

    override string toString() {
        return format(""~Syntax.StringDelim~"Scope at 0x%d."~Syntax.StringDelim, cast(void*) this);
    }

    override uint type() {
        return Type.Collection|Type.Scope|Type.Callable;
    }
}

//FIXME FIXME FIXME FIXME FIXME
// class Scope : Expression, Callable, Collection {
//   Expression[] defines;       ///All defined variables.
//   uint[string] syntax;        ///Syntax scope.
//   uint[string] pures;         ///Pure scope.
//   uint[string] symbols;       ///Other symbols.

//   Scope outter;               ///Outter scope.

//   this(Scope outter = null) {
//     this.outter = outter;
//     this.defineSyntax(Keyword.Self, this); //TODO
//   }

//   /***********************************************************************************
//    * Defines sym in a particular namespace.
//    * Eg. use: scope.definePure(somePure);
//    *********************/

//   private void define(string where)(string sym, Expression expression) {
//     defines ~= expression;
//     static if(where == "syntax")       syntax[sym]= defines.length;
//     else static if(where == "symbols") symbols[sym] = defines.length;
//     else static if(where == "pures")   pures[sym] = defines.length;
//   }

//   alias define!"syntax"   defineSyntax;
//   alias define!"symbols"  defineSymbol;
//   alias define!"pures"    definePure;

//   /***********************************************************************************
//    * Returns the definition of a symbol or throws "Undefined symbol" exception
//    * if a symbol isn't found in this or parents scope.
//    *********************/

//   ref Expression get(string sym) {
//     if(auto s = sym in syntax)   return defines[*s];
//     if(auto s = sym in pures)    return defines[*s];
//     if(auto s = sym in symbols)  return defines[*s];
//     if(outter)                   return outter.get(sym);
//     else throw new SemanticError("Undefined symbol '"~sym~"'.");
//   }

//   Expression* getRef(string sym) {
//     if(auto s = sym in syntax)  return &defines[*s];
//     if(auto s = sym in pures)   return &defines[*s];
//     if(auto s = sym in symbols) return &defines[*s];
//     if(outter)                  return outter.getRef(sym);
//     else throw new SemanticError("Undefined symbol '"~sym~"'.");
//   }

//   /***********************************************************************************
//    * "Outside call", returns values from this scope to the outside world.
//    *********************/

//   Expression call(ref Scope s, Expression[] args) {
//     if(args.length != 1) throw new SemanticError("Scope call requires exactly one argument.");

//     if(args[0].type & Type.Symbol) return get(args[0].toString);
//     throw new SemanticError("The object '"~args[0].toString~"' is not applicable.");
//   }

//   /***********************************************************************************
//    * For iteration.
//    *********************/

//   Expression[] range() {
//     return defines;
//   }

//   override string toString() {
//     return format(""~SyntaxToken.StringDelim~"Scope at 0x%d."~SyntaxToken.StringDelim, cast(void*) this);
//   }

//   override uint type() {
//     return Type.Collection|Type.Scope|Type.Callable;
//   }
// }
