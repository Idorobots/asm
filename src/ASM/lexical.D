/***********************************************************************************
 * Language grammar and definitions.
 *********************/

module ASM.lexical;

/***********************************************************************************
 * ASM Grammar:
 *
 *    Expression:
 *                  Atom
 *                  Collection
 *    Atom:
 *                  NumberLiteral
 *                  Symbol
 *    NumberLiteral:
 *                  [+-]?[0-9]+ //TODO
 *    Symbol:
 *                  [^Syntax]+
 *                  StringLiteral
 *    StringLiteral:
 *                  Syntax.StrDelim .* Syntax.StrDelim
 *    Collection:
 *                  Tuple
 *                  List
 *                  Set
 *    Tuple:
 *                  Syntax.LTuple Expresion+ Syntax.RTuple
 *    List:
 *                  Syntax.LList Expression* Syntax.RListn
 *    Set:
 *                  Syntax.LSet Expression* Syntax.RSet
 *********************/

/***********************************************************************************
 * Syntax Tokens:
 * TODO: Namechange? CoreSyntax? BasicSyntax? Plain Syntax nameclashes with ast.Syntax
 *********************/

enum Syntax : char {
    EndOfLine       = '\n',         ///Line end.
    EndOfFile       = '\0',         ///EOF
    CommentStart    = '#',          ///Both for line and sexp comments.
    LTuple          = '(',          ///Left tuple paren.
    RTuple          = ')',          ///Right tuple paren.
    LList           = '[',          ///Left list paren.
    RList           = ']',          ///Right list paren.
    LSet            = '{',          ///Left set paren.
    RSet            = '}',          ///Right set paren.
    StringDelim     = '\"',         ///String delimiters.
}

/***********************************************************************************
 * Reserved language keywords:
 *********************/

enum Keywords : string {
    //Special keywords:
    Fnord       = "fnord",      ///fnord - the only false arround.
    Self        = "self",       ///self - object self reference         //TODO: out?
    //Math:
    Mult        = "mult",       ///Multiplication.
    Add         = "add",        ///Addition.
    Sub         = "sub",        ///Substraction.
    Div         = "div",        ///Division.
    Mod         = "modulo",     ///Modulus.
    //Comparison:
    LessOrEqual = "leq",        ///Less or equal.
    IsEqual     = "equal?",     ///Equality check.
    //Collection manipulation:
    Car         = "first",      ///Reference/value of the first element in a mutable/immutable collection.
    Cdr         = "rest",       ///Collection refering the rest of the collection.
    Cons        = "join",       ///Joins two collections into one (the types must match).
    //Quoting and quasiquoting:
    Quasiquote  = "qquote",     ///Quasiquoting.
    Quote       = "quote",      ///Quoting.
    Embed       = "embed",      ///Embeding. //TODO Namechange to eval
    //Declarators:
    Var         = "var",        ///Binds a variable to an expression (or fnord).
    Function    = "function",   ///Creates an anonymous function.
    Macro       = "macro",      ///Creates a macro.
    Scope       = "scope",      ///Crates a lexical scope.
    //Setter:
    Set         = "set",        ///Sets writeable memory locations.
    //Flow control:
    If          = "if",         ///if condition then else
    //Predicates:
    IsFnord     = "fnord?",     ///Returns fnord if arg is not fnord.
    IsType      = "type?",      ///NOTE: Not a predicate; returns type touple of an object.
}