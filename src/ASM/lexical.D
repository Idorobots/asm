/***********************************************************************************
 * Language grammar and definitions.
 *********************/

module ASM.lexical;

/***********************************************************************************
 * ASM Grammar:
 *
 *    Expression:
 *                  Atom
 *                  List
 *    Atom:
 *                  NumberLiteral
 *                  Symbol
 *    NumberLiteral:
 *                  [+-]?[0-9]+ //TODO
 *    Symbol:
 *                  [^\] //TODO
 *                  StringLiteral
 *    StringLiteral:
 *                  StringDelim .* StringDelim //TODO
 *    List:
 *                  LParen Expression* RParen
 *********************/

/***********************************************************************************
 * Syntax Tokens:
 * TODO: Namechange? CoreSyntax? BasicSyntax? Plain Syntax nameclashes with ast.Syntax
 *********************/

enum SyntaxToken : char {
    EndOfLine       = '\n',         ///Line end.
    EndOfFile       = '\0',         ///EOF
    CommentStart    = '#',          ///Both for line and sexp comments. Examples:
                                    ///## This is a line comment.\n
                                    ///#(This is a sexp comment.)
                                    ///#so_is_this
    LTuple          = '(',          ///Left tuple paren.
    RTuple          = ')',          ///Right tuple paren.
    LList           = '[',          ///Left list paren.
    RList           = ']',          ///Right list paren.
    LSet            = '{',          ///Left set paren.
    RSet            = '}',          ///Reft set paren.
    StringDelim     = '\"',         ///String delimiters.
}

/***********************************************************************************
 * Reserved language keywords:
 *********************/

enum Keyword : string { //TODO Sort this sheet. Write proper descriptions.
    //Special keywords:
    Fnord       = "fnord",      ///fnord - the only false arround.
    Self        = "self",       ///self - object self reference
    //Math:
    Mult        = "mult",       ///Multiplication.
    Add         = "add",        ///Addition.
    Sub         = "sub",        ///Substraction.
    Div         = "div",        ///Division.
    Mod         = "modulo",     ///Modulus.
    //Comparison:
    LessOrEqual = "leq",        ///Less or equal.
    IsEqual     = "equal?",     ///Equality check.
    //Boolean operations:
    Not         = "not",        ///Not.
    And         = "and",        ///And.
    Or          = "or",         ///Or.
    Xor         = "xor",
    Nand        = "nand",
    //etc
    //Collection manipulation:
    Car         = "first",      ///The first element of a collection.
    Cdr         = "rest",       ///The rest of the collection.
    Cons        = "cons",       ///Joins two collections into one. //TODO Namechange to join?
    //Quoting and quasiquoting:
    Quasiquote  = "quasiquote", ///Quasiquoting.
    Quote       = "quote",      ///Quoting.
    Embed       = "embed",      ///Embeding. //TODO Namechange to eval
    //Declarators:
    Var         = "var",        ///Variable declaration.
    Function    = "function",   ///Function declaration.
    Pure        = "pure",       ///Pure function declaration.
    Macro       = "macro",      ///Macro declaration.
    Scope       = "scope",      ///Scope declaration.
    //Setter:
    Set         = "set",        ///Unified setter.
    //Flow controll:
    If          = "if",         ///Basic if branch.
    Body        = "begin",      ///Block expression. //TODO Namechange to something else.
    //Predicates:
    IsFnord     = "fnord?",     ///Returns fnord if arg is not fnord.
    IsAtom      = "atom?",      ///Returns fnord if arg is not atomic.
    IsCollection= "collection?",///Returns fnord if arg is not a collection.
    IsNan       = "nan?",       ///Returns fnord if arg is not nan.
    IsEmpty     = "empty?",     ///Returns fnord if a collection is not empty.
}

// static struct Keyword { //FIXME This is a temp fix.due to a dmd bug.
//     //Special keywords:
//     enum Fnord       = "fnord";      ///fnord - the only false arround.
//     enum Self        = "self";
//     //Math:
//     enum Mult        = "mult";       ///Multiplication.
//     enum Add         = "add";        ///Addition.
//     enum Sub         = "sub";        ///Substraction.
//     enum Div         = "div";        ///Division.
//     enum Mod         = "modulo";     ///Modulus.
//     //Comparison:
//     enum LessOrEqual = "leq";        ///Less or equal.
//     enum IsEqual     = "equal?";     ///Equality check.
//     //Boolean operations:
//     enum Not         = "not";        ///Not.
//     enum And         = "AND";        ///And.
//     enum Or          = "or";         ///Or.
//     enum Xor         = "xor";
//     enum Nand        = "nand";
//     //etc
//     //Collection manipulation:
//     enum Car         = "first";      ///The first element of a collection.
//     enum Cdr         = "rest";       ///The rest of the collection.
//     enum Cons        = "cons";       ///Joins two collections into one. //TODO Namechange to join?
//     //Quoting and quasiquoting:
//     enum Quasiquote  = "quasiquote"; ///Quasiquoting.
//     enum Quote       = "quote";      ///Quoting.
//     enum Embed       = "embed";      ///Embeding. //TODO Namechange to eval
//     //Declarators:
//     enum Var         = "var";        ///Variable declaration.
//     enum Function    = "function";   ///Function declaration.
//     enum Pure        = "pure";       ///Pure function declaration.
//     enum Macro       = "macro";      ///Macro declaration.
//     enum Scope       = "scope";      ///Scope declaration.
//     //Setter:
//     enum Set         = "set";        ///Unified setter.
//     //Flow controll:
//     enum If          = "if";         ///Basic if branch.
//     enum Body        = "begin";      ///Block expression. //TODO Namechange to something else.
//     //Predicates:
//     enum IsFnord     = "fnord?";     ///Returns fnord if arg is not fnord.
//     enum IsAtom      = "atom?";      ///Returns fnord if arg is not atomic.
//     enum IsCollection= "collection?";///Returns fnord if arg is not a collection.
//     enum IsNan       = "nan?";
//     enum IsEmpty     = "empty?";
// }