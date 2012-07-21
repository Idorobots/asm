/**********************************************************************************
 * Released under the MIT license <http://www.opensource.org/licenses/mit-license.php>
 * For licensing details see the included LICENSE file.
 *********************/

/***********************************************************************************
 * Language grammar definitions.
 *********************/

module dasm.lexical;

/***********************************************************************************
 * Lexical tokens:
 *********************/

enum Lexical : char {
    Space           = ' ',          ///Space, separates tokens.
    EndOfLine       = '\n',         ///Line end.
    EndOfFile       = '\0',         ///EOF
    CommentStart    = '#',          ///Both for line and sexp comments.
    EscapeStart     = '\\',         ///Escape sequence start.
    Bang            = '!',          ///Bang used for shebang.
}

/***********************************************************************************
 * Syntax tokens:
 *********************/

enum Syntax : char {
    LTuple          = '(',          ///Left tuple paren.
    RTuple          = ')',          ///Right tuple paren.
    LVector         = '[',          ///Left vector paren.
    RVector         = ']',          ///Right vector paren.
    LSet            = '{',          ///Left set paren.
    RSet            = '}',          ///Right set paren.
    StringDelim     = '"',          ///String delimiters.
}

/***********************************************************************************
 * Reserved language keywords:
 *********************/

enum Keywords : string {
    //Special keywords:
    Fnord       = "fnord",      ///fnord - the only false arround.
    Self        = "self",       ///self - object self reference.
    Import      = "import",     ///Imports modules.
    //Math:
    Mult        = "mult",       ///Multiplication.
    Add         = "add",        ///Addition.
    Sub         = "sub",        ///Substraction.
    Div         = "div",        ///Division.
    Mod         = "mod",        ///Modulus.
    //Comparison:
    LessOrEqual = "leq?",       ///Less or equal.
    Gr8erOrEqual= "geq?",       ///Greater or equal.
    IsEqual     = "equal?",     ///Equality check.
    //Collection manipulation:
    Car         = "first",      ///Reference/value of the first element in a mutable/immutable collection.
    Cdr         = "rest",       ///Collection refering the rest of the collection.
    Cons        = "join",       ///Joins two objects to form a collection.
    Map         = "map",        ///Maps a function to a collection.
    Reduce      = "reduce",     ///Reduces a collection with a function.
    //Quoting and quasiquoting:
    Quasiquote  = "qquote",     ///Quasiquoting.
    Quote       = "quote",      ///Quoting.
    Embed       = "embed",      ///Embeding.
    //Declarators: //TODO: Add !
    Var         = "var",        ///Binds a variable to an expression (or fnord).
    Lambda      = "lambda",     ///Creates an anonymous closure.
    Function    = "function",   ///Creates a named function.
    Macro       = "macro",      ///Creates a named macro.
    Fexpr       = "vau",        ///Creates a named fexpr.
    Scope       = "scope",      ///Crates a lexical scope.
    //Generic setter and getter:
    Set         = "set!",       ///Sets writeable memory locations.
    Get         = "get",        ///Gets an object bound to a symbol.
    //Flow control:
    If          = "if",         ///if condition then else
    Do          = "do",         ///Evaluates arguments and returns value of the last one.
    //Not quite predicates:
    TypeOf      = "typeof",     ///Returns type touple of an object.
    KeywordsOf  = "keywordsof", ///Returns keyword tuple of an object.
    SizeOf      = "sizeof",     ///Returns the size of an object.
}