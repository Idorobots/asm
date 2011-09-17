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
}