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
 *                  Number
 *                  Symbol
 *    Number:
 *                  Real
.*                  Integer
 *                  Complex
 *    Integer:
 *                  //TODO
 *    Real:
 *                  //TODO
 *    Complex:
 *                  //TODO
 *    Symbol:
 *                  [^Syntax]+
 *                  String
 *    String:
 *                  //TODO
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
 *********************/

enum Syntax : char {
    Space           = ' ',          ///Space, separates tokens.
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
    Embed       = "embed",      ///Embeding. //TODO Namechange to eval?
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