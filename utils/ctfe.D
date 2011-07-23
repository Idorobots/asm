/**********************************************************************************
 * Copyright (c) 2011 Kajetan Rzepecki <kajetan.rzepecki@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software
 * is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies
 * or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
 * FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *********************/

/***********************************************************************************
 * Module containing some compile time evaluatable functions for numerous purposes.
 *********************/

module utils.ctfe;

/***********************************************************************************
 * Capitalizes the first letter in a string.
 *********************/

string capitalize(in string input) {
    if(!input.length) return input;
    return ""~toUpper(input[0])~input[1 .. $];
}

/************************************************************************************
 * Returns toUpper(c) [sic].
 * FIXME: This won't work for w- and dchars.
 *********************/

auto toUpper(Char)(in Char c) {
    if(c > 0x60 && c < 0x7B) return cast(Char)(c-0x20);
    else return c;
}

/************************************************************************************
 * Returns true if character a is found in array array.
 *********************/

bool contains(T1, T2)(T1[] array, T2 a) {
    foreach(el; array) if(a == el) return true;
    return false;
}


/************************************************************************************
 * Translates all characters from 'from' to coresponding ones in 'to' in 'here'..
 *********************/

string tr(string from, string to)(string here) {
    static assert(from.length == to.length, "The lengths of 'from' and 'to' must be equal.");
    string output;
    foreach(c; here) {
       foreach(i, f; from) {
           if(c == f) {
               output ~= to[i];
               goto ICantBelieveImUsingAGoto;
           }
       }
       output ~= c;
       ICantBelieveImUsingAGoto: ;
    }
    return output;
}
