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
 * Module containing some compile time evaluatable functions for numerous purposes,
 * and utilities aiding CTFE design.
 *********************/

module utils.ctfe;


/***********************************************************************************
 * Creates an expression tuple that can be aliased to something later on.
 *********************/

template ETuple(E ...) {
    alias E ETuple;
}


/***********************************************************************************
 * Capitalizes the first letter in a string.
 *********************/

string capitalize(in string input)
body {
    if(!input.length) return "";
    return ""~toUpper(input[0])~input[1 .. $];
}
unittest {
    assert(capitalize("bar") == "Bar");
    assert(capitalize("") == "");
    assert(capitalize("a") == "A");
}

/************************************************************************************
 * Returns toUpper(c) [sic].
 * FIXME: This won't work for w- and dchars.
 *********************/

auto toUpper(in char c) {
    if(c > 0x60 && c < 0x7B) return cast(typeof(c))(c-0x20);
    else return c;
}

/************************************************************************************
 * Returns true if character E e is found in R range.
 * TODO: is for reference parameters.
 *********************/

pure bool contains(R, E)(auto ref R range, auto ref E e)
body {
    foreach(el; range) if(el == e) return true;
    return false;
}
unittest {
    assert(contains([1, 2, 3, 4], 2));
    assert(contains([[1], [2], [3]], [2]));
    auto aa = ["foo":"bar", "bar":"boo"];
    assert(contains(aa.keys, "bar"));
}

/************************************************************************************
 * Translates all elements from 'from' to coresponding ones in 'to' in 'here'.
 *********************/

pure Array tr(alias from, alias to, Array)(Array here)
    if(is(typeof(from) == Array) && is(typeof(to) == Array))
{
    static assert(from.length == to.length, "The lengths of 'from' and 'to' must be equal.");
    Array output;
    foreach(c; here) {
        bool jollyRogger = true;        //A flag to tell if an element was translated.
        foreach(i, f; from) {
            if(c == f) {
                output ~= to[i];
                jollyRogger = false;
                break;
            }
        }
        if(jollyRogger) output ~= c;
    }
    return output;
}

/************************************************************************************
 * Splits "what" array on elements in "at".
 *********************/

pure Array[] split(alias at, Array)(Array what)
    if(is(typeof(at) == Array))
{
     Array[] tokens;
     Array token;

     foreach(c; what) {
        if(contains(at, c)) {
            if(token.length) tokens ~= token;
            token.length = 0;
        }
        else token ~= c;
     }
     if(token.length) tokens ~= token;
     return tokens;
}
unittest {
    assert(split!" "("test test lol") == ["test", "test", "lol"]);
    assert(split!" \t\n"("\ttest \ntest lol\n") == ["test", "test", "lol"]);
}

pure uint find(alias what, Array)(Array where)
    if(is(typeof(what) == Array))
{
    foreach(i, el; where) if(contains(what, el)) return i;
    return where.length;
}