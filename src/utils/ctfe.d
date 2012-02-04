/**********************************************************************************
 * Released under the MIT license <http://www.opensource.org/licenses/mit-license.php>
 * For licensing details see the included LICENSE file.
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

auto capitalize(in string input) {
    if(!input.length) return "";
    return ""~toUpper(input[0])~input[1 .. $];
}
unittest {
    static assert(capitalize("bar") == "Bar");
    static assert(capitalize("") == "");
    static assert(capitalize("a") == "A");
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
    if(is(E[] : R))
{
    foreach(el; range) if(el == e) return true;
    return false;
}
unittest {
    static assert(contains([1, 2, 3, 4], 2));
    static assert(contains([[1], [2], [3]], [2]));
    auto aa = ["foo":"bar", "bar":"boo"];
    assert(contains(aa.keys, "bar"));
}

/************************************************************************************
 * Translates all elements from `from' to coresponding ones in `to' in `here'.
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
 * Splits `what' on elements in `at'.
 *********************/

pure Array[] split(alias at = " \r\n\t\v", Array)(Array what)
    if(is(typeof(at) == Array))
{
     Array[] tokens;
     Array token;

     foreach(c; what) {
        if (contains(at, c)) {
            if(token.length) tokens ~= token;
            token = null;
        }
        else token ~= c;
     }
     if(token.length) tokens ~= token;
     return tokens;
}
unittest {
    static assert(split!" "("test test lol") == ["test", "test", "lol"]);
    static assert(split!" \t\n"("\ttest \ntest lol\n") == ["test", "test", "lol"]);
    static assert(split("\ttest \ntest lol\n") == ["test", "test", "lol"]);
}

/************************************************************************************
 * Joins `what' with `_with' string.
 *********************/

pure Array join(alias _with = "", Array)(Array[] what)
    if(is(typeof(_with) == Array))
{
    if(!what.length) return Array.init;

    Array joined = what[0];
    foreach(e; what[1..$]) joined ~= _with~e;
    return joined;
}
unittest {
    static assert(join!" "(cast(string[])[]) == "");
    static assert(join!" "(["wat"]) == "wat");
    static assert(join!"|"(["a", "b", "c"]) == "a|b|c");
    static assert(join!""(["a", "b"]) == "ab");
    static assert(join(["a", "b"]) == "ab");
}

/************************************************************************************
 * Finds first occurence of an element from `what' array in `where'.
 * Returns the index of the found element, or length of `where'.
 *********************/

pure size_t find(alias what, Array)(Array where)
    if(is(typeof(what) == Array))
{
    foreach(i, el; where) if(contains(what, el)) return i;
    return where.length;
}
unittest {
    static assert(find!"abc"("bdc") == 0);
    static assert(find!"foo"("bar") == 3);
    static assert(find!([1, 2, 3])([4, 5, 3]) == 2);
}

/************************************************************************************
 * Returns max of `a' and `b'.
 *********************/

pure T max(T)(T a, T b) {
    return a > b ? a : b;
}
unittest {
    static assert(max(1, 2) == 2);
    static assert(max(2.34, 2.33) == 2.34);
    static assert(max(-1, 1) == 1);
    static assert(max(-2, 1) == 1);
}