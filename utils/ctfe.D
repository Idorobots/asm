/***********************************************************************************
 * Module containing some compile time evaluatable functions for numerous purposes.
 * Copyright (C) 2011 Kajetan Rzepecki
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
