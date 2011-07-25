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
 * A module aiding object oriented design.
 *********************/

module utils.oop;

/***********************************************************************************
 * Adds properties of a given name that use getter/setter rutines to a scope.
 *********************/

import std.traits : ReturnType;
mixin template Property(string name, alias getter, alias setter,
                        string gtype = ReturnType!(getter).stringof,
                        string stype = ReturnType!(setter).stringof)
{
    mixin(  "@property {"
                ~gtype~" "~name~"() {"
                    "return getter();"
                "}"
                ~stype~" "~name~"("~gtype~" newVal) {"
                    "return setter(newVal);"
                "}"
            "}");
}

/***********************************************************************************
 * Adds default properties of a given name for a variable to a scope.
 *********************/

mixin template Property(string name, alias var, string type = typeof(var).stringof) {
    mixin(  "@property {"
                ~type~" "~name~"() {"
                    "return var;"
                "}"
                ~type~" "~name~"("~type~" newVal) {"
                    "return var = newVal;"
                "}"
            "}");
}
