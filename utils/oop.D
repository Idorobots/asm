/***********************************************************************************
 * A module aiding object oriented design.
 * Copyright (C) 2011 Kajetan Rzepecki
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
