/***********************************************************************************
 * Module containing a few usefull exception handling mechanisms.
 * Copyright (C) 2011 Kajetan Rzepecki
 *********************/

module utils.exception;

/***********************************************************************************
 * Simple exception connector not to include the stacktrace.
 *********************/

class MyException : Exception {
    string what;

    this(string what) {
        super(what);
        this.what = what;
    }

    override string toString() {
        return what;
    }
}
