/**********************************************************************************
 * Released under the MIT license <http://www.opensource.org/licenses/mit-license.php>
 * For licensing details see the included LICENSE file.
 *********************/

/***********************************************************************************
 * Module containing a few usefull exception handling mechanisms.
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
