/**********************************************************************************
 * Released under the MIT license <http://www.opensource.org/licenses/mit-license.php>
 * For licensing details see the included LICENSE file.
 *********************/

/***********************************************************************************
 * ASMKit implementations and global values..
 *********************/

module dasm.kit;

import dasm.lexical;
import dasm.ast;

/***********************************************************************************
 * UTILITIES:
 *********************/

/***********************************************************************************
 * Quickfix for too many evaluations in several places:
 * FIXME: Check why it's needed.
 *********************/

Expression pass(Expression el) {
    Expression quote[];
    return new Tuple([cast(Expression)new Symbol(Keywords.Quote), el]);
}
