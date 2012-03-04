/**********************************************************************************
 * Released under the MIT license <http://www.opensource.org/licenses/mit-license.php>
 * For licensing details see the included LICENSE file.
 *********************/

import std.stdio;
import std.getopt : getopt;
import std.conv : to;

import utils.ctfe : split;

import dasm.vm;

void main(string[] args) {

    /***********************************************************************************
     * GetOpt crap:
     *********************/

    auto vendor = VM.vendor;
    auto ver = to!string(VM.majorRevision) ~ "." ~ to!string(VM.minorRevision);
    auto header = vendor ~ " v" ~ ver ~ " - ASM v" ~ to!string(VM.ASMVersion) ~ " interpreter";
    auto author = "Kajetan Rzepecki <kajetan.rzepecki+asm@gmail.com>";
    auto copyright = "Copyright (c) 2011-2012 " ~ author;
    auto license = "See LICENSE for details.";
    auto tab = "  ";
    auto usage = tab ~ args[0] ~ " file.asm ... { --switch }\n";
    auto help = tab ~ "file.asm \t ASM source file\n" ~
                tab ~ "--vendor \t display vendor info\n" ~
                tab ~ "--version \t display the version number\n" ~
                tab ~ "--license \t display licensing info\n" ~
                tab ~ "--help \t display this message\n";

    bool dispLicense = false;
    bool dispCopyright = false;
    bool dispVendor = false;
    bool dispVersion = false;
    bool dispHelp = false;

    getopt(
        args,
        "help", &dispHelp,
        "license", &dispLicense,
        "copyright", &dispCopyright,
        "vendor", &dispVendor,
        "version", &dispVersion
    );

    if(dispHelp) {
        writeln(header);
        writeln(copyright);
        writeln(license);
        writeln("Usage:");
        writeln(usage);
        writeln(help);
        return;
    }
    if(dispLicense) {
        writeln(license);
        return;
    }
    if(dispCopyright) {
        writeln(copyright);
        return;
    }
    if(dispVendor) {
        writeln(vendor);
        return;
    }
    if(dispVersion) {
        writeln(ver);
        return;
    }

    auto ASM = new VM();
    ASM.doString("(import 'imports.core)");

    /***********************************************************************************
     * File interpretation:
     *********************/

    if(args.length != 1) {
        foreach(arg; args[1 .. $]) {
            try ASM.doFile(arg);
            catch(Exception e) {
                auto lines = split!"\n"(e.toString);
                foreach(line; lines) writeln("\t", line);
            }
        }
        return;
    }

    /***********************************************************************************
     * REPL:
     *********************/

    writeln(header);
    writeln(copyright);
    writeln(license);
    writeln();

    while(true) {
        write("> ");
        auto input = stdin.readln;
        if(input == "q\n") break;

        try {
            writeln("\t", ASM.doString(input.idup));
        }
        catch(Exception e) {
            auto lines = split!"\n"(e.toString);
            foreach(line; lines) writeln("\t", line);
        }
    }
    writeln("Wait! You forgot your parentheses!");
}
