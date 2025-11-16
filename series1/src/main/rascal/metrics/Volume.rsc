module metrics::Volume

import lang::java::m3::AST;
import IO;
import List;
import String;

// ------------------------
// Volume / LOC metrics
int countPhysicalLocFromAsts(list[Declaration] asts) {
    int total = 0;
    for (decl <- asts) {
        total += size(readFileLines(decl.src));
    }
    return total;
}

bool isNoiseLine(str line) {
    str trimmedLine = trim(line);
    // if trimmedLine is equal to empty or commented line, ignore line
    if (trimmedLine == "") return true;
    if (startsWith(trimmedLine, "//")) return true;
    else return false;
}

int countFilteredLocFromAsts(list[Declaration] asts) {
    int total = 0;
    for (decl <- asts) {
        list[str] lines = readFileLines(decl.src);
        for (line <- lines) {
            if (!isNoiseLine(line)) total += 1;
        }
    }
    return total;
}

void printVolumeReportFromAsts(list[Declaration] asts, str label) {
    int fileCount = size(asts);
    int ploc = countPhysicalLocFromAsts(asts);
    int floc = countFilteredLocFromAsts(asts);

    println("=== Volume Report ===");
    println("Project: <label>");
    println("Java files: <fileCount>");
    println("Physical LOC (all lines): <ploc>");
    println("Filtered LOC (no empty LOC and no //): <floc>");
}

// convenience wrapper if you want to call with a project location
void printVolumeReport(list[Declaration] asts) {
    printVolumeReportFromAsts(asts, "AST input");
}
