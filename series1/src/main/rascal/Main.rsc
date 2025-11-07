module Main

import lang::java::m3::Core;
import lang::java::m3::AST;

import IO;

import List;
import Set;
import String;

import Map;

int main(int testArgument=0) {
    println("argument: <testArgument>");
    return testArgument;
}

list[Declaration] getASTs(loc projectLocation) {
    M3 model = createM3FromMavenProject(projectLocation);
    list[Declaration] asts = [createAstFromFile(f, true)
        | f <- files(model.containment), isCompilationUnit(f)];
    return asts;
}

int getNumberOfInterfaces(list[Declaration] asts){
    int interfaces = 0;
    visit(asts){
    case \interface(_, _, _, _, _, _): interfaces += 1;
    }
    return interfaces;
}


// Assignments 
// Problem 1
int getNumberOfForLoops(list[Declaration] asts){
    int count = 0;

    visit (asts) {
        case \foreach(_, _, _): count += 1;
        case \for(_, _, _, _): count += 1;
        case \for(_, _, _): count += 1;
    }

    return count;
}

tuple[int, list[str]] mostOccurringVariables(list[Declaration] asts){
    map[str varName, int counts] counter = ();

    visit (asts) {
        case \variable(\id(str name), _): counter[name] ? 0 += 1;
        case \variable(\id(str name), _, _): counter[name] ? 0 += 1;
        case \fieldAccess(\id(str name)): counter[name] ? 0 += 1;
        case \fieldAccess(_, \id(str name)): counter[name] ? 0 += 1;
        case \parameter(_, _, \id(str name), _): counter[name] ? 0 += 1;
        case \vararg(_, _, \id(str name)): counter[name] ? 0 += 1;
    }

    int maximum = max(counter.counts);
    return <maximum, toList(invert(counter)[maximum])>;
}

// list[loc] findNullReturned(list[Declaration] asts){
//     list[loc] locs = [];map

//     visit (asts) {
//         case \return(Expression expr):
//             if(expr is \null) locs += expr.src;
//     }
    
//     return locs;
// }

// ------------------------
// Assignment 2 
// Counting LOC
int countPhysicalLocFromAsts(list[Declaration] asts) =
    sum([ size(readFileLines(fileDecl.src)) | fileDecl <- asts ]);

bool isNoiseLine(str line) {
    str trimmedLine = trim(line);

    // if trimmedLine is equal to empty or commented line, ignore line
    if (trimmedLine == "") return true;
    else if (trimmedLine == "//") return true;
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