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
int countPhysicalLocFromAsts(list[Declaration] asts) =
  sum([ size(readFileLines(d.src)) | d <- asts ]);

bool isNoiseLine(str line) {
  str t = trim(line);
  return t == "" || startsWith(t, "//"); // simple heuristic: ignores /* ... */ blocks
}

int countHeuristicSlocFromAsts(list[Declaration] asts) =
  sum([ size([ l | l <- readFileLines(d.src), !isNoiseLine(l) ]) | d <- asts ]);

void printVolumeReportFromAsts(list[Declaration] asts, str label) {
  int fileCount = size(asts);
  int ploc = countPhysicalLocFromAsts(asts);
  int sloc = countHeuristicSlocFromAsts(asts);

  println("=== Volume Report ===");
  println("Project: <label>");
  println("Java files: <fileCount>");
  println("Physical LOC (all lines): <ploc>");
  println("Heuristic SLOC (no blanks, no //): <sloc>");
}

// convenience wrapper if you want to call with a project location
void printVolumeReport(loc projectLocation) {
  printVolumeReportFromAsts(getASTs(projectLocation), "<projectLocation>");
}



// int totalLOC = sum([ size(readFileLines(f)) | f <- files(model.containment), isCompilationUnit(f) ]);
// println("Lines of code: <totalLOC>");
