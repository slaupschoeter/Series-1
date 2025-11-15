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

//!!!!!!!!!!!!!!!!!!
// Ashrafs code
// Normalise code: remove whitespace, comments, and put in lowercase
str normalizeCode(list[str] lines) {
    str normalized = "";
    for (line <- lines) {
        str trimmed = trim(line);
        // Skip empty lines and comments
        if (trimmed != "" && !startsWith(trimmed, "//") && !startsWith(trimmed, "/*") && !startsWith(trimmed, "*")) {
            // Remove all whitespace and make lowercase for comparison
            normalized += toLowerCase(replaceAll(trimmed, " ", ""));
        }
    }
    return normalized;
}

// Extract normalised code blocks of minimal minLines big
map[str normalized, list[tuple[loc location, int lineCount]] blocks] extractCodeBlocks(list[Declaration] asts, int minLines) {    
    map[str, list[tuple[loc, int]]] blockMap = ();
    
    visit(asts) {
        // Extract all method bodies
        case \method(_, _, _, _, _, _, Statement impl): {
            if (impl.src?) {
                try {
                    list[str] lines = readFileLines(impl.src);
                    int lineCount = size(lines);
                    
                    if (lineCount >= minLines) {
                        str normalized = normalizeCode(lines);
                        
                        // Only add if normalised code isn't empty
                        if (size(normalized) > 0) {
                            if (normalized in blockMap) {
                                blockMap[normalized] += [<impl.src, lineCount>];
                            } else {
                                blockMap[normalized] = [<impl.src, lineCount>];
                            }
                        }
                    }
                } catch: {

                    // Skipt files that can't be read
                    println("Warning: could not read file at <impl.src>");
                }
            }
        }
        
        // Extract constructors
        case \constructor(_, _, _, _,  Statement impl): {
            if (impl.src?) {
                try {
                    list[str] lines = readFileLines(impl.src);
                    int lineCount = size(lines);
                    
                    if (lineCount >= minLines) {
                        str normalized = normalizeCode(lines);
                        
                        if (size(normalized) > 0) {
                            if (normalized in blockMap) {
                                blockMap[normalized] += [<impl.src, lineCount>];
                            } else {
                                blockMap[normalized] = [<impl.src, lineCount>];
                            }
                        }
                    }
                } catch: {
                    println("Warning: could not read file at <impl.src>");
                }
            }
        }
    }
    
    // Filter: keep only blocks that appear twice (= duplicates)
    return (h : blockMap[h] | h <- blockMap, size(blockMap[h]) >= 2);
}


// Calculate duplication percentage
tuple[int duplicatedLines, int totalLines, real percentage] calculateDuplication(
    list[Declaration] asts,
    int minLines
) {
    map[str, list[tuple[loc, int]]] duplicates = extractCodeBlocks(asts, minLines);
    
    // Count total amount of lines of code
    int totalLines = countPhysicalLocFromAsts(asts);
    
    // Count duplicated lines
    int duplicatedLines = 0;
    for (normalized <- duplicates) {
        list[tuple[loc location, int lineCount]] blocks = duplicates[normalized];
        int blockSize = blocks[0].lineCount;
        int occurrences = size(blocks);
        
        // Count only the extra copies (not the original)
        duplicatedLines += blockSize * (occurrences - 1);
    }
    
    real percentage = totalLines > 0 ? (duplicatedLines * 100.0) / totalLines : 0.0;
    
    return <duplicatedLines, totalLines, percentage>;
}

// Print the detailed duplication report
void printDuplicationReport(list[Declaration] asts, int minLines) {
    map[str, list[tuple[loc, int]]] duplicates = extractCodeBlocks(asts, minLines);
    tuple[int dup, int total, real pct] stats = calculateDuplication(asts, minLines);
    
    println("\n=== Duplication Report ===");
    println("Minimum block size: <minLines> lines");
    println("Total LOC: <stats.total>");
    println("Duplicated LOC: <stats.dup>");
    println("Duplication percentage: <round(stats.pct, 0.01)>%");
    println("\nNumber of duplicate blocks found: <size(duplicates)>");
    
    // SIG rating (based on ISO/IEC 25010 maintainability)
    str rating = getDuplicationRating(stats.pct);
    println("SIG Rating: <rating>");
    
    println("\n--- Duplicate Block Details ---");
    int blockNum = 1;
    for (normalized <- duplicates) {
        list[tuple[loc location, int lineCount]] blocks = duplicates[normalized];
        println("\nDuplicate Block #<blockNum>:");
        println("  Occurrences: <size(blocks)>");
        println("  Block size: <blocks[0].lineCount> lines");
        println("  Locations:");
        for (block <- blocks) {
            println("    - <block.location>");
        }
        blockNum += 1;
    }
}

// SIG Maintainability rating for duplication
str getDuplicationRating(real percentage) {
    if (percentage <= 3.0) return "++ (excellent)";
    if (percentage <= 5.0) return "+ (good)";
    if (percentage <= 10.0) return "o (moderate)";
    if (percentage <= 20.0) return "- (poor)";
    return "-- (very poor)";
}

// Convenience wrapper for project location
void printDuplicationReportFromProject(loc projectLocation) {
    minLines = 6;
    list[Declaration] asts = getASTs(projectLocation);
    printDuplicationReport(asts, minLines);
}

// Combined report: volume + duplication
void printFullQualityReport(loc projectLocation) {
    list[Declaration] asts = getASTs(projectLocation);
    
    // Volume metrics
    printVolumeReportFromAsts(asts, "<projectLocation>");
    
    // Duplication metrics
    printDuplicationReport(asts, 6);
}
