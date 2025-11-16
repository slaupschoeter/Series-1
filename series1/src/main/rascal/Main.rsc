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

// ------------------------
// Series 1
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
                            if (normalized in blockMap) blockMap[normalized] += [<impl.src, lineCount>];
                            else blockMap[normalized] = [<impl.src, lineCount>];
                        }
                    }
                } 
                catch: {
                    // Skip files that can't be read
               
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
                            if (normalized in blockMap) blockMap[normalized] += [<impl.src, lineCount>];
                            else blockMap[normalized] = [<impl.src, lineCount>];
                        }
                    }
                } 
                catch: {
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
    ){

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
    
    real percentage = totalLines > 0 ?
    (duplicatedLines * 100.0) / totalLines : 0.0;
    
    return <duplicatedLines, totalLines, percentage>;
}

// Unit Size
map[loc, int] calculateUnitSize(list[Declaration] asts) {
    map[loc, int] unitSizes = ();
    // Visit all method and constructor implementations
    visit(asts) {
        // Handle methods with a statement body (impl)
        case \method(_, _, _, _, _, _, Statement impl): {
            if (impl.src?) {
                unitSizes[impl.src] = size(readFileLines(impl.src));
            }
        }
        
        // Handle constructors with a statement body (impl)
        case \constructor(_, _, _, _, Statement impl): {
            if (impl.src?) {
                unitSizes[impl.src] = size(readFileLines(impl.src));
            }
        }
        
        // Ignore abstract methods or interfaces which have no body/src location
    }
    
    return unitSizes;
}

// Additional function to calculate the average unit size
real calculateAverageUnitSize(map[loc location, int values] unitSizes) {
    if (size(unitSizes) == 0) return 0.0;
    // sum(map.values) is a shorthand for summing all values in a map
    int totalLoc = sum(unitSizes.values);
    int totalUnits = size(unitSizes);
    
    return (totalLoc * 1.0) / totalUnits;
}

// SIG Maintainability rating for Unit Size
str getUnitSizeRating(real avgLoc) {
    // SIG thresholds for LOC are based on industry standards
    if (avgLoc <= 10.0) return "++ (excellent)";
    if (avgLoc <= 20.0) return "+ (good)";
    if (avgLoc <= 30.0) return "o (moderate)";
    if (avgLoc <= 40.0) return "- (poor)";
    return "-- (very poor)";
}

// Print the Unit Size report
void printUnitSizeReportFromAsts(list[Declaration] asts) {
    map[loc, int] unitSizes = calculateUnitSize(asts);
    int totalUnits = size(unitSizes);
    real avgLoc = calculateAverageUnitSize(unitSizes);
    str rating = getUnitSizeRating(avgLoc);
    
    // Calculate Risk Profile: percentage of units that exceed a threshold (e.g., 60 LOC)
    int riskThreshold = 60; 
    int riskyUnits = 0;
    for (loc unitLoc <- unitSizes) {
        if (unitSizes[unitLoc] > riskThreshold) {
            riskyUnits += 1;
        }
    }
    real riskPct = (totalUnits > 0) ? (riskyUnits * 100.0) / totalUnits : 0.0;
    
    println("\n=== Unit Size Report ===");
    println("Total Units (Methods/Constructors): <totalUnits>");
    println("Average Unit Size (LOC): <avgLoc>");
    println("Risk Profile (<totalUnits> <riskThreshold> LOC): <riskyUnits> (<riskPct>%)");
    println("SIG Rating: <rating>");
}

// SIG Maintainability rating for duplication
str getDuplicationRating(real percentage) {
    if (percentage <= 3.0) return "++ (excellent)";
    if (percentage <= 5.0) return "+ (good)";
    if (percentage <= 10.0) return "o (moderate)";
    if (percentage <= 20.0) return "- (poor)";
    return "-- (very poor)";
}

// Print the detailed duplication report
void printDuplicationReport(list[Declaration] asts, int minLines) {
    map[str, list[tuple[loc, int]]] duplicates = extractCodeBlocks(asts, minLines);
    tuple[int dup, int total, real pct] stats = calculateDuplication(asts, minLines);
    
    println("\n=== Duplication Report ===");
    println("Minimum block size: <minLines> lines");
    println("Total LOC: <stats.total>");
    println("Duplicated LOC: <stats.dup>");
    println("Duplication percentage: <stats.pct>%");
    println("\nNumber of duplicate blocks found: <size(duplicates)>");
    // SIG rating (based on ISO/IEC 25010 maintainability)
    str rating = getDuplicationRating(stats.pct);
    println("SIG Rating: <rating>");
    println("\n--- Duplicate Block Details ---");
}

void printDuplicationReportFromAsts(list[Declaration] asts) {
    minLines = 1;
    printDuplicationReport(asts, minLines);
}

// Helper function: Count logical AND (&&) and OR (||) operators
int countLogicalOperators(\Expression exp) {
    int count = 0;
    
    // Visit only the condition expression tree
    visit(exp) {
        // Standard Java Logical AND and OR
        case \conditionalOr(_, _): count += 1; 
        case \conditionalAnd(_, _): count += 1; 
    }
    return count;
}

// Helper function: Calculate Cyclomatic Complexity (CC) for a single Statement block
int calculateCyclomaticComplexity(Statement impl) {
    int complexity = 1; // Start with 1 for the function/method entry point
    
    // Visit the statement block (impl) to find decision points
    visit (impl) {
        // Conditionals: if, while, do/while, for
        case \if(_, _): complexity += 1;
        case \while(_, _): complexity += 1;
        case \do(_, _): complexity += 1;
        case \for(_, _, _, _): complexity += 1;
        case \foreach(_, _, _): complexity += 1;
        // Logical Operators in Conditions (each operator adds 1 to CC)
        case \conditional(_, \Expression cond, _): {
            complexity += countLogicalOperators(cond);
        }
        case \if(\Expression cond, _): {
            complexity += countLogicalOperators(cond);
        }
        case \while(\Expression cond, _): {
            complexity += countLogicalOperators(cond);
        }
        case \do(_, \Expression cond): {
            complexity += countLogicalOperators(cond);
        }
        case \for(_, \Expression cond, _, _): {
            complexity += countLogicalOperators(cond);
        }
        
        // Switch statements: each case label is a decision point
        case \switch(\Expression _, _): complexity += 1;
        
        // Catch blocks in try/catch
        case \catch(_, _): complexity += 1;
    }
    
    return complexity;
}


// Main Metric function: Maps unit locations to their Cyclomatic Complexity
map[loc, int] calculateUnitComplexity(list[Declaration] asts) {
    map[loc, int] unitComplexities = ();
    
    // Visit all method and constructor implementations
    visit(asts) {
        // Handle methods with a statement body (impl)
        case \method(_, _, _, _, _, _, Statement impl): {
            if (impl.src?) {
                unitComplexities[impl.src] = calculateCyclomaticComplexity(impl);
            }
        }
        
        // Handle constructors with a statement body (impl)
        case \constructor(_, _, _, _, Statement impl): {
            if (impl.src?) {
                unitComplexities[impl.src] = calculateCyclomaticComplexity(impl);
            }
        }
    }
    
    return unitComplexities;
}

// Calculates the average CC across all units
real calculateAverageUnitComplexity(map[loc location, int values] unitComplexities) {
    if (size(unitComplexities) == 0) return 0.0;
    
    int totalCC = sum(unitComplexities.values);
    int totalUnits = size(unitComplexities);
    
    return (totalCC * 1.0) / totalUnits;
}

// SIG Maintainability rating for Unit Complexity (Cyclomatic Complexity)
str getComplexityRating(real avgCC) {
    // SIG thresholds for CC are based on industry standards
    if (avgCC <= 5.0) return "++ (excellent)";
    if (avgCC <= 10.0) return "+ (good)";
    if (avgCC <= 15.0) return "o (moderate)";
    if (avgCC <= 20.0) return "- (poor)";
    return "-- (very poor)";
}

// Print the Unit Complexity report
void printUnitComplexityReportFromAsts(list[Declaration] asts) {
    map[loc, int] unitComplexities = calculateUnitComplexity(asts);
    int totalUnits = size(unitComplexities);
    real avgCC = calculateAverageUnitComplexity(unitComplexities);
    str rating = getComplexityRating(avgCC);
    
    // Calculate Risk Profile: percentage of units that exceed a threshold (e.g., CC 15)
    int riskThreshold = 15; 
    int riskyUnits = 0;
    for (loc unitLoc <- unitComplexities) {
        if (unitComplexities[unitLoc] > riskThreshold) {
            riskyUnits += 1;
        }
    }
    real riskPct = (totalUnits > 0) ? (riskyUnits * 100.0) / totalUnits : 0.0;
    
    println("\n=== Unit Complexity Report ===");
    println("Total Units (Methods/Constructors): <totalUnits>");
    println("Average Cyclomatic Complexity (CC): <avgCC>");
    println("Risk Profile (<totalUnits> CC <riskThreshold>): <riskyUnits> (<riskPct>%)");
    println("SIG Rating: <rating>");
}


// Combined report: volume + unit size + unit complexity + duplication
void printFullQualityReportFromAsts(list[Declaration] asts, str label = "Project") {
    // Volume
    printVolumeReportFromAsts(asts, label);
    
    // Unit Size
    printUnitSizeReportFromAsts(asts);
    
    // Unit Complexity (New)
    printUnitComplexityReportFromAsts(asts);
    
    // Duplication
    printDuplicationReportFromAsts(asts);
}