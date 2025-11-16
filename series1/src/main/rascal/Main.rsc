module Main

import lang::java::m3::Core;
import lang::java::m3::AST;

import IO;

import List;
import Set;
import String;

import Map;

import metrics::Volume;

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
// ------------------------

// ------------------------
// Unit Size
// ------------------------
map[loc, int] calculateUnitSize(list[Declaration] asts) {
    map[loc, int] unitSizes = ();
    // Visit all method and constructor implementations
    visit(asts) {
        // Handle methods with a statement body
        case \method(_, _, _, _, _, _, Statement impl): {
            if (impl.src?) unitSizes[impl.src] = size(readFileLines(impl.src));
        }
        
        case \constructor(_, _, _, _, Statement impl): {
            if (impl.src?) unitSizes[impl.src] = size(readFileLines(impl.src));
        }
    }
    
    return unitSizes;
}

// Additional function to calculate the average unit size - niet echt nodig voor SIG 
real calculateAverageUnitSize(map[loc location, int values] unitSizes) {
    if (size(unitSizes) == 0) return 0.0;

    else {
        int totalLoc = sum(unitSizes.values);
        int totalUnits = size(unitSizes);
        
        return (totalLoc * 1.0) / totalUnits;
    }
}

str getUnitSizeRating(real avgLoc) {
    // SIG thresholds for LOC - staat in de SIG doc 
    if (avgLoc <= 15.0) return "++ (excellent)";
    if (avgLoc <= 30.0) return "+ (good)";
    if (avgLoc <= 60.0) return "o (moderate)";
    if (avgLoc >= 61.0) return "- (poor)";
    return "-- (very poor)";
}

void printUnitSizeReportFromAsts(list[Declaration] asts) {
    map[loc, int] unitSizes = calculateUnitSize(asts);
    int totalUnits = size(unitSizes);

    if (totalUnits == 0) {
        println("\n=== Unit Size Report ===");
        println("No units found.");
        return;
    }

    // Total LOC over all units
    int totalLoc = 0;

    // LOC in units above thresholds (SIG style)
    int locAbove15 = 0;
    int locAbove30 = 0;
    int locAbove60 = 0;

    for (loc u <- unitSizes) {
        int size = unitSizes[u];
        totalLoc += size;

        if (size > 15) locAbove15 += size;
        if (size > 30) locAbove30 += size;
        if (size > 60) locAbove60 += size;
    }

    real avgLoc = (totalLoc * 1.0) / totalUnits;

    // Percentages of LOC (SIG style)
    real pctLocAbove15 = totalLoc > 0 ? (locAbove15 * 100.0) / totalLoc : 0.0;
    real pctLocAbove30 = totalLoc > 0 ? (locAbove30 * 100.0) / totalLoc : 0.0;
    real pctLocAbove60 = totalLoc > 0 ? (locAbove60 * 100.0) / totalLoc : 0.0;

    bool ok15 = pctLocAbove15 <= 47.1;
    bool ok30 = pctLocAbove30 <= 23.1;
    bool ok60 = pctLocAbove60 <= 8.3;

    str rating = getUnitSizeRating(avgLoc);

    println("\n===Unit Size Report ===");
    println("Total Units (Methods/Constructors): <totalUnits>");
    println("Total LOC in units: <totalLoc>");
    println("Average Unit Size (LOC): <avgLoc>");

    println("\n--- SIG Unit Size Risk Profile (Percentage of LOC) ---");
    println("\> 15 LOC: <pctLocAbove15>%  (LOC in such units: <locAbove15>)");
    println("\> 30 LOC: <pctLocAbove30>%  (LOC in such units: <locAbove30>)");
    println("\> 60 LOC: <pctLocAbove60>%  (LOC in such units: <locAbove60>)");

    println("\n--- SIG 4 STAR Compliance Check ---");
    println("\> 15 LOC (should be \<= 47.1%): <pctLocAbove15>%  -\> <ok15>");
    println("\> 30 LOC (should be \<= 23.1%): <pctLocAbove30>%  -\> <ok30>");
    println("\> 60 LOC (should be \<= 8.3%):  <pctLocAbove60>%  -\> <ok60>");

    println("\nSIG-inspired Rating (based on avg LOC): <rating>");
}

// ------------------------
// Unit Complexity Code
// ------------------------
// Helper function: Count logical AND (&&) and OR (||) operators
int countLogicalOperators(\Expression exp) {
    int count = 0;
    
    visit(exp) {
        case \conditionalOr(_, _): count += 1; 
        case \conditionalAnd(_, _): count += 1; 
    }
    return count;
}


int calculateCyclomaticComplexity(Statement impl) {
    int complexity = 1; // Start with 1 for the function/method entry point
    
    visit (impl) {
        case \if(_, _): complexity += 1;
        case \while(_, _): complexity += 1;
        case \do(_, _): complexity += 1;
        case \for(_, _, _, _): complexity += 1;
        case \foreach(_, _, _): complexity += 1;

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
        
        case \switch(\Expression _, _): complexity += 1;
        
        case \catch(_, _): complexity += 1;
    }
    
    return complexity;
}


map[loc, int] calculateUnitComplexity(list[Declaration] asts) {
    // its pretty much the same as unit size, change this later 
    map[loc, int] unitComplexities = ();
    
    visit(asts) {
        case \method(_, _, _, _, _, _, Statement impl): {
            if (impl.src?) unitComplexities[impl.src] = calculateCyclomaticComplexity(impl);
        }
        
        case \constructor(_, _, _, _, Statement impl): {
            if (impl.src?) unitComplexities[impl.src] = calculateCyclomaticComplexity(impl);
        }
    }
    
    return unitComplexities;
}

real calculateAverageUnitComplexity(map[loc location, int values] unitComplexities) {
    if (size(unitComplexities) == 0) return 0.0;
    
    int totalCC = sum(unitComplexities.values);
    int totalUnits = size(unitComplexities);
    
    return (totalCC * 1.0) / totalUnits;
}

str getComplexityRating(real avgCC) {
    if (avgCC <= 5.0) return "++ (excellent)";
    if (avgCC <= 10.0) return "+ (good)";
    if (avgCC <= 15.0) return "o (moderate)";
    if (avgCC <= 20.0) return "- (poor)";
    return "-- (very poor)";
}

void printUnitComplexityReportFromAsts(list[Declaration] asts) {
    map[loc, int] unitComplexities = calculateUnitComplexity(asts);
    int totalUnits = size(unitComplexities);
    real avgCC = calculateAverageUnitComplexity(unitComplexities);
    str rating = getComplexityRating(avgCC);
    
    // Calculate Risk Profile: percentage of units that exceed a threshold 
    int riskThreshold = 15; 
    int riskyUnits = 0;
    for (loc unitLoc <- unitComplexities) {
        if (unitComplexities[unitLoc] > riskThreshold) riskyUnits += 1;
        
    }
    real riskPct = (totalUnits > 0) ? (riskyUnits * 100.0) / totalUnits : 0.0;
    
    println("\n=== Unit Complexity Report ===");
    println("Total Units (Methods/Constructors): <totalUnits>");
    println("Average Cyclomatic Complexity (CC): <avgCC>");
    println("Risk Profile (<totalUnits> CC <riskThreshold>): <riskyUnits> (<riskPct>%)");
    println("SIG Rating: <rating>");
}

// ------------------------
// Duplication Code Feature
// ------------------------
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
                    println("Warning: could not read file at <impl.src>");
                }
            }
        }
        
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
    
    // Filter: keep only blocks that appear twice 
    return (h : blockMap[h] | h <- blockMap, size(blockMap[h]) >= 2);
}

tuple[int duplicatedLines, int totalLines, real percentage] calculateDuplication(
    list[Declaration] asts,
    int minLines
    ){

    map[str, list[tuple[loc, int]]] duplicates = extractCodeBlocks(asts, minLines);
    int totalLines = countPhysicalLocFromAsts(asts);

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

str getDuplicationRating(real percentage) {
    if (percentage <= 3.0) return "++ (excellent)";
    if (percentage <= 5.0) return "+ (good)";
    if (percentage <= 10.0) return "o (moderate)";
    if (percentage <= 20.0) return "- (poor)";
    return "-- (very poor)";
}

void printDuplicationReport(list[Declaration] asts, int minLines) {
    map[str, list[tuple[loc, int]]] duplicates = extractCodeBlocks(asts, minLines);
    tuple[int dup, int total, real pct] stats = calculateDuplication(asts, minLines);
    
    println("\n=== Duplication Report ===");
    println("Minimum block size: <minLines> lines");
    println("Total LOC: <stats.total>");
    println("Duplicated LOC: <stats.dup>");
    println("Duplication percentage: <stats.pct>%");
    println("\nNumber of duplicate blocks found: <size(duplicates)>");
    str rating = getDuplicationRating(stats.pct);
    println("SIG Rating: <rating>");
}

void printDuplicationReportFromAsts(list[Declaration] asts) {
    minLines = 6; // SIG: fragements >= 6 LOC according to doc
    printDuplicationReport(asts, minLines);
}


// Combined report: volume + unit size + unit complexity + duplication
void printFullQualityReportFromAsts(list[Declaration] asts, str label = "Project") {
    // Volume
    printVolumeReportFromAsts(asts, label);
    
    // Unit Size
    printUnitSizeReportFromAsts(asts);

    // Unit Complexity
    printUnitComplexityReportFromAsts(asts);
    
    // Duplication
    printDuplicationReportFromAsts(asts);
}