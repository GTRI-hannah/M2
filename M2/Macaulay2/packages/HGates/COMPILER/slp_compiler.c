#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "matrix.h"

#define MAX_LINE_LENGTH 1000
#define MAX_VARIABLES 100
#define MAX_OUTPUT_SIZE 20000

typedef struct {
    char name[50];
    char c_name[50];
    int is_input;
    int is_constant;
} Variable;

Variable variables[MAX_VARIABLES];
int var_count = 0;

// Utility functions
char* trim(char* str) {
    while(isspace((unsigned char)*str)) str++;
    if(*str == 0) return str;
    
    char* end = str + strlen(str) - 1;
    while(end > str && isspace((unsigned char)*end)) end--;
    end[1] = '\0';
    return str;
}

char* extract_quoted_value(char* str) {
    static char result[100];
    char* start = strchr(str, '\'');
    if (!start) return NULL;
    
    start++;
    char* end = strchr(start, '\'');
    if (!end) return NULL;
    
    int len = end - start;
    strncpy(result, start, len);
    result[len] = '\0';
    return result;
}

void add_variable(const char* slp_name, const char* c_name, int is_input, int is_constant) {
    if (var_count >= MAX_VARIABLES) return;
    strcpy(variables[var_count].name, slp_name);
    strcpy(variables[var_count].c_name, c_name);
    variables[var_count].is_input = is_input;
    variables[var_count].is_constant = is_constant;
    var_count++;
}

char* find_c_name(const char* slp_name) {
    for (int i = 0; i < var_count; i++) {
        if (strcmp(variables[i].name, slp_name) == 0) {
            return variables[i].c_name;
        }
    }
    return (char*)slp_name; // Return the original name if not found
}

int parse_matrix_elements(char* elements_str, char elements[][50], int max_elements) {
    int count = 0;
    char* token = strtok(elements_str, ",");
    
    while (token && count < max_elements) {
        strcpy(elements[count], trim(token));
        count++;
        token = strtok(NULL, ",");
    }
    return count;
}

int parse_matrix_dimensions(char* dim_str, int* rows, int* cols) {
    char* paren_start = strchr(dim_str, '(');
    if (!paren_start) return 0;
    
    return sscanf(paren_start, "(%d, %d)", rows, cols) == 2;
}

// Main compilation function
char* compile_slp_to_c(const char* slp_code) {
    char* output = malloc(MAX_OUTPUT_SIZE);
    if (!output) return NULL;
    
    // Reset state
    var_count = 0;
    strcpy(output, "");
    
    // Headers
    strcat(output, "#include <stdio.h>\n");
    strcat(output, "#include <stdlib.h>\n");
    strcat(output, "#include \"matrix.h\"\n\n");
    strcat(output, "int main() {\n");
    
    // Parse SLP - use a different approach to handle all lines properly
    char* slp_copy = malloc(strlen(slp_code) + 1);
    strcpy(slp_copy, slp_code);
    
    // Split into lines manually to ensure we process all lines
    char* lines[100];  // Assume max 100 lines
    int line_count = 0;
    
    char* line_start = slp_copy;
    char* line_end;
    
    while ((line_end = strchr(line_start, '\n')) != NULL) {
        *line_end = '\0';
        lines[line_count] = malloc(strlen(line_start) + 1);
        strcpy(lines[line_count], line_start);
        line_count++;
        line_start = line_end + 1;
    }
    
    // Handle last line if it doesn't end with newline
    if (strlen(line_start) > 0) {
        lines[line_count] = malloc(strlen(line_start) + 1);
        strcpy(lines[line_count], line_start);
        line_count++;
    }
    
    // Process each line
    for (int line_idx = 0; line_idx < line_count; line_idx++) {
        char* line = trim(lines[line_idx]);
        
        // Skip comments and empty lines
        if (strlen(line) == 0 || line[0] == '-') {
            continue;
        }
        
        char* equals = strchr(line, '=');
        if (!equals) {
            continue;
        }
        
        // Parse variable name and expression
        char var_name[50], expression[500];
        int var_len = equals - line;
        strncpy(var_name, line, var_len);
        var_name[var_len] = '\0';
        var_name[strcspn(var_name, " \t")] = '\0';
        
        strcpy(expression, trim(equals + 1));
        
        char line_output[2000] = "";
        
        if (var_name[0] == 'I') {
            // Input variable
            char* input_name = extract_quoted_value(expression);
            if (input_name) {
                sprintf(line_output, 
                    "    double %s;\n"
                    "    printf(\"Enter %s: \");\n"
                    "    scanf(\"%%lf\", &%s);\n"
                    "    double %s_values[] = {%s};\n"
                    "    Matrix* %s = create_matrix(%s_values, 1, 1);\n",
                    input_name, input_name, input_name, var_name, input_name, var_name, var_name);
                
                add_variable(var_name, input_name, 1, 0);
            }
        }
        else if (var_name[0] == 'C') {
            // Constant
            char* const_value = extract_quoted_value(expression);
            if (const_value) {
                sprintf(line_output,
                    "    double %s_values[] = {%s};\n"
                    "    Matrix* %s = create_matrix(%s_values, 1, 1);\n",
                    var_name, const_value, var_name, var_name);
                
                add_variable(var_name, var_name, 0, 1);
            }
        }
        else if (var_name[0] == 'R') {
            // Result operations
            if (strstr(expression, "matrix{")) {
                // Matrix creation: matrix{elem1, elem2, ...} (rows, cols)
                char* brace_start = strchr(expression, '{');
                char* brace_end = strchr(expression, '}');
                
                if (brace_start && brace_end) {
                    // Extract elements
                    char elements_str[500];
                    int elem_len = brace_end - brace_start - 1;
                    strncpy(elements_str, brace_start + 1, elem_len);
                    elements_str[elem_len] = '\0';
                    
                    char elements[20][50];
                    int element_count = parse_matrix_elements(elements_str, elements, 20);
                    
                    // Extract dimensions
                    int rows, cols;
                    if (parse_matrix_dimensions(brace_end, &rows, &cols)) {
                        sprintf(line_output, "    double %s_values[] = {", var_name);
                        
                        for (int i = 0; i < element_count; i++) {
                            // Check if element is a variable reference
                            int found = 0;
                            for (int j = 0; j < var_count; j++) {
                                if (strcmp(variables[j].name, elements[i]) == 0) {
                                    sprintf(line_output + strlen(line_output), 
                                           "get_element(%s, 0, 0)", elements[i]);
                                    found = 1;
                                    break;
                                }
                            }
                            if (!found) {
                                strcat(line_output, elements[i]);
                            }
                            
                            if (i < element_count - 1) strcat(line_output, ", ");
                        }
                        
                        sprintf(line_output + strlen(line_output),
                                "};\n    Matrix* %s = create_matrix(%s_values, %d, %d);\n",
                                var_name, var_name, rows, cols);
                    }
                }
            }
            else if (strstr(expression, "matrixProduct(")) {
                // Matrix multiplication
                char* paren_start = strchr(expression, '(');
                char* paren_end = strrchr(expression, ')');
                
                if (paren_start && paren_end) {
                    char args[200];
                    int args_len = paren_end - paren_start - 1;
                    strncpy(args, paren_start + 1, args_len);
                    args[args_len] = '\0';
                    
                    char* comma = strchr(args, ',');
                    if (comma) {
                        *comma = '\0';
                        char* arg1 = trim(args);
                        char* arg2 = trim(comma + 1);
                        
                        sprintf(line_output, "    Matrix* %s = multiply_matrices(%s, %s);\n",
                                var_name, arg1, arg2);
                    }
                }
            }
            else if (strchr(expression, '+')) {
                // Matrix addition
                char* plus = strchr(expression, '+');
                char operand1[50], operand2[50];
                
                int op1_len = plus - expression;
                strncpy(operand1, expression, op1_len);
                operand1[op1_len] = '\0';
                strcpy(operand1, trim(operand1));
                strcpy(operand2, trim(plus + 1));
                
                sprintf(line_output, "    Matrix* %s = add_matrices(%s, %s);\n",
                        var_name, operand1, operand2);
            }
            
            add_variable(var_name, var_name, 0, 0);
        }
        
        strcat(output, line_output);
    }
    
    // Clean up line arrays
    for (int i = 0; i < line_count; i++) {
        free(lines[i]);
    }
    
    // Find last result variable for output (look for highest R number)
    char last_result[50] = "";
    int highest_r = -1;
    for (int i = 0; i < var_count; i++) {
        if (variables[i].name[0] == 'R') {
            int r_num = atoi(variables[i].name + 1); // Extract number after 'R'
            if (r_num > highest_r) {
                highest_r = r_num;
                strcpy(last_result, variables[i].name);
            }
        }
    }
    
    // Add output and cleanup
    if (strlen(last_result) > 0) {
        sprintf(output + strlen(output),
                "\n    // Print final result\n"
                "    printf(\"Final result:\\n\");\n"
                "    print_matrix(%s);\n", last_result);
    }
    
    strcat(output, "\n    // Clean up memory\n");
    for (int i = 0; i < var_count; i++) {
        sprintf(output + strlen(output), "    free_matrix(%s);\n", variables[i].name);
    }
    
    strcat(output, "\n    return 0;\n}\n");
    
    free(slp_copy);
    return output;
}

// Helper function to save compiled code to file
int save_compiled_code(const char* c_code, const char* filename) {
    FILE* file = fopen(filename, "w");
    if (!file) return 0;
    
    fprintf(file, "%s", c_code);
    fclose(file);
    return 1;
}

// Test function with multiple examples
void run_tests() {
    printf("=== SLP Compiler Tests ===\n\n");
    
    // Test 1: Basic operations from slp.txt example 1
    printf("Test 1: Basic matrix operations\n");
    printf("Input SLP:\n");
    char* test1 = 
        "C0 = '3'\n"
        "I0 = 'x'\n"
        "I1 = 'y'\n"
        "R0 = I0+I0\n"
        "R1 = R0+I1\n"
        "R2 = matrix{R1} (1, 1)\n"
        "R3 = R2+C0\n";
    printf("%s\n", test1);
    
    char* compiled1 = compile_slp_to_c(test1);
    if (compiled1) {
        printf("Generated C code:\n");
        printf("%s\n", compiled1);
        save_compiled_code(compiled1, "test1_output.c");
        printf("Saved to test1_output.c\n\n");
        free(compiled1);
    }
    
    printf("============================================================\n\n");
    
    // Test 2: Matrix multiplication from slp.txt example 2
    printf("Test 2: Matrix multiplication\n");
    printf("Input SLP:\n");
    char* test2 = 
        "I0 = 'x'\n"
        "I1 = 'y'\n"
        "R0 = I0+I0\n"
        "R1 = matrix{R0, I0, I1, I1} (2, 2)\n"
        "R2 = matrix{I0, I1} (2, 1)\n"
        "R3 = matrixProduct(R1, R2)\n";
    printf("%s\n", test2);
    
    char* compiled2 = compile_slp_to_c(test2);
    if (compiled2) {
        printf("Generated C code:\n");
        printf("%s\n", compiled2);
        save_compiled_code(compiled2, "test2_output.c");
        printf("Saved to test2_output.c\n\n");
        free(compiled2);
    }
    
    printf("============================================================\n\n");
    
    // Test 3: More complex operations
    printf("Test 3: Complex matrix operations\n");
    printf("Input SLP:\n");
    char* test3 = 
        "C0 = '1'\n"
        "C1 = '2'\n"
        "I0 = 'a'\n"
        "I1 = 'b'\n"
        "R0 = matrix{C0, C1, I0, I1} (2, 2)\n"
        "R1 = matrix{I0, I1} (2, 1)\n"
        "R2 = matrixProduct(R0, R1)\n"
        "R3 = R2+R1\n";
    printf("%s\n", test3);
    
    char* compiled3 = compile_slp_to_c(test3);
    if (compiled3) {
        printf("Generated C code:\n");
        printf("%s\n", compiled3);
        save_compiled_code(compiled3, "test3_output.c");
        printf("Saved to test3_output.c\n\n");
        free(compiled3);
    }
    
    printf("============================================================\n\n");
    
    // Test 4: Chained operations
    printf("Test 4: Chained operations\n");
    printf("Input SLP:\n");
    char* test4 = 
        "I0 = 'x'\n"
        "I1 = 'y'\n"
        "I2 = 'z'\n"
        "R0 = I0+I1\n"
        "R1 = R0+I2\n"
        "R2 = matrix{R1, I0} (2, 1)\n"
        "R3 = matrix{I1, I2} (2, 1)\n"
        "R4 = R2+R3\n";
    printf("%s\n", test4);
    
    char* compiled4 = compile_slp_to_c(test4);
    if (compiled4) {
        printf("Generated C code:\n");
        printf("%s\n", compiled4);
        save_compiled_code(compiled4, "test4_output.c");
        printf("Saved to test4_output.c\n\n");
        free(compiled4);
    }
}

// Main function with usage example
int main(int argc, char* argv[]) {
    if (argc > 1) {
        // Command line usage: ./slp_compiler input.slp output.c
        FILE* input_file = fopen(argv[1], "r");
        if (!input_file) {
            printf("Error: Cannot open input file %s\n", argv[1]);
            return 1;
        }
        
        // Read entire file
        fseek(input_file, 0, SEEK_END);
        long file_size = ftell(input_file);
        rewind(input_file);
        
        char* slp_code = malloc(file_size + 1);
        fread(slp_code, 1, file_size, input_file);
        slp_code[file_size] = '\0';
        fclose(input_file);
        
        // Compile
        char* compiled = compile_slp_to_c(slp_code);
        if (compiled) {
            if (argc > 2) {
                // Save to specified output file
                if (save_compiled_code(compiled, argv[2])) {
                    printf("Successfully compiled %s to %s\n", argv[1], argv[2]);
                } else {
                    printf("Error: Cannot write to output file %s\n", argv[2]);
                }
            } else {
                // Print to stdout
                printf("%s", compiled);
            }
            free(compiled);
        } else {
            printf("Error: Compilation failed\n");
        }
        
        free(slp_code);
    } else {
        // Run tests
        run_tests();
    }
    
    return 0;
}