#include <stdio.h>
#include <stdlib.h>

typedef struct {
    double **data;  // 2D array to store matrix elements
    int rows;       // number of rows
    int cols;       // number of columns
} Matrix;

// Function to create a matrix from a list of values
Matrix* create_matrix(double *values, int rows, int cols) {
    // Check if we have enough values
    if (values == NULL || rows <= 0 || cols <= 0) {
        return NULL;
    }
    
    // Allocate memory for the matrix structure
    Matrix *matrix = (Matrix*)malloc(sizeof(Matrix));
    if (matrix == NULL) {
        return NULL;
    }
    
    matrix->rows = rows;
    matrix->cols = cols;
    
    // Allocate memory for row pointers
    matrix->data = (double**)malloc(rows * sizeof(double*));
    if (matrix->data == NULL) {
        free(matrix);
        return NULL;
    }
    
    // Allocate memory for each row
    for (int i = 0; i < rows; i++) {
        matrix->data[i] = (double*)malloc(cols * sizeof(double));
        if (matrix->data[i] == NULL) {
            // Clean up previously allocated rows
            for (int j = 0; j < i; j++) {
                free(matrix->data[j]);
            }
            free(matrix->data);
            free(matrix);
            return NULL;
        }
    }
    
    // Fill the matrix with values (row-major order)
    int index = 0;
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            matrix->data[i][j] = values[index++];
        }
    }
    
    return matrix;
}

// Function to print the matrix
void print_matrix(Matrix *matrix) {
    if (matrix == NULL) {
        printf("Matrix is NULL\n");
        return;
    }
    
    printf("Matrix (%dx%d):\n", matrix->rows, matrix->cols);
    for (int i = 0; i < matrix->rows; i++) {
        printf("[ ");
        for (int j = 0; j < matrix->cols; j++) {
            printf("%.2f ", matrix->data[i][j]);
        }
        printf("]\n");
    }
}

// Function to free matrix memory
void free_matrix(Matrix *matrix) {
    if (matrix == NULL) {
        return;
    }
    
    for (int i = 0; i < matrix->rows; i++) {
        free(matrix->data[i]);
    }
    free(matrix->data);
    free(matrix);
}

// Function to get element at specific position
double get_element(Matrix *matrix, int row, int col) {
    if (matrix == NULL || row < 0 || row >= matrix->rows || 
        col < 0 || col >= matrix->cols) {
        return 0.0; // or handle error appropriately
    }
    return matrix->data[row][col];
}

// Function to set element at specific position
int set_element(Matrix *matrix, int row, int col, double value) {
    if (matrix == NULL || row < 0 || row >= matrix->rows || 
        col < 0 || col >= matrix->cols) {
        return -1; // error
    }
    matrix->data[row][col] = value;
    return 0; // success
}

// Function to add two matrices
Matrix* add_matrices(Matrix *matrix1, Matrix *matrix2) {
    // Check for NULL matrices
    if (matrix1 == NULL || matrix2 == NULL) {
        printf("Error: Cannot add NULL matrices\n");
        return NULL;
    }
    
    // Assert that dimensions match
    if (matrix1->rows != matrix2->rows || matrix1->cols != matrix2->cols) {
        printf("Error: Matrix dimensions do not match for addition\n");
        printf("Matrix1: %dx%d, Matrix2: %dx%d\n", 
               matrix1->rows, matrix1->cols, matrix2->rows, matrix2->cols);
        return NULL;
    }
    
    // Create result matrix with same dimensions
    Matrix *result = (Matrix*)malloc(sizeof(Matrix));
    if (result == NULL) {
        printf("Error: Memory allocation failed for result matrix\n");
        return NULL;
    }
    
    result->rows = matrix1->rows;
    result->cols = matrix1->cols;
    
    // Allocate memory for row pointers
    result->data = (double**)malloc(result->rows * sizeof(double*));
    if (result->data == NULL) {
        free(result);
        printf("Error: Memory allocation failed for result matrix rows\n");
        return NULL;
    }
    
    // Allocate memory for each row and perform addition
    for (int i = 0; i < result->rows; i++) {
        result->data[i] = (double*)malloc(result->cols * sizeof(double));
        if (result->data[i] == NULL) {
            // Clean up previously allocated rows
            for (int j = 0; j < i; j++) {
                free(result->data[j]);
            }
            free(result->data);
            free(result);
            printf("Error: Memory allocation failed for result matrix row %d\n", i);
            return NULL;
        }
        
        // Add corresponding elements
        for (int j = 0; j < result->cols; j++) {
            result->data[i][j] = matrix1->data[i][j] + matrix2->data[i][j];
        }
    }
    
    return result;
}

// Function to multiply two matrices
Matrix* multiply_matrices(Matrix *matrix1, Matrix *matrix2) {
    // Check for NULL matrices
    if (matrix1 == NULL || matrix2 == NULL) {
        printf("Error: Cannot multiply NULL matrices\n");
        return NULL;
    }
    
    // Assert that dimensions are compatible for multiplication
    // For A × B, columns of A must equal rows of B
    if (matrix1->cols != matrix2->rows) {
        printf("Error: Matrix dimensions incompatible for multiplication\n");
        printf("Matrix1: %dx%d, Matrix2: %dx%d\n", 
               matrix1->rows, matrix1->cols, matrix2->rows, matrix2->cols);
        printf("For multiplication A×B, columns of A (%d) must equal rows of B (%d)\n",
               matrix1->cols, matrix2->rows);
        return NULL;
    }
    
    // Result matrix will be matrix1->rows × matrix2->cols
    Matrix *result = (Matrix*)malloc(sizeof(Matrix));
    if (result == NULL) {
        printf("Error: Memory allocation failed for result matrix\n");
        return NULL;
    }
    
    result->rows = matrix1->rows;
    result->cols = matrix2->cols;
    
    // Allocate memory for row pointers
    result->data = (double**)malloc(result->rows * sizeof(double*));
    if (result->data == NULL) {
        free(result);
        printf("Error: Memory allocation failed for result matrix rows\n");
        return NULL;
    }
    
    // Allocate memory for each row and perform multiplication
    for (int i = 0; i < result->rows; i++) {
        result->data[i] = (double*)malloc(result->cols * sizeof(double));
        if (result->data[i] == NULL) {
            // Clean up previously allocated rows
            for (int j = 0; j < i; j++) {
                free(result->data[j]);
            }
            free(result->data);
            free(result);
            printf("Error: Memory allocation failed for result matrix row %d\n", i);
            return NULL;
        }
        
        // Calculate each element of the result matrix
        for (int j = 0; j < result->cols; j++) {
            result->data[i][j] = 0.0;  // Initialize to zero
            
            // Dot product of row i from matrix1 and column j from matrix2
            for (int k = 0; k < matrix1->cols; k++) {
                result->data[i][j] += matrix1->data[i][k] * matrix2->data[k][j];
            }
        }
    }
    
    return result;
}

// Note: main() function removed to avoid conflicts when linking
// This file now contains only the matrix function implementations
// The main() function should be in your application file (like slp_converter.c)