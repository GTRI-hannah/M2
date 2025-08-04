#ifndef MATRIX_H
#define MATRIX_H

#include <stdio.h>
#include <stdlib.h>

// Matrix structure definition
typedef struct {
    double **data;  // 2D array to store matrix elements
    int rows;       // number of rows
    int cols;       // number of columns
} Matrix;

// Function declarations (prototypes)
Matrix* create_matrix(double *values, int rows, int cols);
void print_matrix(Matrix *matrix);
void free_matrix(Matrix *matrix);
double get_element(Matrix *matrix, int row, int col);
int set_element(Matrix *matrix, int row, int col, double value);
Matrix* add_matrices(Matrix *matrix1, Matrix *matrix2);
Matrix* multiply_matrices(Matrix *matrix1, Matrix *matrix2);

#endif // MATRIX_H