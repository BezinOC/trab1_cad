#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void random_fill_matrix(int **matrix, int size_of_matrix, int min, int max) {
    for (int i = 0; i < size_of_matrix; i++) {
        for (int j = 0; j < size_of_matrix; j++) {
            int random_num = 1 + rand() % (max - min + 1);
            matrix[i][j] = random_num;
        }
    }
}

void random_fill_vector(int *vector, int size_of_vector, int min, int max) {
    for (int i = 0; i < size_of_vector; i++) {
            int random_num = min + rand() % (max - min + 1);
            vector[i] = random_num;
    }
}

void multiplication_row_column(int **a, int *x, int *b, int size) {
    for (int i = 0; i < size; i++) {
        for (int j=0; j < size; j++) {
            b[i] = b[i]+a[i][j]*x[j];
        }
    }
}

void multiplication_column_row(int **a, int *x, int *b, int size) {
    for (int j = 0; j < size; j++) {
        for (int i = 0; i < size; i++) {
            b[i] = b[i]+a[i][j]*x[j];
        }
    }
}


int main() {

    int min_number = 1;
    int max_number = 10;
    int start;
    int end;
    double cpu_time_used;
    FILE* f_row_column = fopen("multiplication_results_row_column_c.txt", "w");;
    FILE* f_column_row = fopen("multiplication_resultscolumn_row_c.txt", "w");;

    printf("Size of *int %lu\nSize of int %lu\n\n", sizeof(int*), sizeof(int));
    for (int size = 1000; size <= 32000; size = size + 1000) {

        // Seed the random number generator
        srand(time(0));

        // Allocate memory for the matrix and fill it with random values
        int **matrix = (int **)malloc(size * sizeof(int *));
        if (matrix == NULL) {
            printf("Could not allocate memory for A size %d", size);
            return 1;
        }
        for (int i = 0; i < size; i++) {
            matrix[i] = (int *)malloc(size * sizeof(int));
        }
        random_fill_matrix(matrix, size, min_number, max_number);

        // Allocate memory for the vector and fill it with random values
        int *vector = (int *)malloc(size * sizeof(int));
        if (vector == NULL) {
            printf("Could not allocate memory for X size %d", size);
            return 1;
        }        
        random_fill_vector(vector, size, min_number, max_number);
    
        // Allocate memory for the result vector
        int *result_vector = (int *)malloc(size * sizeof(int)); 
        if (result_vector == NULL) {
            printf("Could not allocate memory for B size %d", size);
            return 1;
        }

        // Perform Calculations and See CPU  Time Used (Row Column Multiplication)
        start = clock();
        multiplication_row_column(matrix, vector, result_vector, size);
        end = clock();
        cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
        fprintf(f_row_column, "%d %f\n", size, cpu_time_used);

        // Re-Initialize Result Vector Values
        for (int i = 0; i < size; i++) {
            result_vector[i] = 0;
        }

        // Perform Calculations and See CPU  Time Used (Column Row Multiplication)
        start = clock();
        multiplication_column_row(matrix, vector, result_vector, size);
        end = clock();
        cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
        fprintf(f_column_row, "%d %f\n", size, cpu_time_used);

        // Deallocate memory
        for (int i = 0; i < size; i++) {
            free(matrix[i]);
        }
        free(matrix);
        free(vector);
        printf("Succeded for %d\n", size);
    }

    fclose(f_column_row);
    fclose(f_row_column);
    return 0;
}
