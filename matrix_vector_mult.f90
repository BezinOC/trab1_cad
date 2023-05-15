program matrix_vector_multiplication
    use iso_fortran_env
    implicit none
    integer, parameter :: min_number = 1
    integer, parameter :: max_number = 10
    integer, parameter :: num_sizes = 32
    integer, parameter :: start_size = 1000
    integer, parameter :: increment_size = 1000
    integer, dimension(num_sizes) :: sizes
    integer :: size, i
    real*8 :: start, finish, cpu_time_used
    integer, dimension(:,:), allocatable :: matrix
    integer, dimension(:), allocatable :: vector, result_vector
    character(len=100) :: row_column_filename = "multiplication_results_row_column_fortran.txt"
    character(len=100) :: column_row_filename = "multiplication_results_column_row_fortran.txt"
    character(len=100) :: error_message

    ! File variables
    integer :: f_row_column, f_column_row

    ! Seed the random number generator
    call random_seed()

    ! Initialize sizes
    do i = 1, num_sizes
        sizes(i) = start_size + (i - 1) * increment_size
    end do

    ! Open files
    open(newunit=f_row_column, file=row_column_filename, status='replace', action='write', iostat=i)
    if (i /= 0) then
        write(error_message, '(a,i0)') "Error opening file '", row_column_filename, "'"
    end if

    open(newunit=f_column_row, file=column_row_filename, status='replace', action='write', iostat=i)
    if (i /= 0) then
        write(error_message, '(a,i0)') "Error opening file '", column_row_filename, "'"
    end if

    write(*, '(a,i0)') "Size of integer pointer: ", storage_size(matrix)
    write(*, '(a,i0)') "Size of integer: ", storage_size(size)

    do i = 1, num_sizes
        size = sizes(i)

        ! Allocate memory for the matrix and fill it with random values
        allocate(matrix(size, size))
        call random_fill_matrix(matrix, size, min_number, max_number)

        ! Allocate memory for the vector and fill it with random values
        allocate(vector(size))
        call random_fill_vector(vector, size, min_number, max_number)

        ! Allocate memory for the result vector
        allocate(result_vector(size))

        ! Perform calculations and measure CPU time used (Row Column Multiplication)
        call cpu_time(start)
        call multiplication_row_column(matrix, vector, result_vector, size)
        call cpu_time(finish)
        cpu_time_used = real(finish - start)
        write(f_row_column, '(i0,f0.6)') size, cpu_time_used

        ! Re-initialize result vector values
        result_vector = 0

        ! Perform calculations and measure CPU time used (Column Row Multiplication)
        call cpu_time(start)
        call multiplication_column_row(matrix, vector, result_vector, size)
        call cpu_time(finish)
        cpu_time_used = real(finish - start)
        write(f_column_row, '(i0,f0.6)') size, cpu_time_used

        ! Deallocate memory
        deallocate(matrix)
        deallocate(vector)
        deallocate(result_vector)

        write(*, '(a,i0)') "Succeeded for size: ", size
    end do

    ! Close files
    close(f_column_row)
    close(f_row_column)

contains

    subroutine random_fill_matrix(matrix, size_of_matrix, min, max)
        integer, intent(in) :: size_of_matrix, min, max
        integer, dimension(size_of_matrix, size_of_matrix), intent(inout) :: matrix
        integer :: i, j, random_num

        do i = 1, size_of_matrix
            do j = 1, size_of_matrix
                random_num = min + int(rand() * real(max - min + 1))
                matrix(i, j) = random_num
            end do
        end do
    end subroutine random_fill_matrix

    subroutine random_fill_vector(vector, size_of_vector, min, max)
        integer, intent(in) :: size_of_vector, min, max
        integer, dimension(size_of_vector), intent(inout) :: vector
        integer :: i, random_num

        do i = 1, size_of_vector
            random_num = min + int(rand() * real(max - min + 1))
            vector(i) = random_num
        end do
    end subroutine random_fill_vector

    subroutine multiplication_row_column(a, x, b, size)
        integer, dimension(:,:), intent(in) :: a
        integer, dimension(:), intent(in) :: x
        integer, dimension(:), intent(inout) :: b
        integer, intent(in) :: size
        integer :: i, j

        do i = 1, size
            do j = 1, size
                b(i) = b(i) + a(i, j) * x(j)
            end do
        end do
    end subroutine multiplication_row_column

    subroutine multiplication_column_row(a, x, b, size)
        integer, dimension(:,:), intent(in) :: a
        integer, dimension(:), intent(in) :: x
        integer, dimension(:), intent(inout) :: b
        integer, intent(in) :: size
        integer :: i, j

        do j = 1, size
            do i = 1, size
                b(i) = b(i) + a(i, j) * x(j)
            end do
        end do
    end subroutine multiplication_column_row

end program matrix_vector_multiplication

