program bubble_sorting
    implicit none

    integer :: in, arr(10)

    ! Initialise the array.
    data arr /1, 313, 423, 5, 75, 23, 58, 987, 24, 10/

    ! Sort the array.
    call bubble_sort(arr)

    ! Display the array.
    do in = 1, size(arr)
        print *, arr(in)
    end do
contains
    ! A subroutine that swaps the values of its arguments.
    subroutine swap(x, y)
        ! The variables whose values we should swap.
        integer, intent(inout) :: x, y
        ! A temporary variable.
        integer:: temp

        ! Swap the variables.
        temp = x
        x = y
        y = temp      
    endsubroutine

    ! An implementation of the bubble sort algorithm.
    subroutine bubble_sort(int_arr)
        ! The array to sort.
        integer, intent(inout) :: int_arr(:)
        ! Loop variables and the length of the array.
        integer :: i, j, len
        ! A flag indicating if the values were swapped.
        logical :: swapped

        ! Get the length of the array.
        len = size(int_arr)

        do i = 1, len - 1
            ! Set the swap flag to false.
            swapped = .false.
            do j = 1, len - i
                if (int_arr(j) > int_arr(j + 1)) then
                    ! Swap the array values at "j" and "j + 1".
                    call swap(int_arr(j), int_arr(j + 1))
                    swapped = .true.
                end if
            end do

            ! If the values weren't swapped, exit the loop.
            if (.not. swapped) then
                exit
            end if
        end do
    end subroutine bubble_sort
end program bubble_sorting
