MODULE class_Sudoku_Cell
  implicit none
  type cell
    integer(kind = 2)   :: value, column_num, row_num, square_num, cell_num
    logical             :: modifiable
    type(cell), pointer :: next_modifiable_cell => null(), previous_modifiable_cell=> null()
  end type cell
contains
  function cell_num(row_num, column_num) result (num)
    integer(kind=2), intent(in)  :: row_num, column_num
    integer(kind=2)              :: num
    num = 9 * (row_num - 1) + column_num
  end function cell_num
  function square_num(row_num, column_num) result (num)
    integer(kind = 2), intent(in) :: row_num, column_num
    integer(kind = 2)             :: square_row_num, square_column_num, num
    square_column_num = (column_num - 1) / 3
    square_row_num    = (row_num    - 1) / 3
    num               = (square_row_num * 3) + square_column_num + 1
  end function square_num
  function cell_init(column_num, row_num, value) result(new_cell)
    integer(kind = 2), intent(in) :: column_num, row_num, value
    type(cell) :: new_cell
    new_cell%value      = value
    new_cell%modifiable = value.eq.0
    new_cell%column_num = column_num
    new_cell%row_num    = row_num
    new_cell%square_num = square_num(row_num, column_num)
    new_cell%cell_num   = cell_num(row_num, column_num)
  end function cell_init
END MODULE class_Sudoku_Cell

MODULE class_Sudoku_Grid
  use class_Sudoku_Cell
  implicit none
  private
  integer(kind = 2), parameter :: grid_size = 9
  type (cell), target          :: grid(grid_size,grid_size)
  type (cell), pointer         :: first_modifiable_cell
  public                       :: initialize_grid, print_grid
contains
  subroutine initialize_grid
    integer(kind = 2)    :: row, column, value, num
    type (cell), pointer :: last_modifiable_cell => null()
    open(unit=10, file="sudoku.in")
    do row=1,grid_size
      do column=1,grid_size
        read(10,*) value
        grid(row, column) = cell_init(column, row, value)
        if (associated(last_modifiable_cell) .eqv. .true.) then
          grid(row, column)%previous_modifiable_cell => last_modifiable_cell
          last_modifiable_cell%next_modifiable_cell  => grid(row, column)
        end if
        last_modifiable_cell => grid(row, column)
      enddo
    enddo
  end subroutine initialize_grid

  subroutine print_grid
    integer(kind = 2) :: row, column
    do row=1,grid_size
      write (*,*) (grid(row,column)%value, column=1, grid_size)
    enddo
  end subroutine print_grid

  function row(num) result (ary)
    integer(kind = 2), intent(in) :: num
    type (cell)                   :: ary(9)
    ary = pack(grid(:,num), grid(:,num)%value /= 0)
  end function row

  function column(num) result (ary)
    integer(kind = 2), intent(in) :: num
    type (cell)                   :: ary(9)
    ary = pack(grid(num,:), grid(num,:)%value /= 0)
  end function column

  function square(num) result (ary)
    integer(kind = 2), intent(in) :: num
    type (cell)                   :: ary(9)
  end function square
END MODULE class_Sudoku_Grid

PROGRAM sudoku_backsolver
  use class_Sudoku_Grid
  implicit none
  call initialize_grid
  call print_grid
END PROGRAM sudoku_backsolver
