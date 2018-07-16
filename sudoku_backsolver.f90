PROGRAM sudoku_backsolver
  implicit none

  integer(kind = 2), parameter :: nsize = 9
  integer(kind = 2)            :: grid(nsize,nsize) = 0
  integer(kind = 2)            :: i, j

  open(unit=10, file="sudoku.in")
  do i=1,nsize
    do j=1,nsize
      read(10,*) grid(i,j)
    enddo
  enddo

  do i=1,nsize
    write (*,*) (grid(i,j), j=1, nsize)
  enddo
END PROGRAM sudoku_backsolver
