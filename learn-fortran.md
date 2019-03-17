## Chap. 2

> 2019-03-07 20:20

Fortran has limitations on the number of characters in one line (132), the
number of continued lines (40 for Fortran95 and 256 for Fortran2003), the
number of characters in one variable's name(31 for Fortran95), the number
of characters in the program's name (31).

Statement label: A number between 1 and 99999. It is the "name" of a
Fortran statement, and may be referred to in other place.
```fortran
999 output = input1 + input2
```
Must be unique in one program.

Comment starts with `!`

Character and string are equal in Fortran. String literature can be
surrounded either by `'` or `"`. There is a limitation on the length of
string. The value varies from compiler to compiler.
```fortran
character(len=10) :: first, last   ! first, last with length of 10
character :: initial               ! initial with length of 1
```

Named constants are created by using the `parameter` attribute of a type
declaration statement.
```fortran
real, parameter :: PI = 3.141593
```
Not necessary to declare the length when the string is constant.
```fortran
character, parameter :: err_msg = 'Unknown error'
```

Functions for converting numbers: `int`, `nint`, `ceiling`, `floor`,
`real`.

Exponentiation is an exception to the rule of mixed-mode arithmetic.

The builtin trigonometric functions expect their arguments to be in
radians.

`read(*,*) <input_list>`  
The first field in the parentheses specifies the io unit. The second field
specifies the format.

Each `read` begins reading from a new line of input data.

All variables must be initialized to be portable.

## Chap. 3
>2019-03-08 11:29

Logical constants: `.true.` `.false.`

Relational operator for .ne.: `/=`

When reading logical value from input, a character or a group of characters
beginning with `T` is set to `.true.`, a character or a group of characters
beginning with `F` is set to `.false.`. When writing a logical value to
output, `.true.` will be written as `T`, and `.false.` will be written as
`F`.

Branch statement:  
```fortran
if (logical_expr_1) then
  statement
else if (logical_expr_2) then
  statement
else
  statement
end if
```

A single statement of `if` statement:  
```fortran
if (logical_expr) statement
```

`select case` statement:  
```fortran
select case (temp_c)
case (:-1)   ! specifies a range
  statement
case (0)
  statement  ! do not need `break` statement like C
case (1, 3, 5, 7, 9)   ! specifies a list of values
  statement
case default
  statement
end select
```

## Chap. 4
> 2019-03-08 14:56

Loops:
```fortran
do
  if (expr) exit
  statement
end do
```
```fortran
do while (logical_expr)
  statement
end do
```
```fortran
do index = istart, iend, incr
  statement
end do
```

The index variable of a `do` loop must not be modified anywhere within the
`do` loop.

The index variable of a `do` loop should always be of type integer.

Never depend on an index variable to retain a specific value after a `do`
loop completes normally.

`cycle` == `continue` in C; `exit` == `break` in C.

String concatenation and substring:
```fortran
character(len=10) :: a
character(len=8) :: b, c
a = 'ABCDEFGHIJ'
b = '12345678'
c = a(1:3) // b(4:5) // a(6:8)
```

## Chap. 5
> 2019-03-09 18:48

Formatted write:
```fortran
write (*, 100) i, result
100 format (' The result for iteration ', I3, ' is ', F7.3)
```

Other forms:
```fortran
write(*, 100) i, x
100 format(1X,I6,F10.2)        ! format in `format` statement

character(len=20) :: string
string = '(1X,I6,F10.2)'
write(*, string) i, x          ! format in character variable
write(*, '(1X,I6,F10.2)') i, x ! format in character constant
```

The first character in the format descriptor is control character, which
has special meaning to the output device. It should be avoided to write
this character. This behaviour has been deleted from Fortran 2003 standard.

Integer output: `rIw` or `rIw.m`  
Real output:    `rFw.d`, `rEw.d` or `rESw.d`  
Logical output: `rLw`  
Character output: `rA` or `rAw`  
Horizontal positioning: `nX` or `Tc`

Unless you want to perform a control action, begin every format associated
with a `write` statement with the `1X` descriptor.

Repeating groups of format descriptors:
```fortran
330 format (1x, 2(i6, 2(f10.2, a)))
```

A new line: `/`
```fortran
write (*, 100) index, time
100 format ('1', T20, 'Results for Test Number ', I3,///, &  ! `/` == `\n` in C
            1x, 'Time    = ', F7.0)

Opening a file for input:
```fortran
integer :: ierror
open(unit=8, file='example.dat', status='old', action='read', iostat=error)
```

Opening a file for output:
```fortran
integer :: unit, ierror
character(len=6) :: filename
unit = 25
filename = 'OUTDAT'
open(unit=unit, file=filename, status='NEW', action='WRITE', iostat=ierror)
! or
! open(unit=unit, file=filename, status='REPLACE', action='WRITE', iostat=ierror)
```

Opening a scratch/temp file:
```fortran
open(unit=12, status='SCRATCH', iostate=ierror)
```

```fortran
open(unit=8, file='input.dat', status='old', iostat=ierror)
read(8,*) x, y, z
close(unit=8)
open(unit=9, file='output.dat', status='replace', iostat=ierror)
write(9, 100) x, y, z
100 format(' x = ', F10.2, ' Y = ', F10.2, ' Z = ', F10.2)
close(unit=9)
```

## Chap. 6
> 2019-03-09 23:29

Declare an array:  
```fortran
real, dimension(16) :: voltage
character(20), dimension(50) :: last_name
```

Array constant:
```fortran
(/ 1, 2, 3, 4, 5 /)   ! Fortran 95
[1, 2, 3, 4, 5]       ! Fortran 2003
```

Initialize all of the elements of an array to a single value:
```fortran
real, dimension(10) :: array1
array1 = 0.
```

Initialize an array with implied loop:
```fortran
integer, dimension(5) :: array2 = (/ (i, i = 1, 5) /)
integer, dimension(25) :: array4 = (/ ((0, i = 1, 4), 5*j, j = 1, 5) /)
```

Change the index range of an array:
```fortran
real, dimension(-2:2) :: a
real, dimension(5:9) :: b
```

Two arrays can be used as operands in an intrinsic operation if and only if
they have the same shape, in which case they do an element-by-element
calculation.
```fortran
real, dimension(4) :: a = (/ 1., 2., 3., 4. /)
real, dimension(4) :: b = (/ 5., 6., 7., 8. /)
real, dimension(4) :: c

c = a + b
```

Scalar values are conformable with arrays.
```fortran
real, dimension(4) :: a = (/ 1., 2., 3., 4. /), c
real :: b = 10
c = a * b
```
Most intrinsic functions can take an array as arguments and apply it on an
element-by-element basis.

Array section/subset using subscript triplet:
```fortran
integer, dimension(10) :: array = (/ (i, i = 1, 10) /)
array(1:10:2)
```

Array section/subset using vector subscript:
```fortran
integer, dimension(5) :: vec = (/ 1, 6, 4, 1, 9 /)
integer, dimension(10) :: a = (/ (i, i = 1, 10) /)
a(vec)
```

Implied do loop when reading/writing array elements:
```fortran
write(*, 100) (a(i), i = 1, 5)
100 format(1x, 'a = ', 5F10.2)

write(*, 1000) (i, 2*i, 3*i, i = 1, 3)
1000 format(1x, 9i6)
```

Nested implied do loops:
```fortran
write(*, 100) ((i, j, j = 1, 3), i = 1, 2)
100 format(1x, i5, 1x, i5)
```

Difference between I/O with standard do loops and implied do loops:
```fortran
integer, dimension(5) :: arr = (/ 1, 2, 3, 4, 5 /)
! standard do loop
do i = 1, 5
  write(*, 1000) arr(i), 2.*arr(i), 3.*arr(i)
end do
! implied do loop
write (*, 1000) (arr(i), 2.*arr(i), 3.*arr(i), i = 1, 5)
1000 format(1x, 6i6)
```
Output:
```
1 2 3
2 4 6
3 6 9
4 8 12
5 10 15
1 2 3 2 4 6
3 6 9 4 8 12
5 10 15
```

## Chap. 7
> 2019-03-10 11:09

```fortran
subroutine calc_hypotenuse(side_1, side_2, hypotenuse)
  implicit none
  real, intent(in) :: side_1
  real, intent(in) :: side_2
  real, intent(out)  :: hypotenuse

  real :: temp
  temp = side_1**2 + side_2**2
  hypotenuse = sqrt(temp)
end subroutine calc_hypotenuse
```

Fortran programs communicate with their subroutines using a
pass-by-reference scheme.

Use explicit-shape or assumed-shape dummy arrays in all new procedures.

When a character variable is used as a dummy subroutine argument, the
length of the character variable is declared with an asterisk.

An error flag is usually set as an argument to a subroutine for handling
error in subroutine.

Module is used to contain the definitions and initial values of the data
that we wish to share between program units.
```fortran
module shared_data
  implicit none
  save    ! to avoid race condition (my opinion)
  integer, parameter :: num_vals = 5
  real, dimension(num_vals) :: values
end module shared_data
```

Use module:
```fortran
program test_module
  use shared_data
  implicit none
  real, parameter :: pi = 3.141592
  values = pi * (/ (i, i = 1, 5) /)
  call sub1
end program test_module

subroutine sub1
  use shared_data
  implicit none
  write (*, *) values
end subroutine sub1
```

Module procedures: procedures declared in a module.
```fortran
module my_subs
  implicit none
  ! Declare shared data here
contains
  subroutine sub1(a, b, c, x, error)
    ! subroutine body
  end subroutine sub1
end module my_subs
```
**Type checking only works for function/subroutine defined in a module.**

The name of the function is the returned variable's name in Fortran.

Type declaration of a function:
```fortran
integer function my_function(i, j)

! or

function my_function(i, j)
  integer :: my_function
```

The name of the function should be declared type both in the function and
in the invoking program unit.

The arguments in a function also use the pass-by-reference scheme.

User-defined functions/subroutines as arguments:
```fortran
program :: test
real, external :: fun_1, fun_2   ! external statement is needed
real :: x, y, output
call evaluate(fun_1, x, y, output)
call evaluate(fun_2, x, y, output)
end program test

subroutine evaluate(fun, a, b, result)
real, external :: fun            ! external statement is needed
real, intent(in) :: a, b
real, intent(out) :: result
result = b * fun(a)
end subroutine evaluate
```

## Chap. 8
> 2019-03-12 15:56

Declare a rank-2 array:
```fortran
real, dimension(3, 6) :: sum
integer, dimension(0:100, 0:20) :: hist
character(len=6), dimension(-3:3, 10) :: counts
```

Fortran always allocates array elements in column major order.

| col-1  |  col-2  |
|--------|---------|
|a(1, 1) | a(1, 2) |
|a(2, 1) | a(2, 2) |
|a(3, 1) | a(3, 2) |

In memory:  

| memo-1 | memo-2 | memo-3 | memo-4 | memo-5 | memo-6 |
|--------|--------|--------|--------|--------|--------|
|a(1, 1) |a(2, 1) |a(3, 1) |a(1, 2) |a(2, 2) |a(3, 2) |

Initialize 2D array with `reshape`:
```fortran
istat = reshape((/ 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3 /), (/4, 3/))
```

Initialize 2D array with `read`:
```fortran
integer, dimension(4, 3) :: istat
open(7, file='initial.dat', status='old', action='read')
read(7, *) istat   ! Will read 12 integers in column first order
```

Change the order in which the elements are initialized:
```fortran
integer :: i, j
integer, dimension(4, 3) :: istat
open(7, file='initial.dat', status='old', action='read')
read(7, *) ((istat(i, j) j = 1, 3), i = 1, 4)  ! Will read 12 integers in row first order
```

Two conformable arrays could be used together in arithmetic operations and
assignment statements, which will be performed on an element-by-element
basis.

Array subset in a 2D array:
```fortran
a(:, 1)   ! the first column of array a
a(1, :)   ! the first row of array a
a(1:3, 1:5:2)   ! rows 1 through 3 and columns 1, 3 and 5 from array a
```

Fortran supports arrays with up to *seven* different subscripts.

Some intrinsic elemental functions in Fortran: abs, sin, cos, tan ,exp, log,
log10, mod, sqrt.

Inquiry intrinsic functions are useful for determining the properties of an
array. Some examples: allocated, lbound, shape, size, ubound.

Some transformational functions: all, any, count, dot_product, matmul,
maxloc, maxval, minloc, minval, product, reshape, sum, transpose.

`where` construct:
```fortran
! without `where` construct:
do i = 1, ndim1
  do j = 1, ndim2
    if (value(i, j) > 0.) then
      logval(i, j) = log(value(i, j))
    else
      logval(i, j) = -99999.
    end if
  end do
end do

! with `where` construct:
where (value > 0.)
  logval = log(value)
elsewhere
  logval = -99999.
end where
```

`forall` construct:
```fortran
real, dimension(10, 10) :: i_matrix = 0.
forall (i = 1:10)
  i_matrix(i, i) = 1.0
end forall

forall (i = 1:n, j = 1:m, work(i, j) /= 0.)
  work(i, j) = 1. / work(i, j)
end forall
```

Declare an allocatable array:
```fortran
real, allocatable, dimension(:, :) :: arr1
allocate(arr1(100, 0:10), stat=status)
deallocate(arr1, stat=status)
```
Another example:
```fortran
real, allocatable, dimension(:) :: input_data
if (allocated(input_data)) then
  read(8, *) input_data
else
  write(*, *) 'Warning: Array not allocated!'
end if
deallocate(arr1, stat=status)
```

If an expression is assigned to a Fortran 2003 allocatable array of the
same rank, the array is automatically allocated to the correct shape
if it is unallocated, or it is automatically deallocated and reallocated to
the correct shape if it was previously allocated with an incompatible
shape.
```fortran
real, dimension(:), allocatable :: arr1
real, dimension(8) :: arr2 = (/ 1., 2., 3., 4., 5., 6., 7., 8. /)
real, dimension(3) :: arr3 = (/ 1., -2., 3. /)
arr1 = 2. * arr3
arr1 = arr2(1:8:2)
arr1 = 2. * arr2(1:4)
```

## Chap. 9
> 2019-03-12 20:05

Explicit-shape dummy multidimensional arrays:
```fortran
subroutine process1(data1, data2, n, m)
  integer, intent(in) :: n, m
  real, intent(in), dimension(n, m) :: data1
  real, intent(out), dimension(n, m) :: datat2
  data2 = 3. * data1
end subroutine process1
```

Assumed-shape dummy multidimensional array (only can be used in subroutine
contained in a module):
```fortran
module test_module
contains
  subroutine process2(data1, data2)
    real, intent(in), dimension(:, :) :: data1
    real, intent(out), dimension(:, :) :: data2
    data2 = 3. * data1
  end subroutine process2
end module test_module
```

To ensure a *local* variable unchanged between successive invocations, use
`save` attribute in the type declaration statement, or **initialize the
variable in its type declaration statement**.  
This is like `static` local variables in C. But an initialized local
variable in type declaration statement does not count as static local
variable in C (I guess).

Pure function/subroutine == Thread safe function in C, except pure
function/subroutine must be declared with `pure` in Fortran.

Internal procedure:
```fortran
program test
  ...
  ...
contains
  real function secant(angle)             ! This is an internal function.
    real :: angle                         ! It can only be used in the program body.
    secant = 1. / cos(angle * pi / 180.)
  end function secant
end program test
```

## Chap. 11
> 2019-03-12 21:35

Real type declaration statement with a kind type parameter:
```fortran
real(kind=1) :: value_1
real(kind=4) :: value_2
real(kind=8), dimension(20) :: array
real(4) :: temp
```

Some compiler will use kind=1 to specify float32 and kind=2 to specify
float64. Some will use kind=4 to specify float32 and kind=8 to specify
float64.

## Chap. 12
> 2019-03-13 14:27

Derived datatype in Fortran:
```fortran
type :: person
  character(len=14) :: first_name
  character :: middle_initial
  character(len=14) :: last_name
  character(len=14) :: phone
  integer :: age
  character :: sex
  character(len=11) :: ssn
end type

type(person) :: john, jane
type(person), dimension(100) :: people

john = person('John', 'R', 'Jones', '323-6439', 21, 'M', '123-45-6789')
```

Component selector in Fortran is `%`.
```fortran
john%age = 35

class(5)%final_exam_grade = 95

class(5)%student%age = 23
```

Variable of derived datatype can be read and write using free-format and
custom format io:
```fortran
write(*, *) 'Free format: ', john
```

The addresses of components in a derived datatype are not in order. It is
free to place them anywhere it wants. To make the elements to be in strict
order, use:
```fortran
type :: vector
  sequence
  real :: a
  real :: b
  real :: c
end type
```

Allocatable derived datatype:
```fortran
type(person), allocatable :: john
allocate(john, stat=istat)
```

## Chap. 13
> 2019-03-14 16:09

Recursive subroutine/function:
```fortran
recursive subroutine factorial(n, result)
  implicit none
  integer, intent(in) :: n
  integer, intent(out) :: result
  integer :: temp
  if (n >= 1) then
    call factorial(n-1, temp)
    result = n * temp
  else
    result = 1
  end if
end subroutine
```
```fortran
recursive function fact(n) result(answer)
  implicit none
  integer, intent(in) :: n
  integer :: answer    ! not allowed to have an `intent` attribute in this
line
  if (n == 1) then
    answer = 1
    return
  answer = n * fact(n-1)
end function
```

Use keyword arguments:
```fortran
calc(3., 1., 2.)
calc(first=3., second=1., third=2.)
calc(second=1., third=2., first=3.)
```

The definition of procedure using keyword argument must have explicit
interface (contained in a module):
```fortran
module procs
contains
  real function calc(first, second, third)
  implicit none
  real, intent(in) :: first, second, third
  calc = (first-second)/third
  end function
end module
```

Optional argument:
```fortran
integer, intent(in), optional :: upper_limit
if (present(upper_limit)) then
  ...
else
  ...
end if
```
Procedures with optional arguments must also have explicit interface.

An interface is like a prototype in C. Create an interface:
```fortran
interface
  subroutine sort(array, n)
    implicit none
    real, dimension(:), intent(inout) :: array
    integer, intent(in) :: n
  end subroutine
end interface
```
And then copy this block of code to the program unit you want to use this
subroutine.

Generic function does not actually exist in Fortran. Whenever the compiler
encounters the generic function, it examines the arguments of the function
and invokes the appropriate specific function for those arguments. Generic
functions are declared with `interface`:
```fortran
interface generic_name
  interface1
  interface2
  ...
end interface
```
```fortran
interface sort
  subroutine sorti(array, nvals)
    implicit none
    integer, intent(in) :: nvals
    integer, intent(inout), dimension(nvals) :: array
  end subroutine

  subroutine sortr(array, nvals)
    implicit none
    integer, intent(in) :: nvals
    real(kind=single), intent(inout), dimension(nvals) :: array
  end subroutine
end interface
```

Functions and subroutines can not be mixed in the interface.

Define generic function in which specific functions are in a module:
```fortran
interface sort
  module procedure sorti
  module procedure sortr
end interface
```
And this interface should be placed in the same module.

Generic bound procedures in a derived data type:
```fortran
type :: point
  real :: x
  real :: y
contains
  generic :: add => point_plus_point, point_plus_scalar
end type point
```

User defined operators:
```fortran
interface operator (operator_symbo)
  module procedure function_1  ! or thorough interface for a procedure
  ! more than one procedure can be associated with the same operator
end interface
```
To define assignment operator, use a special interface block:
```fortran
interface assignment (=)
  module procedure subroutine_1   ! must be a subroutine with two arguments
  ! The first argument corresponds to the left-hand side, and must have
intent(out)
  ! The second argument corresponds to the right-hand side, and must have
intent(in)
  ...
end interface
```

Bound assignment and operators:
```fortran
type :: point
  real :: x
  real :: y
contains
  generic :: assignment(=) => assign1
  generic :: operator(+) => plus1, plus2, plus3
end type point
```

Include a `private` statement in each module, and then list the specific
items that you wish to expose in a separate `public` statement.

```fortran
type vector
  private
  real :: x
  real :: y
end type

type, private :: vector
  real :: x
  real :: y
end type

type :: vector
  real, public :: x
  real, private :: y
end type

type :: vector
  real :: x
  real :: y
end type
type(vector), private :: vec_1
```

Restrict access to certain specific items in a module in `use` statement:
```fortran
use module_name, only: only_list
```

Rename items in `use` statement:
```fortran
use data_fit, only: lsqfit => sp_real_least_squares_fit
```

Access to command line arguments:
```fortran
command_argument_count()  ! return the number of command line arguments
get_command(command, length, status)  ! return the entire command line
arguments in `command`
get_command_argument(number, value, length, status)   ! return a specified
command argument
```

Access to environment variables:
```fortran
call get_environment_variable(name, value, length, stats, trim_name)
```

## Chap. 15
> 2019-03-16 19:48

```fortran
prgram test_ptr
  implicit none
  real, pointer :: p  ! declare a pointer
  real, target :: t1 = 10., t2 = -17.  ! declare the targets
  p => t1
  write(*,*) 'p, t1, t2 =', p, t1, t2
  p => t2
  write(*,*) 'p, t1, t2 =', p, t1, t2
end program test_ptr
```

Assignment between two pointers:
```fortran
program test_ptr2
  implicit none
  real, pointer :: p1, p2
  real, target :: t1 = 10., t2 = -17.  ! declare the targets
  p1 => t1
  p2 => p1  ! p2 is not a pointer to a pointer to real. It points t1 directly.
  write(*,'(A,4F8.2)') ' p1, p2, t1, t2 = ', p1, p2, t1, t2
  p1 => t2  ! does not affect p2
  write(*,'(A,4F8.2)') ' p1, p2, t1, t2 = ', p1, p2, t1, t2
end program test_ptr2
```

Pointer association status: undefined, associated, disassociated.
```fortran
nullify(ptr1, ptr2, ...)
status = associated(pointer)
status = associated(pointer, target)
```

Every pointer's status should be clarified as soon as it is created by
either assigning it to a target or nullifying it.
```fortran
real, pointer :: p1 = null(), p2 = null()
integer, pointer :: i1 = null()
```

Whenever a pointer appears in a Fortran expression where a value is
expected, the value of the target is used instead of the pointer itself.
```fortran
p2 = p1  ! both sides expect a variable
! is exactly same as
b = a

p2 => p1 ! the left side expects a pointer; the right side expects a variable 
```

Using pointers with arrays:
```fortran
real, dimension(100, 100), target :: mydata
real, dimension(:, :), pointer :: pointer
pointer => mydata
```
Pointers to array can point to a subset of the array:
```fortran
program array_ptr
  implicit none
  integer :: i
  integer, dimension(16), target :: info = (/ (i, i = 1, 16) /)
  integer, dimension(:), pointer :: ptr1, ptr2, ptr3, ptr4, ptr5
  ptr1 => info
  ptr2 => ptr1(2::2)
  ptr3 => ptr2(2::2)
  ptr4 => ptr3(2::2)
  ptr5 => ptr4(2::2)
end program array_ptr
```
This can only be used with subscript triplets. Compilation error will
occur when using with vector subscripts.

A case of memory leak:
```fortran
program mem_leak
  implicit none
  integer :: i, istat
  integer, dimension(:), pointer :: ptr1, ptr2
  allocate(ptr1(1:10), stat=istat)
  allocate(ptr2(1:10), stat=istat)
  ptr1 = (/ (i, i = 1, 10) /)
  ptr2 = (/ (i, i = 11, 20) /)
  ptr2 => ptr1   ! the memory area pointed by ptr2 has not been deallocated.
  nullify(ptr1)
  deallocate(ptr2, stat=istat)
end program mem_leak
```

If a piece of allocated memory is deallocated, then *all* of the pointers
to that memory should be nullified or reassigned.

**It is not possible to declare an array of pointers in Fortran directly.**
But you can declare an array of derived data type, which only contains a
pointer.

Procedure pointers (only in Fortran 2003):
```fortran
! p is a pointer to a procedure with identical interface with sub1
procedure(sub1), pointer :: p => null()
p => sub1
call sub1(a, b, c)
call p(a, b, c)
```

## Chap. 16
> 2019-03-17 13:07

If an allocatable item, pointer, or dummy argument is declared with the
`class(type)` keyword, then the item will match that data type or any
extension of that data type.
```fortran
type :: point
  real :: x
  real :: y
end type

type, extends(point) :: point_3d
  real :: z
end type

type(point), pointer :: p  ! only accept point target
class(point), pointer :: p  ! accept either type point or type point_3d
```
An item declared with the `class` keyword is said to be polymorphic. But
you can access only items of the declared type with them.

```fortran
class(point), pointer :: p
type(point), target :: p1
type(point_3d), target :: p2

p => p2  ! can not access z with p
```
```fortran
class(*), pointer :: p  ! unlimited polymorphic
```

Example:
```fortran
module complex_class
  implicit none

  private
  
  type, public :: complex_ob
    private
    real :: re
    real :: im
  contains
    private
    procedure :: ac => add_complex_to_complex
    procedure :: ar => add_real_to_complex
    procedure, public :: add => ac, ar
  end type

contains

  subroutine add_complex_to_complex(this, ...)
    class(complex_ob) :: this
    ...
  end subroutine

  subroutine add_real_to_complex(this, ...)
    class(complex_ob) :: this
    ...
  end subroutine

end module
```

Finalizer:
```fortran
type :: vector
  private
  real, dimension(:), pointer :: v
  logical :: v_allocated = .false.
contains
  final :: clean_vector
end type
```
The finalizer will be called when the object is deallocated.

Inherit a class with `extends` attribute. Only public instance variable and
methods can be inherited.

Use `select type` to use unique variables and methods in a subclass:
```fortran
type(point), target :: p2
type(point3d), target :: p3
type(point_temp), target :: pt
class(point), pointer :: p

p => p3

select type(p)
type is (point3d)
  ...
type is (point_temp)
  ...
class is (point)
  ...
end select
```

Preventing methods from being overridden in subclasses:
```fortran
type :: point
  real :: x
  real :: y
contains
  procedure, non_overridable :: my_proc
end type
```

Abstract types are types containing abstract/deferred methods, which are
methods only with interface definitions.
```fortran
type, abstract :: employee  ! abstract attribute
  character(30) :: first_name
  character(30) :: last_name
  character(11) :: ssn
  real :: pay = 0
contains
  procedure(calc_payx), deferred :: calc_pay  ! deferred attribute
end type

abstract interface  ! abstrct interface; can define more than one interface
  real function calc_payx(this, hours)
    implicit none
    class(employee) :: this
    real, intent(in) :: hours
  end function
end interface
```

Concrete classes must override all abstract methods of the superclass, or
they will be abstract themselves. No objects may be instantiated from an
abstract class.
