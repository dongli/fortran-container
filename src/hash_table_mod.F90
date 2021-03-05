module hash_table_mod

  use linked_list_mod

  implicit none

  private

  public create_hash_table
  public hash_table
  public hash_table_type
  public create_hash_table_iterator
  public hash_table_iterator
  public hash_table_iterator_type

  type hash_table_item_type
    type(linked_list_type) chain
  end type hash_table_item_type

  type hash_table_iterator_type
    type(hash_table_type), pointer :: table => null()
    integer key_index
    character(:), allocatable :: key
    character(:), allocatable :: next_key
    class(*), pointer :: value
  contains
    procedure :: ended => hash_table_iterator_ended
    procedure :: next => hash_table_iterator_next
  end type hash_table_iterator_type

  type hash_table_type
    character(30), allocatable :: keys(:)
    type(hash_table_item_type), allocatable :: items(:)
    integer :: chunk_size = 1000
    integer :: size = 0
    real :: max_load_factor = 0.6
  contains
    procedure :: index_number => hash_table_index_number
    procedure :: expand => hash_table_expand
    procedure :: insert => hash_table_insert
    procedure :: value => hash_table_value
    procedure :: hashed => hash_table_hashed
    final :: hash_table_finalize
  end type hash_table_type

contains

  ! key -> hash code -> index
  function hash_code(key)

    character(*), intent(in) :: key
    integer hash_code

    integer i

    hash_code = 0
    do i = 1, len_trim(key)
      hash_code = hash_code + i * iachar(key(i:i))
    end do

  end function hash_code

  ! FIXME: When table is expanded, the index may be changed!
  ! index_number = hash_code mod table_size
  function hash_table_index_number(this, hash_code)

    class(hash_table_type), intent(in) :: this
    integer, intent(in) :: hash_code
    integer hash_table_index_number

    hash_table_index_number = mod(hash_code, size(this%items) + 1) + 1

  end function hash_table_index_number

  subroutine create_hash_table(chunk_size, max_load_factor, table)

    integer, intent(in), optional :: chunk_size
    real, intent(in), optional :: max_load_factor
    type(hash_table_type), intent(out) :: table

    table = hash_table(chunk_size, max_load_factor)

  end subroutine create_hash_table

  function hash_table(chunk_size, max_load_factor)

    integer, intent(in), optional :: chunk_size
    real, intent(in), optional :: max_load_factor
    type(hash_table_type) hash_table

    if (present(chunk_size)) hash_table%chunk_size = chunk_size
    if (present(max_load_factor)) hash_table%max_load_factor = max_load_factor

    allocate(hash_table%keys(hash_table%chunk_size))
    allocate(hash_table%items(hash_table%chunk_size))

  end function hash_table

  subroutine hash_table_expand(this)

    class(hash_table_type), intent(inout) :: this

    print *, '[Error]: hash_table_mod: We need to implement hash_table_expand subroutine!'
    stop 5

  end subroutine hash_table_expand

  subroutine hash_table_insert(this, key, value)

    class(hash_table_type), intent(inout) :: this
    character(*), intent(in) :: key
    class(*), intent(in) :: value

    integer i

    i = this%index_number(hash_code(key))
    if (.not. this%hashed(key)) then
      this%size = this%size + 1
      this%keys(this%size) = key
    end if
    call this%items(i)%chain%insert(key, value, nodup=.true.)

  end subroutine hash_table_insert

  function hash_table_value(this, key)

    class(hash_table_type), intent(in) :: this
    character(*), intent(in) :: key
    class(*), pointer :: hash_table_value

    integer i

    i = this%index_number(hash_code(key))
    hash_table_value => this%items(i)%chain%value(key)

  end function hash_table_value

  function hash_table_hashed(this, key)

    class(hash_table_type), intent(in) :: this
    character(*), intent(in) :: key
    logical hash_table_hashed

    integer i

    i = this%index_number(hash_code(key))
    hash_table_hashed = associated(this%items(i)%chain%value(key))

  end function hash_table_hashed

  subroutine create_hash_table_iterator(table, iter)

    type(hash_table_type), intent(in), target :: table
    type(hash_table_iterator_type) iter

    iter = hash_table_iterator(table)

  end subroutine create_hash_table_iterator

  function hash_table_iterator(table)

    type(hash_table_type), intent(in), target :: table
    type(hash_table_iterator_type) hash_table_iterator

    hash_table_iterator%table => table
    hash_table_iterator%key_index = 1
    if (table%size > 0) then
      hash_table_iterator%value => table%value(table%keys(1))
      hash_table_iterator%key = table%keys(1)
      if (table%size > 1) hash_table_iterator%next_key = table%keys(2)
    end if

  end function hash_table_iterator

  function hash_table_iterator_ended(this)

    class(hash_table_iterator_type), intent(in) :: this
    logical hash_table_iterator_ended

    hash_table_iterator_ended = this%key_index > this%table%size

  end function hash_table_iterator_ended

  subroutine hash_table_iterator_next(this)

    class(hash_table_iterator_type), intent(inout) :: this

    this%key_index = this%key_index + 1
    if (this%key_index <= this%table%size) then
      this%value => this%table%value(this%next_key)
      this%key = this%next_key
      if (this%key_index + 1 <= this%table%size) this%next_key = this%table%keys(this%key_index + 1)
    end if

  end subroutine hash_table_iterator_next

  subroutine hash_table_finalize(this)

    type(hash_table_type), intent(inout) :: this

    if (allocated(this%keys)) deallocate(this%keys)
    if (allocated(this%items)) deallocate(this%items)
    this%size = 0

  end subroutine hash_table_finalize

end module hash_table_mod
