# fortran-container

This repository contains some container data structure types for Fortran. It is currently in alpha stage, so use it for fun!

# Examples

```Fortran
use hash_table_mod

type(hash_table) table
type(hash_table_iterator) iter

table = hash_table() ! Must call this initializer function to allocate internal data.

table%insert('foo', 1)
table%insert('bar', 4.2)

print *, table%size ! Should be 2.

iter = hash_table_iterator(table)
do while (.not. iter%ended())
  select type (value => iter%value)
  type is (integer)
    print *, iter%key, value
  type is (real)
    print *, iter%key, value
  end select
  call iter%next()
end do
```
