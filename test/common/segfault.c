#include <caml/mlvalues.h>
#include <caml/memory.h>

value caml_cause_segfault(value unit)
{
    CAMLparam1 (unit);
    int *ptr = NULL;
    *ptr = 1;
    CAMLreturn (Val_unit);
}
