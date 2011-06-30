#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <idna.h>

static value domain_utf8_to_ascii(value s)
{
    CAMLparam1(s);
    char *input = String_val(s);
    char *output;
    int pos;
    int rc;
    
    rc = idna_to_ascii_8z(input, &output, 0);

    if (rc == IDNA_SUCCESS) {
	CAMLlocal1(res);
	pos = strlen(output);
	res = caml_alloc_string(pos);
	memcpy(String_val(res), output, pos);
	free(output);
	CAMLreturn(res);
    } else {
	caml_invalid_argument(idna_strerror(rc));
	CAMLreturn(0);
    }
}