typedef long value;

#define Long_val(v) ((v) >> 1)
#define Val_long(n) (((long)(n) << 1) + 1)
#define Int_val(v) ((v) >> 1)
#define Val_int(n) (((n) << 1) + 1)

#define Is_int(v) ((v) & 1)
#define Is_block(v) (((v) & 1) == 0)

typedef unsigned long header_t;

#define Header_val(v) *((header_t *)(v) - 1)
#define Tag_header(h) ((h) & 0xFF)
#define Size_header(h) ((h) >> 11)
#define Tag_val(v) Tag_header(Header_val(v))
#define Size_val(v) Size_header(Header_val(v))

#define Field(v, n) (((value *)(v))[n])

#define Double_val(v) *((double *)(v))

#define No_scan_tag 0xFB

#define Closure_tag 0xFA
#define Double_tag 0xFB
#define String_tag 0xFC
#define Abstract_tag 0xFD
#define Finalized_tag 0xFE
#define Infix_tag 0xFF

#define Modified_mask 0x400

#define Val_false 1
#define Val_true 3
#define Val_unit 1
