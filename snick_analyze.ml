let isValid = ref true

let rec analyse prog =
    gen_sym_table prog

and gen_sym_table prog = ()