let ht_size = 20 (* hash table size *)
let top_expr_type = ref None 
let cur_expr_type = ref None
let cur_scope_ht = ref None
let stack_cnt = ref (-1)
let cur_reg_cnt = ref (-1)
let cur_label_cnt = ref 0 (* what is label for *)

let create_ht = Hashtbl.create ht_size



