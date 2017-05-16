(*
** File:          snick_err.ml
** Description:   Functions to raise errors may appear 
**				  in a snick program.
** Last Modified: Tue. 16th May 2017 
** 
** Group name: Mainframe
** 
** Member names   | usernames
** Xianzhuo REN   | xianzhuor 
** Haoyu LIN      | haoyul3
** Zequn MA       | zequnm
*)

open Snick_symbol

(* Potential errors could be raised while analyzing *)
(* main function not found *)
let error_no_main _ = failwith ("No \'main\' procedure definition!")

(* undefined error *)
let error_undefined _ = failwith ("Undefined error!")

(* duplicate procedure defination *)
let error_dup_proc proc_id = 
    failwith ("Proc "^proc_id^" defined more than once!")

(* duplicate variable declaration in scope *)
let error_dup_decl scope id =
    failwith ("Declare "^id^" more than once in proc: "^scope)

(* illegal bound declared for array in scope *)
let error_illegal_bound scope id =
    failwith ("Illegal bound declared for "^id
                ^" in proc: "^scope)

(* variable undeclared in scope *)
let error_undecl_var scope id =
    failwith ("Variable name \'"^id^"\' is not declared in proc: "
                ^scope)

(* assignment type mismatch in scope *)
let error_assign_type_mismatch scope =
    failwith ("Error in proc \'" ^scope
                ^"\': mismatched types for assignment!")

(* illegal indexing for array in scope *)
let error_illegal_index scope id =
    failwith ("Illegal indexing for array \'" ^id^ "\' in proc: "
                ^scope)

(* indexing out of bound *)
let error_idx_out_of_bound scope id =
    failwith ("Array "^id^"index out of bound in proc: "^scope)

(* arguements type mismatch in a procedure call *)
let error_arg_type_mismatch scope id = 
    failwith ("Arguement types mismatch for calling proc \'"
                ^id^"\' in proc: "^scope)

(* number of arguement mismatch in procedure call *)
let error_arg_count_mismatch scope id =
    failwith ("Number of arguements mismatch for calling proc \'"
                ^id^"\' in proc: "^scope)

(* undefined procedure in scope *)
let error_undef_proc scope id = 
    failwith ("Call to procedure \'"^id^"\' is undefined in proc: "^scope)

(* illegal operator for variable of type in scope *)
let error_illegal_optr scope var_type = 
    failwith ("Error in proc \'"^scope
            	^"\': Illegal operation on type "^var_type^".")

(* type mismatch for operator *)
let error_optr_type_mismatch scope = 
    failwith ("Error in proc \'" ^scope
                ^"\': Mismatching types for operation.")

(* invalid operator *)
let error_invalid_operation scope = 
    failwith ("Error in proc \'"^scope^"\': Invalid operation.")
