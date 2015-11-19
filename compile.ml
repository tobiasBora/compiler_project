open Cparse
open Genlab

let sp = Printf.sprintf
let pr = Printf.printf

(* =============== *)
(* === Address === *)
(* =============== *)

(* A label is a string, like ".main" that refers to the position in
   the asm file*)
type label = string
(* An address is a position in the memory. It can be in the text section
   (Global) or in the stack, in function of the base pointer. For example
   the value "Local_bp (-5)" means that it's 5 bytes before the position
   of the base pointer *)
type address = Global of label | Local_bp of int

let string_of_address address = match address with
    Global lbl -> lbl
  | Local_bp n -> sp "%d(%%rbp)" n           (* TODO : check the syntax *)

(* =================== *)
(* === Print error === *)
(* =================== *)
exception Compilation_error of string
exception Uncomplete_compilation_error of string
let compile_raise loc precision =
  let (fn,l1,c1,l2,c2) = loc in
  raise (Compilation_error
           (if l1 = l2 then
              (sp "Error : Compilation error in file %s, line %d, between column %d and %d (%s)." fn l1 c1 c2 precision)
           else (sp "Error : Compilation error in file %s, between line %d and %d, between column %d and %d (%s)." fn l1 c1 c2 precision))

(* =============================== *)
(* === Abstraction version asm === *)
(* =============================== *)
(* %rax
   %rbx ==> Inchangé après appel de fonction
   %rcx ==> Param 4
   %rdx ==> Param 3
   %rsi ==> Param 2
   %rdi ==> Param 1
   %rbp ==> Inchangé après appel de fonction
   %rsp ==> Inchangé après appel de fonction, multiple de 16 avant appel via callq. On l'utilise en principe comme base à la place de rbp.

   %r8 ==> Param 5
   %r9 ==> Param 6
   ...
   %r12 ==> Inchangé après appel de fonction
   %r13
   %r14
   %r15 ==> Inchangé après appel de fonction
*)
           
let mv_cte n address =
  let s = string_of_address address in
  [(sp "movq $%d,%s" n s ,sp "%s := %d" s n)]

let mv_variable src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "%s := %s" dest_s src_s);
   (sp "movq %s,%%r13" src_s, "");
   (sp "movq %%r13,%s" dest_s, "")]

let mv_into_array src index dest = 
  let src_s = string_of_address src in
  let index_s = string_of_address index in
  let dest_s = string_of_address dest in
  [("", sp "%s[%s] := %s" dest_s index_s src_s);
   (sp "movq %s,%%r13" src_c,"");
   (sp "movq %s,%%r14" index_s,"");
   (sp "movq (%%r13, %%r14, 8), %%r14","");
   (sp "movq %%r14, %s" dest_s, "")]

let asm_jmp addr = match addr with
    Local_bp _ -> Uncomplete_compilation_error "I shouldn't jump into a local bp code ! There should be a bug in the compiler."
  | Global lbl ->
    let addr_s = string_of_address addr in
    [(sp "jmpq %s" addr_s, sp "GOTO %s" addr_s)]

let asm_jl addr = 
  Local_bp _ -> Uncomplete_compilation_error "I shouldn't jump into a local bp code ! There should be a bug in the compiler."
| Global _ ->
  let addr_s = string_of_address addr in
  [(sp "jl %s" addr_s, sp "GOTO %s" addr_s)]

(* Ajoute le contenu de addr dans la pile *)
let push_stack addr =
  let addr_s = string_of_address addr in
  [(sp "pushq %s" addr_s, sp "[...] <- %s : [...]" addr_s)]

let push_empty () =
  [(sp "pushq $0" addr_s, sp "[...] <- 0 : [...]")]


(* Enleve le dernier contenu de la pile et le mets dans addr *)
let pop_stack addr =
  let addr_s = string_of_address addr in
  [(sp "popq %s" addr_s, sp "%s <- [...] !! 0")]

let minus src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "%s <- - (%s)" src);
   (sp "movq %s, %%r13" addr_s, "");
   ("neg %%r13", "");
   (sp "movq %%r13, %s" addr_s)]

let not_bit_a_bit src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "%s <- not bit/bit (%s)" src_s);
   (sp "movq %s, %%r13" addr_s, "");
   ("notq %%r13", "");
   (sp "movq %%r13, %s" addr_s)]

(* NB : The following code could be factorized, but I will do it later
   since it doesn't really improve readability *)

(* Here the src variable is incremented, but a copy the first result
   is made in dest *)
let post_inc src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "(%s)++   (resultat temporaire dans %s)" src_s dest_s);
   (sp "movq %s, %%r13" addr_s, " (Passage dans les registres)");
   (sp "movq %%r13, %s" dest_s, " (Sauvegarde ancienne valeur)");
   ("addq $1,%%r13", " (On incrémente en registre)");
   (sp "movq %%r13, %s" src_s, " (On modifie la valeur initiale sur la pile)")]

(* Same as above, but the old value isn't kept *)
let pre_inc src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "++(%s)  , resultat temporaire dans %s" src_s dest_s);
   (sp "movq %s, %%r13" addr_s, " (Passage dans les registres)");
   ("addq $1,%%r13", " (On incrémente en registre)");
   (sp "movq %%r13, %s" dest_s, " (Sauvegarde ancienne valeur)");
   (sp "movq %%r13, %s" src_s, " (On modifie la valeur initiale sur la pile)")]

(* Here the src variable is incremented, but a copy the first result
   is made in dest *)
let post_dec src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "(%s)--   (resultat temporaire dans %s)" src_s dest_s);
   (sp "movq %s, %%r13" addr_s, " (Passage dans les registres)");
   (sp "movq %%r13, %s" dest_s, " (Sauvegarde ancienne valeur)");
   ("subq $1,%%r13", " (On incrémente en registre)");
   (sp "movq %%r13, %s" src_s, " (On modifie la valeur initiale sur la pile)")]

(* Same as above, but the old value isn't kept *)
let pre_dec src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "--(%s)  , resultat temporaire dans %s" src_s dest_s);
   (sp "movq %s, %%r13" addr_s, " (Passage dans les registres)");
   ("subq $1,%%r13", " (On incrémente en registre)");
   (sp "movq %%r13, %s" dest_s, " (Sauvegarde ancienne valeur)");
   (sp "movq %%r13, %s" src_s, " (On modifie la valeur initiale sur la pile)")]

(* Binary operators *)
(* NB : The code could be factorised, but I don't think it's
   clearer so I may do it later *)

(* Multiply to integers *)
let mult x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in 
  [("", sp "%s = %s * %s" dest_s x_s y_s);
   (sp "movq %s, %%r13" x_s, " (Passage dans les registres)");
   (sp "movq %s, %%r14" y_s, " (Passage dans les registres)");
   ("imulq %%r13,%%r14", " (On fait la multiplication)");
   (sp "movq %%r13, %s" dest_s, " (On met le résultat dans la file)")]

(* Division entière (quotient) *)
let asm_div x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in 
  [("", sp "%s = %s / %s" dest_s x_s y_s);
   (sp "movq %s, %%rax" x_s, " (Passage dans les registres)");
   (sp "movq %s, %%r13" y_s, " (Passage dans les registres)");
   ("cqto", " (On prépare la division)");
   ("idivq %%r13", " (On fait la multiplication : quot = %rax; reste = %rdx;)");
   (sp "movq %%rax, %s" dest_s, " (On met le résultat dans la file)")]

(* Division entière (reste) *)
let asm_mod x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in 
  [("", sp "%s = %s %% %s" dest_s x_s y_s);
   (sp "movq %s, %%rax" x_s, " (Passage dans les registres)");
   (sp "movq %s, %%r13" y_s, " (Passage dans les registres)");
   ("cqto", " (On prépare la division)");
   ("idivq %%r13", " (On fait la multiplication : quot = %rax; reste = %rdx;)");
   (sp "movq %%rdx, %s" dest_s, " (On met le résultat dans la file)")]

(* Addition *)
let asm_add x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in 
  [("", sp "%s = %s + %s" dest_s x_s y_s);
   (sp "movq %s, %%r13" x_s, " (Passage dans les registres)");
   (sp "movq %s, %%r14" y_s, " (Passage dans les registres)");
   ("addq %%r13, %%r14", " (On fait l'addition)");
   (sp "movq %%r14, %s" dest_s, " (On met le résultat dans la file)")]


(* Substraction *)
let asm_sub x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in 
  [("", sp "%s = %s - %s" dest_s x_s y_s);
   (sp "movq %s, %%r13" x_s, " (Passage dans les registres)");
   (sp "movq %s, %%r14" y_s, " (Passage dans les registres)");
   ("subq %%r14, %%r13", " (On fait l'addition)");
   (sp "movq %%r13, %s" dest_s, " (On met le résultat dans la file)")]

(* Access to an element of an array a[i] *)
let asm_index a i dest =
  let a_s = string_of_address x in
  let i_s = string_of_address y in
  let dest_s = string_of_address dest in 
  [("", sp "%s <- %s[%s]" dest_s x_s y_s);
   (sp "movq %s, %%r13" a_s, " (Passage dans les registres)");
   (sp "movq %s, %%r14" i_s, " (Passage dans les registres)");
   ("movq (%%r13, %%r14, 8), %%r14", " (On mets a[i] dans %%r14)");
   (sp "movq %%r14, %s" dest_s, " (On mets le resultat sur la pile)")]

(********* Comparaison operators *********)
let general_comp comp_command func asm_bloc x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in
  let cond_bloc_name = genlab func in
  let after_bloc_name = genlab func in
  (* Put to 0 the dest_s, if the jump is done dest_s will become 1 *)
  asm_bloc#add_content_d
    [("", sp "Comparaison %s < %s, result in %s" x_s y_s dest_s);
     (sp "movq %s, %%r13" a_s, " (Passage dans les registres)");
     (sp "movq %s, %%r14" i_s, " (Passage dans les registres)");
     (sp "movq $0, %%r15", " (On mets à 0 le resultat, valeur par deffaut)")
     ("cmpq %%r14, %%r13", " (On compare, attention à l'inversion)");
     (sp "%s %s" comp_command cond_bloc_name, " (On saute si %%r13 < %%r14)");
     (sp "jmpq %s" after_bloc_name, " (On saute directement au bloc suivant sinon)")]
    [];
  (* Create a sub bloc that will be usefull if the condition is realised *)
  let cond_asm_bloc =
    new asm_block cond_bloc_name
      [("", sp "Si on arrive là c'est car on vient du bloc %s pour une comparaison <." asm_bloc#get_block_name);
       ("movq $1, %%r15", " (On mets donc à 1 la valeur de retour de la condition < )");
       (sp "jmpq %s" after_bloc_name," (On revient au bloc suivant)")
      ]
      []
      [] in
  (* Create a bloc that will be used after to continue a the code after
     the condition. The end of the asm_bloc is removed from asm_bloc and
     put at the end of this new bloc. The next instructions will write
     in this bloc. *)
  let after_asm_bloc =
    new asm_block after_bloc_name
      [("","Next block after the condition on bloc %s" asm_bloc#get_block_name);
       (sp "movq %%r15, %s" dest_s, " (Save the result in the stack)")]
      []
      []
  in
  asm_bloc#fork_bloc after_asm_bloc

let asm_lt func asm_bloc x y dest = general_comp "jl"
let asm_le func asm_bloc x y dest = general_comp "jle"
let asm_eq func asm_bloc x y dest = general_comp "je"

(******** Conditions ternaires : EIF(e1,e2,e3) est e1?e2:e3 *********)

let asm_eif func asm_bloc x y =
  


(* ==================== *)
(* === Environments === *)
(* ==================== *)

type var_name = string
(* type env = var_name -> address *)

module Str_map = Map.Make(String)
(* Here is a functionnal object (you cannot edit it) *)
class env my_map offset return_address =
  object(this)
    val map = my_map
    val current_offset = offset
    val current_return_address = return_address
    method get_map =  my_map
    (** Get the address of a variable *)
    method get var_name = Str_map.find var_name my_map
    (** Get string : Same thing as "get |> string_of_address" *)
    method gets var_name =
      Str_map.find var_name my_map |> string_of_address
    (** You should avoid using this function, use add instead *)
    method addf var_name address =
      new env (Str_map.add var_name address my_map) current_offset current_return_address
    (** This function automatiquely get the address
        (next available in the stack). Use var_name = "" if the variable
        is not defined by the user.*)
    method add asm_block var_name =
      asm_block#add_content_d (push_empty ()) [];
      let new_address = Local_bp (current_offset) in
      new env (Str_map.add var_name new_address my_map) (current_offset - 1) new_address
    (** This function automatiquely get the address
        (next available in the stack). Use var_name = "" if the variable
        is not defined by the user. This function doesn't generate any asm code *)
    method add_no_asm var_name =
      let new_address = Local_bp (current_offset) in
      new env (Str_map.add var_name new_address my_map) (current_offset - 1) new_address
    method get_return_address = current_return_address
    method update_return_address address =
      new env map current_offset address
  end



(* ================== *)
(* === Asm Blocks === *)
(* ================== *)
(* Idea : the blocs are asm code lists. You can add the code inside.
*)

class asm_block (block_name : string) (beg_cont : (string * string) list) (end_cont : (string*string) list) (others_blocks : block list) =
  object(this)
    val mutable bloc_name = block_name   
    val mutable begin_content = beg_cont (* (Content * Debug) list*)
    val mutable end_content = end_cont   (* (Content * Debug) list*)
    val mutable others_blocks = others_blocks (* Bloc list *)
    method get_bloc_name = block_name
    method get_begin_content = begin_content
    method get_end_content = end_content
    method get_others_blocks = others_blocks
    method add_block bloc = others_blocks <- (bloc::others_blocks)
    (* Add some content into the block, it takes content list. The debug value is set to the empty string *)
    method add_content beg_b end_b =
      begin_content <- (begin_content @ (List.map (fun s -> (s,"")) beg_b));
      end_content <- ((List.map (fun s -> (s,"")) end_b) @ end_content)
    (* Same as above, but with a field debug (takes Content * Debug list) *)
    method add_content_d beg_b end_b =
      begin_content <- (begin_content @ beg_b);
      end_content <- (end_b @ end_content)
    (* /!\ This function remove the already existing beginning code *)
    method set_begin_content beg_b =
      begin_content <- beg_b
    (* /!\ This function remove the already existing ending code *)
    method set_end_content end_b =
      end_content <- end_b
    method get_this_content_string =
      (sp "%s\n" block_name)
      ^ (List.map (fun (s,debug) -> sp "    %s # %s\n" s debug) begin_content
         |> List.fold_left (^) "")
      ^ (List.map (fun (s,debug) -> sp "    %s # %s\n" s debug) end_content
         |> List.fold_left (^) "")
    method get_content_string =
      (List.map (fun o -> o#get_this_content_string) others_blocks
       |> List.fold_left (^) "")
      ^ (this#get_this_content_string)
        
    (** When a bloc is cut, a new bloc is created. Since the compiler
       must write in this new bloc, the old bloc is saved in "others_blocks"
       while the blocks takes the values of the new block. It returns
        the address of the old block. *)
    method fork_bloc new_bloc =
      let old_bloc =
        new asm_block bloc_name begin_content [] others_blocks
      in
      bloc_name <- new_bloc#get_bloc_name;
      begin_content <- new_bloc#get_begin_content;
      end_content <- (new_bloc#get_end_content) @ (end_content);
      others_blocks <- new_bloc#get_others_blocks;
      (old_bloc)
  end

(* class block (block_name : string) (beg_cont : (string * string) list) (end_cont : (string*string) list) (others_blocks : block list) = *)
(*   object(this) *)
(*     val bloc_name = block_name    *)
(*     val begin_content = beg_cont (\* (Content * Debug) list*\) *)
(*     val end_content = end_cont   (\* (Content * Debug) list*\) *)
(*     val others_blocks = others_blocks (\* Bloc list *\) *)
(*     method get_bloc_name = block_name *)
(*     method get_begin_content = begin_content *)
(*     method get_end_content = end_content *)
(*     method get_others_blocks = others_blocks *)
(*     method add_bloc bloc = new block block_name begin_content end_content (bloc::others_blocks) *)
(*     (\* Add some content into the block, it takes content list. The debug value is set to the empty string *\) *)
(*     method add_content beg_b end_b = *)
(*       new block block_name *)
(*         (begin_content @ (List.map (fun s -> (s,"")) beg_b)) *)
(*         ((List.map (fun s -> (s,"")) end_b) @ end_content) *)
(*         others_blocks *)
(*     (\* Same as above, but with a field debug (takes Content * Debug list) *\) *)
(*     method add_content_d beg_b end_b = *)
(*       new block block_name (begin_content @ beg_b) (end_b @ end_content) others_blocks *)
(*     method get_this_content_string = *)
(*       (sp "%s\n" block_name) *)
(*       ^ (List.map (fun (s,debug) -> sp "    %s # %s\n" s debug) begin_content *)
(*          |> List.fold_left (^) "") *)
(*       ^ (List.map (fun (s,debug) -> sp "    %s # %s\n" s debug) end_content *)
(*          |> List.fold_left (^) "") *)
(*     method get_content_string = *)
(*       (List.map (fun o -> o#get_this_content_string) others_blocks *)
(*        |> List.fold_left (^) "") *)
(*       ^ (this#get_this_content_string) *)
(*   end *)


    (* We try to convert the long expressions into simplest one (only with var_name)
       For example we convert d = (a+b)*(c+1) into
       e = a + b
       f = 1
       g = c + f
       h = e * g
       d = h
    *)
    (* type expr_3 = *)
    (*     VAR3 of address *)
    (*   | CST3 of int *)
    (*   | STRING3 of string *)
    (*   | SET_VAR3 of address * address  (\* SET_VAR a b <=> SET_VAR a = b *\) *)
    (*   (\* TODO : what is the function of SET_ARRAY3 since array aren't in C-- ? *\) *)
    (*   (\*SET_ARRAY3 x i v <=> x[i]=v*\) *)
    (*   | SET_ARRAY3 of address * address * address *)
    (*   (\* TODO : what is the list of CALL ? *\) *)
    (*   | CALL of string * expr_3 list *)
    (*   | OP1_3 of mon_op * address *)
    (*   | OP2_3 of bin_op * address * address *)
    (*   | CMP3 of cmp_op * address * address *)
    (*   | EIF of address * address * address *)
    (*   | ESEQ of expr_3 list *)

    (* let get_new_varname = *)
    (*   let i = ref 0 in *)
    (*   fun () -> sp "%i" *)

    (* let name_TODO env expr = *)
    (*   let  *)
    (*   match expr with *)
    (*     VAR var_name -> (env, env#gets var_name) *)
    (*   | CST n -> *)
    (*     let new_address =  *)



(* ================== *)
(* === Convertion === *)
(* ================== *)
(* Convert an expression into an asm bloc *)
(* sweet family, family *)
(* This return a couple (env, return_address) *)
let asm_block_of_expr func expr env func_env asm_bloc =
  match expr with
  | VAR var_name ->
    begin
      try
        (env, env#get var_name)
      with Not_found -> raise (Uncomplete_compilation_error (sp "The variable %s doesn't exists." var_name))
    end
  | CST n ->
    let new_env = env#add asm_bloc "" in
    let new_address = new_env#get "" in
    (* new_address := n *)
    asm_bloc#add_content_d
      (mv_cte n new_address)
      [];
    (new_env,
     new_address
    )
  | STRING s -> failwith "TODO : What should I do ???"
  | SET_VAR (var_name, (loc,expr1)) ->
    begin
      try
        (* xxx <- eval(expr)*)
        let (new_env, src_addr) =
          asm_block_of_expr func expr1 env func_env asm_bloc in
        (* Where is var_name ? *)
        let dest_addr = env#get var_name in
        (* var_name <- xxx *)
        asm_bloc#add_content_d
          (mv_variable src_addr dest_addr)
          [];
        (* Check : it may works if you just give env instead of new_env, *)
        (* but optimisations are for later. *)
        (new_env, dest_addr)
      with
        Not_found -> compile_raise loc (sp "Var %s doesn't exists and cannot be assigned." var_name)
      | Uncomplete_compilation_error er -> compile_raise loc er
    end
  | SET_ARRAY (var_name, (loc1, expr1), (loc2, expr2)) -> (** affectation x[e]=e'. *)
    begin
      try
        (* xxx <- eval(expr1)*)
        let (new_env1, index_addr) =
          asm_block_of_expr func expr1 env func_env asm_bloc in
        (* yyy <- eval(expr2)*)
        let (new_env2, src_addr) =
          asm_block_of_expr func expr2 new_env1 func_env asm_bloc in
        (* Where is var_name ? *)
        let dest_addr = new_env2#get var_name in
        (* var_name[xxx] <- yyy *)
        asm_bloc#add_content_d
          (mv_into_array src_addr index_addr dest_addr)
          [];
        (* Check : it may works if you just give env instead of new_env, *)
        (* but optimisations are for later. *)
        (new_env2, dest_addr)
      with
        Not_found -> compile_raise loc (sp "Var %s doesn't exists and cannot be assigned." var_name)
      | Uncomplete_compilation_error er -> compile_raise loc er
    end
  | CALL (f_name, loc_expr_l) -> (** appel de fonction f(e1,...,en) *)
    begin
      (* On évalue les arguments de droite à gauche, et on met les
         résultats dans la liste des addresses de retour dans end_addr_list.
         La liste de retour est volontairement "à l'envers" (dernier argument
         vers premier) car on empile ainsi
         --- Haut de pile ---
         [...]
         argument 3
         argument 2
         argument 1
         Adresse de retour dans le code asm (empilé par call)
         sauvegarde dernier EBP (c'est la première chose que doit faire une fonction)
         variable locale 1
         variable locale 2
         ...
         ===> suite du programme que l'on empile

      *)
      let (end_env, rev_end_addr_list) =
        List.fold_left (fun (curr_env, addr_list) (locn,exprn) ->
            let (new_env, new_addr) = asm_block_of_expr func exprn curr_env func_env asm_bloc in
            (new_env, new_addr::addr_list)) env loc_expr_l
      in
      (* TODO : est-ce qu'on le fait vraiment de droite à gauche ??? *)
      (* On empile les arguments sur la pile
         comme expliqué sur la convention de pile :
         http://www.cs.virginia.edu/~evans/cs216/guides/stack-convention.png
      TODO : en X86 on est censé empiler d'abord dans les registres...*)
      (* List.iter (fun addr -> asm_bloc#add_content (push_stack addr) []) end_addr_list; *)
      (* On appelle ensuite f avec call ? *)
      
    
    end
  | OP1 (op, (loc1, expr1)) ->
    (** OP1(mop, e) dénote -e, ~e, e++, e--, ++e, ou --e. *)
    begin
      (* On évalue l'expression *)
      let (env1, expr1_addr) =
        try
          asm_block_of_expr func expr1 env func_env asm_bloc
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      (* Ajoute adresse temporaire pour le resultat *)
      let new_env = env1#add asm_bloc "" in
      let result_addr = new_env#get "" in
      match op with
        M_MINUS ->
        begin
          asm_bloc#add_content_d
            (minus expr1_addr result_addr);
          (new_env, result_addr)
        end
      | M_NOT ->
        begin
          asm_bloc#add_content_d
            (not_bit_a_bit expr1_addr result_addr);
          (new_env, result_addr)
        end
      | M_POST_INC ->
        begin
          asm_bloc#add_content_d
            (post_inc expr1_addr result_addr);
          (new_env, result_addr)
        end
      | M_PRE_INC ->
        begin
          (* Here the result_addr could be the same as the expr1,
             but anyway  *)
          asm_bloc#add_content_d
            (pre_inc expr1_addr result_addr);
          (new_env, result_addr)
        end
      | M_POST_DEC ->
        begin
          asm_bloc#add_content_d
            (post_dec expr1_addr result_addr);
          (new_env, result_addr)
        end
      | M_PRE_DEC ->
        begin
          (* Here the result_addr could be the same as the expr1,
             but anyway  *)
          asm_bloc#add_content_d
            (pre_dec expr1_addr result_addr);
          (new_env, result_addr)
        end
    end
  | OP2 (op, (loc1, expr1), (loc2, expr2)) ->
    begin
      (** OP2(bop,e,e') dénote e*e', e/e', e%e',
                             e+e', e-e', ou e[e']. *)
      (* On évalue les expressions *)
      let (env1, expr1_addr) =
        try
          asm_block_of_expr func expr1 env func_env asm_bloc
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      let (env2, expr2_addr) =
        try
          asm_block_of_expr func expr1 env1 func_env asm_bloc
        with Uncomplete_compilation_error er -> compile_raise loc2 er
      in
      (* Ajoute adresse temporaire pour le resultat *)
      let new_env = env2#add asm_bloc "" in
      let result_addr = new_env#get "" in
      match op with
        S_MUL ->
        begin
          asm_bloc#add_content_d
            (mult expr1_addr expr2_addr result_addr);
          (new_env, result_addr)
        end
      | S_DIV ->
        begin
          asm_bloc#add_content_d
            (asm_div expr1_addr expr2_addr result_addr);
          (new_env, result_addr)
        end
      | S_MOD ->
        begin
          asm_bloc#add_content_d
            (asm_mod expr1_addr expr2_addr result_addr);
          (new_env, result_addr)
        end
      | S_ADD ->
        begin
          asm_bloc#add_content_d
            (asm_add expr1_addr expr2_addr result_addr);
          (new_env, result_addr)
        end
      | S_SUB ->
        begin
          asm_bloc#add_content_d
            (asm_sub expr1_addr expr2_addr result_addr);
          (new_env, result_addr)
        end
      | S_INDEX ->
        begin
          asm_bloc#add_content_d
            (asm_index expr1_addr expr2_addr result_addr);
          (new_env, result_addr)
        end
    end
  | CMP (op, (loc1,expr1), (loc2, expr2)) ->
    begin 
      (** CMP(cop,e,e') vaut e<e', e<=e', ou e==e' *)
      (* On évalue les expressions *)
      let (env1, expr1_addr) =
        try
          asm_block_of_expr func expr1 env func_env asm_bloc
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      let (env2, expr2_addr) =
        try
          asm_block_of_expr func expr1 env1 func_env asm_bloc
        with Uncomplete_compilation_error er -> compile_raise loc2 er
      in
      (* Ajoute adresse temporaire pour le resultat *)
      let new_env = env2#add asm_bloc "" in
      let result_addr = new_env#get "" in
      match op with
        C_LT ->
        begin
          asm_lt func asm_bloc expr1_addr expr2_addr result_addr;
          (new_env, result_addr)
        end
      | C_LE ->
        begin
          asm_le func asm_bloc expr1_addr expr2_addr result_addr;
          (new_env, result_addr)
        end
      | C_EQ ->
        begin
          asm_eq func asm_bloc expr1_addr expr2_addr result_addr;
          (new_env, result_addr)
        end
    end
  | EIF ((loc1,expr1), (loc2, expr2), (loc3,expr3)) ->
    begin
      (** EIF(e1,e2,e3) est e1?e2:e3 *)
      (* On évalue les expressions *)
      let (env1, expr1_addr) =
        try
          asm_block_of_expr func expr1 env func_env asm_bloc
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      (* Ajoute adresse temporaire pour le resultat *)
      let new_env = env#add asm_bloc "" in
      let result_addr = new_env#get "" in
      (* asm_eif func asm_bloc  *)
      (* TODO *)
    end
  | ESEQ l_expr ->
    begin
      (** e1, ..., en [sequence, analogue a e1;e2 au niveau code];
          si n=0, represente skip. *)
      match l with
        [] -> (env, return_address)
      | (loc1,expr1)::r ->
        begin
          let (env1, expr1_addr) =
            try
              asm_block_of_expr func expr1 env func_env asm_bloc
            with Uncomplete_compilation_error er -> compile_raise loc1 er
          in
          asm_block_of_expr func (ESEQ r) env1 func_env asm_bloc
        end
    end





(*
func is "" if we are not in a function, else it contains the name of
the function
(env, func_env) *)
let asm_block_of_code func code env func_env asm_bloc =
  (* Declaration of variables/function.
     It returns a new env, func_env, and the asm_bloc is modified. *)
  let asm_block_of_var_declaration var_decl env func_env asm_bloc =
    match var_decl with
      CDECL (loc, var_name) ->
      begin
        (* TODO : deal with local functions ? *)
        env#add asm_bloc var_name
      end
    | CFUN (loc, func_name, var_decl_l, (next_loc, code)) ->
      if func <> "" then
        compile_raise loc ("The function %s want to be declared inside the function %s... Pretty strange !" func_name func)
      else
        begin
          (* Create a new label for the function *)
          let lbl = genlab func_name in
          (* Add it in the list of functions *)
          next_func_env = func_env#addf func_name (Global genlab);
          (* TODO : do the declaration of the function variables *)
          (* TODO : correct everything *)
          (* Create the content of the bloc *)
          let new_asm = new asm_bloc lbl [] [] [] in
          try
            (* Eval the inside of the function *)
            let (env,last_func_env) =
              asm_block_of_code func code end_env func_env new_asm in
            (* Add it to the main asm block *)
            asm_bloc#add_block new_asm;
            (env,last_func_env)
          with Uncomplete_compilation_error er -> compile_raise next_loc er
        end
  in    



  (* Main part *)
  match code with
    CBLOCK (var_decl_l, loc_code_l) ->
    begin
      (* We look for the declaration (function or variable) *)
      List.fold_left (fun (env2,func_env2) var_decl ->
          let (env3, func_env3) = asm_block_of_var_declaration var_decl env2 func_env2 asm_bloc in
          (env3, func_env3)
        ) (env, func_env) var_decl_l
    end
  | CEXPR (loc,expr) ->
    begin
      (** une expression e; vue comme instruction. *)
      let (env2, _) = asm_block_of_expr func expr env func_env asm_bloc in
      (env2, func_env)
    end
  | CIF of loc_expr * loc_code * loc_code (** if (e) c1; else c2; *)
  | CWHILE ((loc_e, expr), (loc_c, code2)) -> (** while (e) c1; *)
    asm_block_of_code
      (CBLOCK ([],
               [(loc_e, CIF ((loc_e,expr),
                             (loc_c, code2),
                             (loc_c, CBLOCK ([],[]))))]))
      env func_env asm_bloc
  | CRETURN of loc_expr option (** return; ou return (e); *)


(* =================== *)
(* === Compilation === *)
(* =================== *)
let compile out decl_list =
  Printf.fprintf out "TODO\n"
