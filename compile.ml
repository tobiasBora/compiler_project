(* http://www.lsv.fr/~baelde/prog1/log_all.txt *)

open Cparse
open Genlab

(*
=== Notation and definition ===
   In my code, the variables whose name is .__my_variable_name__ are global,
   but not accessible for the user (a 'real' variable name cannot begin with
   an equal sign). In the same idea, the variables that have the name
   .my_variable_name are local variable name.

   I will call "state" of a program the value of the registers
   rsp (stack pointer) and rip (instruction pointer). I save it in local
   variable (notation with .) and global variable (notation with .__) in
   order to be able to give them to functions.
   
=== Deal with exceptions ===

   To deal with exceptions, the idea I will use is that when I enter
   into a try bloc, I will put in local variables .last_try_rsp and
   .last_try_rip the state of the program, and then put a
   conditionnal jump. If a global variable .__exception_name__ is null, I
   jump in the content of the bloc, if it's not I go in the "catch"
   statement. The first time I enter in such a bloc, .__exception_name__
   will always have the value 0, but when an exception will be raised,
   I will firstly set .__exception_name__ to a not null value (the value
   depends on the name of the exception), and then jump back into the
   last try block that has been found by restoring the state
   function. Like that, the conditionnal jump will be accepted and the
   program will go in the catch statement.

=== Deal with return ===
   The return instruction cannot behave as usual since the finally must be
   read. The idea is to consider return has an exception with the value
   1. Since it is never catched, the behaviour respects the semantic.
   You just need to put at the beginning of the function of try a little
   bit specific that matchs the return :
   try
   {
      ... inside of function ...
   }
   catch(.return n) { return n }
   with return a "real" return here.

Variables used :
   .__exception_name__
   .__exception_value__
   
   .__rsp_before_call__
   .__rip_before_call__
   .last_try_rsp
   .last_try_rip




En pseudo code :

Lancer une exception:
   mettre à jour exception_name
   changer exception_value
   restaurer l'état dans .last_try_rsp puis .last_try_rip.


On appelle état (sous entendu local) les variables
.last_try_rsp et .last_try_rip
et etat global les variables
.__rsp_before_call__ et .__rip_before_call__

Appel de fonction :
   etat --> global
   appel
   global --> etat

BLOC_1:
   creer --retour_bool-- et --retour_val--
   sauvegarder etat
   si exc = 0 alors BLOC_try
   si exn = 1 alors bloc_exc_1
   si exn = 2 alors bloc_exc_2
   si exn = 3 alors bloc_exc_3
   sinon bloc_finally avec ancien état

BLOC_try:
   bloc_classique
     val de retour si existe dans --retour_val--
       PUIS (au cas ou exception relancée) mettre --retour_bool-- à 1
   bloc_finally

bloc_exc_n:
   contenu
   bloc_finally

bloc_finally:
   contenu du bloc
   si --retour_bool-- (vient de bloc_try) alors bloc_finally_return
   si .__exception_name__ non nul, alors relancer l'exception via un bloc finally_exc
   sinon suite du bloc courant

   
bloc_finally_return:
   retourner --retour_val--


======= TODO =======
En pseudo code :

On appelle état (sous entendu local) les variables
.last_try_rsp et .last_try_rip
et etat global les variables
.__rsp_before_call__ et .__rip_before_call__

BLOC_1:
   sinon BLOC_exc

BLOC_try:
     val de retour si existe dans --retour_val--
       PUIS (au cas ou exception relancée) mettre --retour_bool-- à 1
   
BLOC_exc:
   restaurer ancien etat dans global
   si exn = 1 alors bloc_exc_1
   si exn = 2 alors bloc_exc_2
   si exn = 3 alors bloc_exc_3
   sinon bloc_finally


bloc_exc_n:
   contenu
   bloc_finally

A réfléchir :
   Attention aux try imbriqués et aux return !!!
   Modification rax possible ?
   *)

(* If Ocaml version < 4.00 *)
let (|>) a f = f a
    
let sp = Printf.sprintf
let pr = Printf.printf

(* Return the first n elements if possible and the ending elements *)
let rec take_list n l = match n,l with
    _,[] -> ([],[])
  | 0,_  -> ([],l)
  | _,x::r -> let (a,b) = (take_list (n-1) r) in (x::a, b)

let rec zip l r = match l,r with
    [],_ -> []
  | _,[] -> []
  | x::r,y::s -> (x,y)::(zip r s)

let rec gen_list a b =
  if a <= b then
    a :: (gen_list (a+1) b)
  else 
    []

(* This function is usefull to be able to put accent.
   I cannot use String.escaped because it escape in
   decimal while I need here octal.
     XXX joli souci du détail
*)
let octal_escaped s =
  let count = ref 0 in
  let len = String.length s in
  for i = 0 to len - 1 do
    match s.[i] with
    | '\n' | '"' | '\\' ->
      count := !count + 1
    | c when c < ' ' || c > '\x7F' ->
      count := !count + 3
    | _ -> ()
  done;
  if !count = 0 then s
  else
    let s' = Bytes.create (len + !count) in
    let j = ref 0 in
    for i = 0 to len - 1 do
      match s.[i] with
      | '"' | '\\' as c ->
        Bytes.set s' (!j + 0) '\\';
        Bytes.set s' (!j + 1) c;
        j := !j + 2
      | '\n' ->
        Bytes.set s' (!j + 0) '\\';
        Bytes.set s' (!j + 1) 'n';
        j := !j + 2
      | c when c < ' ' || c > '\x7F' ->
        let c = Char.code c in
        Bytes.set s' (!j + 0) '\\';
        Bytes.set s' (!j + 1) Char.(unsafe_chr (code '0' + (c / 64) land 0x7));
        Bytes.set s' (!j + 2) Char.(unsafe_chr (code '0' + ( c / 8) land 0x7));
        Bytes.set s' (!j + 3) Char.(unsafe_chr (code '0' + (     c) land 0x7));
        j := !j + 4
      | c ->
        Bytes.set s' !j c;
        incr j
    done;
    s';;

(* =================== *)
(* === Print error === *)
(* =================== *)
exception Compilation_error of string
exception Uncomplete_compilation_error of string

let string_of_loc loc =
  let (fn,l1,c1,l2,c2) = loc in
  if l1 = l2 then
    sp "file %s, line %d, between column %d and %d" fn l1 c1 c2
  else
    sp "file %s, between line %d and %d, between column %d and %d" fn l1 l2 c1 c2

let compile_raise loc precision =
  raise (Compilation_error (sp "Compilation error in %s (%s)." (string_of_loc loc) precision))


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
type address = Global of label | Local_bp of int | String of label | Register of string | Stdlib of string

let string_of_address address = match address with
    Global lbl -> lbl
  | Local_bp n -> sp "%d(%%rbp)" (8*n)
  | String lbl -> "$" ^ lbl
  | Register s -> sp "%%%s" s
  | Stdlib s -> sp "%s(%%rip)" s
  (* | Array (root_addr, ) *)

let address_of_argument n_arg = match n_arg with
    1 -> Register "rdi"
  | 2 -> Register "rsi"
  | 3 -> Register "rdx"
  | 4 -> Register "rcx"
  | 5 -> Register "r8"
  | 6 -> Register "r9"
  | n -> raise (Uncomplete_compilation_error "This argument must be put on the stack")

(* ================== *)
(* === Asm Blocks === *)
(* ================== *)
(* Idea : the blocs are asm code lists. You can add the code inside.
 * XXX un peu plus de doc, ou un système un peu plus simple, aurait facilité
 * ma lecture, mais en gros j'ai compris à l'usage comment ça marche et je
 * vois que dans plusieurs cas ça fait des choses utiles
*)

class asm_block (block_name' : string) (beg_cont' : (string * string) list) (end_cont' : (string*string) list) (others_blocks_after' : asm_block list) =
  object(this)
    val mutable bloc_name = block_name'
    val mutable before_anything = [(".text","Be sure that the function fact isn't in the .data section")] (* Default value *)
    val mutable begin_content = beg_cont' (* (Content * Debug) list*)
    val mutable end_content = end_cont'   (* (Content * Debug) list*)
    val mutable others_blocks_before = [] (* Bloc list *)
    val mutable others_blocks_after = others_blocks_after' (* Bloc list *)
    method get_block_name = bloc_name
    method get_begin_content = begin_content
    method get_end_content = end_content
    method get_others_blocks_before = others_blocks_before
    method get_others_blocks_after = others_blocks_after
    method get_before_anything =
      before_anything
    method set_before_anything (l : (string*string) list) =
      before_anything <- l
    method set_others_blocks_before bl = others_blocks_before <- bl
    method set_others_blocks_after bl = others_blocks_after <- bl
    method add_block (bloc : asm_block) =
      others_blocks_after <- (others_blocks_after @ [bloc]);
      if bloc#get_before_anything = [] then
        bloc#set_before_anything before_anything
      else ()
    method add_block_before (bloc : asm_block) =
      if bloc#get_before_anything = [] then
        bloc#set_before_anything before_anything
      else ();
      others_blocks_after <- (others_blocks_after @ [bloc]);
    method add_block_after (bloc : asm_block) =
      if bloc#get_before_anything = [] then
        bloc#set_before_anything before_anything
      else ();
      others_blocks_after <- (others_blocks_after @ [bloc]);
    (* Add some content into the block, it takes content list. The debug value is set to the empty string *)
    method add_content beg_b end_b =
      begin_content <- (begin_content @ (List.map (fun s -> (s,"")) beg_b));
      end_content <- ((List.map (fun s -> (s,"")) end_b) @ end_content)
    (* Same as above, but with a field debug (takes Content * Debug list) *)
    method add_content_d beg_b end_b =
      begin_content <- (begin_content @ beg_b);
      end_content <- (end_b @ end_content)
    method add_ending_content_d end_b =
      end_content <- (end_content @ end_b)
    (* /!\ This function remove the already existing beginning code *)
    method set_begin_content beg_b =
      begin_content <- beg_b
    (* /!\ This function remove the already existing ending code *)
    method set_end_content end_b =
      end_content <- end_b
    method get_this_content_string =
      ("##############\n")
      ^ (List.map (fun (s,debug) -> sp "    %s # %s\n" s debug) before_anything
       |> List.fold_left (^) "")
      ^ (sp "%s:\n" bloc_name)
      ^ (List.map (fun (s,debug) -> sp "    %s # %s\n" s debug) begin_content
         |> List.fold_left (^) "")
      ^ (List.map (fun (s,debug) -> sp "    %s # %s\n" s debug) end_content
         |> List.fold_left (^) "")
      ^ ("#-------------\n")

    method get_content_string =
      (List.map (fun o -> o#get_content_string) others_blocks_before
       |> List.fold_left (^) "")
      ^ (this#get_this_content_string)
      ^ (List.map (fun o -> o#get_content_string) others_blocks_after
         |> List.fold_left (^) "")
    (** When a bloc is cut, a new bloc is created. Since the compiler
       must write in this new bloc, the old bloc is saved in "others_blocks"
       while the blocks takes the values of the new block. It returns
        the address of the old block. *)
    method fork_block (new_bloc : asm_block) : (asm_block) =
      let old_bloc =
        new asm_block bloc_name begin_content [] others_blocks_after
      in
      old_bloc#set_others_blocks_before (others_blocks_before);
      old_bloc#set_before_anything before_anything;
      
      bloc_name <- new_bloc#get_block_name;
      begin_content <- new_bloc#get_begin_content;
      end_content <- (new_bloc#get_end_content) @ (end_content);
      others_blocks_before <- (old_bloc :: new_bloc#get_others_blocks_before);
      others_blocks_after <- (new_bloc#get_others_blocks_after);
      (old_bloc)
  end


(* Ajoute le contenu de addr dans la pile *)
let asm_push_stack addr =
  let addr_s = string_of_address addr in
  [(sp "pushq %s" addr_s, sp "[...] <- %s : [...]" addr_s)]

let asm_push_empty var_name current_offset =
  [("pushq $0", sp " Push an empty value on the stack to store the variable \"%s\" at the pos [%d]." var_name (8*current_offset))]

let asm_pop_nowhere () =
  [("add $8, %rsp", "Remove an element from the stack")]



(* ==================== *)
(* === Environments === *)
(* ==================== *)

type var_name = string
(* type env = var_name -> address *)

module Str_map = Map.Make(String)
(* Here is a persistent object (you cannot edit it) *)
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
        is not defined by the user. It also write the asm code to push/pop
        the value *)
    method add (asm_block : asm_block) var_name =
      (* When a variable is pushed, you need to pop it later *)
      let new_address = Local_bp (current_offset) in
      let new_address_s = string_of_address new_address in
      asm_block#add_content_d
        [(sp "movq $0, %s" new_address_s ,sp "On mets 0 par défaut dans la variable \"%s\"" var_name);
         (sp "leaq %s, %%rsp" new_address_s, sp "On bouge le stack pointer pour être sûr de ne pas écraser la variable \"%s\" par la suite" var_name)]
        (* XXX OK, mais... pourquoi pas un simple addq/subq sur %rsp ? *)
        (* (asm_push_empty var_name current_offset) *)
        (* []; *)
        [(sp "leaq %s, %%rsp" (string_of_address (Local_bp (current_offset+1))), sp "On dé-initilise la variable %s." var_name)];
        (* XXX OK *)
        (* (asm_pop_nowhere ()); *)
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
    method mem key =
      Str_map.mem key my_map
  end

(* ================== *)
(* === Exceptions === *)
(* ================== *)
(* Since exceptions are global for the whole program,
   I need a structure that can assign an integer to
   an exception if it has already been defined, and that
   returns a new integer not already used else.
*)
module Str_int_map = Map.Make(String)
class exc () =
  object(this)
    val mutable exc_counter = 0
    val mutable exc_map = Str_int_map.empty
    method get_map = exc_map
    method int_of_name exc_name =
      try
        Str_int_map.find exc_name exc_map
      with Not_found ->
        (exc_counter <- exc_counter + 1;
         exc_map <- Str_int_map.add exc_name exc_counter exc_map;
         exc_counter)
  end
(* 0 = nothing, 1 = return, 2,3... = others exceptions *)
let glob_exc = new exc ()
let _ = glob_exc#int_of_name ".return"


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
                                                                                    XXX ah bon? ça n'a pas l'air d'être le cas (ouf)

   %r8 ==> Param 5
   %r9 ==> Param 6
   ...
   %r12 ==> Inchangé après appel de fonction
   %r13
   %r14
   %r15 ==> Inchangé après appel de fonction
*)

(* On évalue les arguments de droite à gauche, et on met les
   résultats dans la liste des addresses de retour dans end_addr_list.
   La liste de retour est volontairement "à l'envers" (dernier argument
   vers premier) car on empile ainsi
   Arguments 1-6 dans les registres, puis
   --- Haut de pile ---
   [...]
   argument 9
   argument 8
   argument 7
   Adresse de retour dans le code asm (empilé par call)
   sauvegarde dernier EBP (c'est la première chose que doit faire une fonction)
   variable locale 1
   variable locale 2
   ...
   ===> suite du programme que l'on empile

*)


let mv_gen str1 str2 =
  [(sp "movq %s,%s" str1 str2,sp "%s := %s" str2 str1)]  
         
let mv_cte n address =
  let s = string_of_address address in
  [(sp "movq $%d,%s" n s ,sp "%s := %d" s n)]

let mv_variable src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "%s := %s" dest_s src_s);
   (sp "movq %s,%%r13" src_s, "");
   (sp "movq %%r13,%s" dest_s, "")]

let mv_into_array src dest_var index dest = 
  let src_s = string_of_address src in
  let index_s = string_of_address index in
  let dest_var_s = string_of_address dest_var in
  let dest_s = string_of_address dest in
  [("", sp "%s[%s] := %s" dest_var_s index_s src_s);
   (sp "movq %s,%%r15" src_s, "");
   (sp "movq %s,%%r13" dest_var_s,"");
   (sp "movq %s,%%r14" index_s,"");
   (sp "movq %%r15, (%%r13, %%r14, 8)","");
   (sp "movq %%r15,%s" dest_s," Usefull when you have nested arrays")]

let mv_arg_into_stack n_arg dest =
  let dest_s = string_of_address dest in
  [(sp "movq %s,%s" (n_arg |> address_of_argument |> string_of_address) dest_s, sp " (Put the argument %d into the stack)" n_arg)]

let asm_jmp addr = match addr with
    Global lbl ->
    begin
      let addr_s = string_of_address addr in
      [(sp "jmp %s" addr_s, sp "GOTO %s" addr_s)]
    end
  | _ -> raise (Uncomplete_compilation_error "I shouldn't jump into a local bp code ! There should be a bug in the compiler.")

let asm_jl addr = match addr with
  | Global _ ->
    let addr_s = string_of_address addr in
    [(sp "jl %s" addr_s, sp "GOTO %s" addr_s)]
  | _ -> raise (Uncomplete_compilation_error "I shouldn't jump into a local bp code ! There should be a bug in the compiler.")


(* Enleve le dernier contenu de la pile et le mets dans addr *)
let asm_pop addr =
  let addr_s = string_of_address addr in
  [(sp "popq %s" addr_s, sp "%s <- [...] !! 0")]


let retsae rav =
  if rav = "\101\103\103\115"
  then
    (* XXX ^^
     *   j'avoue que j'ai mis un moment à me demander si je devais tester
     *   ça, craignant que ce ne soit pas juste de l'ascii art mais peut
     *   être un hack... j'espère que ce n'était pas le cas ;) *)
    pr "\027[1;32m\010\010\010\032\032\032\032\032\032\032\032\032\046\045\039\045\046\032\032\032\032\032\032\032\032\032\032\032\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\010\032\032\032\032\032\032\032\032\032\092\032\032\032\124\032\032\032\095\095\095\095\032\032\032\047\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\092\010\032\032\032\032\032\032\032\032\032\032\092\032\032\124\032\032\047\032\032\032\032\041\032\040\032\032\069\103\103s??\032\032Wh\101\114\101\032\116h\101\032h\101ll\032\032\041\010\032\032\032\032\032\032\032\032\032\032\032\092\032\124\032\047\032\046\045\039\034\032\032\032\092\095\032\032\032\032a\114\101\032my\032ca\114\114\111\116s?!\032\032\032\047\010\032\032\032\032\032\032\032\032\032\032\032\095\092\124\047\046\039\032\032\032\111\040\041\040\095\095\041\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\047\010\032\032\032\032\032\032\032\032\032\032\047\095\095\095\095\095\092\095\010\032\032\032\032\032\032\032\032\032\047\039\045\045\045\045\124\095\047\095\010\032\032\032\032\032\032\032\032\124\032\032\032\032\032\032\032\032\032\0320\010\032\032\032\032\032\032\032\032\032\092\032\032\032\032\095\095\058\058\047\032\095\032\032\032\032\095\095@\095\095\010\032\032\032\032\032\032\032\032\047\032\032\032\032\032\092\032\032H\032\040\095\041\032\032\047\032\095\032\032\095\092\010\032\032\032\032\032\032\032\047\032\032\032\032\040\095\124\095\045\045\045\124\095\047\032\047\095\040\095\041\040\095\041\092\010\032\032\032\032\032\032\047\032\032\092\095\095\095\095\032\032\092\046\045\039\032\032\047\040\095\041\040\095\041\040\095\041\092\032\032\032\032\032\032\045Dijks\116\101\114\045\010\032\032\095\032\032\047\032\032\032\032\032\032\032\041\092\095\047\032\032\032\032\124\047\092\092\047\047\092\092\047\047\092\092\124\010\032\047\032\092\047\032\032\047\032\032\032\040\047\095\032\032\032\032\032\032\032\124\047\092\092\047\047\092\092\047\047\092\092\124\010\032\092\095\047\092\095\040\095\095\095\095\095\095\095\041\032\032\032\032\032\032\124\047\092\092\047\047\092\092\047\047\092\092\124\010\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\032\034\034\034\034\034\034\034\034\034\034\034\034\034\027\010[5m\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\095\010\010\010\027[0;m"
  else ()


(******** Function CALL *********)
let asm_call_mv_arg asm_bloc env src n_arg =
  let src_s = string_of_address src in
  if n_arg < 7 then
    begin
      let dest_s = string_of_address (address_of_argument n_arg) in
      asm_bloc#add_content_d
        [(sp "movq %s, %s" src_s dest_s, sp " (Argument %d)" n_arg)]
        [];
      (env, n_arg - 1)
    end
  else
    begin
      asm_bloc#add_content_d
        [(sp "movq %s, %%r13" src_s, sp " (Argument %d en registre)" n_arg);
         ("pushq %r13", sp " (Argument %d sur la pile)" n_arg)]
        [];
      (env, n_arg-1)
    end

let asm_call_call f_name =
  [(sp "callq %s" f_name, " (Appel de la fonction)")]

let asm_call_save_result is_64 dst =
  let dst_s = string_of_address dst in
  if is_64 then
    [(sp "movq %%rax,%s" dst_s," (Save the result of the function)")]
  else
    [("movslq %eax, %rax","Convert the result in 32 bits"); (* XXX *from* 32 bits *)
     (sp "movq %%rax,%s" dst_s," (Save the result of the function)")]

(******** Opérators with one argument *********)

let minus src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "%s <- - (%s)" src_s dest_s);
   (sp "movq %s, %%r13" src_s, "");
   ("neg %r13", "");
   (sp "movq %%r13, %s" dest_s, "")]

let not_bit_a_bit src dest =
  let src_s = string_of_address src in
  let dest_s = string_of_address dest in 
  [("", sp "%s <- not bit/bit (%s)" dest_s src_s);
   (sp "movq %s, %%r13" src_s, "");
   ("notq %r13", "");
   (sp "movq %%r13, %s" dest_s, "")]

(* NB : The following code could be factorized, but I will do it later
   since it doesn't really improve readability *)

(* Here the src variable is incremented, but a copy the first result
   is made in dest *)
let post_inc (src_get,src_set) dest =
  let dest_s = string_of_address dest in
  ([("", "Post inc")]
   @ src_get
   @ [(sp "movq %%r13, %s" dest_s, " (Sauvegarde ancienne valeur)");
      ("addq $1,%r13", " (On incrémente en registre)")]
   @ src_set)

(* Same as above, but the old value isn't kept *)
let pre_inc (src_get,src_set) dest =
  let dest_s = string_of_address dest in
  ([("", "Pre inc")]
   @ src_get
   @ [ ("addq $1,%r13", " (On incrémente en registre)");
       (sp "movq %%r13, %s" dest_s, " (Sauvegarde ancienne valeur)")]
   @ src_set)

(* Here the src variable is incremented, but a copy the first result
   is made in dest *)
let post_dec (src_get,src_set) dest =
  let dest_s = string_of_address dest in
  ([("", "Post decrementation")]
   @ src_get
   @ [(sp "movq %%r13, %s" dest_s, " (Sauvegarde ancienne valeur)");
      ("subq $1,%r13", " (On incrémente en registre)")]
   @ src_set)

(* Same as above, but the old value isn't kept *)
let pre_dec (src_get,src_set) dest =
  let dest_s = string_of_address dest in
  ([("", "Pre decrementation")]
   @ src_get
   @ [ ("subq $1,%r13", " (On incrémente en registre)");
       (sp "movq %%r13, %s" dest_s, " (Sauvegarde ancienne valeur)")]
   @ src_set)

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
   ("imulq %r13,%r14", " (On fait la multiplication)");
   (sp "movq %%r14, %s" dest_s, " (On met le résultat dans la file)")]

(* Division entière (quotient) *)
let asm_div x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in 
  [("", sp "%s = %s / %s" dest_s x_s y_s);
   (sp "movq %s, %%rax" x_s, " (Passage dans les registres)");
   (sp "movq %s, %%r13" y_s, " (Passage dans les registres)");
   ("cqto", " (On prépare la division)");
   ("idivq %r13", " (On fait la multiplication : quot = %rax; reste = %rdx;)");
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
   ("idivq %r13", " (On fait la multiplication : quot = %rax; reste = %rdx;)");
   (sp "movq %%rdx, %s" dest_s, " (On met le résultat dans la file)")]

(* Addition *)
let asm_add x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in 
  [("", sp "%s = %s + %s" dest_s x_s y_s);
   (sp "movq %s, %%r13" x_s, " (Passage dans les registres)");
   (sp "movq %s, %%r14" y_s, " (Passage dans les registres)");
   ("addq %r13, %r14", " (On fait l'addition)");
   (sp "movq %%r14, %s" dest_s, " (On met le résultat dans la file)")]


(* Substraction *)
let asm_sub x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in 
  [("", sp "%s = %s - %s" dest_s x_s y_s);
   (sp "movq %s, %%r13" x_s, " (Passage dans les registres)");
   (sp "movq %s, %%r14" y_s, " (Passage dans les registres)");
   ("subq %r14, %r13", " (On fait l'addition)");
   (sp "movq %%r13, %s" dest_s, " (On met le résultat dans la file)")]

(* Access to an element of an array a[i] *)
let asm_index a i dest =
  let a_s = string_of_address a in
  let i_s = string_of_address i in
  let dest_s = string_of_address dest in 
  [("", sp "%s <- %s[%s]" dest_s a_s i_s);
   (sp "movq %s, %%r13" a_s, " (Passage dans les registres)");
   (sp "movq %s, %%r14" i_s, " (Passage dans les registres)");
   ("movq (%r13, %r14, 8), %r14", " (On mets a[i] dans %r14)");
   (sp "movq %%r14, %s" dest_s, " (On mets le resultat sur la pile)")]

(********* Comparaison operators *********)
(* Petite astuce à regarder, merci Laurent
   ==> le mettre sur les conditions si motivé *)
(* ("setl %al"," (On lit directement la valeur de la comparaison et on les mets dans les 8 bits les plus à droites du registre rax)"); *)
(* (sp "movsbq %al, " ,"")] *)

let general_comp comp_command func asm_bloc x y dest =
  let x_s = string_of_address x in
  let y_s = string_of_address y in
  let dest_s = string_of_address dest in
  let cond_bloc_name = genlab func in
  let after_bloc_name = genlab func in
  (* Put to 0 the dest_s, if the jump is done dest_s will become 1 *)
  asm_bloc#add_content_d
    [("", sp "Comparaison %s <-> %s, result in %s" x_s y_s dest_s);
     (sp "movq %s, %%r13" x_s, " (Passage dans les registres)");
     (sp "movq %s, %%r14" y_s, " (Passage dans les registres)");
     (sp "movq $0, %%r15", " (On mets à 0 le resultat, valeur par defaut)");
     ("cmpq %r14, %r13", " (On compare, attention à l'inversion)");
     (sp "%s %s" comp_command cond_bloc_name, " (On saute si %r13 < %r14)");
     (sp "jmp %s" after_bloc_name, " (On saute directement au bloc suivant sinon)")]
    [];
  (* Create a sub bloc that will be usefull if the condition is realised *)
  let cond_asm_bloc =
    new asm_block cond_bloc_name
      [("", sp "Si on arrive là c'est car on vient du bloc %s pour une comparaison <." asm_bloc#get_block_name);
       ("movq $1, %r15", " (On mets donc à 1 la valeur de retour de la condition < )");
       (sp "jmp %s" after_bloc_name," (On revient au bloc suivant)")
      ]
      []
      [] in
  asm_bloc#add_block_after cond_asm_bloc;
  (* Create a bloc that will be used after to continue a the code after
     the condition. The end of the asm_bloc is removed from asm_bloc and
     put at the end of this new bloc. The next instructions will write
     in this bloc. *)
  let after_asm_bloc =
    new asm_block after_bloc_name
      [("",sp "Next block after the condition on bloc %s" asm_bloc#get_block_name);
       (sp "movq %%r15, %s" dest_s, " (Save the result in the stack)")]
      []
      []
  in
  ignore(asm_bloc#fork_block after_asm_bloc)

let asm_lt func asm_bloc x y dest =
  general_comp "jl" func asm_bloc x y dest
let asm_le func asm_bloc x y dest =
  general_comp "jle" func asm_bloc x y dest
let asm_eq func asm_bloc x y dest =
  general_comp "je" func asm_bloc x y dest

(******** Conditions ternaires : EIF(e1,e2,e3) est e1?e2:e3 *********)

let asm_eif_part1 asm_bloc expr1_addr name_cond1 name_cond2 =
  let expr1_s = string_of_address expr1_addr in
  asm_bloc#add_content_d
    [("","Debut de asm_eif_part_1");
     (sp "movq %s, %%r13" expr1_s, " (Sauvegarde en registres)");
     ("cmpq $0,%r13", " (Comparaison avec 0)");
     (sp "je %s" name_cond2, sp " (Si la condition n'est *pas* respectée, GOTO %s)" name_cond2);
     (sp "jmp %s" name_cond1, sp " (Si la condition est respectée, GOTO %s" name_cond2)]
    []
  
(* ******** Condition ******** *)

let asm_condition_part1 func asm_bloc src dest_asm1 dest_asm2 =
  let src_s = string_of_address src in
  asm_bloc#add_content_d
    [("","Début de la condition");
     (sp "movq %s, %%r13" src_s," (Sauvegarde en registres)");
     ("cmpq $0, %r13", " (Comparaison)");
     (sp "je %s" dest_asm2, sp " (Si la condition n'est *pas* respectée, GOTO %s)" dest_asm2);
    (sp "jmp %s" dest_asm1, sp " (Si la condition est respectée, GOTO %s)" dest_asm1)]
    []

(* ******** While loop ******** *)
(* This function only return (string*string) list *)
let asm_while_part1 cond_asm_bloc return_addr main_asm_name after_asm_name =
  let return_s = string_of_address return_addr in
  cond_asm_bloc#add_content_d
    [(sp "cmpq $0,%s" return_s," (Compare the condition with 0)")]
    []


(* ================== *)
(* === Convertion === *)
(* ================== *)


(* Convert an expression into an asm bloc *)
(* sweet family, family *)
(* This return a couple (env, return_address, (real get code, real set code) option) *)
let rec asm_block_of_expr func expr env func_env asm_bloc last_loc : (env * address * (((string * string) list) * ((string * string) list)) option) =
  match expr with
  | VAR var_name ->
    begin
      try
        let addr = env#get var_name in
        let new_env = env#add asm_bloc "" in
        let tmp_addr = new_env#get "" in
        asm_bloc#add_content_d
          (mv_variable addr tmp_addr)
          [];
        let real_s = string_of_address addr in
        (new_env, tmp_addr,
         Some ([(sp "movq %s, %%r13" real_s, sp "Read the real value of the var %s and put it in the registers." var_name )],
               [(sp "movq %%r13, %s" real_s, sp " (On modifie la valeur initiale de %s sur la pile)" var_name)]))
      with Not_found -> (pr "\027[1;33mWarning\027[0;m : The variable %s isn't declared in %s. I hope it's defined in libc...\n" var_name (string_of_loc last_loc); retsae var_name; (env, Stdlib var_name, None))
    end
  | CST n ->
    let new_env = env#add asm_bloc "" in
    let new_address = new_env#get "" in
    (* new_address := n *)
    asm_bloc#add_content_d
      (mv_cte n new_address)
      [];
    (new_env, new_address, None)
  | STRING str ->
    begin
      let str = octal_escaped str in
      let bloc_name = genlab "GLOB" in
      let var_asm_bloc =
        new asm_block bloc_name
          [(sp ".string \"%s\"" str," (Set a default value of 0)")]
          []
          []
      in
      var_asm_bloc#set_before_anything
        [(".data",sp "Définir la chaine constante globale \"%s\"" str);
         (".align 8","")];
      asm_bloc#add_block_before var_asm_bloc;
      (env, String bloc_name, None)
    end
  | SET_VAR (var_name, (loc,expr1)) ->
    begin
      try
        (* xxx <- eval(expr)*)
        let (env1, src_addr, _) =
          asm_block_of_expr func expr1 env func_env asm_bloc loc in
        (* Where is var_name ? *)
        let dest_addr = env#get var_name in
        (* The returned value isn't the dest_addr because it's possible
           to edit the same variable in the same expression *)
        let env2 = env1#add asm_bloc "" in
        let return_value_addr = env2#get "" in
        (* var_name <- xxx *)
        asm_bloc#add_content_d
          ((mv_variable src_addr dest_addr)
           @ (mv_variable src_addr return_value_addr))
          [];
        (env2, return_value_addr, None)
      with
        Not_found -> compile_raise loc (sp "Var %s doesn't exists and cannot be assigned." var_name)
      | Uncomplete_compilation_error er -> compile_raise loc er
    end
  | SET_ARRAY (var_name, (loc1, expr1), (loc2, expr2)) -> (** affectation x[e]=e'. *)
    begin
      try
        (* yyy <- eval(expr2)*)
        let (env2, src_addr, _) =
          asm_block_of_expr func expr2 env func_env asm_bloc loc2 in
        (* xxx <- eval(expr1)*)
        let (env3, index_addr, _) =
          asm_block_of_expr func expr1 env2 func_env asm_bloc loc1 in
        (* Where is var_name ? *)
        let dest_var_addr = env3#get var_name in

        let env4 = env3#add asm_bloc "" in
        let dest_addr = env4#get "" in
        (* var_name[xxx] <- yyy *)
        asm_bloc#add_content_d
          (mv_into_array src_addr dest_var_addr index_addr dest_addr)
          [];
        (* Check : it may works if you just give env instead of new_env, *)
        (* but optimisations are for later. *)
        (env4, dest_addr, None)
      with
        Not_found -> compile_raise loc1 (sp "Var %s doesn't exists and cannot be assigned." var_name)
      | Uncomplete_compilation_error er -> compile_raise loc1 er
    end
  | CALL (f_name, loc_expr_l) -> (** appel de fonction f(e1,...,en) *)
    begin
      (* Save the exception state *)
      asm_bloc#add_content_d
        [(sp "movq %s,%%r8" (env#gets ".last_try_rsp"),"Save the exc state before function call (rsp)");
         (sp "movq %%r8,%s" (env#gets ".__rsp_before_call__"),"  Save rsp");
         (sp "movq %s,%%r8" (env#gets ".last_try_rip"),"  Save the exc state before function call (rip)");
         (sp "movq %%r8,%s" (env#gets ".__rip_before_call__"),"  Save rip");
        ]
        [];
      (* Get all the asm bloc of arguments *)
      let (end_env, rev_end_addr_list) =
        List.fold_left (fun (curr_env, addr_list) (locn, exprn) ->
            let (new_env, new_addr, _) = asm_block_of_expr func exprn curr_env func_env asm_bloc locn in
            (new_env, new_addr::addr_list)) (env,[]) (List.rev loc_expr_l)
      in
      let end_addr_list = List.rev rev_end_addr_list in
      let n = List.length end_addr_list in
      (* Mets les arguments sur les registres et la pile *)
      (* Aligne le CALL *)
      let n_arg = List.length loc_expr_l in
      let n_arg_on_stack = if n_arg <= 6 then 0 else n_arg - 6 in
      (* NB : This code could really be factorized. I'm just checking it
          works before doing it*)
      let restore_after_call =
        if (n_arg_on_stack mod 2 = 1) then
          begin
            (* If we add an odd number of elements on the stack *)
            (* We align at 8 *)
            asm_bloc#add_content_d
              [("","Nombre d'argument impair, on aligne à 8");
               ("movq %rsp,%r13", "Sauve l'ancienne valeur du pointeur de liste");
               ("pushq %r13","Push cette valeur deux fois sur la pile");
               ("pushq %r13","Push cette valeur deux fois sur la pile");
               ("movq %rsp,%r14", "Sauve la nouvelle valeur du pointeur de liste");
               ("subq $8, %r14","... et l'aligne à 0+8 en 2 temps ((0x28 - 0x8) .|. 8)");
               ("orq $8, %r14","... et l'aligne à 0+8 en 2 temps ((0x28 - 0x8) .|. 8)");
               ("movq %r14, %rsp","...puis le remet dans le pointeur de liste")]
              [];
            [("","Restaure le pointeur de liste");
             (sp "movq %d(%%rsp),%%rsp" (8*(n_arg_on_stack + 1))," Reprends l'ancienne valeur de la pile")]
          end
        else
          begin
            (* If we add an even number of elements on the stack *)
            (* We align at 16 *)
            asm_bloc#add_content_d
              [("","Nombre d'argument pair, on aligne à 16");
               ("movq %rsp,%r13", "Sauve l'ancienne valeur du pointeur de liste");
               ("pushq %r13","Push cette valeur deux fois sur la pile");
               ("pushq %r13","Push cette valeur deux fois sur la pile");
               ("movq %rsp,%r14", "Sauve la nouvelle valeur du pointeur de liste");
               ("andq $-16, %r14","... et l'aligne à 16");
               ("movq %r14, %rsp","...puis le remet dans le pointeur de liste")]
              [];
            [("","Restaure le pointeur de liste après le call");
             (sp "movq %d(%%rsp),%%rsp" (8*(n_arg_on_stack + 1))," Reprends l'ancienne valeur de la pile")]

          end
      in
      (* Remets après les derniers arguments sur la liste *)
      let (_, _) = List.fold_left
          (fun (tmp_env, n_arg) src ->
             asm_call_mv_arg asm_bloc tmp_env src n_arg
          )
          (end_env, (n))
          end_addr_list
      in
      (* Appel de la fonction *)
      asm_bloc#add_content_d
        ((asm_call_call f_name)
         @ restore_after_call)
        [];
      let after_call_env = end_env#add asm_bloc "" in
      let dst = after_call_env#get "" in
      asm_bloc#add_content_d
        (asm_call_save_result (List.mem f_name ["malloc"; "realloc"; "calloc"] || func_env#mem f_name) dst)
        [];
      (after_call_env, dst, None)
    end
  | OP1 (op, (loc1, expr1)) ->
    (** OP1(mop, e) dénote -e, ~e, e++, e--, ++e, ou --e. *)
    begin
      (* On évalue l'expression *)
      let (env1, expr1_addr, expr_1_real_addr) =
        try
          asm_block_of_expr func expr1 env func_env asm_bloc loc1
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      (* Ajoute adresse temporaire pour le resultat *)
      let new_env = env1#add asm_bloc "" in
      let result_addr = new_env#get "" in
      match op with
        M_MINUS ->
        begin
          asm_bloc#add_content_d
            (minus expr1_addr result_addr) [];
          (new_env, result_addr, None)
        end
      | M_NOT ->
        begin
          asm_bloc#add_content_d
            (not_bit_a_bit expr1_addr result_addr) [];
          (new_env, result_addr, None)
        end
      | _ ->
        begin
          (* In this part the variable is modified if possible *)
          let get_set_code =
            match expr_1_real_addr with
              None -> raise (Uncomplete_compilation_error "Sorry I don't have any address meaning for this expression (it must be a variable name or an array address).")
            | Some g_s_code -> g_s_code
          in
          match op with
            M_POST_INC ->
            begin
              asm_bloc#add_content_d
                (post_inc get_set_code result_addr) [];
              (new_env, result_addr, expr_1_real_addr)
            end
          | M_PRE_INC ->
            begin
              (* Here the result_addr could be the same as the expr1,
                 but anyway *)
              asm_bloc#add_content_d
                (pre_inc get_set_code result_addr) [];
              (new_env, result_addr, expr_1_real_addr)
            end
          | M_POST_DEC ->
            begin
              asm_bloc#add_content_d
                (post_dec get_set_code result_addr) [];
              (new_env, result_addr, expr_1_real_addr)
            end
          | M_PRE_DEC ->
            begin
              (* Here the result_addr could be the same as the expr1,
                 but anyway  *)
              asm_bloc#add_content_d
                (pre_dec get_set_code result_addr) [];
              (new_env, result_addr, expr_1_real_addr)
            end
          | _ -> raise (Uncomplete_compilation_error "The matching above isn't complete ???")
        end
    end
  | OP2 (op, (loc1, expr1), (loc2, expr2)) ->
    begin
      (** OP2(bop,e,e') dénote e*e', e/e', e%e',
                             e+e', e-e', ou e[e']. *)
      (* On évalue les expressions *)
      let (env1, expr2_addr,_) =
        try
          asm_block_of_expr func expr2 env func_env asm_bloc loc1
        with Uncomplete_compilation_error er -> compile_raise loc2 er
      in
      let (env2, expr1_addr,_) =
        try
          asm_block_of_expr func expr1 env1 func_env asm_bloc loc2
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      (* Ajoute adresse temporaire pour le resultat *)
      let new_env = env2#add asm_bloc "" in
      let result_addr = new_env#get "" in
      match op with
        S_MUL ->
        begin
          asm_bloc#add_content_d
            (mult expr1_addr expr2_addr result_addr) [];
          (new_env, result_addr, None)
        end
      | S_DIV ->
        begin
          asm_bloc#add_content_d
            (asm_div expr1_addr expr2_addr result_addr) [];
          (new_env, result_addr, None)
        end
      | S_MOD ->
        begin
          asm_bloc#add_content_d
            (asm_mod expr1_addr expr2_addr result_addr) [];
          (new_env, result_addr, None)
        end
      | S_ADD ->
        begin
          asm_bloc#add_content_d
            (asm_add expr1_addr expr2_addr result_addr) [];
          (new_env, result_addr, None)
        end
      | S_SUB ->
        begin
          asm_bloc#add_content_d
            (asm_sub expr1_addr expr2_addr result_addr) [];
          (new_env, result_addr, None)
        end
      | S_INDEX ->
        begin
          (* play with array ! *)
          asm_bloc#add_content_d
            (asm_index expr1_addr expr2_addr result_addr) [];
          let expr1_addr_s = string_of_address expr1_addr in
          let expr2_addr_s = string_of_address expr2_addr in
          (new_env, result_addr,
           Some (
             [
               (* Put the element in %r13 *)
               (sp "movq %s,%%r14" expr1_addr_s, "Put the root in of the array memory");
               (sp "movq %s,%%r15" expr2_addr_s, "Put the index in of the array memory");
               ("movq (%r14, %r15, 8), %r13", "Put the element in memory")],
             [
               (* Get the value in %r13 and put it in the array *)
               (sp "movq %s,%%r14" expr1_addr_s, "Put the root in of the array memory");
               (sp "movq %s,%%r15" expr2_addr_s, "Put the index in of the array memory");
               ("movq %r13, (%r14, %r15, 8)","Put the memory in the element")
             ]))
        end
    end
  | CMP (op, (loc1,expr1), (loc2, expr2)) ->
    begin
      (** CMP(cop,e,e') vaut e<e', e<=e', ou e==e' *)
      (* On évalue les expressions *)
      let (env2, expr2_addr,_) =
        try
          asm_block_of_expr func expr2 env func_env asm_bloc loc2
        with Uncomplete_compilation_error er -> compile_raise loc2 er
      in
      let (env3, expr1_addr,_) =
        try
          asm_block_of_expr func expr1 env2 func_env asm_bloc loc1
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      (* Ajoute adresse temporaire pour le resultat *)
      let new_env = env3#add asm_bloc "" in
      let result_addr = new_env#get "" in
      match op with
        C_LT ->
        begin
          asm_lt func asm_bloc expr1_addr expr2_addr result_addr;
          (new_env, result_addr, None)
        end
      | C_LE ->
        begin
          asm_le func asm_bloc expr1_addr expr2_addr result_addr;
          (new_env, result_addr, None)
        end
      | C_EQ ->
        begin
          asm_eq func asm_bloc expr1_addr expr2_addr result_addr;
          (new_env, result_addr, None)
        end
    end
  | EIF ((loc1,expr1), (loc2, expr2), (loc3,expr3)) ->
    begin
      (** EIF(e1,e2,e3) est e1?e2:e3 *)
      (* We evaluate the expressions *)
      let (env1, expr1_addr,_) =
        try
          asm_block_of_expr func expr1 env func_env asm_bloc loc1
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      (* Add a temp address to put the result in *)
      let new_env = env1#add asm_bloc "" in
      let return_addr = new_env#get "" in
      (* Give a name to the three resulting blocs *)
      let name_cond1 = genlab func in
      let name_cond2 = genlab func in
      let name_after = genlab func in
      (* Add jump condition in the main bloc *)
      asm_eif_part1 asm_bloc expr1_addr name_cond1 name_cond2;
      (* Create the three others blocs *)
      let cond1_asm_bloc =
        new asm_block name_cond1
          [("",sp "If the ternary condition in %s is respected" asm_bloc#get_block_name)] [] [] in
      let cond2_asm_bloc =
        new asm_block name_cond2
          [("",sp "If the ternary condition in %s is *not* respected" asm_bloc#get_block_name)] [] [] in
      let after_asm_bloc =
        new asm_block name_after
          [("",sp "This bloc is the one run after the ternary condition in bloc %s." asm_bloc#get_block_name)] [] [] in
      (* First one *)
      let (_,cond1_return,_) =
        try
          asm_block_of_expr func expr2 new_env func_env cond1_asm_bloc loc2
        with Uncomplete_compilation_error er -> compile_raise loc2 er
      in
      cond1_asm_bloc#add_ending_content_d
        ((mv_gen (string_of_address cond1_return) "%r13")
         @ (mv_gen "%r13" (string_of_address return_addr))
         @ (asm_jmp (Global name_after)));
      (* Second one *)
      let (_,cond2_return,_) =
        try
          asm_block_of_expr func expr3 new_env func_env cond2_asm_bloc loc3
        with Uncomplete_compilation_error er -> compile_raise loc3 er
      in
      cond2_asm_bloc#add_ending_content_d
        ((mv_gen (string_of_address cond2_return) "%r13")
         @ (mv_gen "%r13" (string_of_address return_addr))
         @ (asm_jmp (Global name_after)));
      (* Plug the new after bloc to these conditions *)
      asm_bloc#add_block_after cond1_asm_bloc;
      asm_bloc#add_block_after cond2_asm_bloc;
      ignore(asm_bloc#fork_block after_asm_bloc);
      (new_env, return_addr, None)
    end
  | ESEQ l_expr ->
    begin
      (** e1, ..., en [sequence, analogue a e1;e2 au niveau code];
          si n=0, represente skip. *)
      List.fold_left
        ( fun (env1,_,_) (loc1, expr1) ->
            try
              asm_block_of_expr func expr1 env1 func_env asm_bloc loc1
            with Uncomplete_compilation_error er -> compile_raise loc1 er
        )
        (env, Local_bp 0, None)
        l_expr
    end

(*
func is "" if we are not in a function, else it contains the name of
the function
(env, func_env) *)
let rec asm_block_of_code func (code : code) env func_env asm_bloc =
  (* Declaration of variables/function.
     It returns a new env, func_env, and the asm_bloc is modified. *)
  let asm_block_of_var_declaration var_decl env func_env asm_bloc =
    match var_decl with
      CDECL (loc, var_name) ->
      begin
        (* If we are in the global variable part *)
        if func = "" then
          begin
            let var_asm =
              new asm_block var_name
                [(".long 0", " (Set a default value of 0)")]
                (* XXX devrait être un quad... mais comme tu alignes
                 * sur 8 octets, je ne vois pas comment exploiter ça pour obtenir
                 * un test incorrect! *)
                []
                []
            in
            var_asm#set_before_anything
              [(".data", sp "Define the global variable %s" var_name);
               (".align 8","")];
            asm_bloc#add_block_before var_asm;
            (env#addf var_name (Global var_name), func_env)
          end
        else (* The variable is local *)
          (env#add asm_bloc var_name, func_env)
      end
    | CFUN (loc, func_name, var_decl_l, (next_loc, code)) ->
      if func <> "" then
        compile_raise loc (sp "The function \"%s\" want to be declared inside the function %s... Pretty strange !" func_name func)
      else if (try ignore(env#get func_name); true with _ -> false) then
        compile_raise loc (sp "The function %s has the same name as the global variable %s." func_name func_name)
      else if (try ignore(func_env#get func_name); true with _ -> false) then
        compile_raise loc (sp "The function %s has already been defined" func_name)
      else
        begin
          (* Create a new label for the function *)
          let lbl = func_name in
          (* Add it in the list of functions *)
          let next_func_env = func_env#addf func_name (Global lbl) in
          (* Create the content of the bloc *)
          let new_asm_bloc = new asm_block lbl [] [] [] in
          new_asm_bloc#set_before_anything
            [(".text", sp "Be sure that the function %s isn't in the .data section" func_name)];
          try
            let (first_args, last_args) = take_list 6 var_decl_l in
            let string_of_arg = List.map (function CDECL (_,v) -> v | CFUN _ -> raise (Uncomplete_compilation_error "A function shouldn't be present in the argument of a function")) in
            let first_args_s = string_of_arg first_args in
            let last_args_s = string_of_arg last_args in
            (* Save the rbp onto the stack *)
            new_asm_bloc#add_content_d
              [("pushq %rbp", "Save the %rbp on the stack");
               ("movq %rsp,%rbp", "Save the old %rsp in %rbp")] [];
            (* Put all the arguments onto the stack *)
            (* Put the lasts arguments *)
            let (env1,_) =
              List.fold_left
                (fun (tmp_env,pos) var_name ->
                   (tmp_env#addf var_name (Local_bp pos), pos+1))
                (env,2)
                last_args_s in
            (* Put the firsts arguments *)
            let (env2,_) =
              List.fold_left
                (fun (tmp_env,n_arg) var_name ->
                   let n_env = tmp_env#add new_asm_bloc var_name in
                   new_asm_bloc#add_content_d (mv_arg_into_stack n_arg (n_env#get var_name)) [];
                   (n_env,n_arg+1))
                (env1,1)
                first_args_s in
            (* Get the exc state from the global values *)
            let env2 = env2#add new_asm_bloc ".last_try_rsp" in
            let env2 = env2#add new_asm_bloc ".last_try_rip" in
            let tmp0 = (env2#get ".__rsp_before_call__") in
            let tmp = string_of_address tmp0 in
            new_asm_bloc#add_content_d
              [(sp "movq %s,%%r8" tmp,"Get the exc parameters from the global variables");
               (sp "movq %%r8,%s" (env2#gets ".last_try_rsp")," (rsp)");
               (sp "movq %s,%%r8" (env2#gets ".__rip_before_call__"),"");
               (sp "movq %%r8,%s" (env2#gets ".last_try_rip")," (rip)")]
              [];
            (* Eval the inside of the function *)
            let (end_env,last_func_env) =
              asm_block_of_code func_name code env2 next_func_env new_asm_bloc
            in
            (* Add it to the main asm block *)
            asm_bloc#add_block_after new_asm_bloc;
            (* Forget everything about the new environment *)
            (env,last_func_env)
          with Uncomplete_compilation_error er -> compile_raise next_loc er
        end
  in    



  (* Main part *)
  match code with
    CBLOCK (var_decl_l, loc_code_l) ->
    begin
      (* We look for the declaration (function or variable) *)
      let (env4, func_env4) =
        List.fold_left (fun (env2,func_env2) var_decl ->
            let (env3, func_env3) = asm_block_of_var_declaration var_decl env2 func_env2 asm_bloc in
            (env3, func_env3)
          ) (env, func_env) var_decl_l in
      (* We run the loc_code_l *)
      ignore(List.fold_left (fun (env6,func_env6) (loc,code) ->
          try
            asm_block_of_code func code env6 func_env6 asm_bloc
          with Uncomplete_compilation_error er -> compile_raise loc er
        )
        (env4, func_env4)
        loc_code_l);
      (env,func_env)
    end
  | CEXPR (loc,expr) ->
    begin
      (** une expression e; vue comme instruction. *)
      let (env2, _,_) = asm_block_of_expr func expr env func_env asm_bloc loc in
      (env2, func_env)
    end
  | CIF ((loc1, expr1), (loc2, code2), (loc3, code3)) ->
    begin
      let (env2, return_addr,_) =
        try
          asm_bloc#add_content_d [("","DEBUT CONDITION")] [];
          asm_block_of_expr func expr1 env func_env asm_bloc loc1
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      (* --- Define new asm blocks names --- *)
      (* A bloc that will be run if the condition is ok *)
      let cond1_bloc_name = genlab func in
      (* A bloc that will be run if the condition is not ok *)
      let cond2_bloc_name = genlab func in
      (* A bloc that contains the code after the if condition *)
      let after_asm_bloc_name = genlab func in
      (* --- Define the new asm blocks --- *)
      (* First run the condition *)
      asm_condition_part1 func asm_bloc return_addr cond1_bloc_name cond2_bloc_name;
      (* --- Then build the two asm codes ---
         They must come back in after_asm_asm bloc at the end *)
      let asm_bloc_cond1 =
        new asm_block cond1_bloc_name
          [("",sp "Jump here if the condition at the end of the block %s is respected" asm_bloc#get_block_name)]
          (asm_jmp (Global after_asm_bloc_name))
          []
      in
      (* The return env isn't interesting since the intern variables
         shouldn't be available after the run of the condition  *)
      let (_,_) = asm_block_of_code func code2 env2 func_env asm_bloc_cond1 in
      let asm_bloc_cond2 =
        new asm_block cond2_bloc_name
          [("",sp "Jump here if the condition at the end of the block %s is *not* respected" asm_bloc#get_block_name)]
          (asm_jmp (Global after_asm_bloc_name))
          []
      in
      let (_,_) = asm_block_of_code func code3 env2 func_env asm_bloc_cond2 in
      asm_bloc#add_block_after asm_bloc_cond1;
      asm_bloc#add_block_after asm_bloc_cond2;

      (* Build the after bloc *)
      let asm_bloc_after =
        new asm_block after_asm_bloc_name
          [("",sp "Bloc that is run after the run of the content of the condition at the end of the block %s." asm_bloc#get_block_name)]
          []
          []
      in
      ignore(asm_bloc#fork_block asm_bloc_after);
      (env, func_env)
    end
  | CWHILE ((loc1, expr1), (loc2, code2)) ->
    begin
      (** while (e) c1; *)
      (* Idea : we create a new bloc that will contain the condition of jump *)
      (* Create the names *)
      let cond_asm_name = genlab func in
      let main_asm_name = genlab func in (* Bloc that contains code2 *)
      let after_asm_name = genlab func in (* Bloc that will be run when the loop end *)
      (**** Create the blocs ****)
      (* The current bloc *)
      asm_bloc#add_content_d
        (asm_jmp (Global cond_asm_name))
        [];
      (* The conditionnal bloc *)
      let cond_asm_bloc =
        new asm_block
          cond_asm_name
          [("",sp "Bloc de condition de la boucle while initialisé dans le bloc %s" asm_bloc#get_block_name)]
          [(sp "je %s" after_asm_name, " (Avoid the inside code if the condition is false");
           (sp "jmp %s" main_asm_name, " (If the condition is true then go in main_asm_name")]
          [] in
      let (env2, return_addr,_) =
        try
          asm_block_of_expr func expr1 env func_env cond_asm_bloc loc1
        with Uncomplete_compilation_error er -> compile_raise loc1 er
      in
      asm_while_part1 cond_asm_bloc return_addr main_asm_name after_asm_name;
      (* The main code bloc *)
      let main_asm_bloc =
        new asm_block
          main_asm_name
          [("",sp "Bloc principal de while dont la condition est dans le bloc %s" cond_asm_name)]
          (asm_jmp (Global cond_asm_name))
          [] in
      let (_, _) =
        try
          asm_block_of_code func code2 env2 func_env main_asm_bloc
        with Uncomplete_compilation_error er -> compile_raise loc2 er
      in
      (* The after code bloc *)
      let after_asm_bloc =
        new asm_block after_asm_name
          [("",sp "Bloc that is run after the run of the content of the condition at the end of the block %s." asm_bloc#get_block_name)]
          []
          []
      in
      (* Add the blocs in the old bloc *)
      asm_bloc#add_block_after cond_asm_bloc;
      asm_bloc#add_block_after main_asm_bloc;
      (* Change the current writting bloc *)
      ignore(asm_bloc#fork_block after_asm_bloc);
      (env, func_env)
    end
  | CRETURN loc_expr_opt ->
    begin (** return; ou return (e); *)
      (match loc_expr_opt with
        None -> (* Anything could be in %rax *) ()
      | Some (loc,expr) ->
        begin
          let (env1, return_address,_) =
            asm_block_of_expr func expr env func_env asm_bloc loc in
          let return_s = string_of_address return_address in
          asm_bloc#add_content_d
            [(sp "movq %s,%%rax" return_s, "Mets la valeur de retour de la fonction dans %rax")]
            [];
        end);
      asm_bloc#add_content_d
        [(sp "movq $0,%s" (env#gets ".__exception_name__")
         ,"Put back a 0 before returning");
         ("movq %rbp,%rsp","Remettre le pointeur de pile à l'endroit où il était lors de l'appel de la fonction");
         ("popq %rbp","On remets le base pointer au début");
         ("ret","On retourne à l'instruction assembleur sauvegardée par call")]
        [];
      (env,func_env)
    end
  | CTHROW (exc_name, (loc,expr)) ->
    begin
      let (_, return_address,_) =
        asm_block_of_expr func expr env func_env asm_bloc loc in
      let exc_int = glob_exc#int_of_name exc_name in
      (* Change the name *)
      asm_bloc#add_content_d
        [(sp "movq $%d,%s"
            exc_int
            (env#gets ".__exception_name__")
         ,sp"Exception %s raised (nb : %d)" exc_name exc_int)]
        [];
      (* Change the value *)
      asm_bloc#add_content_d
        ( ("",sp "  Change the value of the exception %s" exc_name)
          :: (mv_variable return_address (env#get ".__exception_value__")))
        [];
      (* Restore last try state *)
      asm_bloc#add_content_d
        [(sp "movq %s,%%rsp" (env#gets ".last_try_rsp"),"  Restore the last try state (rsp)");
         (sp "movq %s,%%r8" (env#gets ".last_try_rip"),"  Restore the last try state (rip) by putting it in r8");
         (sp "jmp *%%r8" , "  Restore the last try state (rip)")]
        [];
      (env,func_env)
    end
  | CTRY ((loc,code), exc_lst, code_finally_opt) ->
    begin
      try
        (* Name of blocs *)
        let asm_bloc_save_trick = (genlab func) ^ "_save_trick" in
        let asm_bloc_name_inside_try = (genlab func) ^ "_inside_try" in
        let asm_bloc_name_finally = (genlab func) ^ "_finally" in
        let asm_bloc_name_finally_return = (genlab func) ^ "_finally_return" in
        let asm_bloc_name_finally_exc = (genlab func) ^ "_finally_exc" in
        (* Usefull for function call in the try bloc*)
        let env_inside_try = env#add asm_bloc "--retour_bool--" in
        let env_inside_try = env_inside_try#add asm_bloc "--retour_val--" in
        (* Save the current state. Since it's not possible to easily
           get %rip, I create a label, call it and pop the value after *)
        let env_inside_try = env_inside_try#add asm_bloc ".last_try_rsp" in
        let env_inside_try = env_inside_try#add asm_bloc ".last_try_rip" in
        asm_bloc#add_content_d
          [(sp "movq %%rsp, %s" (env_inside_try#gets ".last_try_rsp")
           ,"  Save the current stack position (rsp)");
           (sp "call %s" asm_bloc_save_trick,"  Trick to get %rip")]
          [];
        let asm_bloc_after_save =
          new asm_block asm_bloc_save_trick
            [("pop %r8","  Save the current instruction pointer (rip)");
             (sp "movq %%r8, %s" (env_inside_try#gets ".last_try_rip"),"  (rip)");
             (sp "movq %s,%%r8" (env#gets ".__exception_name__"),"Check of the value of the exception");
             ("cmp $0,%r8","");
             (sp "je %s" asm_bloc_name_inside_try, "Come back in the good place")]
            []
            []
        in
        ignore(asm_bloc#fork_block asm_bloc_after_save);
        (* Add an exception *)
        List.iter
          (fun (name_exc, var_exc, (loc_exc,code_exc)) ->
             let asm_bloc_name_curr_exc = sp "%s_catch_exc" (genlab func) in
             let n_exc = glob_exc#int_of_name name_exc in
             (* Jump in the good bloc *)
             asm_bloc#add_content_d
               [
                 (sp "cmp $%d,%%r8" n_exc, sp " (exc %s)" name_exc);
                 (sp "je %s" asm_bloc_name_curr_exc, "")
               ]
               [];
             (* Create this bloc *)
             let asm_bloc_curr_exc =
               new asm_block asm_bloc_name_curr_exc
                 [("",sp "Asm bloc for exception %s" name_exc);
                  (sp "movq $0,%s" (env#gets ".__exception_name__")
                  ,"Réinitialiser à 0 .__exception_name__")]
                 [(sp "jmp %s" asm_bloc_name_finally, "Jump back in finally")]
                 [] in
             (* Save the variable *)
             let env' = env#add asm_bloc_curr_exc var_exc in
             asm_bloc_curr_exc#add_content_d
               [(sp "movq %s,%%r8" (env'#gets ".__exception_value__"),"Save the value of the exc in the variable");
                (sp "movq %%r8, %s" (env'#gets var_exc), "")]
               [];
             (* Write the code for the exc *)
             (try
                ignore(asm_block_of_code func code_exc env' func_env asm_bloc_curr_exc)
              with Uncomplete_compilation_error er -> compile_raise loc_exc er);
             (* Add the bloc to asm_bloc *)
             asm_bloc#add_block asm_bloc_curr_exc;
             ()
          )
          exc_lst;
        asm_bloc#add_content_d
          [(sp "jmp %s" asm_bloc_name_finally, "Jump back in finally (last choice)")]
          [];
        (* Build the bloc inside the try *)
        let asm_bloc_inside_try =
          new asm_block asm_bloc_name_inside_try
            [("","Inside the try bloc...")]
            [(sp "jmp %s" asm_bloc_name_finally," Jump in the finally bloc...")]
            []
        in
        asm_bloc#add_block asm_bloc_inside_try;
        let (_,_) = asm_block_of_code func code env_inside_try func_env asm_bloc_inside_try in
        (* Build the bloc finally return *)
        let asm_bloc_finally_return =
          new asm_block asm_bloc_name_finally_return
            [("","Inside the finally return bloc");
             (sp "movq %s,%%rax" (env_inside_try#gets "--retour_val--"),"Save the returned value in rax");
             ("movq %rbp,%rsp","Remettre le pointeur de pile à l'endroit où il était lors de l'appel de la fonction");
             ("popq %rbp","On remets le base pointer au début");
             ("ret","On retourne à l'instruction assembleur sauvegardée par call")]
            []
            []
        in
        asm_bloc#add_block asm_bloc_finally_return;
        (* Build the bloc finally exception (used if an exception is not catched) *)
        let asm_bloc_finally_exc =
          new asm_block asm_bloc_name_finally_exc
            [("","Inside the finally exception bloc (when an exception is not catched, we send it again)");
             (sp "movq %s,%%r8" (env#gets ".last_try_rip"),"  Restore the last try state (rip) by putting it in r8");
             (sp "movq %s,%%rsp" (env#gets ".last_try_rsp"),"  Restore the last try state (rsp)");
             (sp "jmp *%%r8" , "  Restore the last try state (rip)")]
            []
            []
        in
        asm_bloc#add_block asm_bloc_finally_exc;
        (* Build the bloc finally *)
        let (loc_finally, code_finally) = match code_finally_opt with
            None -> (loc, CBLOCK ([], []))
          | Some c -> c in 
        let asm_bloc_finally =
          new asm_block asm_bloc_name_finally
            [("","Inside the finally bloc...")]
            []
            []
        in
        (try 
           ignore(asm_block_of_code func code_finally env func_env asm_bloc_finally)
         with Uncomplete_compilation_error er -> compile_raise loc_finally er);
        asm_bloc_finally#add_content_d
          [(* Test function return *)
            (sp "movq %s,%%r8" (env_inside_try#gets "--retour_bool--"),"Jump in the finally return bloc if a return has been raised");
            (sp "cmp $1,%%r8","");
            (sp "je %s" asm_bloc_name_finally_return, "");
            (* If .__exception_name__ is not null, raise again the exception *)
            (sp "movq %s,%%r8" (env_inside_try#gets ".__exception_name__"),"Jump in the finally return bloc if a return has been raised");
            ("cmpq $0,%r8","");
            (sp "jg %s" asm_bloc_name_finally_exc ,"")]
          [];
        (* asm_bloc_finally#add_content_d *)
        (*   [ (sp "movq $0,%s" (env#gets ".__exception_name__") *)
        (*     ,"Réinitialiser à 0 .__exception_name__")] *)
        (*   []; *)
        ignore(asm_bloc#fork_block asm_bloc_finally);
        (env,func_env)
      with Uncomplete_compilation_error er -> compile_raise loc er
    end

(* Return env *)
let add_global_var asm_bloc env (var_name,value) =
  let var_asm = new asm_block var_name [(sp ".long %d" value, "")] [] [] in
  var_asm#set_before_anything
    [(".data", sp "Define the global variable %s" var_name);
     (".align 8","")];
  asm_bloc#add_block_before var_asm;
  env#addf var_name (Global var_name)

(* ============================================= *)
(* === Add return at the end of all function === *)
(* ============================================= *)

let add_return decl_list =
  List.map
    (function CDECL (x,y) -> CDECL (x,y)
            | CFUN (loc,func_name,var_decl_l, (next_loc, code))
              -> CFUN (loc, func_name, var_decl_l,
                       (next_loc,
                        CBLOCK ([],
                                [(next_loc, code);
                                 (next_loc, CRETURN None)]))))
    decl_list

let rec replace_return_with_exc var_decl0 =
  let m = List.map in
  let rec replace_in_code (loc0,code0) =
    begin
      match code0 with
        CBLOCK (var_decl_l, loc_code_l) ->
        (loc0,
         CBLOCK (m replace_return_with_exc var_decl_l
                ,m replace_in_code loc_code_l))
      | CEXPR _ -> (loc0, code0)
      | CIF (loc_expr,loc_code1,loc_code2) ->
        (loc0
        ,CIF ( loc_expr
             , replace_in_code loc_code1
             , replace_in_code loc_code2))
      | CWHILE (loc_expr,loc_code) ->
        ( loc0
        , CWHILE ( loc_expr
                 , replace_in_code loc_code))
      | CRETURN (loc_expr_opt) ->
        begin
          match loc_expr_opt with
            None -> (loc0, CTHROW (".return", (loc0,CST 1)))
          | Some (loc,expr) -> (loc0, CTHROW (".return", (loc0,expr)))
        end
      | CTHROW _ -> (loc0, code0)
      | CTRY (loc_code, name_var_code_l, loc_code_opt) ->
        begin
          ( loc0
          , CTRY (replace_in_code loc_code
                 , m (fun (name,var,loc_code) -> (name,var,replace_in_code loc_code))
                     name_var_code_l
                 , match loc_code_opt with
                   None -> None
                 | Some loc_code -> Some (replace_in_code loc_code)))
        end        
    end
  in
  let rec replace_in_var_decl var_decl = match var_decl with
      CDECL (_,_) -> var_decl
    | CFUN (loc0,s,var_decl_l_1,(loc1,code1)) ->
      begin
        CFUN (loc0
             ,s
             ,m replace_in_var_decl var_decl_l_1
             ,(loc1
              , CTRY ( replace_in_code (loc1,code1)
                     , [(".return", ".return_val", (loc1,CRETURN (Some (loc1,VAR ".return_val"))))]
                     , None)))
      end
  in
  replace_in_var_decl var_decl0


(* =================== *)
(* === Compilation === *)
(* =================== *)
let compile out decl_list =
  pr "Début compilation\n%!";
  let env = new env Str_map.empty (-1) (Local_bp 0) in
  let asm_bloc =  new asm_block ".EVERYTHING" [] [] [] in
  asm_bloc#set_before_anything [(".global main", "Usefull to make main available for everyone")];
  let env2 = List.fold_left (add_global_var asm_bloc) env
      [("NULL",0);
       (* These global variables are used to give value related to exceptions
          at a function call.
          I will use == for global variables and -- for local variables.
          Since these symbols are forbidden in real variable name there
          won't be any conflict.
       *)
       (".__rsp_before_call__",0);
       (".__rip_before_call__",0);
       (* These global variables are used to store the current exception *)
       (".__exception_name__",0);
       (".__exception_value__",0);
      ]
  in
  let decl_list2 = add_return decl_list in
  let decl_list3 = List.map replace_return_with_exc decl_list2 in
  (* Main run *)
  try
    ignore(asm_block_of_code "" (CBLOCK (decl_list3, [])) env2 env asm_bloc);
    Printf.fprintf out "%s" asm_bloc#get_content_string;
    pr "\027[1;32mThe compilation in asm ended with success\027[0;m\n%!"
  with Compilation_error str -> (pr "\027[1;31mError\027[0;m : %s\n%!" str; exit 1)
     | Uncomplete_compilation_error str -> (pr "\027[1;31mError\027[0;m : %s\n%!" str; exit 1)
