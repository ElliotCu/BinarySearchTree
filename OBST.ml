type 'a tree = Empty | Bin of 'a * 'a tree * 'a tree


let rec bst_search t x = match t with
    Empty -> 0
    |Bin((c, m), lr, rr) -> if c = x then m 
                            else if c > x then bst_search lr x
                            else bst_search rr x;;
                            
let rec bst_insert t x = match t with
    Empty -> Bin((x, 1), Empty, Empty)
    |Bin((c, m), lr, rr) ->  if c = x then Bin((c, m+1), lr, rr)
                        else if x < c then Bin((c, m), (bst_insert lr x), rr)
                        else Bin((c, m), lr, (bst_insert rr x)) ;;


let rec bst_from_list l = let rec aux l t = match l with
                                            [] -> t
                                            |h::x -> aux x ( bst_insert t h)
                                in aux l Empty;;

let rec repeat x n = if n <= 0 then [] else x::repeat x (n-1);;
  
let rec list_from_bst t = match t with
    Empty -> []
   |Bin((x, m), lr, rr) -> list_from_bst lr @ repeat x m @ list_from_bst rr;;
 

let rec min t = match t with
    Empty -> failwith"Invalid Parameters!"
   |Bin((x, m), lr, rr) -> if lr = Empty then x
                           else min lr;;

let rec delete t x = match t with
    Empty -> failwith "Empty Tree"
   |Bin((c, m), lr, rr) -> if c = x then
                                    if lr = Empty && rr = Empty then Empty
                                    else if lr = Empty then rr
                                    else if rr = Empty then lr
                           else Bin((c,m), lr, delete(min rr) rr)
                                else if c > x then Bin((c, m), delete x lr, rr)
                               else Bin((c, m), lr, delete x rr);;


let rec aux t min max=match t with
       |Empty->true
       |Bin((c,m),l,r)-> if (c < min || c > max) then false else (aux l min (c-1)) && (aux r (c+1) max);;
 
let is_bst t= aux t min_int max_int;;


let rec max_in_tree t=
  match t with
  |Empty->(0,0,Empty)
  |Bin((x,m),l,Empty)->(x,m,l)
  |Bin((x,m),l,r)->let (v,e,a)= max_in_tree r in (v,e,Bin((x,m),l,a))

let rec bst_delete t x=
  match t with
    Empty->Empty
  |Bin((y,m),Empty,r) when y=x ->if m=1 then r else Bin((y,m-1),Empty, r)
  |Bin((y,m), l, r) when y=x-> if m=1 then let (v,e,a) = max_in_tree  l in Bin((v,e), a, r) else Bin((y,m-1), l, r)
  |Bin((y,m), l, r)->if x<y then Bin((y,m),bst_delete l x, r) else Bin((y,m), l,bst_delete r x);;
