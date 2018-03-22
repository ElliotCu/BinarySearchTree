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


let rec is_bst t = match t with
    Empty -> true
   |Bin((x, m), lr, rr) -> match (lr, rr) with
                             Empty, Empty -> true
                            |Bin((x1, m1), lr1, rr1), Bin((x2, m2), lr2, rr2) -> if (x1 > x || x2 < x) then false
                                                                                 else is_bst lr && is_bst rr
                            |Bin((x1, m1), lr1, rr1), Empty -> if (x1 > x ) then false
                                                           else is_bst lr && is_bst rr
                            |Empty, Bin((x2, m2), lr2, rr2) -> if (x2 < x) then false
                                                           else is_bst lr && is_bst rr;;
