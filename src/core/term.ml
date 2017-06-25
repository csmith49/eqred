open CCFun

(* terms are rose trees with a different constructor for leaves *)
type 'a t =
    | Leaf of 'a
    | Node of 'a * ('a t) list
type 'a term = 'a t

(* terms are instances of functors *)
let rec fmap (f : 'a -> 'b) : 'a t -> 'b t = function
    | Leaf x -> Leaf (f x)
    | Node (x, xs) -> Node (f x, CCList.map (fmap f) xs)
let (<$>) = fmap

(* sometimes there will be some accessing needed *)
let value : 'a t -> 'a = function
    | Leaf x -> x
    | Node (x, _) -> x

(* and we can wrap making so we choose the right constructor *)
let make (x : 'a) (xs : ('a t) list) : 'a t = match xs with
    | [] -> Leaf x
    | _ -> Node (x, xs)

(* and for ease of maneuvering, we'll use zippers *)
module Zipper = struct
    (* of course, maneuvering will fail often *)
    open CCOpt.Infix

    (* to represent the rest of the term, we need branches *)
    module Branch = struct
        (* note: left list is backwards *)
        type 'a t = 'a * ('a term) list * ('a term) list

        (* basic accessing *)
        let value : 'a t -> 'a = function (x, _, _) -> x
        let left : 'a t -> ('a term) list = function (_, l, _) -> l
        let right : 'a t -> ('a term) list = function (_, _, r) -> r

        (* we need to be able to plug terms back in *)
        (* left list has to be reversed before being concatted *)
        let collapse (xt : 'a term) (b : 'a t) : 'a term =
            let x = value b in
            let xs = CCList.rev_append (left b) (xt :: (right b)) in
                make x xs

        (* helpers for movement functions in Zipper *)
        let push_left (xt : 'a term) (b : 'a t) : ('a term * 'a t) option = match b with
            | (yt, l :: ls, rs) ->
                let b' = (yt, ls, xt :: rs) in
                    Some (l, b')
            | _ -> None
        let push_right (xt : 'a term) (b : 'a t) : ('a term * 'a t) option = match b with
            | (yt, ls, r :: rs) ->
                let b' = (yt, xt :: ls, rs) in
                    Some (r, b')
            | _ -> None

        (* and we'll make branches from terms *)
        let rec push_down (xt : 'a term) : ('a term * 'a t) option = match xt with
            | Leaf _ -> None
            | Node (_, []) -> None
            | Node (x, y :: ys) ->
                let b' = (x, [], ys) in
                    Some (y, b')
    end

    (* so our zipper.t maintains current content and the rest of the term in branch form *)
    type 'a t = ('a term) * ('a Branch.t) list

    (* basic zipper construction *)
    let of_term (xt : 'a term) : 'a t = (xt, [])

    (* value-level getters and setters *)
    let get : 'a t -> 'a = function
        (xt, _) -> value xt
    let set (x : 'a) : 'a t -> 'a t =
        CCPair.map1 (fun lt ->
            match lt with
                | Leaf _ -> Leaf x
                | Node (_, xs) -> Node (x, xs))

    (* term-level getters and setters *)
    let get_term : 'a t -> 'a term = fst
    let set_term (xt : 'a term) : 'a t -> 'a t = CCPair.map1 (fun lt -> xt)

    (* and now we can encode basic motion *)
    let up : 'a t -> 'a t option = function
        | (xt, b :: bs) -> Some (Branch.collapse xt b, bs)
        | _ -> None
    let left : 'a t -> 'a t option = function
        | (xt, b :: bs) -> begin match Branch.push_left xt b with
            | Some (yt, b') -> Some (yt, b' :: bs)
            | None -> None
        end
        | _ -> None
    let right : 'a t -> 'a t option = function
        | (xt, b :: bs) -> begin match Branch.push_right xt b with
            | Some (yt, b') -> Some (yt, b' :: bs)
            | None -> None
        end
        | _ -> None
    let down : 'a t -> 'a t option = function
        (xt, bs) -> match Branch.push_down xt with
            | None -> None
            | Some (yt, b) -> Some (yt, b :: bs)

    (* and then fancy motion *)
    let rec next (z : 'a t) : ('a t) option = (right z) <+> (up z) >>= next
    let preorder (z : 'a t) : ('a t) option = (down z) <+> (next z)
    let rec preorder_until (p : 'a -> bool) (z : 'a t) : ('a t) option =
        (CCOpt.if_ (p % get) z) <+> (preorder z) >>= (preorder_until p)
end
