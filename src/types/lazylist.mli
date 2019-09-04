type 'a t = 
  | Cons of 'a * (unit -> 'a t)
  | Empty
type 'a gen_t = unit-> 'a t