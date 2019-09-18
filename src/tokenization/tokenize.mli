type automata_state
val run:CharWithCoords.t Lazylist.gen_t->TokenWithCoords.t Lazylist.gen_t
val check:(TokenWithCoords.t Lazylist.gen_t)->TokenWithCoords.t Lazylist.gen_t