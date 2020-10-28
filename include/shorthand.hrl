-ifndef(SHORTHAND_HRL).
-define(SHORTHAND_HRL, true).

-define(a2l, atom_to_list).
-define(l2a, list_to_atom).
-define(l2ea, list_to_existing_atom).
-define(b2l, binary_to_list).
-define(l2b, list_to_binary).
-define(i2l, integer_to_list).
-define(l2i, list_to_integer).
-define(l2f, list_to_float).
-define(f2l, float_to_list).
-define(l2t, list_to_tuple).
-define(t2l, tuple_to_list).
-define(t2b, term_to_binary).
-define(b2t, binary_to_term).
-define(a2b, atom_to_binary).
-define(i2b, integer_to_binary).
-define(b2a, binary_to_atom).
-define(b2i, binary_to_integer).
-define(b2f, binary_to_float).
-define(iof(Format, Args), io:format(Format, Args)).

-endif.
