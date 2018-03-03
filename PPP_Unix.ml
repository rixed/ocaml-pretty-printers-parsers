open PPP

(* Some stdlib types, which must be implemented in a way that does not depend
 * on the implementation module so that we can use the ppx preprocessor
 * without depending on the extra libs (here: unix): *)

type inet_addr = Unix.inet_addr
let inet_addr : Unix.inet_addr t = 
  string >>: Unix.(string_of_inet_addr, inet_addr_of_string)
