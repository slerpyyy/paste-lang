# paste - standard library

# Stack manipulation:
# dup  ( a -> a a )
# pop  ( a -> )
# xch  ( a b -> b a )
# over ( a b -> a b a )
;{ 1 copy } dup =
;{ ;_ = } pop =
;{ ;__xch_1 = ;__xch_2 = __xch_1 __xch_2 } xch =
;{ 2 copy pop } over =

# Unary operators
;{ 1 + } ++ =
;{ 1 xch - } -- =
;{ dup * } sq =
;{ 1 - } ! =

# Compareson operators
;{ xch < } > =
;{ == ! } != =
;{ < ! } <= =
;{ > ! } >= =

# while loop
#;{
#    ;__while_stmt = ;
#    { __while_stmt ;__while_stmt while } if
#} while =

# ternary operator
#;{
#    ;__ter_then =
#    ;__ter_else =
#    ;__ter_cond =
#    __ter_cond   ;__ter_then if
#    __ter_cond ! ;__ter_else if
#} ? =
