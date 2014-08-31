variable V; array set V {}

proc eincr {} {
    upvar #0 V(a) a
    incr a
}

proc aincr {} {
    upvar #0 V V
    incr V(a)
}

proc varmod {args} {
    puts stderr "varmod: $args"
}

::trace add variable V(a) {write unset} [list varmod element]
::trace add variable V {write unset} [list varmod array]

puts stderr "global set"
set V(a) 1	;# triggers array and element traces

puts stderr "upvar array element set"
aincr		;# triggers array and element traces

puts stderr "upvar element set"
eincr		;# triggers only element trace
