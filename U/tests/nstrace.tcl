namespace eval X {
    variable V; array set V {}
}

proc test {str} {
    namespace upvar X V(a) a
    if {[info exists a]} {
	puts stderr "$str: a == $a"
    } else {
	puts stderr "$str: a doesn't exist"
    }
}

proc tincr {} {
    namespace upvar X V(a) a
    incr a
}

proc varmod {args} {
    puts stderr "varmod: $args"
}

test initial
set X::V(a) 1
test post-set

unset X::V(a)
test post-unset

::trace add variable X::V(a) {write unset} [list varmod element X::V]
::trace add variable X::V {write unset} [list varmod array {}]
test trace
set X::V(a) 2
test post-trace
tincr
test post-tincr
