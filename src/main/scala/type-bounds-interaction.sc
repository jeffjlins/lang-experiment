def boundedAnyCall[T <: Any](t: T): T = t
def boundedAnyRefCall[T <: Any](t: T): T = t

def nonBoundedCallToAny[T](t: T): T = boundedAnyCall(t)
// this does not compile because T is implicitly bound by the top of the class hierarchy (Any) rather than AnyRef
//def nonBoundedCallToAnyRef[T](t: T): T = boundedAnyRefCall(t)

nonBoundedCallToAny("R")

// For java calls, you have to use AnyRef bounds because of null ?????